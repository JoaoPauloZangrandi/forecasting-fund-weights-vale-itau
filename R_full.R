# =============================================================================
# R_full.R
#
# Codigo COMPLETO do projeto num unico arquivo, sem segmentacao.
# Espelha os scripts modulares em R/ (01, 02, ...). A cada passo aprovado,
# este arquivo cresce com o bloco novo. Usa SOMENTE as bases CONS e SH.
#
# Estado atual: Passos 1 a 6, do painel de pesos de VALE3 ate a amostra filtrada
# com n_cotistas > 3 para modelagem inicial.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

# Raiz do projeto: caminhos relativos (data/raw, data/processed) dependem disso.
# Se o working directory nao for a raiz do repo, fixa aqui (override por env).
PROJ_DIR <- Sys.getenv(
  "PROJ_DIR",
  unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
)
if (dir.exists(PROJ_DIR)) setwd(PROJ_DIR)
cat("Working dir:", getwd(), "\n")

DATA_DIR <- Sys.getenv(
  "CVM_DATA_DIR",
  unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"
)

# ---- utilitarios de limpeza -------------------------------------------------

normalize_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")  # tira acentos
  x <- toupper(x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

parse_num_us <- function(x) {           # formato americano (decimal ".")
  x <- as.character(x)
  x <- gsub(",", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

parse_brl <- function(x) {              # formato "R$ x.xxx,xx" (SH.APLICACAO)
  x <- trimws(as.character(x)); x <- gsub("R\\$", "", x)
  x <- gsub("[[:space:]]", "", x); x <- gsub("\\.", "", x); x <- gsub(",", ".", x)
  x[x == ""] <- NA; suppressWarnings(as.numeric(x))
}

parse_date <- function(x) {
  x   <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  fmts <- c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y")
  for (f in fmts) {
    miss <- is.na(out)
    if (!any(miss)) break
    out[miss] <- as.Date(x[miss], format = f)
  }
  out
}

# =============================================================================
# PASSO 1 - carga/sanidade das bases (amostra), so para conferir colunas
# =============================================================================

YEARS_SANITY <- 2016:2021
sanity <- rbindlist(lapply(YEARS_SANITY, function(year) {
  cons_path <- file.path(DATA_DIR, sprintf("cons_%d.csv", year))
  sh_path   <- file.path(DATA_DIR, sprintf("SH_%d.csv", year))
  cons <- fread(cons_path, encoding = "UTF-8", nrows = 1000, showProgress = FALSE)
  sh   <- fread(sh_path,   encoding = "UTF-8", nrows = 1000, showProgress = FALSE)
  data.table(
    year = year, file = c("CONS", "SH"),
    cols_loaded = c(ncol(cons), ncol(sh)),
    columns = c(paste(names(cons), collapse = " | "),
                paste(names(sh),   collapse = " | "))
  )
}))
cat("Sanidade das bases (amostra 1000 linhas/ano):\n")
print(sanity[, .(year, file, cols_loaded)])

# =============================================================================
# PASSO 2 - painel do PESO de VALE3 nos fundos Itau (ano 2016)
# =============================================================================

YEAR        <- 2016L
ASSET_VALE3 <- "VALE ON N1 - VALE3"
ITAU_PATTERNS <- c("ITAU ASSET", "ITAU DTVM", "ITAU UNIBANCO")

# 1) SH: universo de fundos das gestoras Itau
sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", YEAR)),
            encoding = "UTF-8", showProgress = FALSE)
sh[, GESTORA_NORM := normalize_txt(GESTORA)]
itau_regex <- paste(ITAU_PATTERNS, collapse = "|")
sh_itau    <- sh[grepl(itau_regex, GESTORA_NORM)]
sh_itau[, DATA_D := parse_date(DATA)]

cat("\nLinhas SH total:", nrow(sh), "| linhas Itau:", nrow(sh_itau), "\n")
cat("Gestoras Itau encontradas (distintas):\n")
print(sh_itau[, .N, by = .(GESTORA)][order(-N)])
itau_funds <- unique(sh_itau$COD_FUNDO)
cat("\nFundos Itau distintos (COD_FUNDO):", length(itau_funds), "\n\n")

# 2) CONS: posicoes de VALE3
cons <- fread(file.path(DATA_DIR, sprintf("cons_%d.csv", YEAR)),
              encoding = "UTF-8", showProgress = FALSE)
cons[, NOME_ATIVO_TRIM := trimws(Nome_Ativo)]
cons_vale <- cons[NOME_ATIVO_TRIM == ASSET_VALE3]
n_vale_bruto <- nrow(cons_vale)
cons_vale <- unique(cons_vale)  # TRATAMENTO 1: dedup exata (quirk CVM)
cat("Linhas CONS total:", nrow(cons), "| VALE3:", n_vale_bruto,
    "| apos dedup exata:", nrow(cons_vale), "\n")
cons_vale[, PESO      := parse_num_us(`Participação_Ativo`)]
cons_vale[, DATA_COMP := parse_date(`Data_Competência`)]
n_neg     <- cons_vale[PESO < 0, .N]            # TRATAMENTO 2: remove negativos
cons_vale <- cons_vale[is.na(PESO) | PESO >= 0]
cat("Pesos negativos removidos:", n_neg, "\n")
cons_vale_itau <- cons_vale[`Código` %in% itau_funds]
cat("Linhas VALE3 em fundos Itau:", nrow(cons_vale_itau), "\n\n")

# 3) Merge dia-exato SH x CONS (SH diaria, CONS mensal)
merged <- merge(
  cons_vale_itau,
  sh_itau[, .(COD_FUNDO, DATA_D, GESTORA, NOME_FUNDO)],
  by.x  = c("Código", "DATA_COMP"),
  by.y  = c("COD_FUNDO", "DATA_D"),
  all.x = TRUE
)
n_total  <- nrow(merged)
n_sem_sh <- merged[is.na(GESTORA), .N]
cat("Fund-months VALE3 (Itau):", n_total,
    "| sem match exato na SH:", n_sem_sh,
    sprintf("(%.1f%%)\n", 100 * n_sem_sh / max(n_total, 1)))

# 4) Painel final + auditoria
painel <- merged[, .(
  cod_fundo  = `Código`,
  cnpj       = CNPJ,
  nome_fundo = NOME_FUNDO,
  gestora    = GESTORA,
  data       = DATA_COMP,
  ano        = year(DATA_COMP),
  mes        = month(DATA_COMP),
  peso_vale3 = PESO,
  valor_mil  = parse_num_us(Valor_Ativo_mil)
)][order(cod_fundo, data)]

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
fwrite(painel, "data/processed/painel_vale_itau_2016.csv")

dups_final <- painel[, .N, by = .(cod_fundo, data)][N > 1]
cat("\nPares (fundo, mes) duplicados no painel final:", nrow(dups_final), "\n")

cat("\n==== PAINEL 2016 ====\n")
cat("Obs:", nrow(painel),
    "| fundos:", uniqueN(painel$cod_fundo),
    "| meses:", uniqueN(painel$data), "\n")
cat("Peso VALE3 resumo (unidade a confirmar):\n")
print(summary(painel$peso_vale3))
cat("\nExemplos:\n")
print(head(painel, 10))
cat("\nOK - painel salvo em data/processed/painel_vale_itau_2016.csv\n")

# =============================================================================
# PASSO 3 - fluxo (captacao, resgate, liquido) via Informe Diario CVM
# Fonte oficial com CAPTC_DIA E RESG_DIA (a SH so tem captacao). Limpa:
# sep ';', decimal '.', sem R$. Fluxo liquido mensal = soma diaria.
# =============================================================================

ZIP  <- sprintf("data/raw/inf_diario_fi_%d.zip", YEAR)
csvs <- sprintf("data/raw/inf_diario_fi_%d%02d.csv", YEAR, 1:12)
if (!all(file.exists(csvs))) {
  if (!file.exists(ZIP)) {
    dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
    url <- sprintf("https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/HIST/inf_diario_fi_%d.zip", YEAR)
    cat("Baixando Informe Diario", YEAR, "...\n")
    download.file(url, ZIP, mode = "wb", quiet = TRUE)
  }
  unzip(ZIP, exdir = "data/raw")
}

cnpjs_alvo <- unique(painel$cnpj)
cols_id <- c("CNPJ_FUNDO", "DT_COMPTC", "CAPTC_DIA", "RESG_DIA")
id <- rbindlist(lapply(csvs, function(f) {
  d <- fread(f, sep = ";", encoding = "UTF-8", select = cols_id, showProgress = FALSE)
  d[CNPJ_FUNDO %in% cnpjs_alvo]
}))
stopifnot(!anyNA(id$CAPTC_DIA), !anyNA(id$RESG_DIA))   # parsing critico
id[, DT := as.Date(DT_COMPTC)][, ano := year(DT)][, mes := month(DT)]
flow <- id[, .(captacao  = sum(CAPTC_DIA),
               resgate   = sum(RESG_DIA),
               fluxo_liq = sum(CAPTC_DIA - RESG_DIA),
               n_dias    = .N),
           by = .(cnpj = CNPJ_FUNDO, ano, mes)]

# flag de divergencia: SH.APLICACAO (revisada) vs Informe Diario CAPTC (bruto).
# ~99,7% batem ao centavo; marcamos os ~0,3% (picos de 1 dia). Mantemos o ID.
shf <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", YEAR)), encoding = "UTF-8",
             showProgress = FALSE, colClasses = list(character = "APLICAÇÃO"))
shf <- shf[CNPJ %in% cnpjs_alvo]
shf[, AP := parse_brl(`APLICAÇÃO`)]
shf[, DT := as.Date(DATA, format = "%d/%m/%Y")][, ano := year(DT)][, mes := month(DT)]
sh_capt <- shf[, .(captacao_sh = sum(AP, na.rm = TRUE)), by = .(cnpj = CNPJ, ano, mes)]
flow <- merge(flow, sh_capt, by = c("cnpj", "ano", "mes"), all.x = TRUE)
flow[, div_captacao := as.integer(!is.na(captacao_sh) &
                                  abs(captacao - captacao_sh) >= 0.01)]

painel_fluxos <- merge(painel, flow, by = c("cnpj", "ano", "mes"), all.x = TRUE)
fwrite(painel_fluxos, "data/processed/painel_vale_itau_2016_fluxos.csv")
cat("\nPASSO 3: fluxo casado em",
    painel_fluxos[!is.na(fluxo_liq), .N], "de", nrow(painel_fluxos),
    "fund-months. Salvo em data/processed/painel_vale_itau_2016_fluxos.csv\n")

# =============================================================================
# PASSO 4 - caracteristicas de fundo (SH): aum, n_cotistas, is_fic, classif_anbima
# Snapshot no dia da competencia (merge dia-exato). ANBIMA nao distingue FIC/FI;
# FIC vem do NOME por token (\bFIC\b). PL em MIL -> R$ cheios (x1000).
# =============================================================================

snap <- sh[COD_FUNDO %in% unique(painel_fluxos$cod_fundo)]
snap[, DT     := as.Date(DATA, format = "%d/%m/%Y")]
snap[, pl_mil := parse_brl(`PATRIMONIO_LIQUIDO_(MIL)`)]
stopifnot(!anyNA(snap$pl_mil))
snap <- snap[, .(cod_fundo = COD_FUNDO, data = DT,
                 aum = pl_mil * 1000, n_cotistas = NUMERO_DE_COTISTAS,
                 classif_anbima = CLASSIFICACAO_ANBIMA)]
painel_features <- merge(painel_fluxos, snap, by = c("cod_fundo", "data"), all.x = TRUE)
painel_features[, is_fic := as.integer(grepl("\\bFIC\\b|\\bFICFI\\b",
                                             normalize_txt(nome_fundo)))]
fwrite(painel_features, "data/processed/painel_vale_itau_2016_features.csv")
cat("\nPASSO 4: features adicionadas (aum, n_cotistas, is_fic, classif_anbima).",
    "FIC:", painel_features[is_fic == 1, .N], "FI:", painel_features[is_fic == 0, .N],
    "| NAs aum:", painel_features[is.na(aum), .N], "\n")

# =============================================================================
# PASSO 5 - caracteristicas da ACAO (VALE3): preco nominal/ajustado e beta 252d
# Fonte Yahoo (VALE3.SA, ^BVSP). Retorno simples (VALE pelo adjclose, Ibov pelo
# close). Beta = cov/var movel 252 pregoes, no ULTIMO PREGAO DO MES ANTERIOR.
# =============================================================================

P1 <- 1388534400L; P2 <- 1485907200L
fetch_yahoo <- function(symbol, outfile) {
  if (file.exists(outfile)) return(invisible())
  url <- sprintf(paste0("https://query1.finance.yahoo.com/v8/finance/chart/%s",
                        "?period1=%d&period2=%d&interval=1d"),
                 utils::URLencode(symbol, reserved = TRUE), P1, P2)
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  system2("curl", c("-s", "-m", "60", "-A", shQuote("Mozilla/5.0"),
                    shQuote(url), "-o", shQuote(outfile)))
}
fetch_yahoo("VALE3.SA", "data/raw/yahoo_vale3.json")
fetch_yahoo("^BVSP",    "data/raw/yahoo_ibov.json")

getv <- function(lst) vapply(lst, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
parse_yahoo <- function(path) {
  res <- fromJSON(path, simplifyVector = FALSE)$chart$result[[1]]
  ts  <- vapply(res$timestamp, as.numeric, numeric(1))
  adj <- if (!is.null(res$indicators$adjclose))
           getv(res$indicators$adjclose[[1]]$adjclose) else NA_real_
  data.table(date = as.Date(as.POSIXct(ts, origin = "1970-01-01", tz = "America/Sao_Paulo")),
             close = getv(res$indicators$quote[[1]]$close), adjclose = adj)
}
vv <- parse_yahoo("data/raw/yahoo_vale3.json")
ii <- parse_yahoo("data/raw/yahoo_ibov.json")
stopifnot(!anyNA(vv$close), !anyNA(vv$adjclose), !anyNA(ii$close))
stk <- merge(vv[, .(date, close, adjclose)], ii[, .(date, ibov = close)], by = "date")
setorder(stk, date)
stk[, r_vale := adjclose / shift(adjclose) - 1]
stk[, r_ibov := ibov / shift(ibov) - 1]
Wb <- 252L
stk[, beta_vale := (frollmean(r_vale * r_ibov, Wb) - frollmean(r_vale, Wb) * frollmean(r_ibov, Wb)) /
                   (frollmean(r_ibov * r_ibov, Wb) - frollmean(r_ibov, Wb)^2)]
stk[, ymk := year(date) * 100L + month(date)]
mlast <- stk[stk[, .I[which.max(date)], by = ymk]$V1,
             .(ymk, data_ref = date, preco_nominal = close, preco_ajust = adjclose, beta_vale)]
painel_features[, ymk_prev := ifelse(mes == 1L, (ano - 1L) * 100L + 12L, ano * 100L + (mes - 1L))]
painel_full <- merge(painel_features, mlast, by.x = "ymk_prev", by.y = "ymk", all.x = TRUE)
painel_full[, ymk_prev := NULL]
fwrite(painel_full, "data/processed/painel_vale_itau_2016_full.csv")
cat("\nPASSO 5: preco/beta da VALE adicionados (NAs beta:", painel_full[is.na(beta_vale), .N],
    "). Painel FINAL: data/processed/painel_vale_itau_2016_full.csv\n")

# =============================================================================
# PASSO 6 - remover fundos com poucos cotistas
# Criterio simples aprovado: manter apenas observacoes com n_cotistas > 3.
# O painel completo continua salvo e intacto; esta versao filtrada sera usada
# como base principal para modelagem inicial.
# =============================================================================

stopifnot(!anyNA(painel_full$n_cotistas))
painel_filtrado <- painel_full[n_cotistas > 3]
fwrite(painel_filtrado,
       "data/processed/painel_vale_itau_2016_filtrado_cotistas_gt3.csv")
cat("\nPASSO 6: filtro n_cotistas > 3 aplicado.",
    "Obs antes:", nrow(painel_full),
    "| depois:", nrow(painel_filtrado),
    "| fundos depois:", uniqueN(painel_filtrado$cod_fundo),
    ". Saida: data/processed/painel_vale_itau_2016_filtrado_cotistas_gt3.csv\n")

# =============================================================================
# PASSO 7 - regressao (PASSO 1 do Maurico): cross-section mes a mes (fator
# latente theta_t + residuos) e pooled (com preco/beta). Amostra: filtrada.
# Tratamentos: log(aum), log(n_cotistas), is_fic, fluxo_liq/aum.
# =============================================================================

dr <- copy(painel_filtrado)
dr[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)]
dr[, l_cot := log(n_cotistas)][, flow_aum := fluxo_liq / aum_num]
fml_cs <- peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum

coefs7 <- rbindlist(lapply(sort(unique(dr$mes)), function(mm) {
  fit <- lm(fml_cs, data = dr[mes == mm]); cf <- coef(fit)
  data.table(mes = mm, n = nrow(dr[mes == mm]), r2 = summary(fit)$r.squared,
             intercepto = cf[[1]], b_l_aum = cf[[2]], b_l_cot = cf[[3]],
             b_is_fic = cf[[4]], b_flow_aum = cf[[5]])
}))
resids7 <- rbindlist(lapply(sort(unique(dr$mes)), function(mm) {
  dm <- dr[mes == mm]
  data.table(cod_fundo = dm$cod_fundo, mes = mm,
             resid = residuals(lm(fml_cs, data = dm)))
}))
fwrite(coefs7,  "data/processed/reg07_cross_section_coefs_2016.csv")
fwrite(resids7, "data/processed/reg07_cross_section_resid_2016.csv")
fitp7 <- lm(peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum + preco_nominal + beta_vale,
            data = dr)
cat("\nPASSO 7: cross-section (12 meses) + pooled rodadas.",
    "R2 pooled:", round(summary(fitp7)$r.squared, 3),
    "| coefs em data/processed/reg07_cross_section_coefs_2016.csv\n")

# =============================================================================
# PARTE 2 - PIPELINE MULTI-ANO 2016-2021 + MODELAGEM (espelha R/10-R/17)
# IMPORTANTE: rodar R_full COM CAMINHO ABSOLUTO (caminho relativo pode disparar
# segfault do data.table neste build do R/Windows). Pre-requisito: zips do
# Informe Diario 2017-2020 (HIST anual) e 2021 (mensal) baixados/extraidos em
# data/raw (ver R/11). Series Yahoo (yahoo_*.json) com range ate 2022.
# =============================================================================

ASSET2 <- "VALE ON N1 - VALE3"; ITAU2 <- c("ITAU ASSET","ITAU DTVM","ITAU UNIBANCO")
CCOLS  <- c("cnpj","anbima","codigo","nome","tipo_ativo","data_comp","nome_ativo","valor_mil","participacao")
grep_vale3 <- function(path) { con <- file(path, "r"); on.exit(close(con)); k <- list()
  repeat { ln <- readLines(con, n = 1e6, warn = FALSE); if (length(ln) == 0) break
    h <- ln[grepl(ASSET2, ln, fixed = TRUE, useBytes = TRUE)]; if (length(h)) k[[length(k)+1L]] <- h }
  unlist(k, use.names = FALSE) }

# ---- Passo 10: painel do peso de VALE3, 2016-2021 ----
res2 <- list()
for (yy in 2016:2021) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", yy)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","CNPJ","NOME_FUNDO","GESTORA","DATA"))
  sh[, GN := normalize_txt(GESTORA)]; shi <- sh[grepl(paste(ITAU2, collapse = "|"), GN)]
  shi[, DATA_D := parse_date(DATA)]; itf <- unique(shi$COD_FUNDO)
  ln <- grep_vale3(file.path(DATA_DIR, sprintf("cons_%d.csv", yy)))
  cv <- fread(text = ln, sep = ";", header = FALSE, col.names = CCOLS, showProgress = FALSE)
  cv <- cv[trimws(nome_ativo) == ASSET2]; cv <- unique(cv)
  cv[, PESO := parse_num_us(participacao)]; cv[, DATA_COMP := parse_date(data_comp)]
  cv <- cv[is.na(PESO) | PESO >= 0]; cvi <- cv[codigo %in% itf]
  m <- merge(cvi, shi[, .(COD_FUNDO, DATA_D, GESTORA, NOME_FUNDO)],
             by.x = c("codigo","DATA_COMP"), by.y = c("COD_FUNDO","DATA_D"), all.x = TRUE)
  res2[[length(res2)+1L]] <- m[, .(cod_fundo = codigo, cnpj = cnpj, nome_fundo = NOME_FUNDO,
    gestora = GESTORA, data = DATA_COMP, ano = year(DATA_COMP), mes = month(DATA_COMP),
    peso_vale3 = PESO, valor_mil = valor_mil)]
}
pA <- rbindlist(res2); pA[, data := as.Date(data)]
cat("\nPARTE2 P10: painel 2016-2021 =", nrow(pA), "obs |", uniqueN(pA$cod_fundo), "fundos\n")

# ---- Passo 11: fluxo (Informe Diario; le por NOME de coluna, robusto ao TP_FUNDO) ----
cnpjsA <- unique(pA$cnpj); idl <- list()
for (yy in 2016:2021) for (mm in sprintf("%02d", 1:12)) {
  f <- sprintf("data/raw/inf_diario_fi_%d%s.csv", yy, mm); if (!file.exists(f)) next
  d <- fread(f, sep = ";", select = c("CNPJ_FUNDO","DT_COMPTC","CAPTC_DIA","RESG_DIA"), showProgress = FALSE)
  idl[[length(idl)+1L]] <- d[CNPJ_FUNDO %in% cnpjsA]
}
idA <- rbindlist(idl); idA[, DT := as.Date(DT_COMPTC)][, ano := year(DT)][, mes := month(DT)]
flowA <- idA[, .(captacao = sum(CAPTC_DIA), resgate = sum(RESG_DIA),
                 fluxo_liq = sum(CAPTC_DIA - RESG_DIA), n_dias = .N), by = .(cnpj = CNPJ_FUNDO, ano, mes)]
pA <- merge(pA, flowA, by = c("cnpj","ano","mes"), all.x = TRUE)

# ---- Passo 12: features de fundo (snapshot dia-exato) ----
snl <- list()
for (yy in 2016:2021) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", yy)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","DATA","PATRIMONIO_LIQUIDO_(MIL)","NUMERO_DE_COTISTAS","CLASSIFICACAO_ANBIMA"),
              colClasses = list(character = "PATRIMONIO_LIQUIDO_(MIL)"))
  sh <- sh[COD_FUNDO %in% unique(pA$cod_fundo)]
  sh[, DT := parse_date(DATA)]; sh[, pl := parse_brl(`PATRIMONIO_LIQUIDO_(MIL)`)]
  snl[[length(snl)+1L]] <- sh[, .(cod_fundo = COD_FUNDO, data = DT, aum = pl*1000,
                                  n_cotistas = NUMERO_DE_COTISTAS, classif_anbima = CLASSIFICACAO_ANBIMA)]
}
pA <- merge(pA, rbindlist(snl), by = c("cod_fundo","data"), all.x = TRUE)
pA[, is_fic := as.integer(grepl("\bFIC\b|\bFICFI\b", normalize_txt(nome_fundo)))]

# ---- Passo 13: preco/beta (Yahoo, janela 252 pregoes) ----
vv2 <- parse_yahoo("data/raw/yahoo_vale3.json"); ii2 <- parse_yahoo("data/raw/yahoo_ibov.json")
vv2 <- vv2[!is.na(close) & !is.na(adjclose)]; ii2 <- ii2[!is.na(close)]
s2 <- merge(vv2[, .(date, close, adjclose)], ii2[, .(date, ibov = close)], by = "date"); setorder(s2, date)
s2[, rx := adjclose/shift(adjclose) - 1][, ry := ibov/shift(ibov) - 1]
s2[, beta_vale := (frollmean(rx*ry,252L) - frollmean(rx,252L)*frollmean(ry,252L)) /
                  (frollmean(ry*ry,252L) - frollmean(ry,252L)^2)]
s2[, ymk := year(date)*100L + month(date)]
ml2 <- s2[s2[, .I[which.max(date)], by = ymk]$V1,
          .(ymk, data_ref = date, preco_nominal = close, preco_ajust = adjclose, beta_vale)]
pA[, ymk_prev := ifelse(mes == 1L, (ano-1L)*100L+12L, ano*100L+(mes-1L))]
pA <- merge(pA, ml2, by.x = "ymk_prev", by.y = "ymk", all.x = TRUE); pA[, ymk_prev := NULL]
fwrite(pA, "data/processed/painel_vale_itau_2016_2021_full.csv")
cat("PARTE2 P11-13: painel FINAL multi-ano salvo |", nrow(pA), "obs | NAs fluxo:",
    sum(is.na(pA$fluxo_liq)), "\n")

# ---- Passo 14: filtro (>3 cotistas) + regressao cross-section 72 meses ----
dA <- pA[n_cotistas > 3 & !is.na(fluxo_liq)]
dA[, l_aum := log(as.numeric(aum))][, l_cot := log(n_cotistas)]
dA[, flow_aum := fluxo_liq/as.numeric(aum)][, ym := ano*100L + mes]
coA <- rbindlist(lapply(sort(unique(dA$ym)), function(mn) {
  fit <- lm(peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum, data = dA[ym == mn]); cf <- coef(fit)
  data.table(ym = mn, intercepto = cf[[1]], b_l_aum = cf[[2]], b_l_cot = cf[[3]],
             b_is_fic = cf[[4]], b_flow_aum = cf[[5]], r2 = summary(fit)$r.squared) }))
fwrite(coA, "data/processed/reg14_coefs_2016_2021.csv")
fitpA <- lm(peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum + preco_nominal + beta_vale, data = dA)
cat("PARTE2 P14: regressao 72 meses (", nrow(coA), "meses). R2 medio cross-section:",
    round(mean(coA$r2), 3), "| R2 pooled:", round(summary(fitpA)$r.squared, 3), "\n")
cat("PARTE2: dinamica dos theta_t (R/16) e previsao+matriz de erros (R/17) nos",
    "scripts dedicados.\n")
