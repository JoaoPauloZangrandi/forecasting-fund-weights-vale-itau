# =============================================================================
# R_full.R
#
# Codigo COMPLETO do projeto num unico arquivo, sem segmentacao.
# Espelha os scripts modulares em R/ (01, 02, ...). A cada passo aprovado,
# este arquivo cresce com o bloco novo. Usa SOMENTE as bases CONS e SH.
#
# Estado atual: Passo 1 (carga/sanidade das bases) + Passo 2 (painel de pesos
# de VALE3 nos fundos Itau, ano 2016).
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
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
painel_fluxos <- merge(painel, flow, by = c("cnpj", "ano", "mes"), all.x = TRUE)
fwrite(painel_fluxos, "data/processed/painel_vale_itau_2016_fluxos.csv")
cat("\nPASSO 3: fluxo casado em",
    painel_fluxos[!is.na(fluxo_liq), .N], "de", nrow(painel_fluxos),
    "fund-months. Salvo em data/processed/painel_vale_itau_2016_fluxos.csv\n")
