# =============================================================================
# 31_build_panel_multiasset.R
#
# Generaliza R/10 para TODOS os ativos (nao so VALE3): painel fundo x ativo x
# mes das posicoes em ACOES dos fundos do grupo Itau, 2016-2021. Descoberta
# desta rodada: cons_YYYY.csv JA VEM pre-filtrado para Tipo_Ativo=="Acoes"
# (confirmado: fread direto, 100% das linhas tem esse tipo) -> nao precisa do
# truque de leitura em blocos por grepl do R/10 (que so existia p/ isolar UM
# ativo por texto). fread direto funciona bem (testado: cons_2018, 2,8M
# linhas, 0.8s).
#
# Escala do universo Itau (medida nesta rodada): 1.777 ativos distintos, 906
# fundos, ~7,8 milhoes de linhas fundo-ativo-mes em 2016-2021. RODAR ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
DATA_DIR <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
ITAU <- c("ITAU ASSET","ITAU DTVM","ITAU UNIBANCO")
norm <- function(x){ x<-iconv(as.character(x),"","ASCII//TRANSLIT"); trimws(gsub("[[:space:]]+"," ",toupper(x))) }
pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }

res <- list()
for (y in 2016:2021) {
  cat("== ano", y, "==\n"); flush.console()
  sh <- fread(file.path(DATA_DIR,sprintf("SH_%d.csv",y)), encoding="UTF-8", showProgress=FALSE,
              select=c("COD_FUNDO","CNPJ","NOME_FUNDO","GESTORA","DATA"))
  sh[, GN := norm(GESTORA)]; shi <- sh[grepl(paste(ITAU,collapse="|"),GN)]; shi[, DATA_D := pdate(DATA)]
  itau_funds <- unique(shi$COD_FUNDO)

  cv <- fread(file.path(DATA_DIR, sprintf("cons_%d.csv", y)), encoding="UTF-8", showProgress=FALSE)
  stopifnot(all(cv$Tipo_Ativo == "Ações"))            # confirma a premissa desta rodada
  cv <- unique(cv)                                     # dedup exato (regra do R/10)
  cv[, PESO := as.numeric(Participação_Ativo)]
  cv <- cv[is.na(PESO) | PESO >= 0]                    # remove pesos negativos (regra do R/10)
  cv[, DATA_COMP := pdate(`Data_Competência`)]
  cvi <- cv[Código %in% itau_funds]

  m <- merge(cvi, shi[,.(COD_FUNDO,DATA_D,GESTORA,NOME_FUNDO)],
             by.x = c("Código","DATA_COMP"), by.y = c("COD_FUNDO","DATA_D"), all.x = TRUE)
  pan <- m[, .(cod_fundo = Código, cnpj = CNPJ, nome_fundo = NOME_FUNDO, gestora = GESTORA,
               ativo = Nome_Ativo, data = DATA_COMP, ano = year(DATA_COMP), mes = month(DATA_COMP),
               peso = PESO, valor_mil = Valor_Ativo_mil)]
  res[[length(res)+1L]] <- pan
  cat("  ", nrow(pan), "linhas |", uniqueN(pan$cod_fundo), "fundos |", uniqueN(pan$ativo), "ativos\n")
  flush.console()
}
painel <- rbindlist(res)
cat("\nTOTAL (bruto):", nrow(painel), "linhas |", uniqueN(painel$cod_fundo), "fundos |",
    uniqueN(painel$ativo), "ativos\n")

dup <- painel[, .N, by=.(cod_fundo, ativo, ano, mes)][N > 1]
cat("pares (fundo,ativo,mes) duplicados:", nrow(dup), "\n")

# ---- limpeza: "Acao de Companhia Fechada" com nome contendo virgula corrompe o
# parsing CSV bruto da CVM (campo derrama para as colunas seguintes) -> peso
# vira NA ou um valor sem sentido (>1). Achado nesta rodada: 10 linhas em 6,13
# milhoes (0,00016%). Sao posicoes em empresas de capital fechado, fora do
# escopo de "acoes negociadas em bolsa" do projeto; descartamos.
n0 <- nrow(painel)
ruim <- painel[is.na(peso) | peso > 1 | grepl("Companhia Fechada", ativo, fixed = TRUE)]
cat("linhas descartadas (Cia. Fechada / peso invalido):", nrow(ruim), "\n")
painel <- painel[!(is.na(peso) | peso > 1 | grepl("Companhia Fechada", ativo, fixed = TRUE))]
cat("apos limpeza:", nrow(painel), "linhas (", n0-nrow(painel), "removidas)\n")
cat("peso: min", min(painel$peso), "| max", max(painel$peso), "| NAs:", sum(is.na(painel$peso)), "\n")

# checagem de consistencia: VALE3 deve reproduzir EXATAMENTE o painel original
nvale <- painel[ativo == "VALE ON N1 - VALE3", .N]
cat("linhas VALE3 no painel multiativo:", nvale, "(esperado: 26.123, do painel original R/10)\n")

fwrite(painel, file.path(REPO,"data/processed/painel_multiativo_2016_2021.csv"))
cat("\nOK - salvo em data/processed/painel_multiativo_2016_2021.csv\n")

# =============================================================================
# (2) FLUXO liquido mensal do FUNDO (independe do ativo) — mesma logica do R/11
# =============================================================================
RAW  <- file.path(REPO, "data/raw")
COLS <- c("CNPJ_FUNDO","DT_COMPTC","CAPTC_DIA","RESG_DIA")
cnpjs <- unique(painel$cnpj)
cat("\n== (2) FLUXO: fundos-alvo (cnpj):", length(cnpjs), "==\n")
ler <- list()
for (y in 2016:2021) for (mm in sprintf("%02d", 1:12)) {
  f <- file.path(RAW, sprintf("inf_diario_fi_%d%s.csv", y, mm))
  if (!file.exists(f)) { cat("FALTA:", f, "\n"); next }
  dtmp <- fread(f, sep = ";", select = COLS, showProgress = FALSE)
  ler[[length(ler)+1L]] <- dtmp[CNPJ_FUNDO %in% cnpjs]
}
id <- rbindlist(ler); id[, DT := as.Date(DT_COMPTC)][, ano := year(DT)][, mes := month(DT)]
flow <- id[, .(fluxo_liq = sum(CAPTC_DIA - RESG_DIA, na.rm = TRUE)), by = .(cnpj = CNPJ_FUNDO, ano, mes)]
cat("fundo-meses com fluxo calculado:", nrow(flow), "\n")

# =============================================================================
# (3) CARACTERISTICAS do fundo (aum, cotistas, is_fic) — mesma logica do R/12,
# agora para o universo AMPLIADO de fundos Itau (nao so quem tem VALE3)
# =============================================================================
parse_brl <- function(x){x<-trimws(as.character(x));x<-gsub("R\\$","",x);x<-gsub("[[:space:]]","",x)
  x<-gsub("\\.","",x);x<-gsub(",",".",x);x[x==""]<-NA;suppressWarnings(as.numeric(x))}
res2 <- list()
for (y in 2016:2021) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","DATA","PATRIMONIO_LIQUIDO_(MIL)","NUMERO_DE_COTISTAS"),
              colClasses = list(character = "PATRIMONIO_LIQUIDO_(MIL)"))
  sh <- sh[COD_FUNDO %in% unique(painel$cod_fundo)]
  sh[, DT := pdate(DATA)][, aum := parse_brl(`PATRIMONIO_LIQUIDO_(MIL)`) * 1000]
  snap <- sh[, .(cod_fundo = COD_FUNDO, data = DT, aum, n_cotistas = NUMERO_DE_COTISTAS)]
  py <- merge(painel[ano == y], snap, by = c("cod_fundo","data"), all.x = TRUE)
  res2[[length(res2)+1L]] <- py
  cat("ano", y, "features ok\n"); flush.console()
}
pan2 <- rbindlist(res2)
pan2[, is_fic := as.integer(grepl("\\bFIC\\b|\\bFICFI\\b", norm(nome_fundo)))]
pan2 <- merge(pan2, flow, by = c("cnpj","ano","mes"), all.x = TRUE)
cat("\n== AUDITORIA multiativo ==\n")
cat("NAs aum:", pan2[is.na(aum),.N], "| NAs cotistas:", pan2[is.na(n_cotistas),.N],
    "| NAs fluxo:", pan2[is.na(fluxo_liq),.N], "\n")
cat("Pares (fundo,ativo,mes) duplicados:", nrow(pan2[, .N, by=.(cod_fundo,ativo,ano,mes)][N>1]), "\n")

fwrite(pan2, file.path(REPO, "data/processed/painel_multiativo_2016_2021_full.csv"))
cat("OK - salvo em data/processed/painel_multiativo_2016_2021_full.csv\n")

# =============================================================================
# (4) filtro >3 cotistas (Passo 6, mesma convencao do painel principal)
# =============================================================================
pan3 <- pan2[!is.na(n_cotistas) & n_cotistas > 3 & !is.na(aum) & !is.na(fluxo_liq)]
cat("\nApos filtro >3 cotistas + aum/fluxo disponiveis:", nrow(pan3), "linhas |",
    uniqueN(pan3$cod_fundo), "fundos |", uniqueN(pan3$ativo), "ativos\n")
fwrite(pan3, file.path(REPO, "data/processed/painel_multiativo_2016_2021_filtrado.csv"))
cat("OK - salvo em data/processed/painel_multiativo_2016_2021_filtrado.csv\n")
