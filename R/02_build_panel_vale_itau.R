# =============================================================================
# 02_build_panel_vale_itau.R
#
# Passo 2: montar o painel fundo x mes do PESO de VALE3 (exatamente
# "VALE ON N1 - VALE3", posicao direta) nos fundos das gestoras Itau.
#
# Escopo desta versao: APENAS o ano de 2016 (mais rapido, menos tokens).
# Usa SOMENTE as bases CONS e SH.
#
# NAO calcula caracteristicas nem regressao ainda. So o painel de pesos.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

PROJ_DIR <- Sys.getenv("PROJ_DIR",
                       unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
if (dir.exists(PROJ_DIR)) setwd(PROJ_DIR)

DATA_DIR <- Sys.getenv(
  "CVM_DATA_DIR",
  unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"
)

YEAR        <- 2016L
ASSET_VALE3 <- "VALE ON N1 - VALE3"

# Gestoras-alvo (Itau). Casamos por forma NORMALIZADA (sem acento, maiusculo),
# usando padroes-nucleo robustos a sufixos como "S.A.".
ITAU_PATTERNS <- c("ITAU ASSET", "ITAU DTVM", "ITAU UNIBANCO")

# ---- utilitarios de limpeza -------------------------------------------------

# remove acentos e padroniza: maiusculo, trim, espacos colapsados
normalize_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")  # tira acentos
  x <- toupper(x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

# parse numerico em formato AMERICANO (decimal ".") -> usado na CONS
# remove eventual separador de milhar ","
parse_num_us <- function(x) {
  x <- as.character(x)
  x <- gsub(",", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# parse de data tentando formatos comuns
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

cat("Diretorio das bases:", DATA_DIR, "\n")
cat("Ano:", YEAR, "| Ativo:", ASSET_VALE3, "\n\n")

# ---- 1) SH: universo de fundos das gestoras Itau ---------------------------

sh_path <- file.path(DATA_DIR, sprintf("SH_%d.csv", YEAR))
if (!file.exists(sh_path)) stop("SH nao encontrada: ", sh_path)

sh <- fread(sh_path, encoding = "UTF-8", showProgress = FALSE)

sh[, GESTORA_NORM := normalize_txt(GESTORA)]
itau_regex <- paste(ITAU_PATTERNS, collapse = "|")
sh_itau    <- sh[grepl(itau_regex, GESTORA_NORM)]
sh_itau[, DATA_D := parse_date(DATA)]

cat("Linhas SH total:", nrow(sh), "| linhas Itau:", nrow(sh_itau), "\n")
cat("Gestoras Itau encontradas (distintas):\n")
print(sh_itau[, .N, by = .(GESTORA)][order(-N)])

itau_funds <- unique(sh_itau$COD_FUNDO)
cat("\nFundos Itau distintos (COD_FUNDO):", length(itau_funds), "\n\n")

# ---- 2) CONS: posicoes de VALE3 --------------------------------------------

cons_path <- file.path(DATA_DIR, sprintf("cons_%d.csv", YEAR))
if (!file.exists(cons_path)) stop("CONS nao encontrada: ", cons_path)

cons <- fread(cons_path, encoding = "UTF-8", showProgress = FALSE)

cons[, NOME_ATIVO_TRIM := trimws(Nome_Ativo)]
cons_vale <- cons[NOME_ATIVO_TRIM == ASSET_VALE3]
n_vale_bruto <- nrow(cons_vale)

# TRATAMENTO 1: remover linhas EXATAMENTE duplicadas (quirk da CVM; ver
# docs/log_decisoes.md). Sao copias byte-a-byte da mesma posicao -> seguro.
cons_vale <- unique(cons_vale)
cat("Linhas CONS total:", nrow(cons), "| VALE3:", n_vale_bruto,
    "| apos dedup exata:", nrow(cons_vale),
    "(", n_vale_bruto - nrow(cons_vale), "duplicatas removidas )\n")

cons_vale[, PESO      := parse_num_us(`Participação_Ativo`)]
cons_vale[, DATA_COMP := parse_date(`Data_Competência`)]

# TRATAMENTO 2: remover pesos NEGATIVOS (peso deve viver em [0,1]; sao residuos
# contabeis ~ -1e-6). Decisao do orientando, registrada em docs/log_decisoes.md.
n_neg     <- cons_vale[PESO < 0, .N]
cons_vale <- cons_vale[is.na(PESO) | PESO >= 0]
cat("Pesos negativos removidos:", n_neg, "\n")

cons_vale_itau <- cons_vale[`Código` %in% itau_funds]
cat("Linhas VALE3 em fundos Itau:", nrow(cons_vale_itau), "\n\n")

# ---- 3) Merge dia-exato SH x CONS ------------------------------------------
# SH e diaria, CONS mensal: casamos a SH no MESMO dia da competencia da CONS.

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

# ---- 4) Painel final + auditoria -------------------------------------------

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

# auditoria de integridade: nenhum (fundo, mes) pode estar duplicado
dups_final <- painel[, .N, by = .(cod_fundo, data)][N > 1]
cat("\nPares (fundo, mes) duplicados no painel final:", nrow(dups_final), "\n")
if (nrow(dups_final) > 0) {
  cat("ATENCAO: ainda ha duplicados (valores diferentes?) - investigar.\n")
  print(head(dups_final, 10))
}

cat("\n==== PAINEL 2016 ====\n")
cat("Obs:", nrow(painel),
    "| fundos:", uniqueN(painel$cod_fundo),
    "| meses:", uniqueN(painel$data), "\n")
cat("Peso VALE3 resumo (unidade a confirmar):\n")
print(summary(painel$peso_vale3))
cat("\nExemplos:\n")
print(head(painel, 10))

cat("\nOK - painel salvo em data/processed/painel_vale_itau_2016.csv\n")
