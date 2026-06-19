# =============================================================================
# 01_load_bases.R
#
# Primeiro passo do projeto: carregar CONS e SH em R.
# Este script NAO filtra Itau, NAO filtra VALE, NAO calcula pesos e NAO estima
# modelos. Ele apenas confirma que os arquivos existem e que o R consegue ler
# as colunas principais.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

DATA_DIR <- Sys.getenv(
  "CVM_DATA_DIR",
  unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"
)

YEARS <- 2016:2021

full_load <- toupper(Sys.getenv("FULL_LOAD", unset = "FALSE")) == "TRUE"
load_rows <- suppressWarnings(as.integer(Sys.getenv("LOAD_ROWS", unset = "1000")))
if (is.na(load_rows) || load_rows <= 0) load_rows <- 1000L

read_n <- if (full_load) Inf else load_rows

read_cvm_csv <- function(path, encoding, nrows) {
  fread(
    path,
    encoding = encoding,
    nrows = nrows,
    showProgress = FALSE
  )
}

load_one_year <- function(year) {
  cons_path <- file.path(DATA_DIR, sprintf("cons_%d.csv", year))
  sh_path   <- file.path(DATA_DIR, sprintf("SH_%d.csv", year))

  if (!file.exists(cons_path)) stop("CONS nao encontrada: ", cons_path)
  if (!file.exists(sh_path)) stop("SH nao encontrada: ", sh_path)

  cons <- read_cvm_csv(cons_path, encoding = "UTF-8", nrows = read_n)
  sh   <- read_cvm_csv(sh_path, encoding = "UTF-8", nrows = read_n)

  list(
    cons = cons,
    sh = sh,
    audit = data.table(
      year = year,
      file = c("CONS", "SH"),
      path = c(cons_path, sh_path),
      rows_loaded = c(nrow(cons), nrow(sh)),
      cols_loaded = c(ncol(cons), ncol(sh)),
      columns = c(paste(names(cons), collapse = " | "),
                  paste(names(sh), collapse = " | "))
    )
  )
}

cat("Diretorio das bases:", DATA_DIR, "\n")
cat("Modo:", if (full_load) "FULL_LOAD" else paste0("amostra de ", read_n, " linhas"), "\n\n")

loaded <- lapply(YEARS, load_one_year)
audit <- rbindlist(lapply(loaded, `[[`, "audit"))

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
fwrite(audit, "outputs/tables/load_audit.csv")

print(audit[, .(year, file, rows_loaded, cols_loaded)])

cat("\nColunas CONS 2016:\n")
print(names(loaded[[1]]$cons))

cat("\nColunas SH 2016:\n")
print(names(loaded[[1]]$sh))

cat("\nOK - bases carregadas. Auditoria salva em outputs/tables/load_audit.csv\n")
