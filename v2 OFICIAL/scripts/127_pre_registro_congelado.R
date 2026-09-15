# =============================================================================
# 127_pre_registro_congelado.R  (v2 OFICIAL)
#
# Congela o hold-out. Roda UMA vez, depois do commit do pre-registro.
# Grava o hash do commit junto, para que o split seja rastreavel ate o
# documento que o definiu.
#
# Split POR FUNDO, 50/50, estratificado por gestora, semente 20260915.
# Motivo de ser por fundo e nao por tempo: 23 meses de teste sao curtos
# demais para partir, e x_barra_g precisa continuar estimavel dos dois
# lados.
# =============================================================================
suppressPackageStartupMessages({ library(data.table) })

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
SEMENTE <- 20260915L

cat("=============================================================\n")
cat("127 - Congelamento do hold-out\n")
cat("=============================================================\n\n")

# --- hash do commit do pre-registro -----------------------------------------
hash <- tryCatch(
  system2("git", c("-C", shQuote(REPO), "log", "-1", "--format=%H", "--",
                   shQuote("v2 OFICIAL/PRE_REGISTRO_EXTENSAO.md")),
          stdout = TRUE),
  error = function(e) NA_character_)
cat("Commit do pre-registro:", hash, "\n")

pre <- file.path(REPO, "v2 OFICIAL/PRE_REGISTRO_EXTENSAO.md")
stopifnot(file.exists(pre))
linhas <- readLines(pre, warn = FALSE)
cat("Pre-registro:", length(linhas), "linhas\n\n")

# --- universo: fundos com erro no teste, amostra do plano --------------------
F <- fread(file.path(DD, "diag_fundos_extensao.csv"))
F[, cod_fundo := as.character(cod_fundo)]
U <- F[amostra_plano == TRUE]
cat("Universo do split (amostra do plano):", nrow(U), "fundos |",
    uniqueN(U$gestora_grupo), "gestoras\n")
cat("  dos quais com treino >= 12m (amostra S1):", U[n_meses_treino >= 12, .N], "\n\n")

# --- split estratificado por gestora -----------------------------------------
set.seed(SEMENTE)
U <- U[order(gestora_grupo, cod_fundo)]
U[, r := runif(.N)]
U[, rank_g := frank(r, ties.method = "first"), by = gestora_grupo]
U[, n_g := .N, by = gestora_grupo]
U[, grupo := ifelse(rank_g <= ceiling(n_g / 2), "dev", "conf")]

cat("--- Resultado do split ---\n")
print(U[, .N, by = grupo])
cat("\nPor gestora (primeiras 10):\n")
print(dcast(U[, .N, by = .(gestora_grupo, grupo)], gestora_grupo ~ grupo,
            value.var = "N", fill = 0L)[1:10])

chk <- dcast(U[, .N, by = .(gestora_grupo, grupo)], gestora_grupo ~ grupo,
             value.var = "N", fill = 0L)
cat(sprintf("\nGestoras presentes nos DOIS lados: %d de %d\n",
            chk[conf > 0 & dev > 0, .N], nrow(chk)))
cat(sprintf("Gestoras so em um lado: %d\n", chk[conf == 0 | dev == 0, .N]))

# em S1 (treino>=12m) a estratificacao pode degradar; conferir
S1 <- U[n_meses_treino >= 12]
chk1 <- dcast(S1[, .N, by = .(gestora_grupo, grupo)], gestora_grupo ~ grupo,
              value.var = "N", fill = 0L)
cat(sprintf("\nEm S1 (treino>=12m): %d fundos | gestoras nos dois lados: %d de %d\n",
            nrow(S1), chk1[conf > 0 & dev > 0, .N], nrow(chk1)))
print(S1[, .N, by = grupo])

saida <- U[, .(cod_fundo, gestora_grupo, grupo, n_obs_teste, n_meses_teste,
               n_meses_treino, amostra_B1)]
saida[, semente := SEMENTE]
saida[, commit_pre_registro := hash]
fwrite(saida, file.path(DD, "split_fundos_dev_conf.csv"))

cat("\nOK - split_fundos_dev_conf.csv gravado e CONGELADO.\n")
cat("A partir daqui, o lado 'conf' nao pode ser olhado ate o script 131.\n")
