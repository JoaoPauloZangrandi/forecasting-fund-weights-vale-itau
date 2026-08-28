# =============================================================================
# 106_recalcula_n_testes_total_fase4.R  (v2 OFICIAL -- teste_estrategia)
#
# CORRECAO de um dado obsoleto encontrado durante a auditoria: o arquivo
# n_testes_total_fase4.csv (consumido por 64_significancia_pares_replicantes.R
# como denominador m do Benjamini-Hochberg, m=31.907.281 hoje citado no TCC)
# tem timestamp de 30/jul, ANTES do revert desta sessao pro range 2016-2021
# (23/ago) -- nunca foi regenerado. Nenhum script do repo o gera (grep
# confirma: e' orfao, provavelmente calculado uma vez manualmente).
#
# Este script reproduz a MESMA logica de contagem do R/63 (mesmo filtro de
# posicao-poeira, mesmo MIN_MESES=12, SEM demeaning -- e' a versao "oficial"),
# em duas frentes:
#   (a) confirma que reproduz EXATAMENTE os 1.808.886 pares de
#       pares_replicantes_bruto.csv (valida que a logica bate com o R/63 real)
#   (b) calcula o n_testes_total correto e ATUAL pra virar o novo
#       n_testes_total_fase4.csv
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
LIMIAR_CORR <- 0.6
MIN_MESES <- 12L

M <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_universo_completo_h1.csv"))
M[, cod_fundo := as.character(cod_fundo)]
lam <- 0.0690
M[, u := dw_corrigido - lam * d]
M <- M[!is.na(u)]
M[, mes_idx := (ym %/% 100L - 2016L) * 12L + (ym %% 100L)]

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"))
pp[, cod_fundo := as.character(cod_fundo)]
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
cel_ok <- cel[peso_mediano >= 0.001, .(cod_fundo, ativo)]
M <- merge(M, cel_ok, by = c("cod_fundo","ativo"))
cat("Apos filtro de posicao-poeira:", nrow(M), "obs |", uniqueN(M$ativo), "ativos\n")

idx_min <- min(M$mes_idx); idx_max <- max(M$mes_idx)
todos_idx <- idx_min:idx_max
por_ativo_n <- M[, .(n_fundos = uniqueN(cod_fundo)), by = ativo]
ativos_alvo <- por_ativo_n[n_fundos >= 2]$ativo
cat("Ativos com >=2 fundos:", length(ativos_alvo), "\n")

processa_ativo <- function(av) {
  dm <- M[ativo == av, .(cod_fundo, mes_idx, u)]
  fundos <- unique(dm$cod_fundo)
  n_f <- length(fundos)
  if (n_f < 2) return(list(n_pares_06 = 0L, n_testes = 0L))

  W <- matrix(NA_real_, nrow = length(todos_idx), ncol = n_f, dimnames = list(as.character(todos_idx), fundos))
  W[cbind(match(dm$mes_idx, todos_idx), match(dm$cod_fundo, fundos))] <- dm$u

  ok_col <- colSums(!is.na(W)) >= MIN_MESES
  if (sum(ok_col) < 2) return(list(n_pares_06 = 0L, n_testes = 0L))
  W <- W[, ok_col, drop = FALSE]

  I <- !is.na(W); storage.mode(I) <- "double"
  n_overlap <- t(I) %*% I
  Cc <- suppressWarnings(cor(W, use = "pairwise.complete.obs"))

  ut <- which(upper.tri(n_overlap), arr.ind = TRUE)
  cobertura_ok <- n_overlap[ut] >= MIN_MESES
  keep_06 <- cobertura_ok & abs(Cc[ut]) >= LIMIAR_CORR
  list(n_pares_06 = sum(keep_06), n_testes = sum(cobertura_ok))
}

t0 <- Sys.time()
n_pares_06 <- integer(length(ativos_alvo))
n_testes <- integer(length(ativos_alvo))
for (i in seq_along(ativos_alvo)) {
  r <- tryCatch(processa_ativo(ativos_alvo[i]), error = function(e) list(n_pares_06=0L, n_testes=0L))
  n_pares_06[i] <- r$n_pares_06; n_testes[i] <- r$n_testes
}
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")

cat("\n===== VALIDACAO =====\n")
cat("Pares com |corr|>=0,6 (este script, sem demeaning):", sum(n_pares_06), "\n")
cat("Pares em pares_replicantes_bruto.csv (R/63, ja salvo):",
    nrow(fread(file.path(REPO, "v2 OFICIAL/data/pares_replicantes_bruto.csv"))), "\n")
cat("(devem ser IGUAIS -- confirma que a logica deste script bate com o R/63)\n")

cat("\n===== N_TESTES_TOTAL ATUALIZADO =====\n")
cat("Valor antigo (arquivo orfao, timestamp 30/jul, PRE-revert 2016-2021):", 31907281, "\n")
cat("Valor correto e atual (dado 2016-2021 pos-revert, 23/ago):", sum(n_testes), "\n")

fwrite(data.table(n_testes_total = sum(n_testes)),
       file.path(REPO, "v2 OFICIAL/data/n_testes_total_fase4.csv"))
cat("\nOK - n_testes_total_fase4.csv SOBRESCRITO com o valor correto e atual.\n")
