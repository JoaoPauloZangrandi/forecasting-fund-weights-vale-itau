# =============================================================================
# 105_robustez_demeaning_replicantes.R  (v2 OFICIAL -- teste_estrategia)
#
# ROBUSTEZ pedida pelo Joao apos auditoria externa (agent de rigor
# econometrico apontou que 63_pares_replicantes.R correlaciona "u" bruto
# entre pares de fundos dentro do mesmo ativo, SEM remover antes o choque
# comum aquele ativo-mes -- um par pode aparecer correlacionado so' porque
# os dois levam exposicao ao mesmo evento nao modelado do ativo, nao porque
# um copia o outro).
#
# Mesma logica exata do R/63 (mesmo filtro de posicao-poeira, mesmo
# MIN_MESES=12, mesmo LIMIAR_CORR=0,6, mesma correcao Benjamini-Hochberg
# global com o m correto), com UMA mudanca: antes de calcular correlacao,
# "u" e' demeaned por (ativo, mes) -- ou seja, cada fundo entra com o quanto
# seu erro se desvia da MEDIA de todos os holders daquele ativo naquele mes.
# Isola co-movimento PAR A PAR alem do que e' comum a todo mundo que segura
# aquele ativo naquele mes.
#
# NAO substitui 63/64 -- roda em paralelo, so' pra comparar quanto do
# "1.248.772 pares significativos" sobrevive a essa checagem mais dura.
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
cat("Base de erro (u), antes do filtro de posicao-poeira:", nrow(M), "obs\n")

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"))
pp[, cod_fundo := as.character(cod_fundo)]
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
cel_ok <- cel[peso_mediano >= 0.001, .(cod_fundo, ativo)]
M <- merge(M, cel_ok, by = c("cod_fundo","ativo"))
cat("Apos filtro de posicao-poeira:", nrow(M), "obs |", uniqueN(M$ativo), "ativos\n")

# ---- ESSA E' A UNICA MUDANCA vs R/63: demeaning por (ativo, mes) ----------
M[, u_bruto := u]
M[, u := u_bruto - mean(u_bruto), by = .(ativo, ym)]
cat("Demeaning por (ativo, mes) aplicado -- media de 'u' por celula agora e' zero por construcao.\n")

mapa <- fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
mapa[, cod_fundo := as.character(cod_fundo)]
gest_lookup <- setNames(mapa$gestora_grupo, mapa$cod_fundo)

idx_min <- min(M$mes_idx); idx_max <- max(M$mes_idx)
todos_idx <- idx_min:idx_max

por_ativo_n <- M[, .(n_fundos = uniqueN(cod_fundo)), by = ativo]
ativos_alvo <- por_ativo_n[n_fundos >= 2]$ativo
cat("Ativos com >=2 fundos (apos filtro):", length(ativos_alvo), "\n")

n_testes_total <- 0L

processa_ativo <- function(av) {
  dm <- M[ativo == av, .(cod_fundo, mes_idx, u)]
  fundos <- unique(dm$cod_fundo)
  n_f <- length(fundos)
  if (n_f < 2) return(NULL)

  W <- matrix(NA_real_, nrow = length(todos_idx), ncol = n_f, dimnames = list(as.character(todos_idx), fundos))
  W[cbind(match(dm$mes_idx, todos_idx), match(dm$cod_fundo, fundos))] <- dm$u

  ok_col <- colSums(!is.na(W)) >= MIN_MESES
  if (sum(ok_col) < 2) return(list(pares = NULL, n_testes = 0L))
  W <- W[, ok_col, drop = FALSE]
  fundos <- colnames(W)
  n_f <- ncol(W)

  I <- !is.na(W); storage.mode(I) <- "double"
  n_overlap <- t(I) %*% I
  Cc <- suppressWarnings(cor(W, use = "pairwise.complete.obs"))

  ut <- which(upper.tri(n_overlap), arr.ind = TRUE)
  cobertura_ok <- n_overlap[ut] >= MIN_MESES
  n_testes_ativo <- sum(cobertura_ok)

  keep <- cobertura_ok & abs(Cc[ut]) >= LIMIAR_CORR
  if (!any(keep)) return(list(pares = NULL, n_testes = n_testes_ativo))
  ut <- ut[keep, , drop = FALSE]

  pares <- data.table(
    ativo = av,
    fundo_a = fundos[ut[,1]], fundo_b = fundos[ut[,2]],
    n_meses_contemp = n_overlap[ut],
    corr_contemp = Cc[ut]
  )
  list(pares = pares, n_testes = n_testes_ativo)
}

t0 <- Sys.time()
resultados <- vector("list", length(ativos_alvo))
n_testes_vec <- integer(length(ativos_alvo))
for (i in seq_along(ativos_alvo)) {
  r <- tryCatch(processa_ativo(ativos_alvo[i]), error = function(e) NULL)
  if (!is.null(r)) { resultados[[i]] <- r$pares; n_testes_vec[i] <- r$n_testes }
  if (i %% 25 == 0) cat("...", i, "de", length(ativos_alvo), "ativos |",
                         round(as.numeric(Sys.time()-t0, units="secs")), "s\n")
}
cat("Tempo total:", round(as.numeric(Sys.time()-t0, units="mins"),1), "min\n")

R <- rbindlist(resultados, use.names = TRUE)
n_testes_total <- sum(n_testes_vec)
cat("\n===== (demeaned) Pares com >=", MIN_MESES, "meses em comum E |corr|>=", LIMIAR_CORR, ":", nrow(R), "\n")
cat("Numero total de testes (pares com cobertura minima, antes do corte de correlacao):", n_testes_total, "\n")
cat("Comparar com m=31.907.281 (script 63/64, dado bruto, sem demeaning) -- deve ser IGUAL",
    "(demeaning nao muda quais pares tem cobertura minima, so' os valores de correlacao)\n")

R[, gestora_a := gest_lookup[fundo_a]]
R[, gestora_b := gest_lookup[fundo_b]]
cat("\n% de pares pre-filtrados (|r|>=0,6) com correlacao POSITIVA (demeaned):",
    round(100*mean(R$corr_contemp > 0), 1), "%\n")

# ---- Benjamini-Hochberg, mesma logica do R/64, m = n_testes_total daqui ----
pval_corr <- function(r, n) {
  t <- r * sqrt(n - 2) / sqrt(1 - r^2)
  2 * pt(-abs(t), df = n - 2)
}
R[, pval := pval_corr(corr_contemp, n_meses_contemp)]
setorder(R, pval)
R[, rank_pval := .I]
R[, p_bh := pmin(1, pval * n_testes_total / rank_pval)]
R[, p_bh := rev(cummin(rev(p_bh)))]

sig <- R[p_bh < 0.05]
cat("\n===== RESULTADO PRINCIPAL DA ROBUSTEZ =====\n")
cat("Pares significativos (p_bh<0,05) SEM demeaning (original, R/64):  1.248.772\n")
cat("Pares significativos (p_bh<0,05) COM demeaning (este script):   ", nrow(sig), "\n")
cat("Dos significativos COM demeaning, % com correlacao positiva:",
    round(100*mean(sig$corr_contemp > 0), 1), "%\n")
cat("(original, sem demeaning, % positivo entre TODOS os pre-filtrados |r|>=0,6, nao so' significativos: conferir tab:pares)\n")

fwrite(R, file.path(REPO, "v2 OFICIAL/data/pares_replicantes_bruto_DEMEANED.csv"))
fwrite(sig, file.path(REPO, "v2 OFICIAL/data/pares_replicantes_significativos_DEMEANED.csv"))
cat("\nOK - salvo (arquivos com sufixo _DEMEANED, nao sobrescreve o pipeline original)\n")
