# =============================================================================
# 63_pares_replicantes.R  (v2 OFICIAL -- teste_estrategia)  -- v2, corrigido
#
# FASE 4: deteccao de pares replicantes -- pra cada ativo, testa TODOS os
# pares de fundos que o carregam DE VERDADE, correlacionando a serie de
# erro (u, ajuste parcial COM correcao mecanica, lambda amostra inteira =
# 0,0665, Fase 2) mes a mes. Duas formas: contemporanea e defasada.
#
# CORRECAO v2 (30/07/2026): a v1 rodou sem filtro de posicao-poeira e o
# resultado ficou dominado pelo MESMO artefato ja visto varias vezes nesta
# sessao -- confirmado manualmente (fundo 331333 em AZEV4, peso~1,8e-11,
# 'u' na mesma escala minusscula, correlacao espuria de exatamente 1,0 com
# 10+ fundos diferentes). 63% das celulas (fundo,ativo) do painel inteiro
# sao posicao-poeira (peso mediano <0,1%) -- filtro aplicado ANTES de
# montar a matriz de correlacao por ativo, mesmo limiar de sempre.
#
# CORRECAO v2 tambem no processo: filtro de correlacao (|corr_contemp|>0,6)
# aplicado DENTRO do loop, antes de acumular -- a v1 escreveu 87M linhas
# (11GB) sem filtro nenhum, quase estourou o disco. Aprendizado: filtrar
# cedo, nao depois.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
OUT <- file.path(REPO, "v2 OFICIAL/data/pares_replicantes_bruto.csv")
LIMIAR_CORR <- 0.6

M <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_universo_completo_h1.csv"))
M[, cod_fundo := as.character(cod_fundo)]
lam <- 0.0444  # atualizado 16/08/2026 apos corrigir retorno_fundo_mensal.csv (estava sem cobertura 2022-2026, dw_corrigido ficava NA e truncava a amostra em 202111; era 0.0690)
M[, u := dw_corrigido - lam * d]
M <- M[!is.na(u)]
M[, mes_idx := (ym %/% 100L - 2016L) * 12L + (ym %% 100L)]
cat("Base de erro (u), antes do filtro de posicao-poeira:", nrow(M), "obs\n")

# ---- filtro de posicao-poeira: peso mediano da celula (fundo,ativo) >=0,1% ----
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"))
pp[, cod_fundo := as.character(cod_fundo)]
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
cel_ok <- cel[peso_mediano >= 0.001, .(cod_fundo, ativo)]
M <- merge(M, cel_ok, by = c("cod_fundo","ativo"))
cat("Apos filtro de posicao-poeira (peso mediano celula >=0,1%):", nrow(M), "obs |", uniqueN(M$ativo), "ativos\n")

mapa <- fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
mapa[, cod_fundo := as.character(cod_fundo)]
gest_lookup <- setNames(mapa$gestora_grupo, mapa$cod_fundo)

MIN_MESES <- 12L
idx_min <- min(M$mes_idx); idx_max <- max(M$mes_idx)
todos_idx <- idx_min:idx_max

por_ativo_n <- M[, .(n_fundos = uniqueN(cod_fundo)), by = ativo]
ativos_alvo <- por_ativo_n[n_fundos >= 2]$ativo
cat("Ativos com >=2 fundos (apos filtro):", length(ativos_alvo), "\n")

processa_ativo <- function(av) {
  dm <- M[ativo == av, .(cod_fundo, mes_idx, u)]
  fundos <- unique(dm$cod_fundo)
  n_f <- length(fundos)
  if (n_f < 2) return(NULL)

  W <- matrix(NA_real_, nrow = length(todos_idx), ncol = n_f, dimnames = list(as.character(todos_idx), fundos))
  W[cbind(match(dm$mes_idx, todos_idx), match(dm$cod_fundo, fundos))] <- dm$u

  ok_col <- colSums(!is.na(W)) >= MIN_MESES
  if (sum(ok_col) < 2) return(NULL)
  W <- W[, ok_col, drop = FALSE]
  fundos <- colnames(W)
  n_f <- ncol(W)

  I <- !is.na(W); storage.mode(I) <- "double"
  n_overlap <- t(I) %*% I
  Cc <- suppressWarnings(cor(W, use = "pairwise.complete.obs"))

  Wl <- W[1:(nrow(W)-1), , drop=FALSE]
  Wf <- W[2:nrow(W), , drop=FALSE]
  Il <- !is.na(Wl); storage.mode(Il) <- "double"
  If <- !is.na(Wf); storage.mode(If) <- "double"
  n_overlap_lag <- t(Il) %*% If
  Cl <- suppressWarnings(cor(Wl, Wf, use = "pairwise.complete.obs"))

  ut <- which(upper.tri(n_overlap), arr.ind = TRUE)
  # filtro DUPLO aqui: cobertura minima E |corr|>=LIMIAR -- poupa memoria/disco
  keep <- n_overlap[ut] >= MIN_MESES & abs(Cc[ut]) >= LIMIAR_CORR
  if (!any(keep)) return(NULL)
  ut <- ut[keep, , drop = FALSE]

  data.table(
    ativo = av,
    fundo_a = fundos[ut[,1]], fundo_b = fundos[ut[,2]],
    n_meses_contemp = n_overlap[ut],
    corr_contemp = Cc[ut],
    n_meses_lag = pmin(n_overlap_lag[ut], n_overlap_lag[ut[,2:1,drop=FALSE]]),
    corr_a_lidera = Cl[ut],
    corr_b_lidera = Cl[ut[,2:1,drop=FALSE]]
  )
}

t0 <- Sys.time()
resultados <- vector("list", length(ativos_alvo))
for (i in seq_along(ativos_alvo)) {
  resultados[[i]] <- tryCatch(processa_ativo(ativos_alvo[i]), error = function(e) NULL)
  if (i %% 25 == 0) cat("...", i, "de", length(ativos_alvo), "ativos |",
                         round(as.numeric(Sys.time()-t0, units="secs")), "s\n")
}
cat("Tempo total:", round(as.numeric(Sys.time()-t0, units="mins"),1), "min\n")

R <- rbindlist(resultados, use.names = TRUE)
cat("\nPares com >=", MIN_MESES, "meses em comum E |corr_contemp|>=", LIMIAR_CORR, ":", nrow(R), "\n")

R[, gestora_a := gest_lookup[fundo_a]]
R[, gestora_b := gest_lookup[fundo_b]]
R[, mesma_gestora := gestora_a == gestora_b]

fwrite(R, OUT)
cat("\nOK - salvo em '", OUT, "' (", nrow(R), " pares)\n", sep="")
