# =============================================================================
# 45_best_ideas_conviccao.R  (exploracao_sinais)
#
# Angulo (e) pedido pelo Joao: ideia direta da literatura de "smart money"/
# "skilled managers" ainda nao testada -- Cohen, Polk & Silli (2010, JF,
# "Best Ideas"): a maior posicao ATIVA de um fundo (a acao que ele mais
# "aposta", nao so' o maior peso bruto) tende a performar melhor que o
# resto da carteira -- interpretacao: gestores tem convicao heterogenea
# entre suas proprias posicoes, e a convicao revela informacao real.
#
# Duas versoes testadas aqui:
#   1) NIVEL (replica proxima do desenho original): BEST IDEAS SCORE por
#      acao-mes = soma do AUM dos fundos para os quais essa acao e' uma
#      das TOP-3 posicoes por peso naquele mes (proxy de "quantos gestores
#      apostam pesado nessa acao"), normalizado pelo AUM total do universo
#      no mes. Sem dado de "active share" verdadeiro (exigiria benchmark
#      por fundo, que nao temos) -- top-K por peso e' a proxy padrao usada
#      em replicacoes quando active share nao esta disponivel.
#   2) DINAMICA (extensao NOVA, nao esta no paper original): entre os
#      fundos para os quais a acao JA e' uma top-3 posicao, a CONVICCAO
#      esta aumentando ou diminuindo -- media ponderada por AUM da variacao
#      de peso (peso_t - peso_{t-1}) apenas nessas relacoes fundo-acao de
#      "melhor ideia". Hipotese: aumentar aposta numa posicao ja de alta
#      convicao e' um sinal de informacao mais forte que so' abrir posicao
#      nova ou que mudanca de peso em posicoes marginais (que ja foi
#      testado, sem sucesso, no fluxo/HM_sinal de candidatos anteriores).
#
# METODOLOGIA (as 3 licoes inegociaveis): Fama-MacBeth de verdade; quintil
# RE-CORTADO A CADA MES (metodo B); teste em ym>=202001 (treino so' serve
# de referencia/comparacao, nao fixa nenhum corte usado no teste).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp <- pp[ticker %in% unique(precos$ticker)]

# rank de peso DENTRO de cada fundo-mes (1 = maior posicao do fundo naquele mes)
pp[, rank_peso := frank(-peso, ties.method = "min"), by = .(cod_fundo, ym)]
pp[, top3 := rank_peso <= 3]
cat("Linhas fundo-ativo-mes:", nrow(pp), "| marcadas como top-3 do fundo:", sum(pp$top3), "\n")

# =============================================================================
# SINAL 1: BEST IDEAS (nivel) -- AUM dos fundos que tem essa acao como top-3,
# como fracao do AUM total do universo naquele mes (capital "de alta conviccao"
# alocado nessa acao)
# =============================================================================
aum_total_mes <- unique(pp[, .(cod_fundo, ym, aum_prev)])[, .(aum_total = sum(aum_prev)), by = ym]
bi_nivel <- pp[top3 == TRUE, .(aum_topideia = sum(aum_prev), n_fundos_topideia = .N), by = .(ticker, ym)]
bi_nivel <- merge(bi_nivel, aum_total_mes, by = "ym")
bi_nivel[, best_ideas_score := aum_topideia / aum_total]
cat("\nTicker-mes com Best Ideas score (nivel):", nrow(bi_nivel), "\n")
cat("Resumo best_ideas_score:\n"); print(summary(bi_nivel$best_ideas_score))
cat("Resumo n_fundos_topideia:\n"); print(summary(bi_nivel$n_fundos_topideia))

# =============================================================================
# SINAL 2: CONVICCAO CRESCENTE (dinamico) -- entre relacoes fundo-acao que
# JA sao top-3 do fundo neste mes, delta de peso ponderado por AUM
# =============================================================================
peso_prev <- pp[, .(cod_fundo, ticker, ym = addm(ym, 1), peso_prev = peso)]
d2 <- merge(pp[top3 == TRUE], peso_prev, by = c("cod_fundo","ticker","ym"))
d2 <- d2[is.finite(peso_prev)]
d2[, delta_peso := peso - peso_prev]
conviccao <- d2[, .(conviccao_delta = weighted.mean(delta_peso, aum_prev), n_relacoes = .N,
                     aum_relacoes = sum(aum_prev)), by = .(ticker, ym)]
conviccao <- conviccao[n_relacoes >= 5]
cat("\nTicker-mes com sinal de conviccao dinamica (n_relacoes>=5):", nrow(conviccao), "\n")
cat("Resumo conviccao_delta:\n"); print(summary(conviccao$conviccao_delta))

fama_macbeth_recut <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[ym >= CORTE & is.finite(get(var_x)) & is.finite(get(var_y))])
  if (nrow(teste) < 30) return(NULL)
  teste[, quintil := {
    qs <- quantile(get(var_x), 0:5/5, na.rm = TRUE)
    if (length(unique(qs)) < 6) as.integer(NA) else as.integer(cut(get(var_x), qs, include.lowest = TRUE))
  }, by = ym]
  teste <- teste[!is.na(quintil)]
  chk <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  n_min <- if (nrow(chk) > 0) min(chk$N) else 0
  por_mes <- teste[quintil %in% c(1,5), .(rm = mean(get(var_y)), n = .N), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "rm")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
  sh <- media/dp*sqrt(12)
  cat(sprintf("%-32s h=%d %2d meses n_min=%3d spread=%+7.2fpp/mes (%+6.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()

cat("\n===== SINAL 1: Best Ideas (nivel) -> retorno futuro =====\n")
for (h in c(1,3,6)) {
  m <- copy(bi_nivel); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "best_ideas_score", "retorno", "Best Ideas (nivel)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

cat("\n===== SINAL 2: Conviccao crescente (dinamico, so' entre top-3) -> retorno futuro =====\n")
for (h in c(1,3,6)) {
  m <- copy(conviccao); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  r <- fama_macbeth_recut(m, "conviccao_delta", "retorno", "Conviccao crescente (top-3)", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

# ---- combinacao: nivel ALTO (ja e' best idea de muita gente) E conviccao crescente juntos ----
cat("\n===== SINAL 3: interacao -- Conviccao crescente, SO' em acoes que ja sao Best Ideas 'populares' =====\n")
base3 <- merge(conviccao, bi_nivel[, .(ticker, ym, best_ideas_score)], by = c("ticker","ym"))
for (h in c(1,3,6)) {
  m <- copy(base3); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[ym >= CORTE & is.finite(retorno)]
  m[, mediana_bi := median(best_ideas_score), by = ym]
  popular <- m[best_ideas_score >= mediana_bi]
  r <- fama_macbeth_recut(popular, "conviccao_delta", "retorno", "Conviccao crescente|Best-Idea POPULAR", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_45_best_ideas_conviccao.csv"))
cat("\n\n===== RESUMO (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
