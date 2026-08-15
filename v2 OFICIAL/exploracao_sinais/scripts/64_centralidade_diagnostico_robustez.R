# =============================================================================
# 64_centralidade_diagnostico_robustez.R  (exploracao_sinais / agente_rede)
#
# Diagnostico do achado mais promissor do script 61: NIVEL de centralidade
# (grau ponderado e autovetor) prediz retorno FUTURO NEGATIVAMENTE, com
# significancia CRESCENTE no horizonte (h=1 n.s. -> h=6 p=0,022 -> h=12
# p=0,049 pro autovetor; padrao parecido, mais fraco, pro grau). Direcao
# BATE com a literatura ja mapeada no log principal (achado CWEC / Kita &
# Zhang: "mais centralidade = retorno MENOR").
#
# LICOES A APLICAR (evitar repetir os quase-falsos-positivos do Comomentum
# e do CIO original):
#   (A) A acao mais central e' quase certamente tambem a acao MAIOR (mais
#       fundos donos, mais R$ investido) -- controlar por tamanho e'
#       essencial antes de acreditar que e' um efeito de REDE, nao de
#       TAMANHO disfarcado (mesmo teste ja aplicado ao CIO Peer Momentum,
#       script 23).
#   (B) Checar se o resultado e' dominado por 1-2 meses extremos (mesmo
#       diagnostico que derrubou o Comomentum).
#   (C) Checar robustez a especificacao (rank em vez de nivel bruto).
#   (D) Contagem de especificacoes ja testada NESTA rodada (agente_rede):
#       script 61 = 16 (8 sinais x horizontes de fato rodados) + script 62
#       = 2 + script 63 = 16 = 34 especificacoes antes deste script. Some
#       ao total do log principal (~470+) para qualquer limiar de Bonferroni
#       honesto.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
painel <- fread(file.path(OUT, "centralidade_panel.csv"))

fama_macbeth_recorte_mensal <- function(m, var_x, var_y, nome, h, retornar_series = FALSE) {
  teste <- copy(m[ym >= CORTE])
  teste <- teste[is.finite(get(var_x)) & is.finite(get(var_y))]
  if (nrow(teste) < 30) return(NULL)
  teste[, quintil := {
    qs <- unique(quantile(get(var_x), 0:5/5, na.rm = TRUE, type = 7))
    if (length(qs) < 3) rep(NA_integer_, .N) else as.integer(cut(get(var_x), qs, include.lowest = TRUE, labels = FALSE))
  }, by = ym]
  teste <- teste[!is.na(quintil)]
  teste[, extremo := ifelse(quintil == min(quintil), "Q1", ifelse(quintil == max(quintil), "Q5", NA_character_)), by = ym]
  n_por_mes <- teste[!is.na(extremo), .N, by = .(ym, extremo)]
  if (nrow(n_por_mes) == 0) return(NULL)
  por_mes <- teste[!is.na(extremo), .(retorno_medio = mean(get(var_y))), by = .(ym, extremo)]
  sm <- dcast(por_mes, ym ~ extremo, value.var = "retorno_medio")
  if (!all(c("Q1","Q5") %in% names(sm))) return(NULL)
  sm <- sm[is.finite(Q1) & is.finite(Q5)]; sm[, spread := Q5 - Q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  n_min <- min(n_por_mes$N); n_mediana <- median(n_por_mes$N)
  cat(sprintf("%-42s h=%2d [FM] %2d meses spread=%+7.3fpp/mes t=%6.2f p=%.4f | N min/mediana=%d/%.0f\n",
              nome, h, n_meses, 100*media, t_fm, p_fm, n_min, n_mediana))
  if (retornar_series) return(sm)
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media,
             t_fm = t_fm, p_fm = p_fm, n_min_grupo_mes = n_min, n_mediana_grupo_mes = n_mediana)
}

# -----------------------------------------------------------------------------
# (A) CONTROLE POR TAMANHO: sinal sobrevive apos remover o efeito de
#     log(valor total detido pelos fundos)? (residuo DENTRO de cada mes,
#     mesma logica do "CIO neutro-tamanho" do script 23)
# -----------------------------------------------------------------------------
cat("===== (A) Controle por tamanho =====\n")
pp0 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","peso","aum_prev"))
pp0 <- pp0[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp0[, valor := peso * aum_prev]
pp0[, ticker := trimws(sub(".*- ", "", ativo))]
tamanho <- pp0[, .(valor_total = sum(valor)), by = .(ticker, ym)]
tamanho[, log_tamanho := log(pmax(valor_total, 1))]

painel_tam <- merge(painel, tamanho[, .(ticker, ym, log_tamanho)], by = c("ticker","ym"))
cat("Correlacao deg_avg x log_tamanho (pooled):", round(cor(painel_tam$deg_avg, painel_tam$log_tamanho, use="complete.obs"),3), "\n")
cat("Correlacao eigen_cent x log_tamanho (pooled):", round(cor(painel_tam$eigen_cent, painel_tam$log_tamanho, use="complete.obs"),3), "\n")

painel_tam[, deg_avg_neutro := residuals(lm(deg_avg ~ log_tamanho)), by = ym]
painel_tam[, eigen_cent_neutro := residuals(lm(eigen_cent ~ log_tamanho)), by = ym]

resultados_A <- list()
for (h in c(1,3,6,12)) {
  m <- copy(painel_tam); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  for (var_x in c("deg_avg","eigen_cent","deg_avg_neutro","eigen_cent_neutro")) {
    mm <- m[is.finite(retorno) & is.finite(get(var_x))]
    r <- fama_macbeth_recorte_mensal(mm, var_x, "retorno", paste0("[A] ", var_x), h)
    if (!is.null(r)) resultados_A[[length(resultados_A)+1]] <- r
  }
}
RA <- rbindlist(resultados_A)

# -----------------------------------------------------------------------------
# (B) DIAGNOSTICO DE COMPOSICAO: quais meses dominam o spread em h=6 e h=12
#     (autovetor, nivel bruto)? Um sinal real nao deveria depender de 1-2
#     meses extremos (licao do Comomentum).
# -----------------------------------------------------------------------------
cat("\n===== (B) Composicao mes-a-mes do spread (autovetor nivel) =====\n")
for (h in c(6,12)) {
  m <- copy(painel); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  mm <- m[is.finite(retorno) & is.finite(eigen_cent)]
  sm <- fama_macbeth_recorte_mensal(mm, "eigen_cent", "retorno", "[B] eigen_cent", h, retornar_series = TRUE)
  setorder(sm, spread)
  cat(sprintf("\nh=%d -- spread por mes (ordenado, mais negativo primeiro):\n", h))
  print(sm[, .(ym, spread_pp = round(100*spread,2))])
  cat(sprintf("Top-2 meses mais negativos somam %.1f%% do total (%.2fpp de %.2fpp)\n",
              100*sum(head(sm$spread,2))/sum(sm$spread), 100*sum(head(sm$spread,2)), 100*sum(sm$spread)))
}

# -----------------------------------------------------------------------------
# (C) ROBUSTEZ DE ESPECIFICACAO: rank percentual (em vez de nivel bruto)
#     dentro do mes -- se o efeito for so' de outliers extremos de nivel, o
#     rank deve enfraquecer bastante.
# -----------------------------------------------------------------------------
cat("\n===== (C) Robustez: rank percentual dentro do mes (em vez de nivel bruto) =====\n")
painel_rank <- copy(painel)
painel_rank[, deg_avg_rank := frank(deg_avg, ties.method = "average")/.N, by = ym]
painel_rank[, eigen_cent_rank := frank(eigen_cent, ties.method = "average")/.N, by = ym]
resultados_C <- list()
for (h in c(1,3,6,12)) {
  m <- copy(painel_rank); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  for (var_x in c("deg_avg_rank","eigen_cent_rank")) {
    mm <- m[is.finite(retorno) & is.finite(get(var_x))]
    r <- fama_macbeth_recorte_mensal(mm, var_x, "retorno", paste0("[C] ", var_x), h)
    if (!is.null(r)) resultados_C[[length(resultados_C)+1]] <- r
  }
}
RC <- rbindlist(resultados_C)

# -----------------------------------------------------------------------------
# (D) Excluindo 2020 (mesmo teste de robustez de periodo ja aplicado ao CIO
#     Peer Momentum no log principal, candidato #15) -- sinal sobrevive so'
#     em 2021+?
# -----------------------------------------------------------------------------
cat("\n===== (D) Excluindo 2020 inteiro (so' 2021+) =====\n")
resultados_D <- list()
for (h in c(6,12)) {
  m <- copy(painel); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  for (var_x in c("deg_avg","eigen_cent")) {
    mm <- m[is.finite(retorno) & is.finite(get(var_x)) & ym >= 202101]
    r <- fama_macbeth_recorte_mensal(mm, var_x, "retorno", paste0("[D sem 2020] ", var_x), h)
    if (!is.null(r)) resultados_D[[length(resultados_D)+1]] <- r
  }
}
RD <- rbindlist(resultados_D)

fwrite(RA, file.path(OUT, "candidatos_64A_controle_tamanho.csv"))
fwrite(RC, file.path(OUT, "candidatos_64C_rank_robustez.csv"))
fwrite(RD, file.path(OUT, "candidatos_64D_sem_2020.csv"))
cat("\nOK\n")
