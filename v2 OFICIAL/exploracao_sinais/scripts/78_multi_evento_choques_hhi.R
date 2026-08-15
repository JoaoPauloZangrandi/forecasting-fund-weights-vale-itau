# =============================================================================
# 78_multi_evento_choques_hhi.R  (exploracao_sinais, agente eventos)
#
# Replica o desenho do candidato #40 (LOG_CANDIDATOS.md, script
# 40_covid_event_study.R) -- HHI de posse no mes ANTERIOR ao choque prevendo
# retorno DURANTE o choque, retorno na recuperacao (3 meses seguintes) e
# volatilidade pos-choque (6 meses seguintes) -- em MULTIPLOS choques de
# mercado brasileiro fora da janela COVID (ja testada e sem sinal).
#
# Eventos escolhidos (script 77 identificou os piores meses 2016-2019 a
# partir do proprio proxy de retorno agregado de mercado, restrito a meses
# com HHI pre-evento disponivel -- painel comeca em 201701):
#   1. 201705 - "Joesley Day" (delacao JBS, 18/05/2017) -- mandatado
#   2. 201805 - Greve dos caminhoneiros -- mandatado, tambem o PIOR mes
#      de 2016-2019 no proprio proxy de mercado equal-weight
#   3. 201711 - pior mes organico adicional, temporalmente afastado dos
#      2 anteriores (evita sobreposicao de janela pos-choque)
#   4. 201904 - pior mes organico adicional, tambem afastado
#
# Inferencia: (a) regressao cross-sectional separada por evento (reportada
# individualmente, nenhum resultado escondido); (b) "mini Fama-MacBeth"
# entre eventos -- media dos coeficientes dos 4 eventos / erro-padrao entre
# eventos, t com df=3 (eventos como unidade, nao ticker-evento -- evita o
# erro classico de tratar ticker-evento como observacao independente);
# (c) regressao pooled com efeito fixo de evento e erro-padrao clusterizado
# por evento (mais N mas so 4 clusters -- reportado como checagem adicional,
# nao como inferencia principal, dado que <30-40 clusters e conhecido por
# ser pouco confiavel para cluster-robust SE assintotico).
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(sandwich)
  library(lmtest)
})
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
precos <- precos[is.finite(retorno)]

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

eventos <- data.table(
  nome = c("Joesley Day (mai/2017)", "Greve caminhoneiros (mai/2018)",
           "Choque organico nov/2017", "Choque organico abr/2019"),
  mes_evento = c(201705L, 201805L, 201711L, 201904L)
)
eventos[, mes_pre := addm(mes_evento, -1)]
eventos[, mes_pos_ini := addm(mes_evento, 1)]
eventos[, mes_pos3_fim := addm(mes_evento, 3)]
eventos[, mes_pos6_fim := addm(mes_evento, 6)]

cat("===== Eventos testados =====\n")
print(eventos)

construir_evento <- function(nome_ev, mes_evento, mes_pre, mes_pos_ini, mes_pos3_fim, mes_pos6_fim) {
  hhi_pre <- crowd[ym == mes_pre, .(ticker, hhi_posse, n_fundos)]

  ret_evento <- precos[ymk == mes_evento, .(ticker, ret_evento = retorno)]

  meses_pos3 <- seq(mes_pos_ini, mes_pos3_fim)
  # addm gera sequencia certa so se dentro do mesmo ano; construir explicitamente
  m3 <- mes_pos_ini
  seq_pos3 <- mes_pos_ini
  while (tail(seq_pos3,1) < mes_pos3_fim) seq_pos3 <- c(seq_pos3, addm(tail(seq_pos3,1),1))
  seq_pos6 <- mes_pos_ini
  while (tail(seq_pos6,1) < mes_pos6_fim) seq_pos6 <- c(seq_pos6, addm(tail(seq_pos6,1),1))

  ret_pos3 <- precos[ymk %in% seq_pos3, .(ret_pos3 = prod(1+retorno)-1, n_meses_pos3=.N), by=ticker]
  vol_pos6 <- precos[ymk %in% seq_pos6, .(vol_pos6 = sd(retorno), n_meses_pos6=.N), by=ticker]

  ev <- merge(hhi_pre, ret_evento, by="ticker")
  ev <- merge(ev, ret_pos3[n_meses_pos3>=2], by="ticker")
  ev <- merge(ev, vol_pos6[n_meses_pos6>=4], by="ticker")
  ev <- ev[is.finite(hhi_posse) & is.finite(ret_evento) & is.finite(ret_pos3) & is.finite(vol_pos6)]
  ev[, evento := nome_ev]
  ev[, mes_evento_num := mes_evento]
  ev
}

lista_ev <- list()
for (i in 1:nrow(eventos)) {
  e <- eventos[i]
  lista_ev[[i]] <- construir_evento(e$nome, e$mes_evento, e$mes_pre, e$mes_pos_ini, e$mes_pos3_fim, e$mes_pos6_fim)
}
painel_eventos <- rbindlist(lista_ev)
cat(sprintf("\nN total (empilhado, %d eventos): %d\n", nrow(eventos), nrow(painel_eventos)))
print(painel_eventos[, .N, by=evento])

fwrite(painel_eventos, file.path(OUT, "candidatos_78_multi_evento_painel.csv"))

# ---- (a) regressao por evento, separada ----
alvos <- c("ret_evento", "ret_pos3", "vol_pos6")
resultados_evento <- list()
for (ev_nome in unique(painel_eventos$evento)) {
  sub <- painel_eventos[evento == ev_nome]
  cat(sprintf("\n\n========== EVENTO: %s (N=%d) ==========\n", ev_nome, nrow(sub)))
  for (alvo in alvos) {
    fit <- lm(as.formula(paste0(alvo, " ~ hhi_posse")), data = sub)
    s <- summary(fit)$coefficients
    cat(sprintf("  alvo=%-12s coef_hhi=%+9.5f  t=%6.2f  p=%.4f  N=%d\n",
                alvo, s[2,1], s[2,3], s[2,4], nrow(sub)))
    resultados_evento[[length(resultados_evento)+1]] <- data.table(
      evento=ev_nome, alvo=alvo, coef=s[2,1], t=s[2,3], p=s[2,4], n=nrow(sub))
  }
  # tabela por quintil, mesmo estilo do candidato #40
  sub[, quintil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:5/5), include.lowest=TRUE))]
  tab <- sub[, .(n=.N, ret_evento_medio=mean(ret_evento), ret_pos3_medio=mean(ret_pos3),
                 vol_pos6_media=mean(vol_pos6)), by=quintil_hhi][order(quintil_hhi)]
  print(tab)
}
R_evento <- rbindlist(resultados_evento)
fwrite(R_evento, file.path(OUT, "candidatos_78_resultados_por_evento.csv"))

# ---- (b) mini Fama-MacBeth entre eventos (n=4, eventos como unidade) ----
cat("\n\n===== (b) MINI FAMA-MACBETH ENTRE EVENTOS (n=4 eventos, df=3) =====\n")
cat("Combina os 4 coeficientes cross-sectionais (1 por evento) como se fossem\n")
cat("a serie temporal do Fama-MacBeth padrao -- evento e a unidade independente,\n")
cat("nao ticker-evento. Poder estatistico muito baixo (n=4), reportado com essa ressalva.\n\n")
resumo_fm <- list()
for (alvo_sel in alvos) {
  coefs <- as.data.frame(R_evento)[as.data.frame(R_evento)$alvo == alvo_sel, "coef"]
  media <- mean(coefs); dp <- sd(coefs); n_ev <- length(coefs)
  t_fm <- media/(dp/sqrt(n_ev)); p_fm <- 2*pt(-abs(t_fm), df=n_ev-1)
  cat(sprintf("alvo=%-12s coefs_por_evento=[%s]  media=%+.5f  dp=%.5f  t=%6.2f  p=%.4f\n",
              alvo_sel, paste(sprintf("%.4f",coefs),collapse=", "), media, dp, t_fm, p_fm))
  resumo_fm[[length(resumo_fm)+1]] <- data.table(alvo=alvo_sel, media_coef=media, dp_coef=dp, n_eventos=n_ev, t=t_fm, p=p_fm)
}
R_fm <- rbindlist(resumo_fm)
fwrite(R_fm, file.path(OUT, "candidatos_78_mini_famamacbeth.csv"))

# ---- (c) pooled com efeito fixo de evento + cluster-robust por evento (checagem adicional) ----
cat("\n\n===== (c) POOLED com efeito fixo de evento + erro clusterizado por evento =====\n")
cat("(checagem de robustez adicional -- so 4 clusters, cluster-robust SE assintotico\n")
cat(" nao e confiavel com tao poucos clusters; reportado por transparencia, nao como\n")
cat(" evidencia principal)\n\n")
resumo_pooled <- list()
for (alvo in alvos) {
  fit <- lm(as.formula(paste0(alvo, " ~ hhi_posse + factor(evento)")), data = painel_eventos)
  vcov_cl <- vcovCL(fit, cluster = painel_eventos$evento, type = "HC1")
  ct <- coeftest(fit, vcov. = vcov_cl)
  linha_hhi <- ct["hhi_posse", ]
  cat(sprintf("alvo=%-12s coef_hhi=%+9.5f  t_cluster=%6.2f  p_cluster=%.4f  N_total=%d\n",
              alvo, linha_hhi["Estimate"], linha_hhi["t value"], linha_hhi["Pr(>|t|)"], nrow(painel_eventos)))
  resumo_pooled[[length(resumo_pooled)+1]] <- data.table(
    alvo=alvo, coef=linha_hhi["Estimate"], t_cluster=linha_hhi["t value"],
    p_cluster=linha_hhi["Pr(>|t|)"], n=nrow(painel_eventos))
}
R_pooled <- rbindlist(resumo_pooled)
fwrite(R_pooled, file.path(OUT, "candidatos_78_pooled_cluster.csv"))

cat("\n\nOK\n")
