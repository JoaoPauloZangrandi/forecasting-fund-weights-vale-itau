# =============================================================================
# 33_erro_por_gestora.R  (v2 OFICIAL)
#
# Item 5 do plano: estudo do erro e (residuo da cross-section, Etapa 1) POR
# GESTORA. Reestima a mesma regressao mensal com efeito-fixo de gestora do
# R/20 (MQO), mas agora guardando o residuo (erro = peso_vale3 - ajustado)
# de CADA fundo-mes, com a gestora correspondente -- para depois agregar
# media/dp/histograma/evolucao por gestora.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_final.csv"))
cat("Painel (mesma amostra da Secao 5):", nrow(d), "linhas\n")

z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
meses <- sort(unique(d$ym))

out <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  dt[, gestora_grupo := droplevels(factor(gestora_grupo))]
  if (nrow(dt) < 60 || nlevels(dt$gestora_grupo) < 2) next
  dt[, gestora_grupo := relevel(gestora_grupo, ref = "Itau")]
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)]
  f <- tryCatch(lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + gestora_grupo, data = dt),
                error = function(e) NULL)
  if (is.null(f)) next
  out[[i]] <- data.table(cod_fundo = dt$cod_fundo, ym = t, gestora_grupo = as.character(dt$gestora_grupo),
                          peso_vale3 = dt$peso_vale3, peso_pred = fitted(f), erro = residuals(f))
}
E <- rbindlist(out)
cat("Erro e (com FE de gestora): n =", nrow(E), "| media =", round(mean(E$erro), 8),
    "| dp =", round(sd(E$erro), 6), "\n")
fwrite(E, file.path(REPO, "v2 OFICIAL/data/erro_e_por_gestora.csv"))

# ---- agregado por gestora ---------------------------------------------------
agg <- E[, .(n_obs = .N, n_meses = uniqueN(ym), media = mean(erro), dp = sd(erro)), by = gestora_grupo]
agg[, t := media / (dp / sqrt(n_meses))]
agg[, sig := ifelse(abs(t) > 3.29, "***", ifelse(abs(t) > 2.58, "**", ifelse(abs(t) > 1.96, "*", "n.s.")))]
setorder(agg, media)
cat("\n===== Erro medio por gestora (ordenado) =====\n")
print(agg[, .(gestora_grupo, n_obs, n_meses, media = round(media, 5), dp = round(dp, 5), t = round(t, 2), sig)])

fwrite(agg, file.path(REPO, "v2 OFICIAL/data/erro_e_gestora_resumo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/erro_e_por_gestora.csv' e 'erro_e_gestora_resumo.csv'\n")
