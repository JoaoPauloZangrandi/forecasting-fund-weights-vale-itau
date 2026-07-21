# =============================================================================
# 20_etapa1_com_gestora.R  (v2 OFICIAL)
#
# Junta o painel de todas as gestoras (features predeterminadas + beta do
# fundo) e roda a cross-section (MQO + logit) com EFEITO FIXO DE GESTORA
# (40 dummies, Itau como grupo de referencia, o maior) somado as 5
# caracteristicas ja usadas na Etapa 1.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_predeterminado.csv"))
bf <- fread(file.path(REPO, "v2 OFICIAL/data/beta_fundo_todas_gestoras.csv"))
d[, ymk_prev := ym_prev]
d <- merge(d, bf, by.x = c("cod_fundo","ymk_prev"), by.y = c("cod_fundo","ymk"), all.x = TRUE)

d[, l_aum := log(aum_prev)]
d[, l_cot := log(cotistas_prev)]
d[, flow_aum := fluxo_prev / aum_prev]
d <- d[is.finite(l_aum) & is.finite(l_cot) & is.finite(flow_aum) & is.finite(beta_fundo)]
cat("Fundo-meses com as 5 caracteristicas completas (todas gestoras):", nrow(d), "\n")

d[, gestora_grupo := factor(gestora_grupo)]
d[, gestora_grupo := relevel(gestora_grupo, ref = "Itau")]
cat("Grupos de gestora nesta amostra:", nlevels(d$gestora_grupo), "\n")

z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
meses <- sort(unique(d$ym))
cat("Meses cobertos:", length(meses), "(", min(meses), "-", max(meses), ")\n\n")

# ---- MQO linear COM efeito fixo de gestora, mes a mes -----------------------
out <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  dt[, gestora_grupo := droplevels(gestora_grupo)]
  if (nrow(dt) < 60 || nlevels(dt$gestora_grupo) < 2) next
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)]
  f <- tryCatch(lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + gestora_grupo, data = dt),
                error = function(e) NULL)
  if (is.null(f)) next
  cf <- coef(f)
  out[[i]] <- data.table(ym = t, n_fundos = nrow(dt), n_gestoras = nlevels(dt$gestora_grupo),
                          alpha = cf["(Intercept)"], b_aum = cf["z_aum"], b_cot = cf["z_cot"],
                          b_fic = cf["is_fic"], b_flow = cf["flow_aum"], b_betaf = cf["z_betaf"],
                          r2 = summary(f)$r.squared)
}
theta_fe <- rbindlist(out)

cat("===== Theta_t (COM efeito-fixo de gestora): media, dp, t, sig (caracteristicas) =====\n")
resumo <- theta_fe[, .(
  media = c(mean(alpha),mean(b_aum),mean(b_cot),mean(b_fic),mean(b_flow),mean(b_betaf)),
  dp    = c(sd(alpha),sd(b_aum),sd(b_cot),sd(b_fic),sd(b_flow),sd(b_betaf))
)]
resumo[, variavel := c("alpha","b_aum","b_cot","b_fic","b_flow","b_betaf")]
resumo[, t := media/(dp/sqrt(.N))]
resumo[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo,"variavel")
print(resumo[, .(variavel, media=round(media,5), t=round(t,2), sig)])
cat("R2 medio:", round(mean(theta_fe$r2, na.rm=TRUE),4), "\n")
cat("N medio de fundos/mes:", round(mean(theta_fe$n_fundos),1), "| gestoras/mes: min",
    min(theta_fe$n_gestoras), "max", max(theta_fe$n_gestoras), "\n")

fwrite(theta_fe, file.path(REPO, "v2 OFICIAL/data/theta_com_gestora.csv"))
fwrite(d, file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_final.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/theta_com_gestora.csv' e 'painel_todas_gestoras_final.csv'\n")
