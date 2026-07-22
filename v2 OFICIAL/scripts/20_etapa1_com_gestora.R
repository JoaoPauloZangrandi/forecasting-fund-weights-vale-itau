# =============================================================================
# 20_etapa1_com_gestora.R  (v2 OFICIAL)
#
# LOGIT (nao mais linear -- decisao do Joao: logit e o modelo mae em todo o
# documento). Junta o painel de todas as gestoras (features predeterminadas
# + beta do fundo) e roda a cross-section LOGISTICA com EFEITO FIXO DE
# GESTORA (40 dummies, Itau como grupo de referencia, o maior) somado as 5
# caracteristicas ja usadas na Etapa 1. Inclui APE (efeito marginal medio)
# para interpretar os 5 coeficientes em pontos percentuais.
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

# ---- LOGIT COM efeito fixo de gestora, mes a mes -----------------------------
out <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  dt[, gestora_grupo := droplevels(gestora_grupo)]
  if (nrow(dt) < 60 || nlevels(dt$gestora_grupo) < 2) next
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)]
  f <- tryCatch(glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + gestora_grupo,
                     family = quasibinomial(link="logit"), data = dt), error = function(e) NULL)
  if (is.null(f)) next
  cf <- coef(f)
  pr2 <- 1 - f$deviance/f$null.deviance
  z_pred <- predict(f, type = "link"); dg <- dlogis(z_pred)
  ape_aum <- cf["z_aum"]*mean(dg); ape_cot <- cf["z_cot"]*mean(dg); ape_flow <- cf["flow_aum"]*mean(dg)
  ape_betaf <- cf["z_betaf"]*mean(dg)
  z0 <- z_pred - cf["is_fic"]*dt$is_fic; z1 <- z0 + cf["is_fic"]
  ape_fic <- mean(plogis(z1) - plogis(z0))
  out[[i]] <- data.table(ym = t, n_fundos = nrow(dt), n_gestoras = nlevels(dt$gestora_grupo),
                          alpha = cf["(Intercept)"], b_aum = cf["z_aum"], b_cot = cf["z_cot"],
                          b_fic = cf["is_fic"], b_flow = cf["flow_aum"], b_betaf = cf["z_betaf"],
                          pr2 = pr2, ape_aum = ape_aum, ape_cot = ape_cot, ape_fic = ape_fic,
                          ape_flow = ape_flow, ape_betaf = ape_betaf)
}
theta_fe <- rbindlist(out)

cat("===== Theta_t LOGIT (COM efeito-fixo de gestora): media, dp, t, sig (caracteristicas) =====\n")
n_meses_fe <- nrow(theta_fe)  # NAO usar .N depois de criar resumo (viraria nrow(resumo)=6 -- bug
                               # ja encontrado e corrigido)
resumo <- theta_fe[, .(
  media = c(mean(alpha),mean(b_aum),mean(b_cot),mean(b_fic),mean(b_flow),mean(b_betaf)),
  dp    = c(sd(alpha),sd(b_aum),sd(b_cot),sd(b_fic),sd(b_flow),sd(b_betaf))
)]
resumo[, variavel := c("alpha","b_aum","b_cot","b_fic","b_flow","b_betaf")]
resumo[, t := media/(dp/sqrt(n_meses_fe))]
resumo[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo,"variavel")
print(resumo[, .(variavel, media=round(media,5), t=round(t,2), sig)])
cat("Pseudo-R2 (McFadden) medio:", round(mean(theta_fe$pr2, na.rm=TRUE),4), "\n")
cat("N medio de fundos/mes:", round(mean(theta_fe$n_fundos),1), "| gestoras/mes: min",
    min(theta_fe$n_gestoras), "max", max(theta_fe$n_gestoras), "\n")

resumo_ape <- theta_fe[, .(
  media = c(mean(ape_aum), mean(ape_cot), mean(ape_fic), mean(ape_flow), mean(ape_betaf)),
  dp    = c(sd(ape_aum), sd(ape_cot), sd(ape_fic), sd(ape_flow), sd(ape_betaf))
)]
resumo_ape[, variavel := c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf")]
resumo_ape[, t := media/(dp/sqrt(n_meses_fe))]
resumo_ape[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo_ape, "variavel")
cat("\n===== APE, COM efeito de gestora =====\n")
print(resumo_ape[, .(variavel, media=round(media,5), t=round(t,2), sig)])

fwrite(theta_fe, file.path(REPO, "v2 OFICIAL/data/theta_com_gestora.csv"))
fwrite(d, file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_final.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/theta_com_gestora.csv' e 'painel_todas_gestoras_final.csv'\n")
