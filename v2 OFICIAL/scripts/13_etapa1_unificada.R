# =============================================================================
# 13_etapa1_unificada.R  (v2 OFICIAL)
#
# ETAPA 1 (versao unificada): cross-section MQO + logit, mes a mes, com as
# CINCO caracteristicas -- AUM, cotistas, FIC, fluxo e beta do fundo -- todas
# medidas no ULTIMO DIA DO MES ANTERIOR (predeterminadas, sem look-ahead),
# usando o painel construido no R/12.
#
# Substitui/unifica os scripts 01 (so MQO, so 4 caracteristicas, timing
# contemporaneo) e 02 (logit reestimado em cima do resultado do 01) da
# primeira versao do v2 OFICIAL.
#
#   peso_{i,t} = a_t + b1_t*z(ln AUM_{t-1}) + b2_t*z(ln cotistas_{t-1})
#              + b3_t*FIC_i + b4_t*(fluxo/AUM)_{t-1} + b5_t*z(beta_fundo_{t-1})
#              + erro_{i,t}                                    [MQO linear]
#
#   peso_{i,t} = g(x_{i,t}' theta_t) + e_{i,t}                  [logit]
#
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_predeterminado.csv"))
d[, l_aum := log(aum_prev)]
d[, l_cot := log(cotistas_prev)]
d[, flow_aum := fluxo_prev / aum_prev]
d <- d[is.finite(l_aum) & is.finite(l_cot) & is.finite(flow_aum) & is.finite(beta_fundo)]
cat("Fundo-meses com as 5 caracteristicas completas (predeterminadas):", nrow(d),
    "de 11.532 originais\n")

z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
meses <- sort(unique(d$ym))
cat("Meses cobertos:", length(meses), "(", min(meses), "-", max(meses), ")\n\n")

# ---- MQO linear, mes a mes ---------------------------------------------------
out_lin <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  if (nrow(dt) < 15) next
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)]
  f <- lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf, data = dt)
  cf <- coef(f)
  out_lin[[i]] <- data.table(ym = t, n_fundos = nrow(dt),
                              alpha = cf["(Intercept)"], b_aum = cf["z_aum"],
                              b_cot = cf["z_cot"], b_fic = cf["is_fic"],
                              b_flow = cf["flow_aum"], b_betaf = cf["z_betaf"],
                              r2 = summary(f)$r.squared)
}
theta_lin <- rbindlist(out_lin)

cat("===== Theta_t linear: media, dp, min, max (72 meses) =====\n")
n_meses_lin <- nrow(theta_lin)  # NAO usar .N depois de criar resumo_lin (viraria nrow(resumo_lin)=6, o
                                 # numero de VARIAVEIS, nao de meses -- bug ja encontrado e corrigido)
resumo_lin <- theta_lin[, .(
  media = c(mean(alpha), mean(b_aum), mean(b_cot), mean(b_fic), mean(b_flow), mean(b_betaf)),
  dp    = c(sd(alpha), sd(b_aum), sd(b_cot), sd(b_fic), sd(b_flow), sd(b_betaf))
)]
resumo_lin[, variavel := c("alpha","b_aum","b_cot","b_fic","b_flow","b_betaf")]
resumo_lin[, t := media/(dp/sqrt(n_meses_lin))]
resumo_lin[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo_lin, "variavel")
print(resumo_lin[, .(variavel, media=round(media,5), dp=round(dp,5), t=round(t,2), sig)])
cat("R2 medio:", round(mean(theta_lin$r2, na.rm=TRUE),4), "\n\n")

# ---- logit, mes a mes ---------------------------------------------------------
out_log <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  if (nrow(dt) < 15) next
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)]
  f <- glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf,
           family = quasibinomial(link="logit"), data = dt)
  cf <- coef(f)
  pr2 <- 1 - f$deviance/f$null.deviance  # pseudo-R2 de McFadden
  # ---- efeito marginal medio (APE): converte coeficiente logit p/ pontos
  # percentuais. Continuas: theta_k * media(g'(z)) = theta_k*media(dlogis(z)).
  # is_fic (discreta 0/1): diferenca media de g(z) forcando is_fic=1 vs 0
  # p/ cada fundo, mantendo as outras variaveis observadas (efeito de
  # tratamento medio, nao so a derivada) -- mais correto p/ variavel discreta.
  z_pred <- predict(f, type = "link")
  dg <- dlogis(z_pred)
  ape_aum  <- cf["z_aum"]   * mean(dg)
  ape_cot  <- cf["z_cot"]   * mean(dg)
  ape_flow <- cf["flow_aum"]* mean(dg)
  ape_betaf<- cf["z_betaf"] * mean(dg)
  z0 <- z_pred - cf["is_fic"]*dt$is_fic         # forca is_fic=0 p/ todos
  z1 <- z0 + cf["is_fic"]                        # forca is_fic=1 p/ todos
  ape_fic <- mean(plogis(z1) - plogis(z0))
  out_log[[i]] <- data.table(ym = t, n_fundos = nrow(dt),
                              alpha = cf["(Intercept)"], b_aum = cf["z_aum"],
                              b_cot = cf["z_cot"], b_fic = cf["is_fic"],
                              b_flow = cf["flow_aum"], b_betaf = cf["z_betaf"], pr2 = pr2,
                              ape_aum = ape_aum, ape_cot = ape_cot, ape_fic = ape_fic,
                              ape_flow = ape_flow, ape_betaf = ape_betaf)
}
theta_log <- rbindlist(out_log)

cat("===== Theta_t logit: media, dp, min, max (72 meses) =====\n")
n_meses_log <- nrow(theta_log)
resumo_log <- theta_log[, .(
  media = c(mean(alpha), mean(b_aum), mean(b_cot), mean(b_fic), mean(b_flow), mean(b_betaf)),
  dp    = c(sd(alpha), sd(b_aum), sd(b_cot), sd(b_fic), sd(b_flow), sd(b_betaf))
)]
resumo_log[, variavel := c("alpha","b_aum","b_cot","b_fic","b_flow","b_betaf")]
resumo_log[, t := media/(dp/sqrt(n_meses_log))]
resumo_log[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo_log, "variavel")
print(resumo_log[, .(variavel, media=round(media,4), dp=round(dp,4), t=round(t,2), sig)])
cat("Pseudo-R2 (McFadden) medio:", round(mean(theta_log$pr2, na.rm=TRUE),4), "\n\n")

cat("===== Efeito marginal medio (APE, em pontos percentuais de peso) =====\n")
resumo_ape <- theta_log[, .(
  media = c(mean(ape_aum), mean(ape_cot), mean(ape_fic), mean(ape_flow), mean(ape_betaf)),
  dp    = c(sd(ape_aum), sd(ape_cot), sd(ape_fic), sd(ape_flow), sd(ape_betaf))
)]
resumo_ape[, variavel := c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf")]
resumo_ape[, t := media/(dp/sqrt(n_meses_log))]
resumo_ape[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo_ape, "variavel")
print(resumo_ape[, .(variavel, media=round(media,5), t=round(t,2), sig)])

fwrite(theta_lin, file.path(REPO, "v2 OFICIAL/data/theta_mensal_v2.csv"))
fwrite(theta_log, file.path(REPO, "v2 OFICIAL/data/theta_logit_mensal_v2.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/theta_mensal_v2.csv' e 'theta_logit_mensal_v2.csv'\n")
