# =============================================================================
# 23_etapa1_gestora_antes_depois.R  (v2 OFICIAL)
#
# LOGIT (nao mais linear -- decisao do Joao: logit e o modelo mae em todo o
# documento). Roda a mesma regressao logistica (5 caracteristicas) na
# amostra de TODAS as gestoras, SEM efeito fixo de gestora, para comparar
# com o resultado do R/20 (COM efeito fixo) -- mesma amostra, isola o
# efeito de adicionar as dummies de gestora. Inclui APE (efeito marginal
# medio) para interpretar em pontos percentuais.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_final.csv"))
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
meses <- sort(unique(d$ym))

out <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  if (nrow(dt) < 60) next
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)]
  f <- tryCatch(glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf,
                     family = quasibinomial(link="logit"), data = dt), error = function(e) NULL)
  if (is.null(f)) next
  cf <- coef(f)
  pr2 <- 1 - f$deviance/f$null.deviance
  z_pred <- predict(f, type = "link"); dg <- dlogis(z_pred)
  ape_aum <- cf["z_aum"]*mean(dg); ape_cot <- cf["z_cot"]*mean(dg); ape_flow <- cf["flow_aum"]*mean(dg)
  ape_betaf <- cf["z_betaf"]*mean(dg)
  z0 <- z_pred - cf["is_fic"]*dt$is_fic; z1 <- z0 + cf["is_fic"]
  ape_fic <- mean(plogis(z1) - plogis(z0))
  out[[i]] <- data.table(ym = t, n_fundos = nrow(dt), alpha = cf["(Intercept)"],
                          b_aum = cf["z_aum"], b_cot = cf["z_cot"], b_fic = cf["is_fic"],
                          b_flow = cf["flow_aum"], b_betaf = cf["z_betaf"], pr2 = pr2,
                          ape_aum = ape_aum, ape_cot = ape_cot, ape_fic = ape_fic,
                          ape_flow = ape_flow, ape_betaf = ape_betaf)
}
theta_sem <- rbindlist(out)

cat("===== SEM efeito de gestora, LOGIT (amostra de todas as gestoras) =====\n")
n_meses_sem <- nrow(theta_sem)  # NAO usar .N depois de criar resumo (viraria nrow(resumo)=6 -- bug
                                 # ja encontrado e corrigido)
resumo <- theta_sem[, .(
  media = c(mean(alpha),mean(b_aum),mean(b_cot),mean(b_fic),mean(b_flow),mean(b_betaf)),
  dp    = c(sd(alpha),sd(b_aum),sd(b_cot),sd(b_fic),sd(b_flow),sd(b_betaf))
)]
resumo[, variavel := c("alpha","b_aum","b_cot","b_fic","b_flow","b_betaf")]
resumo[, t := media/(dp/sqrt(n_meses_sem))]
resumo[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo,"variavel")
print(resumo[, .(variavel, media=round(media,5), t=round(t,2), sig)])
cat("Pseudo-R2 (McFadden) medio:", round(mean(theta_sem$pr2),4), "| N medio:", round(mean(theta_sem$n_fundos),1), "\n")

resumo_ape <- theta_sem[, .(
  media = c(mean(ape_aum), mean(ape_cot), mean(ape_fic), mean(ape_flow), mean(ape_betaf)),
  dp    = c(sd(ape_aum), sd(ape_cot), sd(ape_fic), sd(ape_flow), sd(ape_betaf))
)]
resumo_ape[, variavel := c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf")]
resumo_ape[, t := media/(dp/sqrt(n_meses_sem))]
resumo_ape[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo_ape, "variavel")
cat("\n===== APE, SEM efeito de gestora =====\n")
print(resumo_ape[, .(variavel, media=round(media,5), t=round(t,2), sig)])

fwrite(theta_sem, file.path(REPO, "v2 OFICIAL/data/theta_todas_gestoras_sem_fe.csv"))
cat("\nOK - salvo\n")
