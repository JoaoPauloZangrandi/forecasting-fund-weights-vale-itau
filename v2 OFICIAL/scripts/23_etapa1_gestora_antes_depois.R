# =============================================================================
# 23_etapa1_gestora_antes_depois.R  (v2 OFICIAL)
#
# Roda a mesma regressao (5 caracteristicas) na amostra de TODAS as
# gestoras, SEM efeito fixo de gestora, para comparar com o resultado do
# R/20 (COM efeito fixo) -- mesma amostra, isola o efeito de adicionar as
# dummies de gestora.
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
  f <- lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf, data = dt)
  cf <- coef(f)
  out[[i]] <- data.table(ym = t, n_fundos = nrow(dt), alpha = cf["(Intercept)"],
                          b_aum = cf["z_aum"], b_cot = cf["z_cot"], b_fic = cf["is_fic"],
                          b_flow = cf["flow_aum"], b_betaf = cf["z_betaf"], r2 = summary(f)$r.squared)
}
theta_sem <- rbindlist(out)

cat("===== SEM efeito de gestora (amostra de todas as gestoras) =====\n")
resumo <- theta_sem[, .(
  media = c(mean(alpha),mean(b_aum),mean(b_cot),mean(b_fic),mean(b_flow),mean(b_betaf)),
  dp    = c(sd(alpha),sd(b_aum),sd(b_cot),sd(b_fic),sd(b_flow),sd(b_betaf))
)]
resumo[, variavel := c("alpha","b_aum","b_cot","b_fic","b_flow","b_betaf")]
resumo[, t := media/(dp/sqrt(.N))]
resumo[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setcolorder(resumo,"variavel")
print(resumo[, .(variavel, media=round(media,5), t=round(t,2), sig)])
cat("R2 medio:", round(mean(theta_sem$r2),4), "| N medio:", round(mean(theta_sem$n_fundos),1), "\n")

fwrite(theta_sem, file.path(REPO, "v2 OFICIAL/data/theta_todas_gestoras_sem_fe.csv"))
cat("\nOK - salvo\n")
