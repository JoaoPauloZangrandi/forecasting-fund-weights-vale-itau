# =============================================================================
# 14_etapa1_antes_depois.R  (v2 OFICIAL)
#
# Roda a MESMA regressao (linear e logit) na MESMA amostra predeterminada
# (59 meses, 8.335 obs) SEM o beta do fundo, para comparar lado a lado com o
# resultado do R/13 (COM beta do fundo) -- isola o efeito de ADICIONAR a
# variavel, sem misturar com o efeito da mudanca de amostra/timing.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_predeterminado.csv"))
d[, l_aum := log(aum_prev)]
d[, l_cot := log(cotistas_prev)]
d[, flow_aum := fluxo_prev / aum_prev]
d <- d[is.finite(l_aum) & is.finite(l_cot) & is.finite(flow_aum) & is.finite(beta_fundo)]
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
meses <- sort(unique(d$ym))

out_lin <- vector("list", length(meses)); out_log <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  if (nrow(dt) < 15) next
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)]
  f  <- lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum, data = dt)
  fl <- glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum, family = quasibinomial(link="logit"), data = dt)
  cf  <- coef(f); cfl <- coef(fl)
  pr2 <- 1 - fl$deviance/fl$null.deviance  # pseudo-R2 de McFadden
  out_lin[[i]] <- data.table(ym=t, alpha=cf["(Intercept)"], b_aum=cf["z_aum"], b_cot=cf["z_cot"],
                              b_fic=cf["is_fic"], b_flow=cf["flow_aum"])
  out_log[[i]] <- data.table(ym=t, alpha=cfl["(Intercept)"], b_aum=cfl["z_aum"], b_cot=cfl["z_cot"],
                              b_fic=cfl["is_fic"], b_flow=cfl["flow_aum"], pr2=pr2)
}
theta_lin_sem <- rbindlist(out_lin); theta_log_sem <- rbindlist(out_log)

resumo <- function(dt) {
  n_meses <- nrow(dt)  # NAO usar .N depois de criar r (viraria nrow(r)=5, numero de variaveis --
                        # bug ja encontrado e corrigido nos scripts 13/20/23)
  r <- dt[, .(media = c(mean(alpha),mean(b_aum),mean(b_cot),mean(b_fic),mean(b_flow)),
              dp    = c(sd(alpha),sd(b_aum),sd(b_cot),sd(b_fic),sd(b_flow)))]
  r[, variavel := c("alpha","b_aum","b_cot","b_fic","b_flow")]
  r[, t := media/(dp/sqrt(n_meses))]
  r[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
  setcolorder(r,"variavel"); r
}
cat("===== SEM beta do fundo, mesma amostra (linear) =====\n")
print(resumo(theta_lin_sem)[,.(variavel,media=round(media,5),t=round(t,2),sig)])
cat("\n===== SEM beta do fundo, mesma amostra (logit) =====\n")
print(resumo(theta_log_sem)[,.(variavel,media=round(media,4),t=round(t,2),sig)])
cat("Pseudo-R2 (McFadden) medio (logit, sem beta):", round(mean(theta_log_sem$pr2, na.rm=TRUE),4), "\n")

fwrite(theta_lin_sem, file.path(REPO, "v2 OFICIAL/data/theta_mensal_v2_sembeta.csv"))
fwrite(theta_log_sem, file.path(REPO, "v2 OFICIAL/data/theta_logit_mensal_v2_sembeta.csv"))
cat("\nOK - salvo\n")
