# =============================================================================
# 09_theta_significancia.R  (v2 OFICIAL)
#
# Adiciona um teste ingenuo (sem correcao de autocorrelacao, so a formula
# direta) de significancia da MEDIA dos 72 theta_t, para as Tabelas 1 e 2:
#   t = media(theta) / (dp(theta) / sqrt(72))
# Aplicado igual para theta linear (Etapa 1) e theta logit (Etapa 2) -- a
# formula nao depende de como cada theta_t mensal foi estimado, so olha a
# colecao final de 72 pontos.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

estrela <- function(t) {
  at <- abs(t)
  ifelse(at > 3.29, "***", ifelse(at > 2.58, "**", ifelse(at > 1.96, "*", "")))
}

sig_theta <- function(path, vars) {
  th <- fread(path)
  n <- nrow(th)
  out <- data.table(variavel = vars)
  out[, media := sapply(vars, function(v) mean(th[[v]]))]
  out[, dp    := sapply(vars, function(v) sd(th[[v]]))]
  out[, t     := media / (dp / sqrt(n))]
  out[, sig   := estrela(t)]
  out
}

cat("===== Theta linear (Etapa 1): media, dp, t, significancia =====\n")
r1 <- sig_theta(file.path(REPO, "v2 OFICIAL/data/theta_mensal.csv"),
                 c("alpha","b_aum","b_cot","b_fic","b_flow"))
print(r1[, .(variavel, media = round(media,5), dp = round(dp,5), t = round(t,2), sig)])

cat("\n===== Theta logit (Etapa 2): media, dp, t, significancia =====\n")
r2 <- sig_theta(file.path(REPO, "v2 OFICIAL/data/theta_logit_mensal.csv"),
                 c("alpha","b_aum","b_cot","b_fic","b_flow"))
print(r2[, .(variavel, media = round(media,4), dp = round(dp,4), t = round(t,2), sig)])

fwrite(r1, file.path(REPO, "v2 OFICIAL/data/theta_linear_significancia.csv"))
fwrite(r2, file.path(REPO, "v2 OFICIAL/data/theta_logit_significancia.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/theta_linear_significancia.csv' e 'theta_logit_significancia.csv'\n")
