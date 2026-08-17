# =============================================================================
# _piloto_hhi_etapa1_v2.R  (teste piloto v2, NAO documentado no TCC)
#
# Correcao do piloto anterior: aquele tinha amostra enviesada (VALE3 de
# proposito + so 15 tickers). Joao esclareceu que o escopo real do
# trabalho e o universo inteiro (todos os 501 ativos), VALE3 foi so o
# ponto de partida. Este piloto usa amostra ALEATORIA de verdade, sem
# nenhum vies de selecao, pra estimar o efeito medio no universo real.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
hhi_total <- pp[, .(hhi_total = sum(peso^2)), by = .(cod_fundo, ym)]
pp <- merge(pp, hhi_total, by = c("cod_fundo","ym"))
pp[, hhi_resto := hhi_total - peso^2]

z <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)

cnt <- pp[, .N, by = .(ativo, ym)]
celulas_grandes <- cnt[N >= 30]
cat("Celulas (ativo,mes) com >=30 holders:", nrow(celulas_grandes), "\n")

set.seed(123)
n_amostra <- 100
celulas_teste <- celulas_grandes[sample(.N, n_amostra)]
cat("Amostra aleatoria (sem vies):", nrow(celulas_teste), "celulas,",
    uniqueN(celulas_teste$ativo), "ativos distintos\n\n")

resultados <- list()
for (i in 1:nrow(celulas_teste)) {
  av <- celulas_teste$ativo[i]; mm <- celulas_teste$ym[i]
  dm <- pp[ativo == av & ym == mm]
  dm[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)][, z_hhi := z(hhi_resto)]
  dm <- dm[is.finite(z_aum) & is.finite(z_cot) & is.finite(z_betaf) & is.finite(z_hhi) & is.finite(flow_aum)]
  if (nrow(dm) < 20) next

  fit_sem <- tryCatch(glm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf,
                           family = quasibinomial(link="logit"), data = dm), error=function(e) NULL)
  fit_com <- tryCatch(glm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + z_hhi,
                           family = quasibinomial(link="logit"), data = dm), error=function(e) NULL)
  if (is.null(fit_sem) || is.null(fit_com)) next
  if (!fit_sem$converged || !fit_com$converged) next

  pr2_sem <- 1 - fit_sem$deviance/fit_sem$null.deviance
  pr2_com <- 1 - fit_com$deviance/fit_com$null.deviance
  cf <- summary(fit_com)$coefficients
  if (!"z_hhi" %in% rownames(cf)) next
  coef_hhi <- cf["z_hhi","Estimate"]; t_hhi <- cf["z_hhi","t value"]

  resultados[[length(resultados)+1]] <- data.table(
    ativo = av, ym = mm, n = nrow(dm), eh_vale3 = grepl("VALE3", av),
    pr2_sem_hhi = pr2_sem, pr2_com_hhi = pr2_com, ganho_pr2 = pr2_com - pr2_sem,
    coef_hhi = coef_hhi, t_hhi = t_hhi, sig_hhi = abs(t_hhi) > 1.96)
}
R <- rbindlist(resultados)
cat("===== Resultado (amostra aleatoria):", nrow(R), "celulas testadas com sucesso =====\n\n")

cat(sprintf("Celulas onde HHI_resto e significativo (|t|>1,96): %d de %d (%.0f%%)\n",
            sum(R$sig_hhi), nrow(R), 100*mean(R$sig_hhi)))
cat(sprintf("Sinal do coeficiente quando significativo: %d positivos, %d negativos\n",
            sum(R$sig_hhi & R$coef_hhi>0), sum(R$sig_hhi & R$coef_hhi<0)))
cat(sprintf("Ganho medio de pseudo-R2: %.4f | mediana: %.4f\n", mean(R$ganho_pr2), median(R$ganho_pr2)))
cat(sprintf("Pseudo-R2 medio SEM HHI: %.4f | COM HHI: %.4f\n", mean(R$pr2_sem_hhi), mean(R$pr2_com_hhi)))

cat("\n===== Distribuicao do ganho de pseudo-R2 =====\n")
print(summary(R$ganho_pr2))

cat("\n===== As 10 celulas com maior ganho =====\n")
print(R[order(-ganho_pr2)][1:10, .(ativo, ym, n, ganho_pr2=round(ganho_pr2,4), t_hhi=round(t_hhi,2))])

cat("\n===== As 10 celulas com menor (ou pior) ganho =====\n")
print(R[order(ganho_pr2)][1:10, .(ativo, ym, n, ganho_pr2=round(ganho_pr2,4), t_hhi=round(t_hhi,2))])
