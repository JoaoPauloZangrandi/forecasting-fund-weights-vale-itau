# =============================================================================
# _piloto_hhi_etapa1.R  (teste piloto, NAO documentado no TCC ainda)
#
# Pergunta do Joao: vale a pena adicionar concentracao (HHI do RESTO da
# carteira, excluindo o ativo-alvo -- pra evitar a circularidade ja
# discutida) como 6a caracteristica na Etapa 1 (peso ~ caracteristicas)?
#
# Teste barato: pega uma amostra de celulas (ativo, mes) reais, roda a
# MESMA especificacao da Etapa 1 (logit quase-binomial, 5 caracteristicas
# padronizadas) com e sem HHI_resto, e compara significancia +
# pseudo-R2, celula por celula -- sem reconstruir o pipeline inteiro.
#
# HHI_resto(fundo,ativo,mes) = HHI_total(fundo,mes) - peso(fundo,ativo,mes)^2
# (subtrair o proprio peso do ativo-alvo do HHI total da carteira).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("Painel:", nrow(pp), "linhas\n")

hhi_total <- pp[, .(hhi_total = sum(peso^2)), by = .(cod_fundo, ym)]
pp <- merge(pp, hhi_total, by = c("cod_fundo","ym"))
pp[, hhi_resto := hhi_total - peso^2]
cat("HHI_resto: min =", min(pp$hhi_resto), "| max =", max(pp$hhi_resto), "\n")
stopifnot(all(pp$hhi_resto >= -1e-9))  # nunca deveria ser negativo

z <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
g <- function(zz) 1/(1+exp(-zz))

cnt <- pp[, .N, by = .(ativo, ym)]
celulas_grandes <- cnt[N >= 30]
cat("\nCelulas (ativo,mes) com >=30 holders:", nrow(celulas_grandes), "\n")

set.seed(42)
alvo_vale <- celulas_grandes[ativo == "VALE ON N1 - VALE3"][sample(.N, min(10,.N))]
outros_ativos <- unique(celulas_grandes[ativo != "VALE ON N1 - VALE3"]$ativo)
alvo_outros <- celulas_grandes[ativo %in% sample(outros_ativos, 15)][sample(.N, min(20,.N))]
celulas_teste <- rbind(alvo_vale, alvo_outros)
cat("Celulas selecionadas pro piloto:", nrow(celulas_teste), "\n\n")

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
    ativo = av, ym = mm, n = nrow(dm),
    pr2_sem_hhi = pr2_sem, pr2_com_hhi = pr2_com, ganho_pr2 = pr2_com - pr2_sem,
    coef_hhi = coef_hhi, t_hhi = t_hhi, sig_hhi = abs(t_hhi) > 1.96)
}
R <- rbindlist(resultados)
cat("===== Resultado do piloto:", nrow(R), "celulas testadas com sucesso =====\n\n")
print(R[order(-t_hhi)])

cat("\n===== Resumo =====\n")
cat(sprintf("Celulas onde HHI_resto e significativo (|t|>1,96): %d de %d (%.0f%%)\n",
            sum(R$sig_hhi), nrow(R), 100*mean(R$sig_hhi)))
cat(sprintf("Sinal do coeficiente quando significativo: %d positivos, %d negativos\n",
            sum(R$sig_hhi & R$coef_hhi>0), sum(R$sig_hhi & R$coef_hhi<0)))
cat(sprintf("Ganho medio de pseudo-R2 ao adicionar HHI_resto: %.4f (mediana %.4f)\n",
            mean(R$ganho_pr2), median(R$ganho_pr2)))
cat(sprintf("Pseudo-R2 medio SEM HHI: %.4f | COM HHI: %.4f\n", mean(R$pr2_sem_hhi), mean(R$pr2_com_hhi)))
