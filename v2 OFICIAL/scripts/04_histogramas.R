# =============================================================================
# 04_histogramas.R  (v2 OFICIAL)
#
# Histogramas dos erros das Etapas 2 e 3 (cross-section/logit e ajuste
# parcial), para conferencia visual e para o v2 OFICIAL.pdf.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

e_cs <- fread(file.path(REPO, "v2 OFICIAL/data/erro_cross_section.csv"))$e
u_pa <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros.csv"))$u

pdf(file.path(FIG, "hist_erro_cross_section.pdf"), width = 6, height = 4.2)
hist(e_cs, breaks = 80, col = "#2E5C8A", border = "white",
     main = "Erro da Etapa 2 (cross-section + logit)",
     xlab = "e  =  peso observado  -  g(x'theta)", ylab = "Frequência")
abline(v = 0, col = "#8A2E2E", lwd = 2, lty = 2)
dev.off()

pdf(file.path(FIG, "hist_erro_ajuste_parcial.pdf"), width = 6, height = 4.2)
hist(u_pa, breaks = 80, col = "#2E5C8A", border = "white",
     main = "Erro da Etapa 3 (ajuste parcial)",
     xlab = "u  =  variação do peso observada  -  lambda * distância", ylab = "Frequência")
abline(v = 0, col = "#8A2E2E", lwd = 2, lty = 2)
dev.off()

cat("OK - salvos em 'v2 OFICIAL/figuras/hist_erro_cross_section.pdf' e 'hist_erro_ajuste_parcial.pdf'\n")
