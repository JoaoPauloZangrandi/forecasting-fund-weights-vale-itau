# =============================================================================
# 78_figura_beta_vs_erro.R  (v2 OFICIAL)
#
# Ideia 2: beta do fundo (a caracteristica que mais explica o PESO na
# Etapa 1) tambem explica quem e' mais/menos previsivel fora da amostra
# (Etapa 3)? Filtro de posicao-poeira aplicado ANTES de agregar por fundo
# (mesmo padrao de sempre) -- sem isso, a correlacao preliminar (R/checagem
# anterior) misturava fundos com posicoes residuais/dust, que tem RMSE
# quase zero so por nao terem posicao de verdade, nao por serem "previsiveis".
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"), select=c("cod_fundo","ativo","peso"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))
cat("h1 antes do filtro:", nrow(h1), "| depois (so celulas com posicao de verdade):", nrow(h1f), "\n")

por_fundo <- h1f[, .(rmse = sqrt(mean(erro_oos^2)), n_obs = .N), by = cod_fundo]
por_fundo <- por_fundo[n_obs >= 6]
cat("Fundos com >=6 obs de teste (pos filtro):", nrow(por_fundo), "\n")

beta <- fread(file.path(REPO, "v2 OFICIAL/data/beta_fundo_todas_gestoras.csv"))
beta_medio <- beta[, .(beta_medio = mean(beta_fundo)), by = cod_fundo]
m <- merge(por_fundo, beta_medio, by="cod_fundo")
cat("Amostra final (RMSE + beta, pos filtro):", nrow(m), "fundos\n")

cat("\nCorrelacao RMSE vs beta bruto (pos filtro):", round(cor(m$rmse, m$beta_medio),3), "\n")
cat("Correlacao RMSE vs |beta-1| (pos filtro):", round(cor(m$rmse, abs(m$beta_medio-1)),3), "\n")

fit <- lm(rmse ~ beta_medio, data = m)
cat("\nRegressao RMSE ~ beta: coef=", round(coef(fit)[2],5), " p=", signif(summary(fit)$coefficients[2,4],3),
    " R2=", round(summary(fit)$r.squared,4), "\n")

pdf(file.path(FIG, "fig_beta_vs_erro.pdf"), width = 6.5, height = 5.5)
par(mar=c(4,4.5,1,1))
plot(m$beta_medio, m$rmse, pch=16, col=adjustcolor("#2E5C8A",0.35), cex=0.6,
     xlab="Beta médio do fundo (janela de 252 pregões)", ylab="RMSE fora da amostra (h=1)",
     xlim=quantile(m$beta_medio, c(0.01,0.99)), ylim=quantile(m$rmse, c(0,0.98)))
abline(fit, col="#B8452E", lwd=2)
legend("topleft", legend=sprintf("Correlação = %.3f | R² = %.3f", cor(m$rmse,m$beta_medio), summary(fit)$r.squared),
       bty="n", cex=0.85)
dev.off()
cat("\nOK\n")
