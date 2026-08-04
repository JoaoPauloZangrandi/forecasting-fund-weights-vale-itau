# =============================================================================
# 79_figura_persistencia_erro.R  (v2 OFICIAL)
#
# Ideia 3: um fundo que erra muito (em qualquer direcao) num mes tende a
# errar muito de novo no mes seguinte? Testa persistencia (autocorrelacao
# lag-1) do erro fora da amostra, agregado por fundo-mes (media entre os
# ativos que o fundo carrega naquele mes, nao fundo-ativo -- cobertura
# muito melhor: mediana de 23 meses de teste por fundo).
#
# Autocorrelacao calculada so' em PARES DE MESES CONSECUTIVOS de verdade
# (nao so "o fundo tem 2 meses quaisquer") -- monta indice de mes
# sequencial e testa t vs t+1 apenas onde ambos existem.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"), select=c("cod_fundo","ativo","peso"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))

por_fundo_mes <- h1f[, .(erro_medio = mean(erro_oos)), by = .(cod_fundo, ym)]
por_fundo_mes[, mes_idx := (ym %/% 100L - 2020L)*12L + (ym %% 100L)]
setorder(por_fundo_mes, cod_fundo, mes_idx)

# pares (t, t+1) consecutivos de verdade
atual <- por_fundo_mes[, .(cod_fundo, mes_idx, erro_t = erro_medio)]
prox  <- por_fundo_mes[, .(cod_fundo, mes_idx = mes_idx - 1L, erro_t1 = erro_medio)]
pares <- merge(atual, prox, by = c("cod_fundo","mes_idx"))
cat("Pares (t,t+1) consecutivos de verdade:", nrow(pares), "|", uniqueN(pares$cod_fundo), "fundos\n")

cat("\nCorrelacao pooled (erro_t vs erro_t+1, todos os fundos juntos):",
    round(cor(pares$erro_t, pares$erro_t1),3), "\n")
fit <- lm(erro_t1 ~ erro_t, data = pares)
cat("Regressao erro_t+1 ~ erro_t: coef=", round(coef(fit)[2],3), " p=", signif(summary(fit)$coefficients[2,4],3),
    " R2=", round(summary(fit)$r.squared,4), "\n")

# autocorrelacao POR FUNDO (so' fundos com >=8 pares), distribuicao
por_fundo_corr <- pares[, if(.N>=8) .(corr = cor(erro_t, erro_t1), n=.N), by = cod_fundo]
cat("\nFundos com >=8 pares consecutivos:", nrow(por_fundo_corr), "\n")
cat("Autocorrelacao media entre fundos:", round(mean(por_fundo_corr$corr, na.rm=TRUE),3), "\n")
cat("% de fundos com autocorrelacao positiva:", round(100*mean(por_fundo_corr$corr>0, na.rm=TRUE),1), "%\n")

pdf(file.path(FIG, "fig_persistencia_erro.pdf"), width = 8, height = 5)
par(mfrow=c(1,2), mar=c(4,4.5,2,1))
plot(pares$erro_t, pares$erro_t1, pch=16, col=adjustcolor("#2E5C8A",0.15), cex=0.5,
     xlim=quantile(pares$erro_t,c(0.02,0.98)), ylim=quantile(pares$erro_t1,c(0.02,0.98)),
     xlab=expression("Erro médio do fundo, mês "*t), ylab=expression("Erro médio do fundo, mês "*t+1),
     main="Erro consecutivo\n(todos os pares fundo-mês)")
abline(fit, col="#B8452E", lwd=2); abline(h=0,v=0,col="grey85")
legend("topleft", legend=sprintf("corr=%.3f",cor(pares$erro_t,pares$erro_t1)), bty="n", cex=0.8)

hist(por_fundo_corr$corr, breaks=30, col="#2E5C8A", border="white",
     xlab="Autocorrelação lag-1 do erro, por fundo", main="Distribuição entre fundos\n(≥8 pares cada)")
abline(v=0, col="#B8452E", lwd=2, lty=2)
abline(v=mean(por_fundo_corr$corr,na.rm=TRUE), col="#1E7A4D", lwd=2)
legend("topleft", legend=c("Zero","Média"), col=c("#B8452E","#1E7A4D"), lwd=2, lty=c(2,1), bty="n", cex=0.75)
dev.off()
cat("\nOK\n")
