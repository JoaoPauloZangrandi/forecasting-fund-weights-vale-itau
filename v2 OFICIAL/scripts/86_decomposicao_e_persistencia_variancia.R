# =============================================================================
# 86_decomposicao_e_persistencia_variancia.R  (v2 OFICIAL)
#
# Segue os scripts 84 (vies persistente, corr=0,83) e 85 (corrigir pelo
# vies so melhora RMSE agregado em 0,07% -- porque RMSE^2 = vies^2 +
# variancia, e a variancia domina). Duas pecas:
#
#  C1) Decomposicao RMSE^2 = vies^2 + variancia, por gestora -- formaliza
#      com numero exato o que o script 85 sugeriu qualitativamente.
#  C2) Persistencia do DESVIO-PADRAO do erro por gestora entre a 1a e a
#      2a metade do periodo de teste (mesmo desenho do script 84, mas
#      pra dispersao em vez de media) -- a variancia e um traco estavel
#      da gestora, ou muda ano a ano?
#
# Mesma base (etapa3_multiativo_h1.csv, filtro de posicao-poeira) dos
# scripts anteriores desta linha.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"), select=c("cod_fundo","ativo","peso"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))
cat("Base (h1, filtrada):", nrow(h1f), "obs\n")

# =============================================================================
# C1) Decomposicao RMSE^2 = vies^2 + variancia, por gestora
# =============================================================================
dec <- h1f[, .(vies = mean(erro_oos), rmse2 = mean(erro_oos^2), n = .N), by = gestora_grupo]
dec[, vies2 := vies^2]
dec[, variancia := rmse2 - vies2]
dec[, rmse := sqrt(rmse2)]
dec[, pct_vies := 100*vies2/rmse2]
dec <- dec[n >= 100]
setorder(dec, -rmse)
cat("\n===== Decomposicao RMSE^2 = vies^2 + variancia, por gestora (", nrow(dec), "gestoras) =====\n")
cat(sprintf("Media do %% do RMSE^2 explicado pelo vies^2, entre gestoras: %.2f%%\n", mean(dec$pct_vies)))
cat(sprintf("Mediana: %.2f%% | Maximo: %.2f%% (gestora: %s)\n",
            median(dec$pct_vies), max(dec$pct_vies), dec[which.max(pct_vies)]$gestora_grupo))
print(dec[order(-pct_vies)][1:8, .(gestora_grupo, rmse=round(rmse,5), pct_vies=round(pct_vies,2))])

# pooled: todo mundo junto (nao media simples entre gestoras, e o vies^2/rmse2 do pool inteiro)
vies_pool <- mean(h1f$erro_oos); rmse2_pool <- mean(h1f$erro_oos^2)
cat(sprintf("\nPooled (todas as obs juntas): vies^2/RMSE^2 = %.3f%%\n", 100*vies_pool^2/rmse2_pool))

setorder(dec, -rmse)
pdf(file.path(FIG, "fig_decomposicao_vies_variancia.pdf"), width = 7.5, height = 9)
par(mar = c(4, 13, 3, 1))
barplot(t(as.matrix(dec[, .(variancia, vies2)])), horiz = TRUE, names.arg = dec$gestora_grupo,
        las = 1, cex.names = 0.55, col = c("#3B6E9E", "#B8452E"), border = NA,
        xlab = expression("RMSE"^2*" fora da amostra ("*h*"=1)"),
        main = "Decomposição do erro: variância domina o viés")
legend("bottomright", legend = c("Variância", expression("Viés"^2)), fill = c("#3B6E9E", "#B8452E"), bty = "n")
dev.off()
cat("\nOK - fig_decomposicao_vies_variancia.pdf salva\n")

# =============================================================================
# C2) Persistencia do desvio-padrao do erro, por gestora, entre metades do teste
# =============================================================================
meses <- sort(unique(h1f$ym))
corte_meio <- meses[ceiling(length(meses)/2)]
h1f[, metade := ifelse(ym <= corte_meio, "primeira", "segunda")]
cat("\n1a metade: ate", corte_meio, "| 2a metade: depois de", corte_meio, "\n")

dp_metade <- h1f[, .(dp = sd(erro_oos), n = .N), by = .(gestora_grupo, metade)]
wide <- dcast(dp_metade, gestora_grupo ~ metade, value.var = c("dp","n"))
wide <- wide[!is.na(dp_primeira) & !is.na(dp_segunda) & n_primeira >= 50 & n_segunda >= 50]
cat("Gestoras com >=50 obs em cada metade:", nrow(wide), "\n")

corr_dp <- cor(wide$dp_primeira, wide$dp_segunda)
fit_dp <- lm(dp_segunda ~ dp_primeira, data = wide)
cat(sprintf("\n===== Persistencia do DESVIO-PADRAO do erro entre metades do teste =====\n"))
cat(sprintf("Correlacao (Pearson) = %.4f\n", corr_dp))
cat(sprintf("Correlacao (Spearman) = %.4f\n", cor(wide$dp_primeira, wide$dp_segunda, method="spearman")))
cat(sprintf("R2 = %.4f | p(coef) = %.4g\n", summary(fit_dp)$r.squared, summary(fit_dp)$coefficients[2,4]))
cat(sprintf("(Para comparacao: persistencia do VIES no script 84 foi corr=0,832, R2=0,693)\n"))

lim_comum <- range(c(wide$dp_primeira, wide$dp_segunda))
pdf(file.path(FIG, "fig_persistencia_dp_gestora.pdf"), width = 6.5, height = 6.5)
plot(wide$dp_primeira, wide$dp_segunda, pch = 19, col = adjustcolor("#3B6E9E", 0.6),
     xlim = lim_comum, ylim = lim_comum, asp = 1,
     xlab = "Desvio-padrão do erro, 1ª metade do teste", ylab = "Desvio-padrão do erro, 2ª metade do teste",
     main = sprintf("Persistência da dispersão do erro por gestora\ncorrelação = %.3f", corr_dp))
abline(0, 1, col = "grey70", lty = 2)
abline(fit_dp, col = "firebrick", lwd = 2)
legend("topleft", legend = c("Ajuste linear", "Identidade (y=x)"), col = c("firebrick","grey70"),
       lty = c(1,2), lwd = c(2,1), bty = "n", cex = 0.8)
dev.off()
cat("OK - fig_persistencia_dp_gestora.pdf salva\n")

fwrite(dec, file.path(REPO, "v2 OFICIAL/data/decomposicao_vies_variancia_gestora.csv"))
fwrite(wide, file.path(REPO, "v2 OFICIAL/data/persistencia_dp_gestora.csv"))
cat("\nOK - tudo salvo\n")
