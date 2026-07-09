# =============================================================================
# 38_fig_lambda_especificacoes.R — gera docs/fig_lambda_especificacoes.pdf:
# grafico de barras horizontais comparando a velocidade de ajuste lambda
# estimada por cada uma das oito especificacoes da Tabela "lambda_full" do
# tcc I (MQO, VI em 3 clusters, VI+efeito de tempo, FE+VI, Arellano-Bond em
# 2 construcoes de instrumento). Visualiza de forma direta o achado central:
# sinal positivo robusto em 6 das 8, e a inversao de sinal do Arellano-Bond.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

especs <- c("MQO (enviesado)", "VI, cluster fundo", "VI, cluster mês",
            "VI, cluster two-way", "VI + efeito de tempo", "FE de fundo + VI",
            "VI, instrumento d(t-2)", "VI, horizonte 3 meses",
            "Arellano-Bond, instr. completos", "Arellano-Bond, instr. colapsados")
lam <- c(0.0976, 0.0413, 0.0413, 0.0413, 0.0409, 0.1853, 0.0210, 0.0858, 0.2923, -0.3312)
cor <- ifelse(lam < 0, "firebrick", ifelse(especs == "MQO (enviesado)", "grey60", "steelblue"))

pdf(file.path(REPO, "docs/fig_lambda_especificacoes.pdf"), width = 9, height = 5.5)
op <- par(mar = c(4, 15, 2.5, 1))
bp <- barplot(rev(lam), horiz = TRUE, names.arg = rev(especs), las = 1,
              col = rev(cor), border = NA, cex.names = 0.8,
              xlab = expression(hat(lambda)~"(velocidade de ajuste)"),
              main = "Velocidade de ajuste por especificação de estimador",
              xlim = c(-0.4, 0.35))
abline(v = 0, col = "black", lwd = 1)
text(rev(lam), bp, labels = sprintf("%.3f", rev(lam)),
     pos = ifelse(rev(lam) >= 0, 4, 2), cex = 0.75, xpd = TRUE)
legend("bottomright", bty = "n", cex = 0.8,
       legend = c("MQO (enviesado, referência)", "VI / FE (sinal robusto)", "Arellano-Bond (sinal instável)"),
       fill = c("grey60", "steelblue", "firebrick"), border = NA)
par(op); dev.off()
cat("OK - docs/fig_lambda_especificacoes.pdf gerado\n")
