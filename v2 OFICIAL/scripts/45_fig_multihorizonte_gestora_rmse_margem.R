# =============================================================================
# 45_fig_multihorizonte_gestora_rmse_margem.R  (v2 OFICIAL)
#
# Grafico pedido pelo Joao, no estilo da Figura 12 (vies x dispersao, script
# 38): cruza RMSE fora da amostra (quao dificil e prever a gestora em termos
# absolutos) com a MARGEM vs ingenua (se o ajuste parcial realmente ajuda ou
# atrapalha) -- mesma logica de "duas medidas de qualidade diferentes pro
# mesmo problema", so que agora pra Tabela 25/26 em vez do erro em-amostra
# da Secao 6.3.
#
# Gera a figura para h=1 (a que entrou no documento, Figura 14) E h=3
# (pedido de acompanhamento do Joao) -- mesma funcao, so muda o horizonte.
#
# Joao pediu a MESMA ESCALA nos dois graficos, pra comparar visualmente sem
# risco de leitura errada (um ponto "mais a direita" em h=3 so e comparavel
# a h=1 se os eixos forem identicos). xlim/ylim calculados uma vez, com os
# dados dos DOIS horizontes juntos, e reaplicados nas duas figuras.
#
# So rotula os extremos (mesma regra da Fig 12): top-6 por |RMSE| ou top-6
# por margem mais negativa, pra nao poluir o grafico com as 41 gestoras.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

W <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))
cat("Gestoras:", nrow(W), "\n")

xlim_comum <- range(c(W$rmse_h1, W$rmse_h3), finite = TRUE)
ylim_comum <- range(c(W$margem_h1, W$margem_h3), finite = TRUE)
cat("Escala comum -- RMSE:", paste(round(xlim_comum,4), collapse=" a "),
    "| Margem:", paste(round(ylim_comum,1), collapse=" a "), "\n")

faz_grafico <- function(h) {
  col_rmse <- paste0("rmse_h", h); col_margem <- paste0("margem_h", h)
  rmse <- W[[col_rmse]]; margem <- W[[col_margem]]
  ok <- is.finite(rmse) & is.finite(margem)

  # h=1 mantem o nome original (ja referenciado na Figura 14 do documento);
  # os demais horizontes ganham sufixo, pra nao quebrar essa referencia.
  nome <- if (h == 1) "fig_multihorizonte_gestora_rmse_margem.pdf"
          else sprintf("fig_multihorizonte_gestora_rmse_margem_h%d.pdf", h)
  arq <- file.path(FIG, nome)
  pdf(arq, width = 7, height = 6)
  par(mar = c(4,4,1,1))
  plot(rmse[ok], margem[ok], pch = 16, col = "#2E5C8A", cex = 1.1,
       xlim = xlim_comum, ylim = ylim_comum,
       xlab = sprintf("RMSE fora da amostra, h=%d (dificuldade absoluta)", h),
       ylab = sprintf("margem vs. ingênua, h=%d (%%) -- negativo = ajuste parcial piora", h))
  abline(h = 0, col = "grey70", lty = 2)
  rank_rmse <- rank(-rmse[ok]); rank_margem <- rank(margem[ok])
  rotula <- rank_rmse <= 6 | rank_margem <= 6
  text(rmse[ok][rotula], margem[ok][rotula], labels = W$gestora_grupo[ok][rotula],
       pos = 3, cex = 0.65, col = "grey20")
  dev.off()
  cat("OK - salvo em", arq, "(", sum(ok), "gestoras com dado em h=", h, ")\n")
}

faz_grafico(1)
faz_grafico(3)
