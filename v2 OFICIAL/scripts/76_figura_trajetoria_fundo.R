# =============================================================================
# 76_figura_trajetoria_fundo.R  (v2 OFICIAL)
#
# Pedido do orientador: figura concreta e didatica pra ilustrar o erro --
# a trajetoria de peso REAL vs. peso PREVISTO pelo ajuste parcial, pro
# fundo com o maior erro fora da amostra ja identificado no texto (fundo
# 292826, Vinci Partners, VALE3, h=3: salto real de 13% pra 80% em 2
# meses). Identidade usada: peso_previsto(t+1) = peso_fut - erro_oos
# (equivalente a peso(t) + lambda_h*d(t), sem precisar saber lambda_h
# explicitamente).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h3 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))
fundo <- h3[cod_fundo == 292826 & ativo == "VALE ON N1 - VALE3"]
setorder(fundo, ym)
cat("Fundo 292826 (Vinci Partners), VALE3, h=3 -- observações de teste:", nrow(fundo), "\n")
print(fundo[, .(ym, ym_fut, peso, peso_fut, erro_oos)])

fundo[, peso_previsto := peso_fut - erro_oos]
fundo[, data_atual := as.Date(paste0(substr(ym,1,4),"-",substr(ym,5,6),"-01"))]
fundo[, data_fut := as.Date(paste0(substr(ym_fut,1,4),"-",substr(ym_fut,5,6),"-01"))]

# serie de peso real: um ponto por "data_atual" (peso no inicio do periodo) +
# o ultimo peso_fut, pra fechar a serie
serie_real <- unique(rbind(
  fundo[, .(data = data_atual, peso = peso)],
  fundo[.N, .(data = data_fut, peso = peso_fut)]
))
setorder(serie_real, data)

pdf(file.path(FIG, "fig_trajetoria_fundo_vinci_vale3.pdf"), width = 7, height = 5)
par(mar = c(4,4.5,2,1))
plot(serie_real$data, 100*serie_real$peso, type="o", pch=16, col="#2E5C8A", lwd=2,
     xlab="Mês", ylab="Peso de VALE3 na carteira (%)",
     main="Fundo 292826 (Vinci Partners) --- peso real vs. previsto (h=3)",
     ylim = c(0, max(100*serie_real$peso, 100*fundo$peso_previsto)*1.1))
points(fundo$data_fut, 100*fundo$peso_previsto, pch=17, col="#B8452E", cex=1.1)
segments(fundo$data_fut, 100*fundo$peso_fut, fundo$data_fut, 100*fundo$peso_previsto,
         col="grey60", lty=3)
legend("topleft", legend=c("Peso real observado","Peso previsto pelo ajuste parcial (3 meses à frente)"),
       col=c("#2E5C8A","#B8452E"), pch=c(16,17), lwd=c(2,NA), bty="n", cex=0.75)
dev.off()
cat("\nOK - salvo em 'fig_trajetoria_fundo_vinci_vale3.pdf'\n")
