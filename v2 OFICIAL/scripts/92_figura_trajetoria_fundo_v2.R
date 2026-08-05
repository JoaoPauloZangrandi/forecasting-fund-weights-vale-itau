# =============================================================================
# 92_figura_trajetoria_fundo_v2.R  (v2 OFICIAL)
#
# Substitui a Figura 11 (script 76, fundo 292826/Vinci Partners/VALE3).
# Investigacao (scripts 88-91) encontrou que o "salto de 13% para 80%"
# daquele fundo era um ERRO DE DADO: a soma dos pesos de TODAS as
# posicoes do fundo, na CDA bruta da CVM, chegava a 498% do patrimonio
# em mar/2021 -- confirmado na fonte, nao e bug de merge. Limpando a
# base (excluindo fundo-mes com soma_peso > 1,05, script 90), esse fundo
# cai da posicao #1 de maior RMSE (h=3) para a #95 de 1.911 -- deixa de
# ser um caso extremo.
#
# Novo campeao de RMSE (h=3) na base LIMPA: fundo 396303 (Guepardo
# Investimentos). Checado (script _checar_396303): fundo genuinamente
# concentrado (a maior parte do historico, 1 unica posicao ~99,9% do PL,
# soma de peso sempre <=1, sem anomalia). O erro vem de uma posicao real
# em JSLG3 (JSL ON), que caiu de 69,5% (jan/2020) pra 3,8% (abr/2020) --
# um desmonte real e abrupto de uma aposta concentrada, nao erro de dado.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h3 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3_limpo.csv"))
fundo <- h3[cod_fundo == 396303 & ativo == "JSL ON NM - JSLG3"]
setorder(fundo, ym)
cat("Fundo 396303 (Guepardo), JSLG3, h=3 -- observacoes de teste:", nrow(fundo), "\n")
print(fundo[, .(ym, ym_fut, peso, peso_fut, erro_oos)])

fundo[, peso_previsto := peso_fut - erro_oos]
fundo[, data_atual := as.Date(paste0(substr(ym,1,4),"-",substr(ym,5,6),"-01"))]
fundo[, data_fut := as.Date(paste0(substr(ym_fut,1,4),"-",substr(ym_fut,5,6),"-01"))]

serie_real <- unique(rbind(
  fundo[, .(data = data_atual, peso = peso)],
  fundo[.N, .(data = data_fut, peso = peso_fut)]
))
setorder(serie_real, data)

pdf(file.path(FIG, "fig_trajetoria_fundo_guepardo_jslg3.pdf"), width = 8, height = 5)
par(mar = c(4,4.5,2,1))
plot(serie_real$data, 100*serie_real$peso, type="o", pch=16, col="#2E5C8A", lwd=2,
     xlab="Mês", ylab="Peso de JSLG3 na carteira (%)",
     main="Fundo 396303 (Guepardo Investimentos) --- peso real vs. previsto (h=3)",
     cex.main = 0.95,
     ylim = c(0, max(100*serie_real$peso, 100*fundo$peso_previsto)*1.15))
points(fundo$data_fut, 100*fundo$peso_previsto, pch=17, col="#B8452E", cex=1.1)
segments(fundo$data_fut, 100*fundo$peso_fut, fundo$data_fut, 100*fundo$peso_previsto,
         col="grey60", lty=3)
legend("topright", legend=c("Peso real observado","Peso previsto pelo ajuste parcial (3 meses à frente)"),
       col=c("#2E5C8A","#B8452E"), pch=c(16,17), lwd=c(2,NA), bty="n", cex=0.75)
dev.off()
cat("\nOK - salvo em 'fig_trajetoria_fundo_guepardo_jslg3.pdf'\n")
