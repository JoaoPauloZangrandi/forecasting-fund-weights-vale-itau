# =============================================================================
# 92_figura_trajetoria_fundo_v2.R  (v2 OFICIAL)
#
# Ilustra um caso extremo de erro fora da amostra (h=3): a celula
# (fundo,ativo) de maior RMSE, entre as que tem posicao de verdade (peso
# mediano >=0,1%, mesmo filtro de posicao-poeira usado em todo o projeto)
# e uma trajetoria minimamente longa (>=8 obs de teste) pra dar pra plotar.
#
# Atualizado 12/08/2026: painel_multiativo_final.csv passou a ser o
# universo completo (todas as gestoras, todas as acoes) -- o par
# (fundo,ativo) de maior RMSE NAO e mais hardcoded, e recalculado aqui
# dinamicamente a cada rodada (era fundo 396303/Guepardo em JSLG3 na base
# antiga; pode ser outro na base nova).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h3 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"), select=c("cod_fundo","ativo","peso"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h3f <- merge(h3, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))
cat("h3 antes do filtro de posicao-poeira:", nrow(h3), "| depois:", nrow(h3f), "\n")

por_par <- h3f[, .(rmse = sqrt(mean(erro_oos^2)), n_obs = .N), by = .(cod_fundo, ativo)]
por_par <- por_par[n_obs >= 8]
setorder(por_par, -rmse)
alvo <- por_par[1]
cat("Par (fundo,ativo) de maior RMSE h=3 (>=8 obs, sem posicao-poeira):", alvo$cod_fundo,
    "/", alvo$ativo, "-- RMSE =", round(alvo$rmse,5), "\n")

fundo <- h3[cod_fundo == alvo$cod_fundo & ativo == alvo$ativo]
setorder(fundo, ym)
cat("Fundo", alvo$cod_fundo, ",", alvo$ativo, ", h=3 -- observacoes de teste:", nrow(fundo), "\n")
print(fundo[, .(ym, ym_fut, peso, peso_fut, erro_oos)])

fundo[, peso_previsto := peso_fut - erro_oos]
fundo[, data_atual := as.Date(paste0(substr(ym,1,4),"-",substr(ym,5,6),"-01"))]
fundo[, data_fut := as.Date(paste0(substr(ym_fut,1,4),"-",substr(ym_fut,5,6),"-01"))]

serie_real <- unique(rbind(
  fundo[, .(data = data_atual, peso = peso)],
  fundo[.N, .(data = data_fut, peso = peso_fut)]
))
setorder(serie_real, data)

mapa <- fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
mapa[, cod_fundo := as.character(cod_fundo)]
gestora <- mapa[cod_fundo == as.character(alvo$cod_fundo)]$gestora_grupo[1]
ticker <- trimws(sub(".*- ", "", alvo$ativo))

pdf(file.path(FIG, "fig_trajetoria_fundo_pior_rmse.pdf"), width = 8, height = 5)
par(mar = c(4,4.5,2,1))
plot(serie_real$data, 100*serie_real$peso, type="o", pch=16, col="#2E5C8A", lwd=2,
     xlab="Mês", ylab=sprintf("Peso de %s na carteira (%%)", ticker),
     main=sprintf("Fundo %s (%s) --- peso real vs. previsto (h=3)", alvo$cod_fundo, gestora),
     cex.main = 0.95,
     ylim = c(0, max(100*serie_real$peso, 100*fundo$peso_previsto)*1.15))
points(fundo$data_fut, 100*fundo$peso_previsto, pch=17, col="#B8452E", cex=1.1)
segments(fundo$data_fut, 100*fundo$peso_fut, fundo$data_fut, 100*fundo$peso_previsto,
         col="grey60", lty=3)
legend("topright", legend=c("Peso real observado","Peso previsto pelo ajuste parcial (3 meses à frente)"),
       col=c("#2E5C8A","#B8452E"), pch=c(16,17), lwd=c(2,NA), bty="n", cex=0.75)
dev.off()
cat(sprintf("\nOK - salvo em 'fig_trajetoria_fundo_pior_rmse.pdf' (fundo %s, %s, gestora %s)\n",
            alvo$cod_fundo, ticker, gestora))
