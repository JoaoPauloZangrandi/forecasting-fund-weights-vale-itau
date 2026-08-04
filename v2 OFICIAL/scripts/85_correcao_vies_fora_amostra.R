# =============================================================================
# 85_correcao_vies_fora_amostra.R  (v2 OFICIAL)
#
# Segue o achado do script 84 (vies por gestora e persistente, corr=0,83
# entre metades do teste). Pergunta agora: usar esse vies como CORRECAO
# de fato melhora o RMSE fora da amostra, de forma honesta (sem
# vazamento)? Estima vies_gestora SO na 1a metade do teste (2020-01 a
# 2020-12) e aplica como ajuste fixo as previsoes da 2a metade
# (2021-01 a 2021-11) -- que o "estimador do vies" nunca viu.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"), select=c("cod_fundo","ativo","peso"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))

meses <- sort(unique(h1f$ym))
corte_meio <- meses[ceiling(length(meses)/2)]
h1f[, metade := ifelse(ym <= corte_meio, "primeira", "segunda")]
cat("1a metade: ate", corte_meio, "| 2a metade: depois de", corte_meio, "\n")

rmse <- function(x) sqrt(mean(x^2))

# --- vies estimado SO na 1a metade (o "treino" desta correcao) ---
vies_1a <- h1f[metade == "primeira", .(vies_gestora = mean(erro_oos), n_1a = .N), by = gestora_grupo]
vies_1a <- vies_1a[n_1a >= 50]
cat("Gestoras com vies estimavel (>=50 obs na 1a metade):", nrow(vies_1a), "\n")

# --- aplica na 2a metade (nunca usada pra estimar o vies) ---
seg <- h1f[metade == "segunda"]
seg <- merge(seg, vies_1a, by = "gestora_grupo")
seg[, erro_corrigido := erro_oos - vies_gestora]
cat("Obs de 2a metade cobertas por essas gestoras:", nrow(seg), "de", nrow(h1f[metade=="segunda"]), "\n")

cat("\n===== RMSE na 2a metade: ajuste parcial puro vs. com correcao de vies =====\n")
rmse_puro <- rmse(seg$erro_oos)
rmse_corr <- rmse(seg$erro_corrigido)
cat(sprintf("RMSE sem correcao      = %.6f\n", rmse_puro))
cat(sprintf("RMSE com correcao vies = %.6f\n", rmse_corr))
cat(sprintf("Melhora = %.2f%%\n", 100*(rmse_puro-rmse_corr)/rmse_puro))

# --- por gestora: quantas melhoram individualmente? ---
por_gestora <- seg[, .(rmse_puro = rmse(erro_oos), rmse_corr = rmse(erro_corrigido), n = .N), by = gestora_grupo]
por_gestora[, melhora_pct := 100*(rmse_puro-rmse_corr)/rmse_puro]
por_gestora <- por_gestora[n >= 20]
setorder(por_gestora, -melhora_pct)
cat(sprintf("\nGestoras (n>=20 na 2a metade) que MELHORAM com a correcao: %d de %d\n",
            sum(por_gestora$melhora_pct > 0), nrow(por_gestora)))
cat("\nMelhores melhoras:\n")
print(por_gestora[1:5, .(gestora_grupo, rmse_puro=round(rmse_puro,5), rmse_corr=round(rmse_corr,5), melhora_pct=round(melhora_pct,1))])
cat("\nPiores (correcao atrapalha mais):\n")
print(tail(por_gestora,5)[, .(gestora_grupo, rmse_puro=round(rmse_puro,5), rmse_corr=round(rmse_corr,5), melhora_pct=round(melhora_pct,1))])

# --- comparacao contra a ingenua tambem, pra referencia ---
seg[, erro_naive := dw]
cat(sprintf("\nPara referencia -- RMSE ingenua (mesmas obs) = %.6f\n", rmse(seg$erro_naive)))

fwrite(por_gestora, file.path(REPO, "v2 OFICIAL/data/correcao_vies_por_gestora.csv"))
cat("\nOK\n")
