# =============================================================================
# 65_resumo_pares_replicantes.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 4, resumo final: 1.286.553 pares significativos e' volume grande
# demais pra ser um achado apresentavel por si so -- este script resume em
# 3 vistas gerenciaveis:
#   (a) fundos "hub": quem tem mais parceiros significativos (separado
#       mesma-gestora vs gestora-diferente) -- candidatos a "super-
#       replicante" ou "lider" de mercado.
#   (b) pares de GESTORA (nao fundo): quantos pares fundo-fundo
#       significativos existem entre cada dupla de gestoras -- mapeia
#       direto pra hipotese original do Joao ("X% dos fundos da gestora A
#       agem como a gestora B").
#   (c) lideranca: entre os pares significativos, quais tem assimetria
#       clara na correlacao defasada (A lidera B muito mais forte que B
#       lidera A, ou vice-versa) -- candidatos a relacao causal genuina,
#       nao so co-movimento.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

sig <- fread(file.path(REPO, "v2 OFICIAL/data/pares_replicantes_significativos.csv"))
cat("Pares significativos:", nrow(sig), "\n")

# ---- (a) fundos hub ---------------------------------------------------------
long <- rbindlist(list(
  sig[, .(cod_fundo = fundo_a, gestora = gestora_a, parceiro_gestora = gestora_b, mesma_gestora, ativo)],
  sig[, .(cod_fundo = fundo_b, gestora = gestora_b, parceiro_gestora = gestora_a, mesma_gestora, ativo)]
))
hub <- long[, .(n_conexoes = .N,
                 n_conexoes_mesma_gestora = sum(mesma_gestora),
                 n_conexoes_gestora_diferente = sum(!mesma_gestora),
                 n_ativos_distintos = uniqueN(ativo),
                 n_gestoras_parceiras_distintas = uniqueN(parceiro_gestora[!mesma_gestora])),
             by = .(cod_fundo, gestora)]
setorder(hub, -n_conexoes_gestora_diferente)
cat("\n===== TOP 20 fundos-hub (mais conexoes com gestoras DIFERENTES) =====\n")
print(hub[1:20])

# ---- (b) pares de gestora (so' cross-gestora) -------------------------------
cross <- sig[mesma_gestora == FALSE]
cross[, par_gestora := paste(pmin(gestora_a, gestora_b), pmax(gestora_a, gestora_b), sep = " <-> ")]
por_par_gestora <- cross[, .(n_pares_fundo = .N, n_ativos_distintos = uniqueN(ativo),
                              n_fundos_distintos = uniqueN(c(fundo_a, fundo_b))),
                          by = par_gestora]
setorder(por_par_gestora, -n_pares_fundo)
cat("\n===== TOP 20 pares de GESTORA com mais conexoes fundo-fundo significativas =====\n")
print(por_par_gestora[1:20])

# ---- (c) lideranca (assimetria na correlacao defasada) ----------------------
sig[, forca_a_lidera := abs(corr_a_lidera)]
sig[, forca_b_lidera := abs(corr_b_lidera)]
sig[, assimetria := abs(forca_a_lidera - forca_b_lidera)]
sig[, lider := fifelse(forca_a_lidera > forca_b_lidera, fundo_a, fundo_b)]
sig[, seguidor := fifelse(forca_a_lidera > forca_b_lidera, fundo_b, fundo_a)]
sig[, forca_lideranca := pmax(forca_a_lidera, forca_b_lidera)]

lideranca_clara <- sig[assimetria > 0.2 & forca_lideranca > 0.5]
cat("\n===== Pares com lideranca CLARA (assimetria>0,2 E forca do lider>0,5):", nrow(lideranca_clara), "de", nrow(sig), "=====\n")
cat("Desses, cross-gestora:", lideranca_clara[mesma_gestora==FALSE,.N], "\n")
top_lid <- lideranca_clara[order(-forca_lideranca)][1:15]
print(top_lid[, .(ativo, lider, seguidor, forca_lideranca = round(forca_lideranca,3),
                   assimetria = round(assimetria,3), mesma_gestora)])

fwrite(hub, file.path(REPO, "v2 OFICIAL/data/resumo_fundos_hub.csv"))
fwrite(por_par_gestora, file.path(REPO, "v2 OFICIAL/data/resumo_pares_gestora.csv"))
fwrite(lideranca_clara, file.path(REPO, "v2 OFICIAL/data/pares_lideranca_clara.csv"))
cat("\nOK - salvo 'resumo_fundos_hub.csv', 'resumo_pares_gestora.csv', 'pares_lideranca_clara.csv'\n")
