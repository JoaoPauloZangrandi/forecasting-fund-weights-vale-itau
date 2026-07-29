# =============================================================================
# 46_erro_ativo_por_gestora_e_fundo.R  (v2 OFICIAL)
#
# Pedido do Joao pro robo caca-replicantes: "testar nesse universo todos os
# ativos com mais e menos erros de cada fundo de cada gestora". Quebra o erro
# FORA da amostra (ja calculado nos scripts 42/44, h=1 e h=3) por ATIVO,
# tanto agregando por GESTORA (41 grupos) quanto por FUNDO individual (2.347
# fundos) -- reaproveita os dados linha a linha ja salvos (etapa3_multiativo_
# h1/h3.csv tem cod_fundo, gestora_grupo E ativo por observacao), sem
# precisar de nenhum merge novo.
#
# Fundo x ativo tem muito mais celulas esparsas que gestora x ativo (um
# fundo so' segura um ativo por poucos meses, tipicamente) -- aplicado um
# filtro minimo de n_obs>=12 (aprox. metade da janela de teste) pra evitar
# o mesmo artefato de cobertura fina ja visto varias vezes nesta sessao.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
h3 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))
rmse <- function(x) sqrt(mean(x^2))

# ---- por GESTORA x ATIVO -----------------------------------------------
por_gestora_ativo <- function(dt, h) {
  g <- dt[, .(n_obs = .N, rmse_oos = rmse(erro_oos), rmse_naive = rmse(erro_naive)),
          by = .(gestora_grupo, ativo)]
  g[, margem_pct := 100*(rmse_naive - rmse_oos)/rmse_naive]
  g[, horizonte := h]; g
}
GA1 <- por_gestora_ativo(h1, 1); GA3 <- por_gestora_ativo(h3, 3)
cat("Gestora x ativo -- h1:", nrow(GA1), "celulas | h3:", nrow(GA3), "celulas\n")

# ---- por FUNDO x ATIVO (com filtro de cobertura minima) -----------------
por_fundo_ativo <- function(dt, h) {
  f <- dt[, .(n_obs = .N, gestora_grupo = gestora_grupo[1],
              rmse_oos = rmse(erro_oos), rmse_naive = rmse(erro_naive)),
          by = .(cod_fundo, ativo)]
  f[, margem_pct := 100*(rmse_naive - rmse_oos)/rmse_naive]
  f[, horizonte := h]; f
}
FA1 <- por_fundo_ativo(h1, 1); FA3 <- por_fundo_ativo(h3, 3)
cat("Fundo x ativo -- h1:", nrow(FA1), "celulas brutas |", sum(FA1$n_obs>=12), "com n_obs>=12\n")
cat("Fundo x ativo -- h3:", nrow(FA3), "celulas brutas |", sum(FA3$n_obs>=12), "com n_obs>=12\n")

FA1f <- FA1[n_obs >= 12]; FA3f <- FA3[n_obs >= 12]

# ---- exemplo concreto: os fundos mais errantes da Tabela 21 (VALE3,h=3) --
# agora olhados no universo TODO de ativos que eles carregam, pra ver QUAIS
# ativos especificos puxam o erro alto deles.
cat("\n===== Fundo 292826 (Vinci Partners, o mais errante em VALE3 h=3) -- top ativos por RMSE, h=1 =====\n")
print(FA1f[cod_fundo==292826][order(-rmse_oos)][1:10, .(ativo, n_obs, rmse_oos=round(rmse_oos,4), margem_pct=round(margem_pct,1))])

cat("\n===== Fundo 191914 (BTG Pactual, 2o mais errante em VALE3 h=3) -- top ativos por RMSE, h=1 =====\n")
print(FA1f[cod_fundo==191914][order(-rmse_oos)][1:10, .(ativo, n_obs, rmse_oos=round(rmse_oos,4), margem_pct=round(margem_pct,1))])

# ---- exemplo gestora: Guepardo (nucleo duro, margem sempre negativa) ----
cat("\n===== Guepardo Investimentos -- ativos com mais e menos erro, h=1 =====\n")
gp <- GA1[gestora_grupo=="Guepardo Investimentos"][order(-rmse_oos)]
cat("Top5 (mais erro):\n"); print(gp[1:5, .(ativo,n_obs,rmse_oos=round(rmse_oos,4),margem_pct=round(margem_pct,1))])
cat("Bottom5 (menos erro):\n"); print(tail(gp,5)[, .(ativo,n_obs,rmse_oos=round(rmse_oos,4),margem_pct=round(margem_pct,1))])

fwrite(GA1, file.path(REPO, "v2 OFICIAL/data/erro_ativo_por_gestora_h1.csv"))
fwrite(GA3, file.path(REPO, "v2 OFICIAL/data/erro_ativo_por_gestora_h3.csv"))
fwrite(FA1f, file.path(REPO, "v2 OFICIAL/data/erro_ativo_por_fundo_h1_filtrado.csv"))
fwrite(FA3f, file.path(REPO, "v2 OFICIAL/data/erro_ativo_por_fundo_h3_filtrado.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/erro_ativo_por_gestora_h*.csv' e 'erro_ativo_por_fundo_h*_filtrado.csv'\n")
