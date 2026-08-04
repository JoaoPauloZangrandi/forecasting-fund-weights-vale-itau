# =============================================================================
# 68_sinal_robo_reais_agregado.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 5, passo 3 (final): converte a previsao ajustada em R$ (AUM do
# seguidor no mes t x dw_ajustado) e agrega -- por gestora, depois net de
# mercado por ativo -- gerando o sinal final de pressao compradora/vendedora.
#
# AUM usado: aum_prev do proprio mes t (o mesmo dado predeterminado ja
# usado na Etapa 1 pra gerar a previsao daquele fundo naquele mes -- nao
# olha AUM futuro).
#
# RESSALVA (documentada, nao resolvida nesta rodada): o beta foi estimado
# DENTRO da amostra usada aqui pra avaliar -- a melhora de RMSE (33%) e'
# esperada mecanicamente e NAO prova que o sinal generaliza fora da
# amostra. Fica registrado como limitacao conhecida do teste_estrategia.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

prev <- fread(file.path(REPO, "v2 OFICIAL/data/previsao_ajustada_seguidor.csv"))
prev[, seguidor := as.character(seguidor)]
cat("Previsoes ajustadas (seguidor,ativo,mes):", nrow(prev), "\n")

feat <- fread(file.path(REPO, "v2 OFICIAL/data/features_predeterminado_completo.csv"),
              select = c("cod_fundo","ym","aum_prev"))
feat[, cod_fundo := as.character(cod_fundo)]
feat <- unique(feat, by = c("cod_fundo","ym"))

prev <- merge(prev, feat, by.x = c("seguidor","ym"), by.y = c("cod_fundo","ym"), all.x = TRUE)
cat("Com AUM disponivel:", prev[!is.na(aum_prev), .N], "de", nrow(prev),
    sprintf(" (%.1f%%)\n", 100*prev[!is.na(aum_prev),.N]/nrow(prev)))

prev[, fluxo_reais_ajustado := aum_prev * dw_ajustado]
prev[, fluxo_reais_normal   := aum_prev * dw_previsto_normal]

mapa <- fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
mapa[, cod_fundo := as.character(cod_fundo)]
prev <- merge(prev, mapa[, .(cod_fundo, gestora_grupo)], by.x = "seguidor", by.y = "cod_fundo", all.x = TRUE)

cat("\n===== Agregacao por GESTORA (soma de fluxo previsto, por ativo-mes) =====\n")
por_gestora <- prev[!is.na(fluxo_reais_ajustado), .(
  fluxo_reais_ajustado = sum(fluxo_reais_ajustado),
  fluxo_reais_normal = sum(fluxo_reais_normal),
  n_fundos_seguidores = uniqueN(seguidor)
), by = .(gestora_grupo, ativo, ym)]
cat("Linhas (gestora,ativo,mes):", nrow(por_gestora), "\n")

cat("\n===== NET DE MERCADO por ATIVO-MES (soma de TODAS as gestoras) =====\n")
net_mercado <- prev[!is.na(fluxo_reais_ajustado), .(
  fluxo_liquido_reais = sum(fluxo_reais_ajustado),
  fluxo_liquido_normal = sum(fluxo_reais_normal),
  n_fundos = uniqueN(seguidor),
  n_gestoras = uniqueN(gestora_grupo)
), by = .(ativo, ym)]
setorder(net_mercado, ym, -fluxo_liquido_reais)

cat("\n--- Top 10 maior pressao COMPRADORA liquida (qualquer ativo-mes) ---\n")
top_compra <- net_mercado[order(-fluxo_liquido_reais)][1:10]
print(top_compra[, .(ativo, ym, fluxo_liquido_reais = round(fluxo_liquido_reais/1e6,2), n_fundos, n_gestoras)])

cat("\n--- Top 10 maior pressao VENDEDORA liquida (qualquer ativo-mes) ---\n")
top_venda <- net_mercado[order(fluxo_liquido_reais)][1:10]
print(top_venda[, .(ativo, ym, fluxo_liquido_reais = round(fluxo_liquido_reais/1e6,2), n_fundos, n_gestoras)])

cat("\n--- Resumo geral do fluxo liquido (em R$ milhoes) ---\n")
print(summary(net_mercado$fluxo_liquido_reais/1e6))

fwrite(prev, file.path(REPO, "v2 OFICIAL/data/sinal_robo_fundo_nivel.csv"))
fwrite(por_gestora, file.path(REPO, "v2 OFICIAL/data/sinal_robo_por_gestora.csv"))
fwrite(net_mercado, file.path(REPO, "v2 OFICIAL/data/sinal_robo_net_mercado.csv"))
cat("\nOK - salvo 'sinal_robo_fundo_nivel.csv', 'sinal_robo_por_gestora.csv', 'sinal_robo_net_mercado.csv'\n")
