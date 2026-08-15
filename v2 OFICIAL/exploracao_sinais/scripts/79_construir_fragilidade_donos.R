# =============================================================================
# 79_construir_fragilidade_donos.R  (exploracao_sinais, agente eventos)
#
# ANGULO (B) do agente: em vez de olhar concentracao (HHI, ja exaustivamente
# testado como preditor de volatilidade -- achado mais solido da exploracao),
# constroi caracteristicas da BASE DE FUNDOS DONOS de cada acao-mes -- nao
# da acao em si. Variante do mecanismo Greenwood-Thesmar que olha quem sao
# os donos, nao so quao concentrada e a posse.
#
# 3 caracteristicas por fundo-mes, agregadas por acao-mes ponderando pelo
# valor (R$) da posicao de cada fundo dono:
#   (1) tamanho do fundo dono (l_aum, ja pre-calculado no pipeline oficial;
#       log do AUM) -- fundos pequenos tem risco de fechamento/resgate maior
#       (documentado na literatura de fund flows, Chevalier-Ellison 1997 etc.)
#   (2) fluxo recente do fundo dono (flow_aum, captacao/resgate/AUM do mes,
#       ja pre-calculado) -- fluxo negativo = fundo em resgate liquido
#   (3) taxa de "churn"/saida do fundo dono em OUTRAS posicoes -- fracao das
#       posicoes que o fundo tinha no mes anterior e que zerou neste mes
#       (proxy de "fundo em dificuldade, liquidando carteira"), calculada
#       sobre TODO o livro do fundo (nao sobre a acao-alvo -- por construcao
#       nao ha circularidade: um fundo so aparece como "dono" da acao-alvo
#       no mes t se ele NAO zerou essa posicao especifica, entao o churn e
#       sempre sobre as OUTRAS posicoes dele)
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

cat("Carregando painel completo (pode demorar, ~8.4M linhas)...\n")
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","ym","peso","aum_prev","flow_aum","cotistas_prev","l_aum"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
cat("N linhas apos filtro:", nrow(pp), "\n")

# ---- (3) churn/exit rate do fundo: fracao das posicoes de t-1 zeradas em t ----
cat("Calculando churn (saida de posicoes) por fundo-mes...\n")
hold <- unique(pp[, .(cod_fundo, ticker, ym)])
hold_prev <- copy(hold)[, ym_atual := addm(ym, 1)]
setnames(hold_prev, "ym", "ym_origem")
# para cada posicao que o fundo tinha em ym_origem, existe ela em ym_atual (=ym_origem+1)?
chave_atual <- hold[, .(cod_fundo, ticker, ym)]
chave_atual[, mantido := TRUE]
setnames(chave_atual, "ym", "ym_atual")
merged_churn <- merge(hold_prev, chave_atual, by = c("cod_fundo","ticker","ym_atual"), all.x = TRUE)
merged_churn[is.na(mantido), mantido := FALSE]
churn_fundo <- merged_churn[, .(n_prev = .N, n_dropado = sum(!mantido)), by = .(cod_fundo, ym = ym_atual)]
churn_fundo <- churn_fundo[n_prev >= 3]  # exige pelo menos 3 posicoes no mes anterior p/ taxa fazer sentido
churn_fundo[, exit_rate := n_dropado / n_prev]
cat("N fundo-mes com churn calculado:", nrow(churn_fundo), "\n")
cat("Resumo exit_rate:\n"); print(summary(churn_fundo$exit_rate))

# ---- caracteristicas de fundo por mes (nivel fundo, 1 linha por cod_fundo-ym) ----
carac_fundo <- unique(pp[, .(cod_fundo, ym, aum_prev, flow_aum, cotistas_prev, l_aum)])
carac_fundo <- merge(carac_fundo, churn_fundo[, .(cod_fundo, ym, exit_rate, n_prev)], by = c("cod_fundo","ym"), all.x = TRUE)

# ---- agregacao por acao-mes, ponderada por valor (R$) da posicao ----
cat("Agregando por acao-mes (ponderado por valor da posicao)...\n")
pp2 <- merge(pp[, .(cod_fundo, ticker, ym, peso, aum_prev)], carac_fundo, by = c("cod_fundo","ym","aum_prev"))
pp2[, valor_posicao := peso * aum_prev]

frag <- pp2[is.finite(valor_posicao) & valor_posicao > 0, .(
  n_fundos = .N,
  n_fundos_com_churn = sum(is.finite(exit_rate)),
  frag_laum_medio = weighted.mean(l_aum, valor_posicao, na.rm = TRUE),
  frag_flow_medio = weighted.mean(flow_aum, valor_posicao, na.rm = TRUE),
  frag_cotistas_medio = weighted.mean(log1p(cotistas_prev), valor_posicao, na.rm = TRUE),
  frag_exit_rate_medio = weighted.mean(exit_rate, valor_posicao, na.rm = TRUE)
), by = .(ticker, ym)]

frag <- frag[n_fundos >= 10]
cat("N ticker-mes final:", nrow(frag), "\n")
cat("Resumo das caracteristicas:\n")
print(summary(frag[, .(frag_laum_medio, frag_flow_medio, frag_cotistas_medio, frag_exit_rate_medio)]))

# correlacao entre as 3 caracteristicas de fragilidade (sao coisas distintas?)
cat("\nCorrelacao entre caracteristicas (Pearson):\n")
print(cor(frag[, .(frag_laum_medio, frag_flow_medio, frag_cotistas_medio, frag_exit_rate_medio)], use = "pairwise.complete.obs"))

fwrite(frag, file.path(OUT, "candidatos_79_fragilidade_donos.csv"))
cat("\nOK\n")
