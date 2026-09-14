# =============================================================================
# 112_torneio_teste_b_desvio_alvo.R  (v2 OFICIAL / trilha TORNEIO)
#
# TESTE B -- replica de Li, Tiwari e Tong (2022, J. Financial Stability), que
# acharam nos EUA que fundos mal posicionados ate o 3o trimestre AUMENTAM o
# Active Share no 4o trimestre. So que aqui o benchmark e' melhor.
#
# O Active Share original mede distancia entre a carteira do fundo e um INDICE
# FIXO. O erro da Etapa 1 do nosso TCC,
#     e_{i,n,t} = peso_{i,n,t} - g(x'theta)_{n,t},
# mede distancia entre a carteira do fundo e o que fundos COM AS MESMAS
# CARACTERISTICAS estavam carregando naquele mes naquela acao. E' um benchmark
# endogeno e movel -- um "active peer benchmark" no espirito de Hunter, Kandel,
# Kandel e Wermers (2014), construido dentro de um sistema de demanda no
# espirito de Koijen e Yogo (2019).
#
# Duas agregacoes do desvio, por fundo e mes (a literatura nao fixa uma):
#   AS_i,t   = soma_n |e_{i,n,t}|        -- analogo direto do Active Share
#   RMS_i,t  = raiz(media_n e^2)         -- dispersao, insensivel ao numero
#                                           de posicoes da carteira
#
# Teste: a razao entre o desvio do 2o semestre e o do 1o semestre, dentro do
# MESMO fundo e do MESMO ano, contra o desempenho interino. Como e' razao
# intra-fundo, ja' controla o nivel de "ativismo" caracteristico da casa --
# que e', pelo TCC, justamente a variavel que mais separa gestoras.
#
# Tambem na versao de Li-Tiwari-Tong: 3 primeiros trimestres -> 4o trimestre.
#
# Entradas: erro_e_multiativo.csv (script 99), torneio_desempenho_fundo_mes.csv
# Saidas:   data/torneio_b_desvio_alvo_fundo_semestre.csv
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DD <- file.path(REPO, "v2 OFICIAL/data")
setFixest_notes(FALSE)

cat("== lendo erro da Etapa 1 ==\n")
E <- fread(file.path(DD, "erro_e_multiativo.csv"),
           select = c("cod_fundo","ym","erro"))
E[, cod_fundo := as.character(cod_fundo)]
cat("Obs:", nrow(E), "| fundos:", uniqueN(E$cod_fundo), "\n")

# --- desvio agregado por fundo-mes ------------------------------------------
FM <- E[, .(as_abs = sum(abs(erro)),
            rms    = sqrt(mean(erro^2)),
            n_pos  = .N), by = .(cod_fundo, ym)]
rm(E); invisible(gc())
FM[, ano := ym %/% 100L]
FM[, mes := ym %% 100L]
cat("Fundo-mes:", nrow(FM), "| mediana de posicoes por carteira:", median(FM$n_pos), "\n")

P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"))
P[, cod_fundo := as.character(cod_fundo)]

# =============================================================================
# Desenho 1 -- BHS: 1o semestre define, 2o semestre mede
# =============================================================================
cat("\n\n=========== B1: DESENHO SEMESTRAL (corte em junho) ===========\n")
CLS6 <- unique(P[!is.na(perdedor_bench_jun),
                 .(cod_fundo, ano, excesso_interim_jun, rank_interim_jun,
                   perdedor_bench_jun, quartil_rank_jun)])

agg <- function(dt, meses, sufixo) {
  z <- dt[mes %in% meses, .(as_abs = mean(as_abs), rms = mean(rms), n_meses = .N),
          by = .(cod_fundo, ano)]
  setnames(z, c("as_abs","rms","n_meses"), paste0(c("as_abs_","rms_","n_meses_"), sufixo))
  z
}
S <- merge(agg(FM, 1:6,  "h1"), agg(FM, 7:12, "h2"), by = c("cod_fundo","ano"))
S <- S[n_meses_h1 >= 4 & n_meses_h2 >= 4]          # semestre razoavelmente coberto
S <- merge(S, CLS6, by = c("cod_fundo","ano"))
S[, l_ratio_as  := log(as_abs_h2 / as_abs_h1)]
S[, l_ratio_rms := log(rms_h2 / rms_h1)]
S <- S[is.finite(l_ratio_as) & is.finite(l_ratio_rms)]
cat("Fundo-ano:", nrow(S), "| fundos:", uniqueN(S$cod_fundo), "\n\n")

cat("--- razao 2o/1o semestre do desvio, por grupo (mediana) ---\n")
print(S[, .(n = .N,
            razao_AS  = round(median(as_abs_h2/as_abs_h1), 3),
            razao_RMS = round(median(rms_h2/rms_h1), 3)),
        by = perdedor_bench_jun][order(perdedor_bench_jun)])
cat("(0 = venceu o Ibovespa ate junho; 1 = perdeu)\n")

cat("\n--- por quartil de ranking de junho (1 = pior) ---\n")
print(S[, .(n = .N,
            razao_AS  = round(median(as_abs_h2/as_abs_h1), 3),
            razao_RMS = round(median(rms_h2/rms_h1), 3)),
        by = quartil_rank_jun][order(quartil_rank_jun)])

cat("\n--- Teste formal, efeito fixo de ano, EP clusterizado por fundo ---\n")
for (yv in c("l_ratio_as","l_ratio_rms")) {
  cat("\n>>> ", yv, " ~ perdedor\n", sep = "")
  print(summary(feols(as.formula(paste0(yv, " ~ perdedor_bench_jun | ano")),
                      data = S, cluster = ~cod_fundo)))
  cat("\n>>> ", yv, " ~ excesso acumulado ate junho (continuo)\n", sep = "")
  print(summary(feols(as.formula(paste0(yv, " ~ excesso_interim_jun | ano")),
                      data = S, cluster = ~cod_fundo)))
}

fwrite(S, file.path(DD, "torneio_b_desvio_alvo_fundo_semestre.csv"))

# =============================================================================
# Desenho 2 -- Li, Tiwari e Tong: 3 primeiros trimestres -> 4o trimestre
# =============================================================================
cat("\n\n=========== B2: DESENHO DE LI, TIWARI E TONG (corte em setembro) ===========\n")
CLS9 <- unique(P[!is.na(perdedor_bench_set),
                 .(cod_fundo, ano, excesso_interim_set, rank_interim_set,
                   perdedor_bench_set, quartil_rank_set)])
Q <- merge(agg(FM, 1:9,  "q13"), agg(FM, 10:12, "q4"), by = c("cod_fundo","ano"))
Q <- Q[n_meses_q13 >= 6 & n_meses_q4 >= 2]
Q <- merge(Q, CLS9, by = c("cod_fundo","ano"))
Q[, l_ratio_as  := log(as_abs_q4 / as_abs_q13)]
Q[, l_ratio_rms := log(rms_q4 / rms_q13)]
Q <- Q[is.finite(l_ratio_as) & is.finite(l_ratio_rms)]
cat("Fundo-ano:", nrow(Q), "| fundos:", uniqueN(Q$cod_fundo), "\n\n")

cat("--- razao 4o trimestre / 3 primeiros, por grupo (mediana) ---\n")
print(Q[, .(n = .N,
            razao_AS  = round(median(as_abs_q4/as_abs_q13), 3),
            razao_RMS = round(median(rms_q4/rms_q13), 3)),
        by = perdedor_bench_set][order(perdedor_bench_set)])

cat("\n--- Teste formal ---\n")
print(summary(feols(l_ratio_as ~ perdedor_bench_set | ano, data = Q, cluster = ~cod_fundo)))
print(summary(feols(l_ratio_as ~ excesso_interim_set | ano, data = Q, cluster = ~cod_fundo)))

fwrite(Q, file.path(DD, "torneio_b_desvio_alvo_fundo_trimestre.csv"))

# =============================================================================
# Desenho 3 -- painel mensal com efeito fixo de fundo
# =============================================================================
# Versao mais exigente: dentro do MESMO fundo, meses em que ele estava mais
# atras do Ibovespa no acumulado do ano sao meses de desvio maior?
cat("\n\n=========== B3: PAINEL MENSAL, EFEITO FIXO DE FUNDO ===========\n")
PM <- merge(FM, P[, .(cod_fundo, ym, excesso_acum, rank_pct, mes_p = mes)],
            by = c("cod_fundo","ym"))
PM[, l_as := log(as_abs)]
PM[, l_rms := log(rms)]
PM <- PM[is.finite(l_as) & is.finite(l_rms)]
cat("Fundo-mes no painel:", nrow(PM), "| fundos:", uniqueN(PM$cod_fundo), "\n")
cat("\n>>> log(AS) ~ excesso acumulado no ano | fundo + mes-calendario\n")
print(summary(feols(l_as ~ excesso_acum | cod_fundo + ym, data = PM, cluster = ~cod_fundo)))
cat("\n>>> log(RMS) ~ excesso acumulado no ano | fundo + mes-calendario\n")
print(summary(feols(l_rms ~ excesso_acum | cod_fundo + ym, data = PM, cluster = ~cod_fundo)))

# --- B3b: versao PREDETERMINADA --------------------------------------------
# O excesso acumulado ate o mes t inclui o retorno do proprio mes t, que e'
# contemporaneo ao desvio medido em t. Isso abre causalidade reversa: quem se
# afasta muito do que os pares carregam tende, por isso mesmo, a render
# diferente deles. Defasar o excesso resolve: o desempenho ja' estava dado
# antes de o desvio ser observado.
cat("\n\n--- B3b: excesso DEFASADO (predeterminado) ---\n")
setorder(PM, cod_fundo, ym)
PM[, excesso_lag1 := shift(excesso_acum, 1L), by = .(cod_fundo, ano)]
PM[, excesso_lag3 := shift(excesso_acum, 3L), by = .(cod_fundo, ano)]
cat("\n>>> log(AS) ~ excesso acumulado em t-1 | fundo + mes-calendario\n")
print(summary(feols(l_as ~ excesso_lag1 | cod_fundo + ym, data = PM, cluster = ~cod_fundo)))
cat("\n>>> log(AS) ~ excesso acumulado em t-3 | fundo + mes-calendario\n")
print(summary(feols(l_as ~ excesso_lag3 | cod_fundo + ym, data = PM, cluster = ~cod_fundo)))

# --- B4: o efeito se intensifica perto da data de avaliacao? ----------------
# Esta e' A previsao central da teoria do torneio, e e' o que reconcilia B1
# com B2: se o incentivo vem da avaliacao de fim de ano, o efeito tem que ser
# fraco no comeco do ano e forte no 4o trimestre.
cat("\n\n=========== B4: O EFEITO CRESCE PERTO DO FIM DO ANO? ===========\n")
PM[, trimestre := factor(ceiling(mes/3), levels = 1:4,
                         labels = c("T1","T2","T3","T4"))]
cat("\n>>> log(AS) ~ excesso_{t-1} x trimestre | fundo + mes-calendario\n")
print(summary(feols(l_as ~ excesso_lag1:trimestre | cod_fundo + ym,
                    data = PM, cluster = ~cod_fundo)))
cat("\n>>> log(RMS) ~ excesso_{t-1} x trimestre | fundo + mes-calendario\n")
print(summary(feols(l_rms ~ excesso_lag1:trimestre | cod_fundo + ym,
                    data = PM, cluster = ~cod_fundo)))

fwrite(PM, file.path(DD, "torneio_b_painel_mensal.csv"))
cat("\nOK - 112 concluido\n")
