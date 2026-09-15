# =============================================================================
# 125_diagnostico_amostra_extensao.R  (v2 OFICIAL)
#
# PARADA 1 da extensao "explicar a outra metade da variancia do erro".
# Este script NAO estima nada. So mede se a amostra aguenta o desenho.
#
# Perguntas que ele responde, em ordem de importancia:
#   Q1) Dos fundos com erro no teste, quantos tem TREINO suficiente para
#       calcular media E desvio-padrao de caracteristica? (risco #1 do plano:
#       fundo que so existe no teste nao tem treino, e o Bloco B1 morre)
#   Q2) Quantos fundos sobrevivem a cada corte de n_obs no teste?
#   Q3) Distribuicao Anbima restrita a esses fundos (nao o cache inteiro)
#   Q4) soma_peso (fatia de acoes) por classe Anbima -- o confundidor
#       que decide o Bloco B3
#   Q5) n_ativos bruto vs. pos-filtro de poeira
#   Q6) cobertura de preco, ponderada por peso
#
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
})

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
CORTE_TREINO <- 202001L
PISO_POEIRA  <- 0.001

out <- list()
add <- function(bloco, metrica, valor) {
  out[[length(out) + 1L]] <<- data.table(bloco = bloco, metrica = metrica,
                                         valor = as.character(valor))
}

cat("=============================================================\n")
cat("125 - Diagnostico da amostra da extensao\n")
cat("=============================================================\n\n")

# --- painel completo: so as colunas necessarias -----------------------------
cat("Lendo painel_multiativo_final.csv (colunas selecionadas)...\n")
pp <- fread(file.path(DD, "painel_multiativo_final.csv"),
            select = c("cod_fundo", "ym", "ativo", "peso", "gestora_grupo"),
            showProgress = FALSE)
pp[, cod_fundo := as.character(cod_fundo)]
cat("  painel:", nrow(pp), "linhas |", uniqueN(pp$cod_fundo), "fundos |",
    uniqueN(pp$ym), "meses\n\n")

# filtro de poeira: mesma definicao dos scripts 86/95
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
cel_ok <- cel[peso_mediano >= PISO_POEIRA]
cat("Celulas (fundo x ativo):", nrow(cel), "| pos-filtro poeira:", nrow(cel_ok),
    sprintf("(%.1f%%)\n\n", 100 * nrow(cel_ok) / nrow(cel)))
add("poeira", "celulas_brutas", nrow(cel))
add("poeira", "celulas_pos_filtro", nrow(cel_ok))

# --- erro no teste (h=1) ----------------------------------------------------
cat("Lendo etapa3_multiativo_h1.csv...\n")
h1 <- fread(file.path(DD, "etapa3_multiativo_h1.csv"),
            select = c("cod_fundo", "ativo", "ym", "gestora_grupo", "erro_oos"),
            showProgress = FALSE)
h1[, cod_fundo := as.character(cod_fundo)]
cat("  h1 bruto:", nrow(h1), "linhas |", uniqueN(h1$cod_fundo), "fundos\n")
h1f <- merge(h1, cel_ok[, .(cod_fundo, ativo)], by = c("cod_fundo", "ativo"))
cat("  h1 pos-filtro poeira:", nrow(h1f), "linhas |",
    uniqueN(h1f$cod_fundo), "fundos\n\n")
add("teste", "obs_h1_pos_filtro", nrow(h1f))
add("teste", "fundos_h1_pos_filtro", uniqueN(h1f$cod_fundo))

fundo_teste <- h1f[, .(n_obs_teste = .N, n_meses_teste = uniqueN(ym)),
                   by = .(cod_fundo, gestora_grupo)]

# --- Q1: TREINO por fundo ---------------------------------------------------
cat("--- Q1: cobertura de TREINO por fundo (o risco numero 1 do plano) ---\n")
pp_treino <- pp[ym < CORTE_TREINO]
fundo_treino <- pp_treino[, .(n_meses_treino = uniqueN(ym),
                              n_obs_treino   = .N), by = cod_fundo]

F <- merge(fundo_teste, fundo_treino, by = "cod_fundo", all.x = TRUE)
F[is.na(n_meses_treino), `:=`(n_meses_treino = 0L, n_obs_treino = 0L)]

n_total <- nrow(F)
cat(sprintf("Fundos com erro no teste (pos-filtro): %d\n", n_total))
for (m in c(0, 1, 6, 12, 24, 36)) {
  k <- F[n_meses_treino >= m, .N]
  cat(sprintf("  com >= %2d meses de treino: %5d  (%.1f%%)\n",
              m, k, 100 * k / n_total))
  add("Q1_treino", paste0("fundos_treino_ge_", m, "m"), k)
}
cat(sprintf("\nSem NENHUM mes de treino: %d (%.1f%%) -- estes matam qualquer media/sd de treino\n",
            F[n_meses_treino == 0, .N], 100 * F[n_meses_treino == 0, .N] / n_total))
cat("Quantis de n_meses_treino:\n")
print(quantile(F$n_meses_treino, c(0, .1, .25, .5, .75, .9, 1)))

# --- Q2: cortes de n_obs no teste -------------------------------------------
cat("\n--- Q2: fundos por corte de n_obs no teste ---\n")
cat("(a ultima coluna e a amostra efetiva do Bloco B1)\n")
for (k in c(6, 12, 24, 48, 100)) {
  a <- F[n_obs_teste >= k, .N]
  b <- F[n_obs_teste >= k & n_meses_teste >= 6, .N]
  d <- F[n_obs_teste >= k & n_meses_teste >= 6 & n_meses_treino >= 12, .N]
  cat(sprintf("  n_obs >= %3d: %5d fundos | + n_meses_teste>=6: %5d | + treino>=12m: %5d\n",
              k, a, b, d))
  add("Q2_cortes", paste0("n_obs_ge_", k), a)
  add("Q2_cortes", paste0("n_obs_ge_", k, "_e_meses_ge6"), b)
  add("Q2_cortes", paste0("n_obs_ge_", k, "_e_meses_ge6_e_treino_ge12"), d)
}
cat(sprintf("\nMedia de obs por fundo no teste: %.1f | mediana: %.0f\n",
            mean(F$n_obs_teste), median(F$n_obs_teste)))

# amostra de trabalho do plano: n_obs>=24, n_meses_teste>=6
F[, amostra_plano := n_obs_teste >= 24 & n_meses_teste >= 6]
F[, amostra_B1    := amostra_plano & n_meses_treino >= 12]
cat(sprintf("\nAMOSTRA DO PLANO (n_obs>=24 e meses>=6): %d fundos, %d gestoras\n",
            F[amostra_plano == TRUE, .N], F[amostra_plano == TRUE, uniqueN(gestora_grupo)]))
cat(sprintf("AMOSTRA DO BLOCO B1 (+ treino>=12m):     %d fundos, %d gestoras\n",
            F[amostra_B1 == TRUE, .N], F[amostra_B1 == TRUE, uniqueN(gestora_grupo)]))
add("amostra", "fundos_amostra_plano", F[amostra_plano == TRUE, .N])
add("amostra", "fundos_amostra_B1", F[amostra_B1 == TRUE, .N])
add("amostra", "gestoras_amostra_B1", F[amostra_B1 == TRUE, uniqueN(gestora_grupo)])

# --- Q3/Q4: Anbima + soma_peso ----------------------------------------------
cat("\n--- Q3: distribuicao Anbima restrita a amostra do plano ---\n")
an <- fread(file.path(DD, "_cache_classif_anbima.csv"), showProgress = FALSE)
setnames(an, tolower(names(an)))
an[, cod_fundo := as.character(cod_fundo)]
an_treino <- an[ym < CORTE_TREINO]
# classe modal no treino
cls <- an_treino[, .N, by = .(cod_fundo, classif_anbima)][order(cod_fundo, -N)]
cls_modal <- cls[, .SD[1], by = cod_fundo][, .(cod_fundo, classe = classif_anbima)]
n_classes <- an_treino[, .(n_classes = uniqueN(classif_anbima)), by = cod_fundo]
cls_modal <- merge(cls_modal, n_classes, by = "cod_fundo")

F <- merge(F, cls_modal, by = "cod_fundo", all.x = TRUE)
F[is.na(classe), classe := "SEM CLASSIFICACAO NO TREINO"]

tab_an <- F[amostra_plano == TRUE, .N, by = classe][order(-N)]
tab_an[, pct := round(100 * N / sum(N), 1)]
print(tab_an)
for (i in seq_len(nrow(tab_an))) {
  add("Q3_anbima", paste0("n_", tab_an$classe[i]), tab_an$N[i])
}
cat(sprintf("\nFundos que TROCAM de classe no treino: %d (%.1f%% da amostra do plano)\n",
            F[amostra_plano == TRUE & n_classes > 1, .N],
            100 * F[amostra_plano == TRUE & n_classes > 1, .N] / F[amostra_plano == TRUE, .N]))

cat("\n--- Q4: soma_peso (fatia de acoes) por classe -- O CONFUNDIDOR ---\n")
sp <- fread(file.path(DD, "checagem_soma_peso_universo_completo.csv"), showProgress = FALSE)
sp[, cod_fundo := as.character(cod_fundo)]
sp[, ym := ano * 100L + mes]
sp_treino <- sp[ym < CORTE_TREINO, .(soma_peso_medio = mean(soma_peso),
                                     n_ativos_bruto  = mean(n_ativos)),
                by = cod_fundo]
F <- merge(F, sp_treino, by = "cod_fundo", all.x = TRUE)

q4 <- F[amostra_plano == TRUE & !is.na(soma_peso_medio),
        .(n = .N,
          soma_peso_medio = round(mean(soma_peso_medio), 3),
          mediana         = round(median(soma_peso_medio), 3),
          pct_abaixo_20   = round(100 * mean(soma_peso_medio < 0.20), 1),
          n_ativos        = round(median(n_ativos_bruto), 0)),
        by = classe][order(-n)]
print(q4)

# --- Q5: n_ativos bruto vs filtrado ------------------------------------------
# As duas contagens TEM que ser feitas na mesma unidade (fundo-mes) e no mesmo
# periodo (treino), senao a versao "filtrada" pode sair maior que a bruta: ao
# longo de 36 meses um fundo carrega mais ativos DISTINTOS do que carrega em
# qualquer mes isolado. Conta por fundo-mes e so depois promedia.
cat("\n--- Q5: n_ativos por fundo-mes, bruto vs. pos-filtro de poeira ---\n")
pp_treino[, ok_poeira := FALSE]
pp_treino[cel_ok, on = .(cod_fundo, ativo), ok_poeira := TRUE]
nat_mes <- pp_treino[, .(n_bruto = .N, n_filt = sum(ok_poeira)), by = .(cod_fundo, ym)]
nat <- nat_mes[, .(n_ativos_bruto_mes = mean(n_bruto),
                   n_ativos_filt_mes  = mean(n_filt)), by = cod_fundo]
F <- merge(F, nat, by = "cod_fundo", all.x = TRUE)
Fp <- F[amostra_plano == TRUE & !is.na(n_ativos_bruto_mes) & n_ativos_bruto_mes > 0]
cat(sprintf("Mediana n_ativos/mes bruto: %.0f | filtrado: %.0f | razao mediana: %.2f\n",
            median(Fp$n_ativos_bruto_mes), median(Fp$n_ativos_filt_mes),
            median(Fp$n_ativos_filt_mes / Fp$n_ativos_bruto_mes)))
cat("Quantis da razao filtrado/bruto (quanto da carteira NAO e poeira):\n")
print(round(quantile(Fp$n_ativos_filt_mes / Fp$n_ativos_bruto_mes, c(.1,.25,.5,.75,.9)), 3))
add("Q5_breadth", "mediana_n_ativos_bruto_mes", round(median(Fp$n_ativos_bruto_mes), 1))
add("Q5_breadth", "mediana_n_ativos_filt_mes", round(median(Fp$n_ativos_filt_mes), 1))
add("Q5_breadth", "mediana_razao_filt_bruto",
    round(median(Fp$n_ativos_filt_mes / Fp$n_ativos_bruto_mes), 3))

# --- Q6: cobertura de preco --------------------------------------------------
cat("\n--- Q6: cobertura de preco, ponderada por peso ---\n")
pr <- fread(file.path(DD, "precos_mensais_final.csv"),
            select = c("ticker", "ymk", "retorno", "fonte"), showProgress = FALSE)
tick_pr <- unique(pr$ticker)
pp[, ticker := trimws(sub(".*- ", "", ativo))]
cob <- pp[, .(peso_total = sum(peso)), by = ticker]
cob[, tem_preco := ticker %in% tick_pr]
cat(sprintf("Tickers no painel: %d | com preco: %d | sem preco: %d\n",
            nrow(cob), cob[tem_preco == TRUE, .N], cob[tem_preco == FALSE, .N]))
cat(sprintf("Fracao do peso total SEM preco: %.4f%%\n",
            100 * cob[tem_preco == FALSE, sum(peso_total)] / cob[, sum(peso_total)]))
cat("Tickers sem preco (top 12 por peso):\n")
print(cob[tem_preco == FALSE][order(-peso_total)][1:min(12, .N), .(ticker, peso_total = round(peso_total, 2))])
add("Q6_preco", "tickers_sem_preco", cob[tem_preco == FALSE, .N])
add("Q6_preco", "pct_peso_sem_preco",
    round(100 * cob[tem_preco == FALSE, sum(peso_total)] / cob[, sum(peso_total)], 4))

cat("\nFonte do preco (linhas):\n")
print(pr[, .N, by = fonte])
add("Q6_preco", "linhas_yahoo_ajustado", pr[fonte == "yahoo_ajustado", .N])
add("Q6_preco", "linhas_b3_bruto", pr[fonte == "b3_bruto", .N])

# --- grava -------------------------------------------------------------------
fwrite(rbindlist(out), file.path(DD, "diag_amostra_extensao.csv"))
fwrite(F, file.path(DD, "diag_fundos_extensao.csv"))
cat("\n=============================================================\n")
cat("OK - gravado diag_amostra_extensao.csv e diag_fundos_extensao.csv\n")
cat("=============================================================\n")
