# =============================================================================
# 111_torneio_teste_a_lambda.R  (v2 OFICIAL / trilha TORNEIO)
#
# TESTE A -- "o fundo que esta' atras do benchmark muda a posicao dele mais
# abruptamente?" (pergunta levantada pelo professor; hipotese do torneio de
# Brown, Harlow e Starks 1996).
#
# Duas leituras da mesma pergunta, e elas NAO sao a mesma coisa:
#
#   A1. VELOCIDADE DE AJUSTE (lambda). O TCC estima um lambda unico pra todo
#       fundo. Aqui o lambda e' reestimado separadamente por grupo de
#       desempenho interino. Se o perdedor persegue o alvo mais rapido,
#       lambda_perdedor > lambda_vencedor.
#
#   A2. INTENSIDADE DE REALOCACAO. Analogo direto do Risk Adjustment Ratio de
#       BHS, so que sobre PESO de carteira em vez de volatilidade de retorno:
#         RAR_i,ano = media|dw| no 2o semestre / media|dw| no 1o semestre
#       Se o perdedor "mexe mais" depois de ficar pra tras, RAR_perdedor > 1 e
#       maior que o RAR do vencedor. Como e' uma razao dentro do MESMO fundo e
#       do MESMO ano, ela ja' controla o nivel de atividade caracteristico da
#       casa -- que e', pelo proprio TCC, a variavel que mais separa gestoras.
#
# CONFUNDIDOR TRATADO: um fundo perdedor perde porque as acoes dele cairam, e
# isso MECANICAMENTE desloca os pesos, sem nenhuma decisao do gestor. Entao
# todo teste roda em duas versoes:
#   dw          = peso_{t+1} - peso_t                    (bruto)
#   dw_corr     = peso_{t+1} - peso_t*(1+r_ativo)/(1+r_fundo)   (realocacao ativa)
# A versao corrigida e' a que responde a pergunta do professor; a bruta fica
# como comparacao, mesma convencao do script 62.
#
# Entradas: erro_e_multiativo.csv (motor oficial, script 99)
#           torneio_desempenho_fundo_mes.csv (script 110)
#           precos_mensais_final.csv, retorno_fundo_mensal.csv
# Saidas:   data/torneio_a_lambda_por_grupo.csv
#           data/torneio_a_intensidade_realocacao.csv
# RODAR COM CAMINHO ABSOLUTO. Pesado (le ~930 MB).
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DD <- file.path(REPO, "v2 OFICIAL/data")
setFixest_notes(FALSE)

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

cat("== lendo Etapa 1 (erro_e_multiativo) ==\n")
E <- fread(file.path(DD, "erro_e_multiativo.csv"),
           select = c("cod_fundo","ativo","ym","peso","peso_pred"))
E[, cod_fundo := as.character(cod_fundo)]
E[, d := peso_pred - peso]
cat("Obs Etapa 1:", nrow(E), "| fundos:", uniqueN(E$cod_fundo),
    "| meses:", min(E$ym), "a", max(E$ym), "\n")

# --- pares h=1 --------------------------------------------------------------
fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
M <- merge(E[, .(cod_fundo, ativo, ym, d, peso, ym_fut = addm(ym, 1L))],
           fut, by.x = c("cod_fundo","ativo","ym_fut"),
           by.y = c("cod_fundo","ativo","ym_fut_key"))
M[, dw := peso_fut - peso]
rm(E, fut); invisible(gc())
cat("Pares h=1:", nrow(M), "\n")

# --- correcao do efeito mecanico de preco -----------------------------------
M[, ticker := trimws(sub(".*- ", "", ativo))]
precos <- fread(file.path(DD, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setnames(precos, c("ymk","retorno"), c("ym_fut","r_ativo"))
rfundo <- fread(file.path(DD, "retorno_fundo_mensal.csv"), select = c("cod_fundo","ymk","retorno_fundo"))
rfundo[, cod_fundo := as.character(cod_fundo)]
setnames(rfundo, "ymk", "ym_fut")
M <- merge(M, precos, by = c("ticker","ym_fut"), all.x = TRUE)
M <- merge(M, rfundo, by = c("cod_fundo","ym_fut"), all.x = TRUE)
M[, peso_mecanico := peso * (1 + r_ativo) / (1 + retorno_fundo)]
M[, dw_corr := peso_fut - peso_mecanico]
cat("Pares com dw_corr disponivel:", M[is.finite(dw_corr), .N],
    sprintf("(%.1f%%)\n", 100*M[is.finite(dw_corr), .N]/nrow(M)))

# --- desempenho relativo ----------------------------------------------------
P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"))
P[, cod_fundo := as.character(cod_fundo)]
CLS <- unique(P[!is.na(perdedor_bench_jun),
                .(cod_fundo, ano, excesso_interim_jun, rank_interim_jun,
                  perdedor_bench_jun, quartil_rank_jun)])
M[, ano := ym %/% 100L]
M[, mes := ym %% 100L]
M <- merge(M, CLS, by = c("cod_fundo","ano"), all.x = TRUE)

H2 <- M[mes >= 7L & mes <= 11L & !is.na(perdedor_bench_jun)]
H1 <- M[mes >= 1L & mes <= 5L & !is.na(perdedor_bench_jun)]
cat("\nObs no 2o semestre com classificacao:", nrow(H2),
    "| fundos:", uniqueN(H2$cod_fundo), "\n")

# =============================================================================
# A1 -- lambda por grupo de desempenho interino
# =============================================================================
cat("\n\n=========== A1: VELOCIDADE DE AJUSTE (lambda) ===========\n")

lam_grupo <- function(dt, yvar, grupo_col, rotulo) {
  out <- dt[is.finite(get(yvar)) & is.finite(d),
            { fit <- feols(as.formula(paste0(yvar, " ~ 0 + d")), data = .SD, cluster = ~cod_fundo)
              .(lambda = as.numeric(coef(fit)["d"]),
                se = as.numeric(se(fit)["d"]),
                meia_vida = log(0.5)/log(1 - as.numeric(coef(fit)["d"])),
                n_obs = .N, n_fundos = uniqueN(cod_fundo)) },
            by = grupo_col, .SDcols = c(yvar, "d", "cod_fundo")]
  out[, `:=`(medida = yvar, criterio = rotulo)]
  setnames(out, grupo_col, "grupo")
  out[, grupo := as.character(grupo)]
  out[order(grupo)]
}

RES <- rbindlist(list(
  lam_grupo(H2, "dw",      "perdedor_bench_jun", "perdedor vs Ibovespa (jun)"),
  lam_grupo(H2, "dw_corr", "perdedor_bench_jun", "perdedor vs Ibovespa (jun)"),
  lam_grupo(H2, "dw",      "quartil_rank_jun",   "quartil de ranking (jun)"),
  lam_grupo(H2, "dw_corr", "quartil_rank_jun",   "quartil de ranking (jun)")
), use.names = TRUE)

print(RES[criterio == "perdedor vs Ibovespa (jun)"][order(medida, grupo)])
cat("\n(grupo 0 = venceu o Ibovespa ate junho; 1 = perdeu)\n")
cat("\n--- por quartil de ranking (1 = pior 25%, 4 = melhor 25%) ---\n")
print(RES[criterio == "quartil de ranking (jun)"][order(medida, grupo)])

# --- teste formal de diferenca (interacao, EP clusterizado por fundo) -------
cat("\n--- Interacao: dw ~ d + d:perdedor, EP clusterizado por fundo ---\n")
for (yv in c("dw","dw_corr")) {
  dt <- H2[is.finite(get(yv)) & is.finite(d)]
  fit <- feols(as.formula(paste0(yv, " ~ 0 + d + d:perdedor_bench_jun")),
               data = dt, cluster = ~cod_fundo)
  cat("\n>>> ", yv, "\n", sep = "")
  print(summary(fit))
}

# --- placebo: mesma classificacao de junho aplicada ao 1o semestre ----------
# O desempenho de junho nao pode ter causado o comportamento de janeiro a
# maio. Se a diferenca aparecer aqui tambem, ela e' traco permanente do fundo
# e nao resposta ao torneio.
cat("\n--- PLACEBO: classificacao de junho aplicada ao 1o SEMESTRE ---\n")
PLA <- rbindlist(list(
  lam_grupo(H1, "dw",      "perdedor_bench_jun", "PLACEBO 1o semestre"),
  lam_grupo(H1, "dw_corr", "perdedor_bench_jun", "PLACEBO 1o semestre")
), use.names = TRUE)
print(PLA[order(medida, grupo)])

fwrite(rbindlist(list(RES, PLA), use.names = TRUE),
       file.path(DD, "torneio_a_lambda_por_grupo.csv"))

# =============================================================================
# A2 -- intensidade de realocacao (RAR de BHS aplicado a peso)
# =============================================================================
cat("\n\n=========== A2: INTENSIDADE DE REALOCACAO ===========\n")

intens <- function(dt, yvar, semestre) {
  z <- dt[is.finite(get(yvar)), .(m = mean(abs(get(yvar))), n = .N),
          by = .(cod_fundo, ano)]
  setnames(z, c("m","n"), paste0(c("m_","n_"), semestre))
  z
}
A <- merge(intens(H1, "dw_corr", "h1"), intens(H2, "dw_corr", "h2"),
           by = c("cod_fundo","ano"))
B <- merge(intens(H1, "dw", "h1"), intens(H2, "dw", "h2"),
           by = c("cod_fundo","ano"))
setnames(A, c("m_h1","m_h2"), c("m_h1_corr","m_h2_corr"))
setnames(B, c("m_h1","m_h2"), c("m_h1_bruto","m_h2_bruto"))
RAR <- merge(A[, .(cod_fundo, ano, m_h1_corr, m_h2_corr, n_h1, n_h2)],
             B[, .(cod_fundo, ano, m_h1_bruto, m_h2_bruto)],
             by = c("cod_fundo","ano"))
# exige um minimo de observacoes nos dois semestres pra razao ser estavel
RAR <- RAR[n_h1 >= 10 & n_h2 >= 10]
RAR[, rar_corr  := m_h2_corr  / m_h1_corr]
RAR[, rar_bruto := m_h2_bruto / m_h1_bruto]
RAR <- merge(RAR, CLS, by = c("cod_fundo","ano"))
RAR <- RAR[is.finite(rar_corr) & is.finite(rar_bruto) & rar_corr > 0 & rar_bruto > 0]

cat("Fundo-ano com RAR calculavel:", nrow(RAR), "| fundos:", uniqueN(RAR$cod_fundo), "\n\n")
cat("--- RAR por grupo (mediana; 1 = mexeu igual nos dois semestres) ---\n")
print(RAR[, .(n = .N,
              rar_corr_mediana  = round(median(rar_corr), 3),
              rar_bruto_mediana = round(median(rar_bruto), 3)),
          by = .(perdedor_bench_jun)][order(perdedor_bench_jun)])
cat("\n--- RAR por quartil de ranking de junho ---\n")
print(RAR[, .(n = .N,
              rar_corr_mediana  = round(median(rar_corr), 3),
              rar_bruto_mediana = round(median(rar_bruto), 3)),
          by = .(quartil_rank_jun)][order(quartil_rank_jun)])

cat("\n--- Teste formal: log(RAR) ~ perdedor, com efeito fixo de ano ---\n")
RAR[, l_rar_corr := log(rar_corr)]
RAR[, l_rar_bruto := log(rar_bruto)]
print(summary(feols(l_rar_corr ~ perdedor_bench_jun | ano, data = RAR, cluster = ~cod_fundo)))
cat("\n--- Versao continua: log(RAR) ~ excesso acumulado ate junho ---\n")
print(summary(feols(l_rar_corr ~ excesso_interim_jun | ano, data = RAR, cluster = ~cod_fundo)))
cat("\n--- Mesma coisa na medida BRUTA (com efeito mecanico de preco) ---\n")
print(summary(feols(l_rar_bruto ~ excesso_interim_jun | ano, data = RAR, cluster = ~cod_fundo)))

fwrite(RAR, file.path(DD, "torneio_a_intensidade_realocacao.csv"))
cat("\nOK - 111 concluido\n")
