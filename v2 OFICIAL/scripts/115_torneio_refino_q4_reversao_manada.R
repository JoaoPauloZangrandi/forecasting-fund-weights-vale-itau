# =============================================================================
# 115_torneio_refino_q4_reversao_manada.R  (v2 OFICIAL / trilha TORNEIO)
#
# Tres perguntas que ficaram abertas depois dos scripts 111-114.
#
# (A) INVERSAO DENTRO DO ANO. O desenho semestral diz que quem realoca mais e'
#     o VENCEDOR; o desenho do 4o trimestre diz que e' o PERDEDOR. Se fosse so'
#     diluicao -- o efeito do 4o tri se perdendo dentro de 6 meses -- o
#     coeficiente semestral iria a zero, nao trocaria de sinal. Ele troca. Logo
#     o 3o trimestre tem que estar puxando para o outro lado. Aqui o ano e'
#     cortado trimestre a trimestre, com a MESMA classificacao predeterminada
#     de junho e a MESMA base (1o semestre), para ver o perfil.
#
# (B) TORNEIO OU WINDOW DRESSING? Marques, Sampaio e Silva (2020, RC&F) acharam
#     window dressing no Brasil concentrado em "gestora pequena, perdedora
#     contra o Ibovespa, tracking error alto". Window dressing tambem e' um
#     aumento de movimentacao no fim do periodo -- entao o achado do 4o
#     trimestre do script 114 pode ser maquiagem de vitrine, nao aumento
#     genuino de risco. As duas hipoteses se separam por uma previsao nitida:
#       - window dressing REVERTE: o que foi comprado em dezembro e' vendido
#         em janeiro, porque a posicao existia so' para a foto da carteira;
#       - torneio PERMANECE: a aposta foi feita para mudar o retorno, e o
#         retorno so' vem se a posicao ficar.
#     Teste: dw de dezembro->janeiro regredido no dw de novembro->dezembro.
#     Coeficiente negativo = reversao. Interacao com "perdedor" negativa =
#     o perdedor reverte mais, ou seja, maquiagem. Placebo no meio do ano,
#     onde nao ha' incentivo de fim de exercicio.
#
# (C) MANADA CONTEMPORANEA vs COPIA DEFASADA. O script 113 nao achou nenhum
#     sinal de seguir o lider com defasagem. Isso parece contradizer a
#     literatura brasileira de efeito manada (Kutchukian 2010 e seguintes, que
#     usam a medida de Lakonishok, Shleifer e Vishny 1992). Nao contradiz: LSV
#     mede aglomeracao CONTEMPORANEA na direcao das operacoes, e nos medimos
#     seguir com atraso. Sao objetos diferentes. Colocando os dois na mesma
#     regressao, a comparacao fica explicita -- e um contemporaneo grande com
#     defasado nulo e' a assinatura de informacao comum, nao de imitacao.
#
# Entradas: erro_e_multiativo.csv, torneio_desempenho_fundo_mes.csv,
#           precos_mensais_final.csv, retorno_fundo_mensal.csv
# Saidas:   data/torneio_refino_perfil_trimestral.csv
#           data/torneio_refino_reversao_janeiro.csv
# RODAR COM CAMINHO ABSOLUTO. Pesado.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DD <- file.path(REPO, "v2 OFICIAL/data")
setFixest_notes(FALSE)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

cat("== montando painel de realocacao ativa ==\n")
E <- fread(file.path(DD, "erro_e_multiativo.csv"),
           select = c("cod_fundo","ativo","ym","peso","peso_pred"))
E[, cod_fundo := as.character(cod_fundo)]
E[, d := peso_pred - peso]
fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
M <- merge(E[, .(cod_fundo, ativo, ym, d, peso, ym_fut = addm(ym, 1L))],
           fut, by.x = c("cod_fundo","ativo","ym_fut"),
           by.y = c("cod_fundo","ativo","ym_fut_key"))
rm(E, fut); invisible(gc())
M[, ticker := trimws(sub(".*- ", "", ativo))]
precos <- fread(file.path(DD, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setnames(precos, c("ymk","retorno"), c("ym_fut","r_ativo"))
rfundo <- fread(file.path(DD, "retorno_fundo_mensal.csv"), select = c("cod_fundo","ymk","retorno_fundo"))
rfundo[, cod_fundo := as.character(cod_fundo)]
setnames(rfundo, "ymk", "ym_fut")
M <- merge(M, precos, by = c("ticker","ym_fut"), all.x = TRUE)
M <- merge(M, rfundo, by = c("cod_fundo","ym_fut"), all.x = TRUE)
M[, dw := peso_fut - peso * (1 + r_ativo) / (1 + retorno_fundo)]
M <- M[is.finite(dw) & is.finite(d)]
M[, ano := ym %/% 100L]
M[, mes := ym %% 100L]
cat("Obs:", nrow(M), "| fundos:", uniqueN(M$cod_fundo), "\n")

P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"))
P[, cod_fundo := as.character(cod_fundo)]
CLS6 <- unique(P[!is.na(perdedor_bench_jun),
                 .(cod_fundo, ano, excesso_interim_jun, perdedor_bench_jun)])
CLS9 <- unique(P[!is.na(perdedor_bench_set),
                 .(cod_fundo, ano, excesso_interim_set, perdedor_bench_set)])

# =============================================================================
# (A) PERFIL TRIMESTRAL -- inversao ou diluicao?
# =============================================================================
cat("\n\n=========== (A) PERFIL TRIMESTRAL DA REALOCACAO ===========\n")
cat("Base fixa = 1o semestre. Classificacao fixa = junho. So muda o trimestre medido.\n")

Q <- M[, .(m = mean(abs(dw)), n = .N), by = .(cod_fundo, ano, mes)]
base <- Q[mes %in% 1:5, .(base = weighted.mean(m, n), n_base = sum(n)), by = .(cod_fundo, ano)]
tri <- Q[mes >= 7L, .(tri = weighted.mean(m, n), n_tri = sum(n)),
         by = .(cod_fundo, ano, trimestre = fifelse(mes <= 9L, "T3", "T4"))]
A <- merge(tri, base, by = c("cod_fundo","ano"))
A <- A[n_base >= 10 & n_tri >= 5]
A <- merge(A, CLS6, by = c("cod_fundo","ano"))
A[, l_ratio := log(tri / base)]
A <- A[is.finite(l_ratio)]

cat("\nFundo-ano-trimestre:", nrow(A), "\n")
cat("\n--- mediana da razao trimestre / 1o semestre ---\n")
print(dcast(A[, .(razao = round(median(tri/base), 3)), by = .(trimestre, perdedor_bench_jun)],
            trimestre ~ perdedor_bench_jun, value.var = "razao"))
cat("(colunas: 0 = venceu o Ibovespa ate junho, 1 = perdeu)\n")

for (tq in c("T3","T4")) {
  cat("\n>>> ", tq, ": log(razao) ~ excesso acumulado ate junho | ano\n", sep = "")
  print(summary(feols(l_ratio ~ excesso_interim_jun | ano,
                      data = A[trimestre == tq], cluster = ~cod_fundo)))
}
cat("\n>>> teste direto da INVERSAO: interacao trimestre x excesso\n")
A[, t4 := as.integer(trimestre == "T4")]
print(summary(feols(l_ratio ~ excesso_interim_jun + excesso_interim_jun:t4 + t4 | ano,
                    data = A, cluster = ~cod_fundo)))
fwrite(A, file.path(DD, "torneio_refino_perfil_trimestral.csv"))

# =============================================================================
# (B) TORNEIO OU WINDOW DRESSING? o teste de reversao em janeiro
# =============================================================================
cat("\n\n=========== (B) O MOVIMENTO DE DEZEMBRO REVERTE EM JANEIRO? ===========\n")

par_meses <- function(m1, rotulo) {
  x1 <- M[mes == m1, .(cod_fundo, ativo, ano, move1 = dw)]
  x2 <- M[mes == m1 + 1L, .(cod_fundo, ativo, ano, move2 = dw)]
  z <- merge(x1, x2, by = c("cod_fundo","ativo","ano"))
  z[, par := rotulo]
  z
}
# mes 11 -> dw de nov para dez (a carteira da foto de dezembro)
# mes 12 -> dw de dez para jan (a reversao, se houver)
DEZ <- merge(par_meses(11L, "nov-dez -> dez-jan"), CLS9, by = c("cod_fundo","ano"))
# placebo no meio do ano, sem incentivo de fim de exercicio
MEIO <- merge(par_meses(5L, "mai-jun -> jun-jul (placebo)"), CLS9, by = c("cod_fundo","ano"))

cat("Pares dezembro:", nrow(DEZ), "| pares placebo:", nrow(MEIO), "\n")
cat("Anos cobertos no teste de dezembro:", paste(sort(unique(DEZ$ano)), collapse = ", "),
    "-- 2021 sai porque exige janeiro de 2022\n")

roda_rev <- function(dt, rotulo) {
  cat("\n>>> ", rotulo, "\n", sep = "")
  cat("--- reversao media (todos os fundos) ---\n")
  print(summary(feols(move2 ~ move1 | cod_fundo, data = dt, cluster = ~cod_fundo)))
  cat("--- reversao separada por perdedor vs vencedor (setembro) ---\n")
  print(summary(feols(move2 ~ move1 + move1:perdedor_bench_set + perdedor_bench_set | cod_fundo,
                      data = dt, cluster = ~cod_fundo)))
}
roda_rev(DEZ, "DEZEMBRO -> JANEIRO")
roda_rev(MEIO, "PLACEBO MEIO DO ANO")

fwrite(rbindlist(list(DEZ, MEIO), use.names = TRUE, fill = TRUE),
       file.path(DD, "torneio_refino_reversao_janeiro.csv"))

# =============================================================================
# (C) MANADA CONTEMPORANEA vs COPIA DEFASADA
# =============================================================================
cat("\n\n=========== (C) COMOVIMENTO CONTEMPORANEO vs SEGUIR COM ATRASO ===========\n")
P2 <- P[, .(cod_fundo, ym, rank_pct)]
P2[, decil := cut(rank_pct, breaks = c(0, .1, .9, 1), labels = c("D1","meio","D10"),
                  include.lowest = TRUE)]
M <- merge(M, P2[, .(cod_fundo, ym, decil)], by = c("cod_fundo","ym"), all.x = TRUE)
M <- M[!is.na(decil)]

AGG <- M[, .(soma_tod = sum(dw), k_tod = .N), by = .(ativo, ym)]
AGG <- merge(AGG, M[decil == "D10", .(soma_lid = sum(dw), k_lid = .N), by = .(ativo, ym)],
             by = c("ativo","ym"), all.x = TRUE)
AGG[is.na(soma_lid), `:=`(soma_lid = 0, k_lid = 0L)]
OWN <- M[, .(cod_fundo, ativo, ym, dw_own = dw, era_lid = as.integer(decil == "D10"))]

junta <- function(L, sufixo) {
  A <- copy(AGG); A[, ym_alvo := addm(ym, L)]
  O <- copy(OWN); O[, ym_alvo := addm(ym, L)]
  K <- merge(M[, .(cod_fundo, ativo, ym)],
             A[, .(ativo, ym_alvo, soma_tod, k_tod, soma_lid, k_lid)],
             by.x = c("ativo","ym"), by.y = c("ativo","ym_alvo"))
  K <- merge(K, O[, .(cod_fundo, ativo, ym_alvo, dw_own, era_lid)],
             by.x = c("cod_fundo","ativo","ym"), by.y = c("cod_fundo","ativo","ym_alvo"),
             all.x = TRUE)
  K[is.na(dw_own), `:=`(dw_own = 0, era_lid = 0L)]
  K[, lid := fifelse(k_lid - era_lid > 0, (soma_lid - era_lid*dw_own)/(k_lid - era_lid), NA_real_)]
  K[, tod := fifelse(k_tod - 1L > 0, (soma_tod - dw_own)/(k_tod - 1L), NA_real_)]
  out <- K[, .(cod_fundo, ativo, ym, lid, tod)]
  setnames(out, c("lid","tod"), paste0(c("lid_","tod_"), sufixo))
  out
}
M <- merge(M, junta(0L, "c"), by = c("cod_fundo","ativo","ym"), all.x = TRUE)  # contemporaneo
M <- merge(M, junta(1L, "l"), by = c("cod_fundo","ativo","ym"), all.x = TRUE)  # defasado 1 mes
invisible(gc())

dt <- M[is.finite(lid_c) & is.finite(tod_c) & is.finite(lid_l) & is.finite(tod_l)]
cat("Obs:", nrow(dt), "\n")
dt[, ativo_ym := paste0(ativo, "_", ym)]
cat("\n>>> contemporaneo e defasado na MESMA regressao\n")
print(summary(feols(dw ~ d + lid_c + tod_c + lid_l + tod_l | cod_fundo + ym,
                    data = dt, cluster = ~ativo_ym)))
cat("\nLeitura: contemporaneo grande com defasado nulo = informacao comum, nao imitacao.\n")

cat("\nOK - 115 concluido\n")
