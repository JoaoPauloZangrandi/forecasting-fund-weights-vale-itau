# =============================================================================
# 116_torneio_diagnostico_q4.R  (v2 OFICIAL / trilha TORNEIO)
#
# POR QUE O 4o TRIMESTRE DA' RESULTADOS OPOSTOS EM DOIS DESENHOS?
#
#   script 114: classificacao em SETEMBRO, base = 1o ao 3o trimestre
#               -> perdedor realoca MAIS no 4o tri (+0,090 log-ponto)
#   script 115: classificacao em JUNHO,   base = 1o semestre
#               -> vencedor realoca MAIS no 4o tri (+0,547 sobre o excesso)
#
# A janela medida e' a MESMA (4o trimestre). So' mudam duas coisas: a data em
# que o fundo e' classificado e o periodo que vai no denominador. Este script
# cruza as duas, 2 x 2, para ver qual das duas carrega a inversao.
#
# A suspeita e' o DENOMINADOR. Se o vencedor realoca mais no 3o trimestre --
# e o script 115 mostra que sim (+0,275, p = 0,010) -- entao por' o 3o
# trimestre dentro da base infla o denominador do vencedor e derruba a razao
# dele por construcao, fabricando um "perdedor mexe mais" que e' artefato.
# Se for isso, o desenho com base no 1o semestre e' o correto, e o achado do
# script 114 cai.
#
# Entradas: erro_e_multiativo.csv, torneio_desempenho_fundo_mes.csv,
#           precos_mensais_final.csv, retorno_fundo_mensal.csv
# Saida:    data/torneio_diagnostico_q4.csv
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DD <- file.path(REPO, "v2 OFICIAL/data")
setFixest_notes(FALSE)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

E <- fread(file.path(DD, "erro_e_multiativo.csv"),
           select = c("cod_fundo","ativo","ym","peso","peso_pred"))
E[, cod_fundo := as.character(cod_fundo)]
fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
M <- merge(E[, .(cod_fundo, ativo, ym, peso, ym_fut = addm(ym, 1L))],
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
M <- M[is.finite(dw)]
M[, ano := ym %/% 100L]
M[, mes := ym %% 100L]

Q <- M[, .(m = mean(abs(dw)), n = .N), by = .(cod_fundo, ano, mes)]
rm(M); invisible(gc())

P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"))
P[, cod_fundo := as.character(cod_fundo)]
CLS <- merge(
  unique(P[!is.na(perdedor_bench_jun), .(cod_fundo, ano, exc_jun = excesso_interim_jun,
                                          perd_jun = perdedor_bench_jun)]),
  unique(P[!is.na(perdedor_bench_set), .(cod_fundo, ano, exc_set = excesso_interim_set,
                                          perd_set = perdedor_bench_set)]),
  by = c("cod_fundo","ano"))

agg <- function(meses) Q[mes %in% meses, .(v = weighted.mean(m, n), n = sum(n)),
                         by = .(cod_fundo, ano)]
Q4   <- agg(10:12); setnames(Q4, c("v","n"), c("q4","n_q4"))
B_H1 <- agg(1:5);   setnames(B_H1, c("v","n"), c("base_h1","n_h1"))
B_Q13<- agg(1:9);   setnames(B_Q13, c("v","n"), c("base_q13","n_q13"))

D <- merge(Q4, B_H1, by = c("cod_fundo","ano"))
D <- merge(D, B_Q13, by = c("cod_fundo","ano"))
D <- merge(D, CLS, by = c("cod_fundo","ano"))
D <- D[n_q4 >= 5 & n_h1 >= 10 & n_q13 >= 15]
D[, l_base_h1  := log(q4 / base_h1)]
D[, l_base_q13 := log(q4 / base_q13)]
D <- D[is.finite(l_base_h1) & is.finite(l_base_q13)]
cat("Fundo-ano no diagnostico:", nrow(D), "| fundos:", uniqueN(D$cod_fundo), "\n")

cat("\n\n=========== 2 x 2: data de classificacao x denominador ===========\n")
cat("Variavel dependente: log(realocacao no 4o tri / realocacao na base)\n")
cat("Variavel de interesse: excesso acumulado ate a data de classificacao\n")
cat("Sinal POSITIVO = vencedor realoca mais no 4o tri.\n")

res <- list()
for (cls in c("jun","set")) {
  for (bs in c("h1","q13")) {
    dep <- paste0("l_base_", bs)
    reg <- paste0("exc_", cls)
    f <- feols(as.formula(paste0(dep, " ~ ", reg, " | ano")), data = D, cluster = ~cod_fundo)
    cat(sprintf("\n>>> classificacao em %s | base = %s\n", toupper(cls), toupper(bs)))
    print(summary(f))
    res[[length(res)+1]] <- data.table(classificacao = cls, base = bs,
                                       coef = as.numeric(coef(f)[reg]),
                                       se = as.numeric(se(f)[reg]),
                                       p = as.numeric(pvalue(f)[reg]),
                                       n = f$nobs)
  }
}
R <- rbindlist(res)
cat("\n\n=========== RESUMO ===========\n")
print(R[, .(classificacao, base, coef = round(coef,3), se = round(se,3),
            p = signif(p,3), n)])

cat("\n--- mesma coisa com o DUMMY de perdedor (replica exata do script 114) ---\n")
for (cls in c("jun","set")) {
  for (bs in c("h1","q13")) {
    f <- feols(as.formula(paste0("l_base_", bs, " ~ perd_", cls, " | ano")),
               data = D, cluster = ~cod_fundo)
    cat(sprintf("classificacao %s | base %s : coef = %+.4f (p = %.2e)\n",
                toupper(cls), toupper(bs),
                coef(f)[paste0("perd_", cls)], pvalue(f)[paste0("perd_", cls)]))
  }
}

cat("\n--- prova direta: a base com 3o trimestre depende do desempenho? ---\n")
D[, l_b_h1 := log(base_h1)]
D[, l_b_q13 := log(base_q13)]
cat("\n>>> log(base 1o semestre) ~ excesso ate junho\n")
print(summary(feols(l_b_h1 ~ exc_jun | ano, data = D, cluster = ~cod_fundo)))
cat("\n>>> log(base 1o-3o tri) ~ excesso ate junho\n")
print(summary(feols(l_b_q13 ~ exc_jun | ano, data = D, cluster = ~cod_fundo)))

fwrite(R, file.path(DD, "torneio_diagnostico_q4.csv"))
cat("\nOK - 116 concluido\n")
