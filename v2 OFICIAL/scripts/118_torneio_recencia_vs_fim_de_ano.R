# =============================================================================
# 118_torneio_recencia_vs_fim_de_ano.R  (v2 OFICIAL / trilha TORNEIO)
#
# O TESTE QUE DECIDE A INTERPRETACAO.
#
# O script 117 achou que a realocacao do 4o trimestre responde fortemente ao
# retorno do 3o trimestre (-1,61) e, com sinal oposto, ao acumulado ate junho
# (+0,61) -- e que nada disso e' resgate.
#
# Sobram duas leituras, e elas sao empiricamente distinguiveis:
#
#   TORNEIO   -- o incentivo vem da avaliacao ANUAL. Entao o efeito tem que ser
#                especifico da passagem 3o -> 4o trimestre, que e' a ultima
#                chance de mudar o numero do ano.
#   RECENCIA  -- o gestor simplesmente reage ao trimestre que acabou de passar,
#                o ano todo. Entao o efeito aparece igual em 1o->2o, 2o->3o e
#                3o->4o, e nao tem nada de fim de exercicio.
#
# Teste: painel fundo-ano-trimestre, realocacao do trimestre corrente contra o
# excesso de retorno do trimestre ANTERIOR, com efeito fixo de fundo (absorve
# o nivel de atividade da casa) e de ano-trimestre (absorve sazonalidade e
# condicao de mercado). Depois interage com a dummy do 4o trimestre: se o
# coeficiente extra do 4o trimestre for nulo, e' recencia, nao torneio.
#
# Entradas: erro_e_multiativo.csv, torneio_desempenho_fundo_mes.csv,
#           precos_mensais_final.csv, retorno_fundo_mensal.csv
# Saida:    data/torneio_recencia_painel_trimestral.csv
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
M[, ano := ym %/% 100L]; M[, mes := ym %% 100L]
M[, tri := ceiling(mes/3)]

# realocacao ativa media por fundo-ano-trimestre
R <- M[, .(realoc = mean(abs(dw)), n = .N), by = .(cod_fundo, ano, tri)]
rm(M); invisible(gc())
R <- R[n >= 5]
R[, l_realoc := log(realoc)]
R <- R[is.finite(l_realoc)]
cat("Fundo-ano-trimestre:", nrow(R), "| fundos:", uniqueN(R$cod_fundo), "\n")

# --- excesso de retorno DO trimestre (nao acumulado) ------------------------
P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"))
P[, cod_fundo := as.character(cod_fundo)]
P[, mes := ym %% 100L]
# excesso acumulado nos fins de trimestre; a diferenca entre eles e' o excesso
# do trimestre
fim <- P[mes %in% c(3L,6L,9L,12L), .(cod_fundo, ano, tri = mes/3L,
                                     exc_acum = excesso_acum)]
setorder(fim, cod_fundo, ano, tri)
fim[, exc_tri := exc_acum - shift(exc_acum, 1L, fill = 0), by = .(cod_fundo, ano)]
fim[, exc_acum_ant := shift(exc_acum, 1L), by = .(cod_fundo, ano)]

# regressor: trimestre ANTERIOR ao medido
ant <- fim[, .(cod_fundo, ano, tri_alvo = tri + 1L,
               exc_tri_ant = exc_tri, exc_acum_ate_ant = exc_acum)]
D <- merge(R, ant, by.x = c("cod_fundo","ano","tri"),
           by.y = c("cod_fundo","ano","tri_alvo"))
D <- D[is.finite(exc_tri_ant) & is.finite(exc_acum_ate_ant)]
D[, anotri := paste0(ano, "T", tri)]
D[, t4 := as.integer(tri == 4L)]
cat("Amostra de estimacao:", nrow(D), "| fundos:", uniqueN(D$cod_fundo), "\n")
cat("\nObservacoes por trimestre medido:\n"); print(D[, .N, by = tri][order(tri)])

cat("\n\n=========== RECENCIA OU FIM DE ANO? ===========\n")
cat("Dependente: log(realocacao ativa do trimestre)\n")
cat("EF de fundo (nivel de atividade da casa) + EF de ano-trimestre (mercado)\n")

cat("\n>>> (1) efeito do trimestre anterior, todos os trimestres juntos\n")
print(summary(feols(l_realoc ~ exc_tri_ant | cod_fundo + anotri,
                    data = D, cluster = ~cod_fundo)))

cat("\n>>> (2) separando o efeito por trimestre medido\n")
print(summary(feols(l_realoc ~ i(tri, exc_tri_ant) | cod_fundo + anotri,
                    data = D, cluster = ~cod_fundo)))

cat("\n>>> (3) O TESTE: o 4o trimestre tem efeito EXTRA?\n")
print(summary(feols(l_realoc ~ exc_tri_ant + exc_tri_ant:t4 | cod_fundo + anotri,
                    data = D, cluster = ~cod_fundo)))

cat("\n>>> (4) recencia contra posicao acumulada, lado a lado\n")
print(summary(feols(l_realoc ~ exc_tri_ant + exc_acum_ate_ant | cod_fundo + anotri,
                    data = D, cluster = ~cod_fundo)))

cat("\n>>> (5) o mesmo, deixando o acumulado ter efeito extra no 4o trimestre\n")
print(summary(feols(l_realoc ~ exc_tri_ant + exc_acum_ate_ant +
                      exc_tri_ant:t4 + exc_acum_ate_ant:t4 | cod_fundo + anotri,
                    data = D, cluster = ~cod_fundo)))

fwrite(D, file.path(DD, "torneio_recencia_painel_trimestral.csv"))
cat("\nOK - 118 concluido\n")
