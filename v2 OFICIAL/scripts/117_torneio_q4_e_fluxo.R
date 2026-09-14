# =============================================================================
# 117_torneio_q4_e_fluxo.R  (v2 OFICIAL / trilha TORNEIO)
#
# O script 116 mostrou que a realocacao do 4o trimestre responde ao desempenho
# RECENTE (3o trimestre), nao a posicao acumulada no ano: classificar em junho
# e classificar em setembro dao sinais opostos para a MESMA janela medida, e o
# denominador nao tem nada a ver com isso.
#
# Isso abre uma explicacao bem mais banal que torneio. Quem vai mal no 3o
# trimestre sofre resgate no 4o. Resgate forca negociacao -- o gestor vende
# porque precisa de caixa, nao porque decidiu apostar. O movimento de carteira
# seria consequencia do passivo, nao estrategia de ranking.
#
# As duas hipoteses se separam: se o efeito do 3o trimestre sobre a realocacao
# do 4o SOBREVIVER ao controle de fluxo, sobra espaco para torneio; se ele
# sumir, era resgate.
#
# O fluxo ja' e' uma das seis caracteristicas do TCC (flow_aum = fluxo sobre
# patrimonio), entao nao ha' dado novo a extrair.
#
# Entradas: erro_e_multiativo.csv, painel_universo_completo_final.csv,
#           torneio_desempenho_fundo_mes.csv, precos_mensais_final.csv,
#           retorno_fundo_mensal.csv
# Saida:    data/torneio_q4_fluxo.csv
# RODAR COM CAMINHO ABSOLUTO. Pesado (le o painel de 1,7 GB).
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DD <- file.path(REPO, "v2 OFICIAL/data")
setFixest_notes(FALSE)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# --- realocacao ativa por fundo-mes -----------------------------------------
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
Q <- M[, .(m = mean(abs(dw)), n = .N), by = .(cod_fundo, ano, mes)]
rm(M); invisible(gc())

agg <- function(meses, nm) { z <- Q[mes %in% meses, .(v = weighted.mean(m, n), n = sum(n)),
                                    by = .(cod_fundo, ano)]
                             setnames(z, c("v","n"), paste0(c("v_","n_"), nm)); z }
D <- merge(agg(10:12, "q4"), agg(1:5, "h1"), by = c("cod_fundo","ano"))
D <- D[n_q4 >= 5 & n_h1 >= 10]
D[, l_realoc_q4 := log(v_q4 / v_h1)]
D <- D[is.finite(l_realoc_q4)]

# --- desempenho: acumulado ate junho e retorno do 3o trimestre --------------
P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"))
P[, cod_fundo := as.character(cod_fundo)]
P[, mes := ym %% 100L]
jun <- P[mes == 6L, .(cod_fundo, ano, exc_jun = excesso_acum, acum_jun = ret_acum_fundo)]
set <- P[mes == 9L, .(cod_fundo, ano, exc_set = excesso_acum, acum_set = ret_acum_fundo)]
DES <- merge(jun, set, by = c("cod_fundo","ano"))
# retorno do 3o trimestre, em excesso: a diferenca dos acumulados
DES[, exc_q3 := exc_set - exc_jun]
D <- merge(D, DES, by = c("cod_fundo","ano"))

# --- fluxo do 4o trimestre (caracteristica ja' existente no painel) ---------
cat("== lendo fluxo do painel (grande) ==\n")
PN <- fread(file.path(DD, "painel_universo_completo_final.csv"),
            select = c("cod_fundo","ym","flow_aum"))
PN[, cod_fundo := as.character(cod_fundo)]
FL <- unique(PN)[, .(flow = flow_aum[1]), by = .(cod_fundo, ym)]
rm(PN); invisible(gc())
FL[, ano := ym %/% 100L]; FL[, mes := ym %% 100L]
FQ <- FL[mes %in% 10:12, .(flow_q4 = sum(flow, na.rm = TRUE),
                           absflow_q4 = sum(abs(flow), na.rm = TRUE),
                           n_f = .N), by = .(cod_fundo, ano)]
FQ <- FQ[n_f >= 2]
D <- merge(D, FQ, by = c("cod_fundo","ano"))
D <- D[is.finite(exc_jun) & is.finite(exc_q3) & is.finite(flow_q4)]
cat("Fundo-ano final:", nrow(D), "| fundos:", uniqueN(D$cod_fundo), "\n")
cat("\nDistribuicao do fluxo liquido do 4o tri (sobre patrimonio):\n")
print(round(quantile(D$flow_q4, c(.05,.25,.5,.75,.95)), 4))

cat("\n\n=========== O QUE PREVE A REALOCACAO DO 4o TRIMESTRE? ===========\n")
cat("Dependente: log(realocacao ativa no 4o tri / no 1o semestre)\n")

cat("\n>>> (1) so' o acumulado ate junho\n")
print(summary(feols(l_realoc_q4 ~ exc_jun | ano, data = D, cluster = ~cod_fundo)))
cat("\n>>> (2) so' o 3o trimestre\n")
print(summary(feols(l_realoc_q4 ~ exc_q3 | ano, data = D, cluster = ~cod_fundo)))
cat("\n>>> (3) os dois juntos -- qual carrega o efeito?\n")
print(summary(feols(l_realoc_q4 ~ exc_jun + exc_q3 | ano, data = D, cluster = ~cod_fundo)))
cat("\n>>> (4) A PERGUNTA: o 3o trimestre sobrevive ao controle de fluxo?\n")
print(summary(feols(l_realoc_q4 ~ exc_jun + exc_q3 + flow_q4 + absflow_q4 | ano,
                    data = D, cluster = ~cod_fundo)))

cat("\n>>> (5) o 3o trimestre prediz o fluxo do 4o? (o elo da hipotese de resgate)\n")
print(summary(feols(flow_q4 ~ exc_q3 + exc_jun | ano, data = D, cluster = ~cod_fundo)))

fwrite(D, file.path(DD, "torneio_q4_fluxo.csv"))
cat("\nOK - 117 concluido\n")
