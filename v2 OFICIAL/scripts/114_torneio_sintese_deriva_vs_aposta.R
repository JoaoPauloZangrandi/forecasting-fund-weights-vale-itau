# =============================================================================
# 114_torneio_sintese_deriva_vs_aposta.R  (v2 OFICIAL / trilha TORNEIO)
#
# SINTESE DOS TESTES A, B e C. Os tres resultados parecem brigar entre si:
#   A) o fundo que esta' atras ajusta MAIS DEVAGAR (lambda menor) e realoca
#      MENOS (razao 2o/1o semestre menor);
#   B) e, ainda assim, no desenho de Li, Tiwari e Tong, ele termina o ano com
#      desvio MAIOR em relacao ao alvo das caracteristicas.
#
# So ha' uma maneira de as duas coisas serem verdade ao mesmo tempo: o desvio
# do perdedor nao cresce porque ele mexe na carteira, e sim porque ele NAO
# mexe. O preco move os pesos sozinho, o gestor nao recompoe, e a carteira
# vai ficando cada vez mais longe do que as caracteristicas dele implicam.
# Nao e' aposta -- e' deriva.
#
# Este script testa isso decompondo a variacao de peso em duas parcelas que o
# resto da literatura nao consegue separar, porque nao observa carteira:
#   ATIVA   = peso_{t+1} - peso_t*(1+r_ativo)/(1+r_fundo)   (decisao do gestor)
#   PASSIVA = peso_t*(1+r_ativo)/(1+r_fundo) - peso_t       (efeito de preco)
#
# Previsao: quem esta' atras tem parcela ATIVA menor e parcela PASSIVA maior.
#
# Entradas: erro_e_multiativo.csv, torneio_desempenho_fundo_mes.csv,
#           precos_mensais_final.csv, retorno_fundo_mensal.csv,
#           torneio_b_desvio_alvo_fundo_semestre.csv (script 112)
# Saidas:   data/torneio_sintese_decomposicao.csv
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DD <- file.path(REPO, "v2 OFICIAL/data")
setFixest_notes(FALSE)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

cat("== montando decomposicao ativa/passiva ==\n")
E <- fread(file.path(DD, "erro_e_multiativo.csv"),
           select = c("cod_fundo","ativo","ym","peso","peso_pred"))
E[, cod_fundo := as.character(cod_fundo)]
fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
M <- merge(E[, .(cod_fundo, ativo, ym, peso, peso_pred, ym_fut = addm(ym, 1L))],
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

M[, peso_mec := peso * (1 + r_ativo) / (1 + retorno_fundo)]
M[, dw_ativa  := peso_fut - peso_mec]     # o que o gestor fez
M[, dw_passiva := peso_mec - peso]        # o que o preco fez sozinho
M <- M[is.finite(dw_ativa) & is.finite(dw_passiva)]
M[, ano := ym %/% 100L]
M[, mes := ym %% 100L]
cat("Obs:", nrow(M), "| fundos:", uniqueN(M$cod_fundo), "\n")

# --- agrega por fundo-semestre ---------------------------------------------
S <- M[mes %in% c(1:5, 7:11),
       .(ativa = mean(abs(dw_ativa)), passiva = mean(abs(dw_passiva)), n = .N),
       by = .(cod_fundo, ano, sem = fifelse(mes <= 6L, 1L, 2L))]
S <- S[n >= 10]
W <- dcast(S, cod_fundo + ano ~ sem, value.var = c("ativa","passiva","n"))
setnames(W, c("ativa_1","ativa_2","passiva_1","passiva_2","n_1","n_2"),
         c("ativa_h1","ativa_h2","passiva_h1","passiva_h2","n_h1","n_h2"))
W <- W[!is.na(ativa_h1) & !is.na(ativa_h2)]

P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"))
P[, cod_fundo := as.character(cod_fundo)]
CLS <- unique(P[!is.na(perdedor_bench_jun),
                .(cod_fundo, ano, excesso_interim_jun, perdedor_bench_jun, quartil_rank_jun)])
W <- merge(W, CLS, by = c("cod_fundo","ano"))
W[, l_ativa   := log(ativa_h2 / ativa_h1)]
W[, l_passiva := log(passiva_h2 / passiva_h1)]
W[, share_ativa_h2 := ativa_h2 / (ativa_h2 + passiva_h2)]
W <- W[is.finite(l_ativa) & is.finite(l_passiva)]
cat("Fundo-ano:", nrow(W), "| fundos:", uniqueN(W$cod_fundo), "\n")

cat("\n=========== DERIVA OU APOSTA? ===========\n")
cat("\n--- medianas por grupo (razao 2o/1o semestre) ---\n")
print(W[, .(n = .N,
            realocacao_ATIVA  = round(median(ativa_h2/ativa_h1), 3),
            deriva_PASSIVA    = round(median(passiva_h2/passiva_h1), 3),
            fracao_ativa_no_2sem = round(median(share_ativa_h2), 3)),
        by = perdedor_bench_jun][order(perdedor_bench_jun)])
cat("(0 = venceu o Ibovespa ate junho; 1 = perdeu)\n")

cat("\n--- por quartil de ranking de junho (1 = pior) ---\n")
print(W[, .(n = .N,
            realocacao_ATIVA = round(median(ativa_h2/ativa_h1), 3),
            deriva_PASSIVA   = round(median(passiva_h2/passiva_h1), 3)),
        by = quartil_rank_jun][order(quartil_rank_jun)])

cat("\n--- Teste formal: parcela ATIVA ---\n")
print(summary(feols(l_ativa ~ excesso_interim_jun | ano, data = W, cluster = ~cod_fundo)))
cat("\n--- Teste formal: parcela PASSIVA ---\n")
print(summary(feols(l_passiva ~ excesso_interim_jun | ano, data = W, cluster = ~cod_fundo)))
cat("\n--- Fracao do movimento que e' decisao do gestor, no 2o semestre ---\n")
print(summary(feols(share_ativa_h2 ~ excesso_interim_jun | ano, data = W, cluster = ~cod_fundo)))

# --- teste de mediacao: o desvio maior do perdedor sobrevive ao controle? ---
cat("\n\n=========== MEDIACAO: o desvio do perdedor e' explicado pela deriva? ===========\n")
B <- fread(file.path(DD, "torneio_b_desvio_alvo_fundo_semestre.csv"))
B[, cod_fundo := as.character(cod_fundo)]
Z <- merge(B[, .(cod_fundo, ano, l_ratio_rms, l_ratio_as)],
           W[, .(cod_fundo, ano, l_ativa, l_passiva, excesso_interim_jun, perdedor_bench_jun)],
           by = c("cod_fundo","ano"))
cat("Fundo-ano na mediacao:", nrow(Z), "\n")
cat("\n>>> (1) sem controle\n")
print(summary(feols(l_ratio_rms ~ excesso_interim_jun | ano, data = Z, cluster = ~cod_fundo)))
cat("\n>>> (2) controlando pela realocacao ATIVA\n")
print(summary(feols(l_ratio_rms ~ excesso_interim_jun + l_ativa | ano, data = Z, cluster = ~cod_fundo)))
cat("\n>>> (3) controlando pelas duas parcelas\n")
print(summary(feols(l_ratio_rms ~ excesso_interim_jun + l_ativa + l_passiva | ano,
                    data = Z, cluster = ~cod_fundo)))

fwrite(W, file.path(DD, "torneio_sintese_decomposicao.csv"))

# =============================================================================
# O UNICO RESULTADO PRO-TORNEIO: o 4o trimestre. E' decisao ou deriva?
# =============================================================================
# O script 112 achou, no desenho de Li, Tiwari e Tong (classifica em setembro,
# mede no 4o trimestre), que o perdedor termina o ano com desvio MAIOR -- o
# unico achado da bateria que vai no sentido da hipotese do torneio. Como
# aqui a carteira e' observada, da' pra perguntar o que os autores americanos
# nao conseguem perguntar: esse desvio maior veio de o gestor MEXER, ou de ele
# NAO mexer enquanto o preco mexia por ele?
cat("\n\n=========== O 4o TRIMESTRE: DECISAO OU DERIVA? ===========\n")
Q <- M[mes %in% c(1:9, 10:12),
       .(ativa = mean(abs(dw_ativa)), passiva = mean(abs(dw_passiva)), n = .N),
       by = .(cod_fundo, ano, per = fifelse(mes <= 9L, "q13", "q4"))]
Q <- Q[n >= 2]
QW <- dcast(Q, cod_fundo + ano ~ per, value.var = c("ativa","passiva","n"))
QW <- QW[!is.na(ativa_q13) & !is.na(ativa_q4) & n_q13 >= 6 & n_q4 >= 2]
CLS9 <- unique(P[!is.na(perdedor_bench_set),
                 .(cod_fundo, ano, excesso_interim_set, perdedor_bench_set)])
QW <- merge(QW, CLS9, by = c("cod_fundo","ano"))
QW[, l_ativa_q   := log(ativa_q4 / ativa_q13)]
QW[, l_passiva_q := log(passiva_q4 / passiva_q13)]
QW[, share_ativa_q4 := ativa_q4 / (ativa_q4 + passiva_q4)]
QW <- QW[is.finite(l_ativa_q) & is.finite(l_passiva_q)]
cat("Fundo-ano:", nrow(QW), "\n")

cat("\n--- medianas por grupo (razao 4o trimestre / 3 primeiros) ---\n")
print(QW[, .(n = .N,
             realocacao_ATIVA = round(median(ativa_q4/ativa_q13), 3),
             deriva_PASSIVA   = round(median(passiva_q4/passiva_q13), 3),
             fracao_ativa_q4  = round(median(share_ativa_q4), 3)),
         by = perdedor_bench_set][order(perdedor_bench_set)])

cat("\n--- parcela ATIVA no 4o trimestre ---\n")
print(summary(feols(l_ativa_q ~ perdedor_bench_set | ano, data = QW, cluster = ~cod_fundo)))
cat("\n--- parcela PASSIVA no 4o trimestre ---\n")
print(summary(feols(l_passiva_q ~ perdedor_bench_set | ano, data = QW, cluster = ~cod_fundo)))
cat("\n--- fracao do movimento que e' decisao do gestor no 4o trimestre ---\n")
print(summary(feols(share_ativa_q4 ~ perdedor_bench_set | ano, data = QW, cluster = ~cod_fundo)))

fwrite(QW, file.path(DD, "torneio_sintese_decomposicao_q4.csv"))
cat("\nOK - 114 concluido\n")
