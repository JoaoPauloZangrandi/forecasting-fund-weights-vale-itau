# =============================================================================
# 133_decomposicao_estrutural_lambda.R  (v2 OFICIAL)
#
# A peca que liga a extensao de volta a Etapa 1. Com e = peso - peso_pred
# (residuo da Etapa 1) e d = -e, a previsao de ajuste parcial e'
#   peso_prev_{t+h} = peso_t + lambda * d_t
# logo o erro fora da amostra se abre EXATAMENTE em tres pedacos:
#
#   erro_oos = (peso_pred_{t+h} - peso_pred_t)  +  e_{t+h}  -  (1-lambda) e_t
#               \_____ deriva do alvo _____/
#
# e portanto
#   Var(erro_oos) = Var(dpred) + Var(e_fut) + (1-l)^2 Var(e_t)
#                   + 2Cov(dpred, e_fut) - 2(1-l)Cov(dpred, e_t)
#                   - 2(1-l)Cov(e_fut, e_t)
#
# "Gestora dificil de prever" vira UM DE TRES mecanismos economicamente
# distintos: residuo de Etapa 1 mais volatil, residuo menos persistente
# (Cov baixa), ou alvo que se move muito. A regressao de sigma_g nunca os
# separou. Este script separa, por fundo e por gestora.
#
# RODAR COM CAMINHO ABSOLUTO. Le um CSV de 931 MB.
# =============================================================================
suppressPackageStartupMessages({ library(data.table) })

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")
PISO_POEIRA <- 0.001
CORTE <- 202001L
LAMBDA <- 0.069      # Etapa 2 do TCC; reconferido abaixo contra o dado

addm <- function(ym, h) {
  a <- ym %/% 100L; m <- ym %% 100L
  tot <- a * 12L + (m - 1L) + h
  (tot %/% 12L) * 100L + (tot %% 12L) + 1L
}

cat("=============================================================\n")
cat("133 - Decomposicao estrutural do erro\n")
cat("=============================================================\n\n")

E <- fread(file.path(DD, "erro_e_multiativo.csv"), showProgress = FALSE)
E[, cod_fundo := as.character(cod_fundo)]
cel <- E[, .(pm = median(peso)), by = .(cod_fundo, ativo)][pm >= PISO_POEIRA, .(cod_fundo, ativo)]
E <- merge(E, cel, by = c("cod_fundo","ativo"))
cat("Base erro_e pos-filtro:", nrow(E), "linhas\n")

# --- monta t e t+1 -----------------------------------------------------------
FUT <- E[, .(cod_fundo, ativo, ym_key = ym, peso_fut = peso,
             peso_pred_fut = peso_pred, e_fut = erro)]
A <- E[, .(cod_fundo, gestora_grupo, ativo, ym, peso, peso_pred, e = erro,
           ym_fut = addm(ym, 1L))]
M <- merge(A, FUT, by.x = c("cod_fundo","ativo","ym_fut"),
           by.y = c("cod_fundo","ativo","ym_key"))
M <- M[ym >= CORTE & ym_fut <= 202112L]      # periodo de teste, igual ao script 99
cat("Teste (h=1):", nrow(M), "obs |", uniqueN(M$cod_fundo), "fundos\n\n")

M[, dpred   := peso_pred_fut - peso_pred]
M[, dw      := peso_fut - peso]
M[, d       := peso_pred - peso]              # distancia ate o alvo
M[, erro_rec := dw - LAMBDA * d]              # reconstrucao do erro_oos

# --- confere a identidade ----------------------------------------------------
M[, ident := dpred + e_fut - (1 - LAMBDA) * e]
dif <- M[, max(abs(erro_rec - ident))]
cat(sprintf("--- Conferencia da identidade algebrica ---\n"))
cat(sprintf("max |erro_reconstruido - (dpred + e_fut - (1-l) e_t)| = %.3e\n", dif))
cat(sprintf("Identidade vale? %s\n\n", ifelse(dif < 1e-9, "SIM", "NAO -- revisar")))

# confere contra o erro_oos gravado pelo script 99
H <- fread(file.path(DD, "etapa3_multiativo_h1.csv"),
           select = c("cod_fundo","ativo","ym","erro_oos"), showProgress = FALSE)
H[, cod_fundo := as.character(cod_fundo)]
CK <- merge(M[, .(cod_fundo, ativo, ym, erro_rec)], H, by = c("cod_fundo","ativo","ym"))
cat(sprintf("Contra o erro_oos do script 99: n = %d | cor = %.6f | max dif = %.3e\n\n",
            nrow(CK), cor(CK$erro_rec, CK$erro_oos), max(abs(CK$erro_rec - CK$erro_oos))))

# --- decomposicao da variancia, por fundo ------------------------------------
l1 <- 1 - LAMBDA
decomp <- function(dt, por) {
  dt[, .(
    var_erro    = var(erro_rec),
    v_dpred     = var(dpred),
    v_efut      = var(e_fut),
    v_et        = l1^2 * var(e),
    c_dpred_efut=  2 * cov(dpred, e_fut),
    c_dpred_et  = -2 * l1 * cov(dpred, e),
    c_efut_et   = -2 * l1 * cov(e_fut, e),
    persist_e   = cor(e_fut, e),
    n = .N), by = por]
}

FD <- decomp(M, "cod_fundo")
FD <- FD[n >= 24 & is.finite(var_erro) & var_erro > 0]
FD[, soma := v_dpred + v_efut + v_et + c_dpred_efut + c_dpred_et + c_efut_et]
cat(sprintf("--- Decomposicao por fundo (n = %d) ---\n", nrow(FD)))
cat(sprintf("Erro maximo da identidade de variancia: %.3e\n", FD[, max(abs(soma - var_erro))]))
for (v in c("v_dpred","v_efut","v_et","c_dpred_efut","c_dpred_et","c_efut_et")) {
  FD[, (paste0("p_", v)) := 100 * get(v) / var_erro]
}
cat("\nParticipacao mediana de cada termo na variancia do erro (%):\n")
print(round(FD[, lapply(.SD, median, na.rm = TRUE),
               .SDcols = paste0("p_", c("v_dpred","v_efut","v_et",
                                        "c_dpred_efut","c_dpred_et","c_efut_et"))], 1))
cat(sprintf("\nPersistencia do residuo da Etapa 1, cor(e_t+1, e_t) -- mediana entre fundos: %.4f\n",
            median(FD$persist_e, na.rm = TRUE)))

# --- leitura de DOIS termos (a legivel) --------------------------------------
# Os seis termos acima quase se cancelam, porque e_t e' muito persistente
# (mediana 0,88): Var(e_fut) e -2(1-l)Cov(e_fut,e_t) sao ambos enormes e de
# sinais opostos. A leitura economica sai agrupando:
#     erro = dpred + inov,   inov = e_fut - (1-lambda) e_t
# ou seja, o erro de previsao e' a DERIVA DO ALVO mais a INOVACAO do residuo
# da Etapa 1. Se o residuo fosse passeio aleatorio puro, so restaria a inovacao.
M[, inov := e_fut - l1 * e]
dois <- function(dt, por) {
  dt[, .(var_erro = var(erro_rec),
         v_alvo   = var(dpred),
         v_inov   = var(inov),
         c_cruz   = 2 * cov(dpred, inov),
         n = .N), by = por]
}
F2 <- dois(M, "cod_fundo")[n >= 24 & is.finite(var_erro) & var_erro > 0]
for (v in c("v_alvo","v_inov","c_cruz")) F2[, (paste0("p_", v)) := 100 * get(v)/var_erro]
cat("\n--- Leitura de dois termos: erro = deriva do alvo + inovacao do residuo ---\n")
cat("Participacao mediana entre fundos (%):\n")
print(round(F2[, lapply(.SD, median, na.rm=TRUE), .SDcols = c("p_v_alvo","p_v_inov","p_c_cruz")], 1))
cat(sprintf("Fundos em que a INOVACAO responde por mais de 80%% da variancia: %d de %d (%.1f%%)\n",
            F2[p_v_inov > 80, .N], nrow(F2), 100*F2[p_v_inov > 80, .N]/nrow(F2)))

G2 <- dois(M, "gestora_grupo")[n >= 100]
for (v in c("v_alvo","v_inov","c_cruz")) G2[, (paste0("p_", v)) := 100 * get(v)/var_erro]
cat("\nPor gestora, participacao mediana (%):\n")
print(round(G2[, lapply(.SD, median, na.rm=TRUE), .SDcols = c("p_v_alvo","p_v_inov","p_c_cruz")], 1))
fwrite(F2, file.path(DD, "decomp_estrutural_dois_termos_fundo.csv"))
fwrite(G2, file.path(DD, "decomp_estrutural_dois_termos_gestora.csv"))

GD <- decomp(M, "gestora_grupo")
GD <- GD[n >= 100]
for (v in c("v_dpred","v_efut","v_et","c_efut_et")) GD[, (paste0("p_", v)) := 100 * get(v)/var_erro]
cat("\n--- Por gestora: as 5 mais dificeis e as 5 mais faceis ---\n")
setorder(GD, -var_erro)
print(GD[c(1:5, (.N-4):.N),
         .(gestora_grupo, dp = round(sqrt(var_erro),5),
           p_alvo = round(p_v_dpred,1), p_e_fut = round(p_v_efut,1),
           p_e_t = round(p_v_et,1), p_cov = round(p_c_efut_et,1),
           persist = round(persist_e,3))])

# --- o que diferencia gestora dificil de facil? ------------------------------
cat("\n--- Qual dos tres mecanismos separa gestora dificil de facil? ---\n")
GD[, dp_erro := sqrt(var_erro)]
for (v in c("v_dpred","v_efut","v_et","persist_e")) {
  r <- cor(GD$dp_erro, GD[[v]], method = "spearman")
  cat(sprintf("  cor(dp_erro, %-10s) = %+.4f (Spearman)\n", v, r))
}
cat("\n  (o termo com maior correlacao e' o mecanismo dominante)\n")

fwrite(FD, file.path(DD, "decomp_estrutural_fundo.csv"))
fwrite(GD, file.path(DD, "decomp_estrutural_lambda.csv"))

pdf(file.path(FIG, "fig_ext_decomposicao_estrutural.pdf"), width = 7.5, height = 5.5)
par(mar = c(4.5, 4.5, 3, 1))
plot(GD$v_efut, GD$dp_erro^2, pch = 19, col = adjustcolor("#3B6E9E", .7),
     xlab = "Var(residuo da Etapa 1 em t+1)", ylab = "Var(erro fora da amostra)",
     main = "O erro de previsao e, sobretudo, o residuo da Etapa 1")
abline(0, 1, lty = 2, col = "grey60")
dev.off()
cat("\nOK - decomp_estrutural_fundo.csv, decomp_estrutural_lambda.csv, figura\n")
