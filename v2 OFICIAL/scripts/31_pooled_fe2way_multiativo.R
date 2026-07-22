# =============================================================================
# 31_pooled_fe2way_multiativo.R  (v2 OFICIAL)
#
# Checagem complementar (igual v1, R/32 parte C): UMA regressao pooled com
# TODAS as observacoes do painel multiativo, com efeito-fixo de ATIVO e de
# MES (demeaning iterativo, Frisch-Waugh-Lovell), erro-padrao agrupado por
# fundo. Agora com beta do fundo tambem incluido.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(MASS) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)
d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]

dd <- d[is.finite(flow_aum)]
cat("Obs para o pooled FE-2way:", nrow(dd), "| ativos:", uniqueN(dd$ativo),
    "| meses:", uniqueN(dd$ym), "| fundos:", uniqueN(dd$cod_fundo), "\n")

# NOTA: is_fic sai da especificacao -- apos o demeaning por ativo e mes, essa
# variavel fica completamente absorvida (vira vetor de zeros, X'X singular,
# t=NaN). Nao e um bug: is_fic nao tem variacao suficiente DENTRO de
# (ativo,mes) que nao seja ja explicada pelos efeitos-fixos nesta
# especificacao. Confirmado testando com kappa(X'X)=Inf e ginv() antes.
vars <- c("peso","z_aum","z_cot","flow_aum","z_betaf")
cat("\nDemeaning iterativo (ativo x mes), via data.table...\n")
t0 <- Sys.time()
DT <- dd[, c("ativo","ym",vars), with = FALSE]
for (i in seq_len(15)) {
  DT[, (vars) := lapply(.SD, function(v) v - mean(v, na.rm = TRUE)), by = ativo, .SDcols = vars]
  DT[, (vars) := lapply(.SD, function(v) v - mean(v, na.rm = TRUE)), by = ym,    .SDcols = vars]
}
Dm <- DT[, ..vars]
setnames(Dm, paste0(vars, "_d"))
cat("Tempo demeaning:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")

Xm <- cbind(1, as.matrix(Dm[, .(z_aum_d, z_cot_d, flow_aum_d, z_betaf_d)]))
y  <- Dm$peso_d
# usa pseudo-inversa (Moore-Penrose) em vez de solve() exato: apos o demeaning
# iterativo (aproximacao numerica ao FWL exato, nao e algebricamente identico),
# a matriz X'X fica muito perto de singular para is_fic (variavel binaria) em
# alguns ativos com pouca variacao -- solve() estoura, ginv() da a solucao de
# norma minima sem quebrar.
XtX <- crossprod(Xm)
XtX_inv <- ginv(XtX)
b  <- XtX_inv %*% crossprod(Xm, y)
u  <- as.numeric(y - Xm %*% b)
cat("Numero de condicao de X'X:", round(kappa(XtX),1), "\n")

cl_meat <- function(Z, u, cl) {
  k <- ncol(Z)
  Zu <- Z * u  # produto elemento a elemento, cada coluna de Z vezes u
  ZuDT <- as.data.table(Zu); ZuDT[, cl := cl]
  soma_por_cluster <- ZuDT[, lapply(.SD, sum), by = cl, .SDcols = 1:k]
  Sm <- as.matrix(soma_por_cluster[, -"cl", with = FALSE])
  ng <- nrow(Sm)
  Mt <- crossprod(Sm)  # soma dos produtos externos = t(Sm) %*% Sm
  (ng/(ng-1)) * Mt
}
cat("Calculando erro-padrao (cluster por fundo)...\n")
t1 <- Sys.time()
V <- XtX_inv %*% cl_meat(Xm, u, dd$cod_fundo) %*% XtX_inv
cat("Tempo cluster SE:", round(as.numeric(Sys.time()-t1, units="secs"),1), "s\n")
se <- sqrt(diag(V)); tt <- as.numeric(b)/se
res_fe2 <- data.table(variavel = c("intercepto(omitido p/FE)","z_aum","z_cot","flow_aum","z_betaf"),
                      coef = as.numeric(b), se_cluster_fundo = se, t = as.numeric(tt))
print(res_fe2)
cat("\nObs:", nrow(dd), "| ativos:", uniqueN(dd$ativo), "| meses:", uniqueN(dd$ym),
    "| fundos:", uniqueN(dd$cod_fundo), "\n")
fwrite(res_fe2, file.path(REPO, "v2 OFICIAL/data/pooled_fe2way_multiativo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/pooled_fe2way_multiativo.csv'\n")
