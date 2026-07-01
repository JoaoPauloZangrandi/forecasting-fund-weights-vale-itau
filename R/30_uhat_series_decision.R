# =============================================================================
# 30_uhat_series_decision.R — completa a folha 2 do orientador:
#   (a) SERIE TEMPORAL do erro de previsao u_{n,t+1} = dw_{t+1} - lambda * d_t
#       -> caracteriza media, desvio, autocorrelacao e estacionariedade (Dickey-
#          Fuller), usando AS DUAS estimativas de lambda (MQO e VI).
#   (b) REGRA DE DECISAO recursiva: previsao fora da amostra (janela expansiva)
#       do peso pelo ajuste parcial, w_{t+h} = w_t + [1-(1-lambda)^h] d_t, nos
#       horizontes h=1 e h=3 (opacidade de 90 dias), com lambda reestimado a cada
#       origem por MQO e por VI, comparando RMSE contra a previsao ingenua.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq/aum_num][, ym := ano*100L+mes]
d <- d[!is.na(flow_aum)]
z <- function(x)(x-mean(x))/sd(x)
d[, z_aum := z(l_aum)][, z_cot := z(l_cot)]

# peso desejado (logit por mes) e distancia d = w* - w
d[, w_star := NA_real_]
for (mm in sort(unique(d$ym))) { ix <- d$ym==mm
  fg <- suppressWarnings(glm(peso_vale3~z_aum+z_cot+is_fic+flow_aum,
                             family=quasibinomial(link="logit"), data=d[ix]))
  d[ix, w_star := fitted(fg)] }
d[, dist := w_star - peso_vale3]

addm <- function(ym,k){ tot<-(ym%/%100L)*12L+(ym%%100L-1L)+k; (tot%/%12L)*100L+(tot%%12L)+1L }
key <- function(col) setNames(d[[col]], paste(d$cod_fundo, d$ym))
W  <- key("peso_vale3"); D <- key("dist")
getv <- function(map, fund, ym) unname(map[paste(fund, ym)])

d[, ym_n1 := addm(ym,1L)][, ym_p1 := addm(ym,-1L)]
d[, dw1 := getv(W, cod_fundo, ym_n1) - peso_vale3]     # Delta w_{t+1}
d[, dist_l1 := getv(D, cod_fundo, ym_p1)]              # d_{t-1}

# ---- estimadores pontuais de lambda (com intercepto, como no R/26) ----
lam_ols <- function(dw, dd) coef(lm(dw ~ dd))[2]
lam_iv  <- function(dw, dd, zz){ X<-cbind(1,dd); Z<-cbind(1,zz)
  as.numeric(solve(crossprod(Z,X), crossprod(Z,dw))[2]) }

# =====================================================================
# (a) SERIE TEMPORAL DE u_{n,t+1}
# =====================================================================
A <- d[is.finite(dw1) & is.finite(dist) & is.finite(dist_l1)]
lamO <- lam_ols(A$dw1, A$dist); lamI <- lam_iv(A$dw1, A$dist, A$dist_l1)
cat(sprintf("lambda MQO = %.4f | lambda VI = %.4f  (amostra: %d obs)\n\n", lamO, lamI, nrow(A)))

df_t <- function(x){ n<-length(x); s<-summary(lm(diff(x)~x[-n]))$coefficients; s[2,3] }
carac <- function(uhat, nome){
  A2 <- copy(A); A2[, u := uhat]
  um <- A2[, .(m=mean(u)), by=ym][order(ym)]$m; n<-length(um)
  data.table(serie=nome, media_painel=mean(uhat), dp_painel=sd(uhat),
             media_mensal=mean(um), autocorr1=cor(um[-1],um[-n]), df_t=df_t(um))
}
cat("===== (a) CARACTERIZACAO DA SERIE u_{n,t+1} (media por mes) =====\n")
res_a <- rbind(carac(A$dw1 - lamO*A$dist, "u (lambda MQO)"),
               carac(A$dw1 - lamI*A$dist, "u (lambda VI)"))
print(res_a[, lapply(.SD, function(z) if(is.numeric(z)) round(z,5) else z)])
cat("(DF t < -2,9 ~ rejeita raiz unitaria -> serie estacionaria)\n")
fwrite(res_a, file.path(REPO,"data/processed/reg30_uhat_series.csv"))

# =====================================================================
# (b) REGRA DE DECISAO RECURSIVA (previsao OOS por ajuste parcial)
# =====================================================================
meses <- sort(unique(d$ym)); t0 <- 24L   # comeca a prever apos 24 meses
rmse <- function(e) sqrt(mean(e^2, na.rm=TRUE))
run_oos <- function(h){
  e_naive <- e_ols <- e_iv <- numeric(0)
  for (oi in t0:(length(meses)-h)) {
    t <- meses[oi]; tgt <- addm(t, h)
    tr <- d[ym < t & is.finite(dw1) & is.finite(dist) & is.finite(dist_l1)]   # pares que POUSAM ate t (sem look-ahead)
    if (nrow(tr) < 50) next
    lo <- lam_ols(tr$dw1, tr$dist); li <- lam_iv(tr$dw1, tr$dist, tr$dist_l1)
    cur <- d[ym==t & is.finite(dist)]
    yreal <- getv(W, cur$cod_fundo, tgt); ok <- is.finite(yreal)
    w_t <- cur$peso_vale3[ok]; dd <- cur$dist[ok]; yr <- yreal[ok]
    e_naive <- c(e_naive, yr - w_t)
    e_ols <- c(e_ols, yr - (w_t + (1-(1-lo)^h)*dd))
    e_iv  <- c(e_iv,  yr - (w_t + (1-(1-li)^h)*dd))
  }
  data.table(horizonte=paste0("h=",h), n=length(e_naive),
             rmse_ingenua=rmse(e_naive), rmse_pa_mqo=rmse(e_ols), rmse_pa_vi=rmse(e_iv))
}
cat("\n===== (b) DECISAO RECURSIVA: RMSE fora da amostra (janela expansiva) =====\n")
res_b <- rbind(run_oos(1L), run_oos(3L))
print(res_b[, lapply(.SD, function(z) if(is.numeric(z)) round(z,5) else z)])
fwrite(res_b, file.path(REPO,"data/processed/reg30_decisao_oos.csv"))
cat("\nOK - salvo em data/processed/reg30_*.csv\n")
