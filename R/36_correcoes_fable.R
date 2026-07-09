# =============================================================================
# 36_correcoes_fable.R
#
# Implementa as correcoes apontadas em correcoes_fable.pdf (revisao critica de
# 06/07/2026):
#   (1) Fama-MacBeth com fluxo WINSORIZADO (1%/99%) como especificacao
#       PRINCIPAL (I5), com Newey-West L=3 (padrao) e sensibilidade L=6,12 (I2).
#   (2) Ajuste parcial: tabela completa de especificacoes de erro-padrao —
#       cluster por fundo (original), cluster por mes, cluster two-way
#       (fundo x mes, Cameron-Gelbach-Miller), e VI com EFEITO DE TEMPO
#       delta_t (absorve o choque comum do mes, inclusive o mecanico do
#       preco) — ataca C2 na causa, nao so no sintoma do cluster. FE+VI (I1)
#       reapresentado como especificacao co-principal.
#   (3) Teste de Diebold-Mariano (C1) sobre a comparacao OOS do R/30 (h=1 e
#       h=3, lambda MQO e VI): testa formalmente se o ajuste parcial supera a
#       ingenua.
#   (4) APE DISCRETO (nao a derivada) para a dummy FIC no logit fracionario
#       (M1).
#   (5) F efetivo do primeiro estagio COM cluster por fundo (I6).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq/aum_num][, ym := ano*100L+mes]
d <- d[!is.na(flow_aum)]
z <- function(x) (x-mean(x))/sd(x)
d[, z_aum := z(l_aum)][, z_cot := z(l_cot)]
nw_se <- function(x, L) { T<-length(x); u<-x-mean(x); g0<-sum(u^2); a<-0
  if (L>=1) for (l in 1:L) a <- a + (1-l/(L+1))*sum(u[(l+1):T]*u[1:(T-l)]); sqrt((g0+2*a)/T^2) }

# =============================================================================
# (1) FM com fluxo WINSORIZADO 1%/99% como especificacao PRINCIPAL
# =============================================================================
cat("========== (1) FAMA-MACBETH: fluxo cru vs. WINSORIZADO (1%/99%) ==========\n")
q <- quantile(d$flow_aum, c(.01,.99))
d[, flow_w := pmin(pmax(flow_aum, q[1]), q[2])]
cat("Winsorizacao do fluxo/PL: [", round(q[1],3), ",", round(q[2],3), "] (cru: min",
    round(min(d$flow_aum),1), "max", round(max(d$flow_aum),1), ")\n\n")

meses <- sort(unique(d$ym))
fm_run <- function(flowvar) {
  M <- rbindlist(lapply(meses, function(m) as.data.table(as.list(coef(
    lm(as.formula(paste("peso_vale3 ~ z_aum + z_cot + is_fic +", flowvar)), data=d[ym==m]))))))
  setnames(M, c("int","z_aum","z_cot","is_fic","flow"))
  M
}
resumo_L <- function(M, rot) rbindlist(lapply(seq_along(M), function(j) {
  x <- M[[j]]; m <- mean(x)
  data.table(variavel=rot[j], coef=m,
             se_L3=nw_se(x,3), t_L3=m/nw_se(x,3),
             se_L6=nw_se(x,6), t_L6=m/nw_se(x,6),
             se_L12=nw_se(x,12), t_L12=m/nw_se(x,12))
}))
rot <- c("Intercepto","ln(PL) [dp]","ln(cotistas) [dp]","Fundo de cotas","Captacao/PL")

cat("---- (1a) fluxo CRU (especificacao antiga) ----\n")
Mcru <- fm_run("flow_aum"); r_cru <- resumo_L(Mcru, rot)
print(r_cru[, lapply(.SD, function(z) if(is.numeric(z)) round(z,5) else z)])

cat("\n---- (1b) fluxo WINSORIZADO 1%/99% (especificacao PRINCIPAL a partir desta rodada) ----\n")
Mwin <- fm_run("flow_w"); r_win <- resumo_L(Mwin, rot)
print(r_win[, lapply(.SD, function(z) if(is.numeric(z)) round(z,5) else z)])

fwrite(r_cru, file.path(REPO,"data/processed/reg36_fm_fluxo_cru.csv"))
fwrite(r_win, file.path(REPO,"data/processed/reg36_fm_fluxo_winsor.csv"))

# =============================================================================
# (2) AJUSTE PARCIAL: tabela completa de EP e a especificacao com efeito de
#     tempo (delta_t) — ataca o choque comum do mes na causa
# =============================================================================
cat("\n\n========== (2) AJUSTE PARCIAL: EP por especificacao + efeito de tempo ==========\n")
d[, w_star := NA_real_]
for (mm in meses) { ix <- d$ym==mm
  fg <- suppressWarnings(glm(peso_vale3~z_aum+z_cot+is_fic+flow_aum,
                             family=quasibinomial(link="logit"), data=d[ix]))
  d[ix, w_star := fitted(fg)] }
d[, dist := w_star - peso_vale3]
nextym <- function(y) ifelse(y%%100L==12L,(y%/%100L+1L)*100L+1L,y+1L)
prevym <- function(y) ifelse(y%%100L==1L,(y%/%100L-1L)*100L+12L,y-1L)
d[, ym_next := nextym(ym)][, ym_prev := prevym(ym)]
wlead <- d[, .(cod_fundo, ymj=ym, peso_next=peso_vale3)]
M <- merge(d, wlead, by.x=c("cod_fundo","ym_next"), by.y=c("cod_fundo","ymj"))
dlag <- d[, .(cod_fundo, ymj=ym, dist_lag=dist)]
M <- merge(M, dlag, by.x=c("cod_fundo","ym_prev"), by.y=c("cod_fundo","ymj"))
M[, dw := peso_next - peso_vale3]
M <- M[is.finite(dw) & is.finite(dist) & is.finite(dist_lag)]

cl_meat <- function(Z,u,cl){ k<-ncol(Z); Mt<-matrix(0,k,k)
  for(g in unique(cl)){i<-cl==g; s<-crossprod(Z[i,,drop=FALSE],u[i]); Mt<-Mt+s%*%t(s)}
  (length(unique(cl))/(length(unique(cl))-1))*Mt }
X <- cbind(1, M$dist); Zi <- cbind(1, M$dist_lag); y <- M$dw
bi <- solve(crossprod(Zi,X), crossprod(Zi,y)); ui <- as.numeric(y - X%*%bi)
Bm <- solve(crossprod(Zi,X))
V_fundo <- Bm %*% cl_meat(Zi,ui,M$cod_fundo) %*% t(Bm)
V_mes   <- Bm %*% cl_meat(Zi,ui,M$ym) %*% t(Bm)
V_2way  <- Bm %*% (cl_meat(Zi,ui,M$cod_fundo)+cl_meat(Zi,ui,M$ym)-cl_meat(Zi,ui,paste(M$cod_fundo,M$ym))) %*% t(Bm)

rep_row <- function(nome,b,V){ se<-sqrt(diag(V))[2]; t<-b[2]/se
  data.table(especificacao=nome, lambda=b[2], se=se, t=t, p=2*pnorm(-abs(t))) }
tab_cluster <- rbind(
  rep_row("VI, cluster fundo (original)", bi, V_fundo),
  rep_row("VI, cluster mes", bi, V_mes),
  rep_row("VI, cluster two-way (fundo x mes)", bi, V_2way))
cat("---- (2a) MESMO lambda_VI=0.0413, EP por esquema de cluster ----\n")
print(tab_cluster[, lapply(.SD, function(z) if(is.numeric(z)) round(z,5) else z)])

# --- VI com efeito de tempo delta_t (absorve choque comum do mes) ---
cat("\n---- (2b) VI COM EFEITO DE TEMPO delta_t (dummies de mes), cluster fundo ----\n")
M[, ymf := factor(ym)]
Xf <- model.matrix(~ dist + ymf, data = M)
Zf <- model.matrix(~ dist_lag + ymf, data = M)
bf <- solve(crossprod(Zf,Xf), crossprod(Zf,y))
uf <- as.numeric(y - Xf %*% bf)
Bf <- solve(crossprod(Zf,Xf))
Vf <- Bf %*% cl_meat(Zf,uf,M$cod_fundo) %*% t(Bf)
lam_fe_t <- bf["dist",1]; se_fe_t <- sqrt(diag(Vf))["dist"]
cat(sprintf("lambda (VI + efeito de tempo) = %.4f | EP (cluster fundo) = %.4f | t = %.2f\n",
            lam_fe_t, se_fe_t, lam_fe_t/se_fe_t))

# --- FE de fundo + VI (I1, reapresentado como especificacao co-principal) ---
demean <- function(x,g) x - ave(x,g,FUN=function(z) mean(z,na.rm=TRUE))
Fd <- M[, .(cod_fundo, dw, dist, dist_lag)]
Fd[, `:=`(dwd=demean(dw,cod_fundo), distd=demean(dist,cod_fundo), l1d=demean(dist_lag,cod_fundo))]
biFE <- sum(Fd$l1d*Fd$dwd) / sum(Fd$l1d*Fd$distd)
uiFE <- Fd$dwd - biFE*Fd$distd
ZFE <- cbind(Fd$l1d); XFE <- cbind(Fd$distd)
VFE <- (1/sum(ZFE*XFE))^2 * cl_meat(ZFE, uiFE, Fd$cod_fundo)
se_FE <- sqrt(VFE[1,1])
cat(sprintf("\nlambda (FE de fundo + VI, I1) = %.4f | EP (cluster fundo) = %.4f | t = %.2f  [ja em R/27]\n",
            biFE, se_FE, biFE/se_FE))

tab_completa <- rbind(tab_cluster,
  data.table(especificacao="VI + efeito de tempo (dummies de mes), cluster fundo",
             lambda=lam_fe_t, se=se_fe_t, t=lam_fe_t/se_fe_t, p=2*pnorm(-abs(lam_fe_t/se_fe_t))),
  data.table(especificacao="FE de fundo + VI, cluster fundo",
             lambda=biFE, se=se_FE, t=biFE/se_FE, p=2*pnorm(-abs(biFE/se_FE))))
fwrite(tab_completa, file.path(REPO,"data/processed/reg36_lambda_clusters.csv"))

# =============================================================================
# (3) DIEBOLD-MARIANO sobre a comparacao OOS do R/30
# =============================================================================
cat("\n\n========== (3) DIEBOLD-MARIANO: ajuste parcial vs. ingenua ==========\n")
key <- function(col) setNames(d[[col]], paste(d$cod_fundo, d$ym))
W <- key("peso_vale3"); D <- key("dist")
getv <- function(map,f,y) unname(map[paste(f,y)])
addm <- function(ym,k){ tot<-(ym%/%100L)*12L+(ym%%100L-1L)+k; (tot%/%12L)*100L+(tot%%12L)+1L }
d[, ym_n1 := addm(ym,1L)][, ym_p1 := addm(ym,-1L)]
d[, dw1 := getv(W,cod_fundo,ym_n1) - peso_vale3]
d[, dist_l1 := getv(D,cod_fundo,ym_p1)]
lam_ols <- function(dw,dd) coef(lm(dw~dd))[2]
lam_iv  <- function(dw,dd,zz){ X<-cbind(1,dd); Z<-cbind(1,zz); as.numeric(solve(crossprod(Z,X),crossprod(Z,dw))[2]) }

dm_test <- function(h) {
  rows <- list()
  for (oi in 24L:(length(meses)-h)) {
    t <- meses[oi]; tgt <- addm(t,h)
    tr <- d[ym < t & is.finite(dw1) & is.finite(dist) & is.finite(dist_l1)]
    if (nrow(tr) < 50) next
    lo <- lam_ols(tr$dw1,tr$dist); li <- lam_iv(tr$dw1,tr$dist,tr$dist_l1)
    cur <- d[ym==t & is.finite(dist)]
    yr <- getv(W,cur$cod_fundo,tgt); ok <- is.finite(yr)
    w_t <- cur$peso_vale3[ok]; dd <- cur$dist[ok]; yy <- yr[ok]
    e_n <- yy - w_t
    e_o <- yy - (w_t + (1-(1-lo)^h)*dd)
    e_i <- yy - (w_t + (1-(1-li)^h)*dd)
    rows[[length(rows)+1L]] <- data.table(t=t, d_ols=mean(e_n^2-e_o^2), d_iv=mean(e_n^2-e_i^2))
  }
  R <- rbindlist(rows)
  out <- list()
  for (v in c("d_ols","d_iv")) {
    x <- R[[v]]; m <- mean(x); se <- nw_se(x, h)
    out[[v]] <- data.table(horizonte=paste0("h=",h), especificacao=v, media_dif_eqm=m,
                           se_nw=se, t_dm=m/se, p=2*pnorm(-abs(m/se)), n_origens=length(x))
  }
  rbindlist(out)
}
res_dm <- rbind(dm_test(1L), dm_test(3L))
cat("(diferenca = EQM ingenua - EQM ajuste parcial; >0 e t>1.96 favorecem o ajuste parcial)\n")
print(res_dm[, lapply(.SD, function(z) if(is.numeric(z)) round(z,4) else z)])
fwrite(res_dm, file.path(REPO,"data/processed/reg36_diebold_mariano.csv"))

# =============================================================================
# (4) APE DISCRETO (nao a derivada) para a dummy FIC
# =============================================================================
cat("\n\n========== (4) APE do FIC: derivada vs. DIFERENCA DISCRETA ==========\n")
ape_deriv <- numeric(0); ape_disc <- numeric(0)
for (mm in meses) {
  dm <- d[ym==mm]
  fg <- suppressWarnings(glm(peso_vale3~z_aum+z_cot+is_fic+flow_aum,
                             family=quasibinomial(link="logit"), data=dm))
  cg <- coef(fg); p <- fitted(fg); s <- mean(p*(1-p))
  ape_deriv <- c(ape_deriv, cg["is_fic"]*s)
  d1 <- dm; d1$is_fic <- 1; d0 <- dm; d0$is_fic <- 0
  g1 <- predict(fg, d1, type="response"); g0 <- predict(fg, d0, type="response")
  ape_disc <- c(ape_disc, mean(g1-g0))
}
cat("APE FIC (derivada, usado ate agora):    media =", round(mean(ape_deriv),5), "\n")
cat("APE FIC (diferenca discreta, correto):  media =", round(mean(ape_disc),5), "\n")
cat("diferenca:", round(mean(ape_disc)-mean(ape_deriv),5), "\n")
fwrite(data.table(metodo=c("derivada","diferenca_discreta"), ape_medio=c(mean(ape_deriv),mean(ape_disc))),
       file.path(REPO,"data/processed/reg36_ape_fic.csv"))

# =============================================================================
# (5) F efetivo do 1o estagio COM cluster por fundo
# =============================================================================
cat("\n\n========== (5) 1o estagio: F sem cluster vs. F efetivo com cluster ==========\n")
fs <- lm(dist ~ dist_lag, data=M)
Xfs <- model.matrix(fs); ufs <- resid(fs)
Vfs <- solve(crossprod(Xfs)) %*% cl_meat(Xfs,ufs,M$cod_fundo) %*% solve(crossprod(Xfs))
t_cl <- coef(fs)[2]/sqrt(diag(Vfs))[2]
cat("coef d_{t-1}:", round(coef(fs)[2],4), "\n")
cat("F (OLS, sem cluster):", round(summary(fs)$fstatistic[1],0), "\n")
cat("t (cluster fundo):", round(t_cl,1), "-> F efetivo (cluster):", round(t_cl^2,0), "\n")
fwrite(data.table(coef=coef(fs)[2], F_sem_cluster=summary(fs)$fstatistic[1],
                  t_cluster=t_cl, F_efetivo_cluster=t_cl^2),
       file.path(REPO,"data/processed/reg36_primeiro_estagio.csv"))

cat("\n\nOK - todas as correcoes salvas em data/processed/reg36_*.csv\n")
