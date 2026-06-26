# =============================================================================
# 21_forecast_acoes.R
#
# Previsao do peso de VALE3 RESTRITA A FUNDOS DE ACOES (universo da margem
# extensiva), nos horizontes h=1 e h=3. Mesmos modelos do R/19:
#   M0 Ingenuo | M1 Efeito-fixo | M2 FE+Kalman | M3 Variacao dw.
# Re-estima os coeficientes mensais (cross-section) SO com fundos de Acoes para
# consistencia. RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d  <- fread(file.path(REPO,"data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
ext<- fread(file.path(REPO,"data/processed/painel_extensivo_acoes.csv"))  # universo Acoes
d[, aum_num:=as.numeric(aum)][, l_aum:=log(aum_num)][, l_cot:=log(n_cotistas)]
d[, flow_aum:=fluxo_liq/aum_num][, ym:=ano*100L+mes]
d <- d[!is.na(flow_aum)]
# restringe a fundo-meses de ACOES
eqkey <- unique(ext[, .(cod_fundo, ym)]); eqkey[, eq:=1L]
d <- merge(d, eqkey, by=c("cod_fundo","ym"), all.x=TRUE); d <- d[eq==1L]
d[, monidx := frank(ym, ties.method="dense")]
cat("Painel Acoes (VALE3, >3 cotistas):", nrow(d), "obs |", uniqueN(d$cod_fundo), "fundos\n")

# coeficientes mensais (cross-section) so com Acoes -> serie p/ o Kalman
co <- rbindlist(lapply(sort(unique(d$ym)), function(m){
  fit<-lm(peso_vale3~l_aum+l_cot+is_fic+flow_aum, data=d[ym==m]); cf<-coef(fit)
  data.table(ym=m, b_l_aum=cf[2], b_l_cot=cf[3], b_is_fic=cf[4], b_flow_aum=cf[5])}))
setorder(co, ym); co[, monidx:=frank(ym, ties.method="dense")]

nll <- function(par,y){Q<-exp(par[1]);R<-exp(par[2]);a<-y[1];P<-1e7;ll<-0
  for(t in seq_along(y)){F<-P+R;v<-y[t]-a;ll<-ll-0.5*(log(2*pi*F)+v^2/F);K<-P/F;a<-a+K*v;P<-(1-K)*P+Q};-ll}
kal_pred <- function(y){fit<-optim(c(log(var(diff(y))),log(var(y))),nll,y=y,method="Nelder-Mead")
  Q<-exp(fit$par[1]);R<-exp(fit$par[2]);a<-y[1];P<-1e7;pr<-numeric(length(y))
  for(t in seq_along(y)){pr[t]<-a;F<-P+R;v<-y[t]-a;K<-P/F;a<-a+K*v;P<-(1-K)*P+Q};pr}
sl <- c("b_l_aum","b_l_cot","b_is_fic","b_flow_aum")
kal <- as.data.table(lapply(sl, function(v) kal_pred(co[[v]]))); setnames(kal,sl); kal[, monidx:=co$monidx]
xv <- c("l_aum","l_cot","is_fic","flow_aum")

forecast_h <- function(h) {
  tg <- d[, .(cod_fundo, mi=monidx-h, peso_tgt=peso_vale3)]
  base <- merge(d, tg, by.x=c("cod_fundo","monidx"), by.y=c("cod_fundo","mi"))
  E <- list()
  for (t in (h+6):(max(d$monidx)-h)) {
    tr <- d[monidx<=t]; mu <- tr[,.(mu=mean(peso_vale3)),by=cod_fundo]
    xb <- tr[,lapply(.SD,mean),by=cod_fundo,.SDcols=xv]; setnames(xb,xv,paste0(xv,"_b"))
    bt <- base[monidx==t]; if(!nrow(bt)) next
    bt <- merge(bt,mu,by="cod_fundo"); bt <- merge(bt,xb,by="cod_fundo")
    kt <- as.numeric(kal[monidx==t, ..sl])
    bt[, p_fek := mu + kt[1]*(l_aum-l_aum_b)+kt[2]*(l_cot-l_cot_b)+kt[3]*(is_fic-is_fic_b)+kt[4]*(flow_aum-flow_aum_b)]
    trd <- base[monidx<=t-h]; trd[, chg:=peso_tgt-peso_vale3]
    fitd <- lm(chg~l_aum+l_cot+is_fic+flow_aum, data=trd)
    bt[, p_dw := peso_vale3 + as.numeric(predict(fitd,bt))]
    E[[length(E)+1L]] <- bt[, .(e0=peso_tgt-peso_vale3, e1=peso_tgt-mu, e2=peso_tgt-p_fek, e3=peso_tgt-p_dw)]
  }
  E <- rbindlist(E); rm <- function(x) sqrt(mean(x^2,na.rm=TRUE))
  data.table(h=h, n=nrow(E), M0_ingenuo=rm(E$e0), M1_FE=rm(E$e1), M2_FE_Kalman=rm(E$e2), M3_variacao=rm(E$e3))
}
res <- rbindlist(lapply(c(1,3), forecast_h))
cat("\n==== RMSE da previsao (SO FUNDOS DE ACOES) por horizonte ====\n")
print(res[, lapply(.SD, function(z) if(is.double(z)) round(z,5) else z)])
fwrite(res, file.path(REPO,"data/processed/reg21_acoes_horizon.csv"))
