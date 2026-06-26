# =============================================================================
# 19_forecast_opacity_h3.R
#
# Opacidade de 90 dias: os fundos divulgam a carteira (o peso) com defasagem
# trimestral. Logo, em t so conhecemos o peso ate t; prever o peso ATUAL equivale
# a prever t+3 com informacao de t. Refazemos a comparacao de previsao para os
# horizontes h=1 (o que faziamos) e h=3 (opacidade), mesmos modelos:
#   M0 Ingenuo: w_{i,t}        | M1 Efeito-fixo: mu_i
#   M2 FE+Kalman: mu_i + theta_t.(x_{i,t}-xbar_i) (Kalman local-level, flat h-step)
#   M3 Variacao: w_{i,t} + delta'.x_{i,t} (regride a mudanca de h meses)
# Tudo OOS, info ate t. RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
co <- fread(file.path(REPO,"data/processed/reg14_coefs_2016_2021.csv")); setorder(co, ym)
d  <- fread(file.path(REPO,"data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num:=as.numeric(aum)][, l_aum:=log(aum_num)][, l_cot:=log(n_cotistas)]
d[, flow_aum:=fluxo_liq/aum_num][, ym:=ano*100L+mes]
d <- d[!is.na(flow_aum)]
d[, monidx := frank(ym, ties.method="dense")]
co[, monidx := frank(ym, ties.method="dense")]
xv <- c("l_aum","l_cot","is_fic","flow_aum")

# Kalman local-level: previsao causal (filtrada) dos slopes por mes
nll <- function(par,y){Q<-exp(par[1]);R<-exp(par[2]);a<-y[1];P<-1e7;ll<-0
  for(t in seq_along(y)){F<-P+R;v<-y[t]-a;ll<-ll-0.5*(log(2*pi*F)+v^2/F);K<-P/F;a<-a+K*v;P<-(1-K)*P+Q};-ll}
kal_pred <- function(y){fit<-optim(c(log(var(diff(y))),log(var(y))),nll,y=y,method="Nelder-Mead")
  Q<-exp(fit$par[1]);R<-exp(fit$par[2]);a<-y[1];P<-1e7;pr<-numeric(length(y))
  for(t in seq_along(y)){pr[t]<-a;F<-P+R;v<-y[t]-a;K<-P/F;a<-a+K*v;P<-(1-K)*P+Q};pr}
sl <- c("b_l_aum","b_l_cot","b_is_fic","b_flow_aum")
kal <- as.data.table(lapply(sl, function(v) kal_pred(co[[v]]))); setnames(kal, sl); kal[, monidx:=co$monidx]

forecast_h <- function(h) {
  # alvo: peso h meses a frente, mesmo fundo
  tg <- d[, .(cod_fundo, mi = monidx - h, peso_tgt = peso_vale3)]
  base <- merge(d, tg, by.x=c("cod_fundo","monidx"), by.y=c("cod_fundo","mi"))
  rng <- (h + 6):(max(d$monidx) - h)             # meses de decisao com treino e alvo
  E <- list()
  for (t in rng) {
    tr  <- d[monidx <= t]                          # info ate t
    mu  <- tr[, .(mu=mean(peso_vale3)), by=cod_fundo]
    xb  <- tr[, lapply(.SD, mean), by=cod_fundo, .SDcols=xv]; setnames(xb, xv, paste0(xv,"_b"))
    bt  <- base[monidx == t]
    if (!nrow(bt)) next
    bt  <- merge(bt, mu, by="cod_fundo"); bt <- merge(bt, xb, by="cod_fundo")
    kt  <- as.numeric(kal[monidx==t, ..sl])
    bt[, p_fe  := mu]
    bt[, p_fek := mu + kt[1]*(l_aum-l_aum_b) + kt[2]*(l_cot-l_cot_b) +
                      kt[3]*(is_fic-is_fic_b) + kt[4]*(flow_aum-flow_aum_b)]
    # M3: regride a mudanca de h meses (alvo - base) nas caracteristicas, no treino conhecido (mi+h<=t)
    trd <- base[monidx <= t - h]; trd[, chg := peso_tgt - peso_vale3]
    fitd <- lm(chg ~ l_aum + l_cot + is_fic + flow_aum, data=trd)
    bt[, p_dw := peso_vale3 + as.numeric(predict(fitd, bt))]
    E[[length(E)+1L]] <- bt[, .(peso_tgt, e0=peso_tgt-peso_vale3, e1=peso_tgt-p_fe,
                                e2=peso_tgt-p_fek, e3=peso_tgt-p_dw)]
  }
  E <- rbindlist(E)
  rmse <- function(x) sqrt(mean(x^2, na.rm=TRUE))
  data.table(h=h, n=nrow(E),
             M0_ingenuo=rmse(E$e0), M1_FE=rmse(E$e1), M2_FE_Kalman=rmse(E$e2), M3_variacao=rmse(E$e3))
}

res <- rbindlist(lapply(c(1,3), forecast_h))
cat("==== RMSE da previsao por horizonte (h meses a frente) ====\n")
print(res[, lapply(.SD, function(z) if(is.double(z)) round(z,5) else z)])
cat("\n(h=1: prever t+1 com info de t [o que faziamos]. h=3: opacidade de 90 dias.)\n")
fwrite(res, file.path(REPO,"data/processed/reg19_horizon_rmse.csv"))
