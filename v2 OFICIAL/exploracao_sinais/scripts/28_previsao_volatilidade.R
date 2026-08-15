# =============================================================================
# 28_previsao_volatilidade.R  (exploracao_sinais)
#
# NOVO OBJETO ALVO: em vez de prever RETORNO (direcao), testa se os
# sinais ja construidos preveem VOLATILIDADE FUTURA da acao -- pergunta
# diferente, testavel sem dado de opcoes (so precisa de preco/retorno).
#
# Alvo: volatilidade realizada (dp do retorno mensal) nos PROXIMOS h
# meses, comecando em t+1 (nao inclui o mes t, sem look-ahead).
#
# Sinais testados: crowding (HHI posse), CIO peer momentum (nivel E
# magnitude absoluta), herding (LSV), FIT, demanda agregada revelada.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/350

# ---- volatilidade FUTURA (dp do retorno, t+1 a t+h) ----
setorder(precos, ticker, ymk)
calc_vol_futura <- function(dt_precos, h) {
  # pra cada (ticker, t), vol dos retornos de t+1 a t+h
  pw <- copy(dt_precos)
  cols <- list()
  for (k in 1:h) {
    aux <- pw[, .(ticker, ym_base = addm(ymk, -k), ret_k = retorno)]
    setnames(aux, "ret_k", paste0("r",k))
    cols[[k]] <- aux
  }
  base <- Reduce(function(a,b) merge(a,b,by=c("ticker","ym_base"), all=TRUE), cols)
  ret_cols <- paste0("r",1:h)
  base[, vol_futura := apply(.SD, 1, sd, na.rm=TRUE), .SDcols = ret_cols]
  base[, n_obs_vol := apply(.SD, 1, function(x) sum(is.finite(x))), .SDcols = ret_cols]
  base <- base[n_obs_vol >= max(2,round(h*0.7))]  # exige maioria dos meses disponivel
  base[, .(ticker, ym = ym_base, vol_futura)]
}

cat("Calculando volatilidade futura para h=3,6,12...\n")
vol_h3 <- calc_vol_futura(precos, 3)
vol_h6 <- calc_vol_futura(precos, 6)
vol_h12 <- calc_vol_futura(precos, 12)
cat("h=3:", nrow(vol_h3), "| h=6:", nrow(vol_h6), "| h=12:", nrow(vol_h12), "\n")

# ---- sinais ja calculados ----
cio_dt <- fread(file.path(OUT, "cio_peer_return.csv"))[, .(ticker, ym, peer_ret)]
cio_dt[, peer_ret_abs := abs(peer_ret)]

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

fit_completo <- fread(file.path(DATA, "fit_ativo_mes.csv"))
fit_completo[, ticker := trimws(sub(".*- ", "", ativo))]
fit_completo[, FIT_abs := abs(FIT)]

pp2 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso"))
pp2[, ticker := trimws(sub(".*- ", "", ativo))]
peso_prev2b <- pp2[, .(cod_fundo, ticker, ym = addm(ym, 1), peso_prev = peso)]
d2 <- merge(pp2, peso_prev2b, by = c("cod_fundo","ticker","ym"))
d2 <- d2[is.finite(peso_prev)]
d2[, compra := peso > peso_prev]; d2[, venda := peso < peso_prev]
por_am <- d2[, .(n_fundos=.N, n_compra=sum(compra), n_venda=sum(venda)), by=.(ticker,ym)]
por_am <- por_am[n_fundos >= 10]
media_mes2 <- por_am[, .(p_compra_medio_mes = mean(n_compra/(n_compra+n_venda), na.rm=TRUE)), by=ym]
por_am[, p_compra := n_compra/(n_compra+n_venda)]
por_am <- merge(por_am, media_mes2, by="ym")
por_am[, HM_sinal := p_compra - p_compra_medio_mes]
por_am[, HM_sinal_abs := abs(HM_sinal)]

fama_macbeth_vol <- function(m, var_x, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
  fit_lm <- lm(vol_futura ~ get(var_x), data = treino)
  pred <- predict(fit_lm, newdata = teste[, .(x=get(var_x))], newx=NULL)
  # (predict com formula generica via get() pode falhar no newdata -- refeito abaixo com nome fixo)
  return(NULL)
}

# versao robusta (evita o problema de newdata com get() dentro de formula)
testa_sinal_vol <- function(m, var_x, nome, h) {
  setnames(m, var_x, "x", skip_absent=TRUE)
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
  fit_lm <- lm(vol_futura ~ x, data = treino)
  pred <- predict(fit_lm, newdata = teste)
  sse_m <- sum((teste$vol_futura-pred)^2); sse_n <- sum((teste$vol_futura-mean(treino$vol_futura))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(vol_futura~x, data=m))$coefficients

  breaks <- unique(quantile(treino$x, seq(0,1,0.2), na.rm=TRUE))
  if (length(breaks)<4) return(data.table(sinal=nome,horizonte=h,r2_oos_pct=100*r2_oos,coef_full=s_full[2,1],
                                            t_full=s_full[2,3],p_full=s_full[2,4],n_meses=NA,spread_vol=NA,t_fm=NA,p_fm=NA))
  breaks[1]<--Inf; breaks[length(breaks)]<-Inf
  teste[, quintil := as.integer(cut(x, breaks, labels=FALSE, include.lowest=TRUE))]
  ng <- length(breaks)-1
  if (ng<5) teste[, quintil := ifelse(quintil==1,1,ifelse(quintil==ng,5,quintil))]
  teste <- teste[!is.na(quintil)]
  por_mes <- teste[quintil %in% c(1,5), .(vm=mean(vol_futura)), by=.(ym,quintil)]
  sm <- dcast(por_mes, ym~quintil, value.var="vm")
  if (!all(c("1","5") %in% names(sm))) return(data.table(sinal=nome,horizonte=h,r2_oos_pct=100*r2_oos,coef_full=s_full[2,1],
                                                            t_full=s_full[2,3],p_full=s_full[2,4],n_meses=NA,spread_vol=NA,t_fm=NA,p_fm=NA))
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1)&is.finite(q5)]
  sm[, spread:=q5-q1]; nmes<-nrow(sm)
  if (nmes<6) return(data.table(sinal=nome,horizonte=h,r2_oos_pct=100*r2_oos,coef_full=s_full[2,1],
                                 t_full=s_full[2,3],p_full=s_full[2,4],n_meses=nmes,spread_vol=NA,t_fm=NA,p_fm=NA))
  media<-mean(sm$spread); dp<-sd(sm$spread); t_fm<-media/(dp/sqrt(nmes)); p_fm<-2*pt(-abs(t_fm),df=nmes-1)
  cat(sprintf("%-25s h=%2d n_tr=%5d n_te=%4d R2_OOS=%7.3f%% | %2d meses spread(vol Q5-Q1)=%+.5f t=%6.2f p=%.5f | Bonf:%s\n",
              nome,h,nrow(treino),nrow(teste),100*r2_oos,nmes,media,t_fm,p_fm,ifelse(p_fm<LIMIAR_BONFERRONI,"SIM","nao")))
  data.table(sinal=nome,horizonte=h,r2_oos_pct=100*r2_oos,coef_full=s_full[2,1],t_full=s_full[2,3],p_full=s_full[2,4],
             n_meses=nmes,spread_vol=media,t_fm=t_fm,p_fm=p_fm,sig_bonferroni=p_fm<LIMIAR_BONFERRONI)
}

resultados <- list()
sinais <- list(
  list(dt=crowd[,.(ticker,ym,hhi_posse)], var="hhi_posse", nome="Crowding(HHI)"),
  list(dt=cio_dt[,.(ticker,ym,peer_ret)], var="peer_ret", nome="CIO(nivel)"),
  list(dt=cio_dt[,.(ticker,ym,peer_ret_abs)], var="peer_ret_abs", nome="CIO(magnitude-abs)"),
  list(dt=fit_completo[,.(ticker,ym,FIT)], var="FIT", nome="FIT(nivel)"),
  list(dt=fit_completo[,.(ticker,ym,FIT_abs)], var="FIT_abs", nome="FIT(magnitude-abs)"),
  list(dt=por_am[,.(ticker,ym,HM_sinal)], var="HM_sinal", nome="Herding(nivel)"),
  list(dt=por_am[,.(ticker,ym,HM_sinal_abs)], var="HM_sinal_abs", nome="Herding(magnitude-abs)")
)

for (vol_h in list(list(dt=vol_h3,h=3), list(dt=vol_h6,h=6), list(dt=vol_h12,h=12))) {
  for (s in sinais) {
    m <- merge(copy(s$dt), vol_h$dt, by=c("ticker","ym"))
    m <- m[is.finite(vol_futura) & is.finite(get(s$var))]
    if (nrow(m) < 150) next
    r <- testa_sinal_vol(copy(m), s$var, s$nome, vol_h$h)
    if (!is.null(r)) resultados[[length(resultados)+1]] <- r
  }
}

R <- rbindlist(resultados, fill=TRUE)
fwrite(R, file.path(OUT, "candidatos_28_previsao_volatilidade.csv"))
cat("\n\n===== RESUMO GERAL, ordenado por p_fm =====\n")
print(R[order(p_fm)][, .(sinal,horizonte,n_meses,r2_oos_pct=round(r2_oos_pct,3),t_fm=round(t_fm,2),p_fm=round(p_fm,5),sig_bonferroni)])
cat("\nOK\n")
