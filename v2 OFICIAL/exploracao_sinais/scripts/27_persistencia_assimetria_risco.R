# =============================================================================
# 27_persistencia_assimetria_risco.R  (exploracao_sinais)
#
# Continuacao do candidato #26 (persistencia raw, nula em 20/20).
#
# (B) ASSIMETRIA: Carhart (1997) achou que fundos RUINS continuam ruins
#     (persistencia forte do decil inferior sozinho), mas fundos BONS nao
#     necessariamente continuam bons -- testa cada PONTA separadamente
#     (retorno medio do decil 1 sozinho vs decil 10 sozinho, cada um
#     comparado a media do resto), nao so' o spread 10-1.
# (C) AJUSTADO POR RISCO: alfa (residuo do fundo vs beta_fundo x retorno
#     de mercado) em vez de retorno bruto -- sera que a falta de
#     persistencia e' porque skill genuino nao persiste, ou porque
#     persistencia de RISCO (fundos arriscados continuam arriscados,
#     mecanico) mascara a leitura do retorno bruto?
# (D) CONDICIONAL EM TAMANHO: fundos pequenos tem mais persistencia
#     (menos capacidade limita arbitragem do proprio desempenho)?
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, ret_acum_12m := exp(frollsum(log_ret, 12, align="right")) - 1]

# ---- alfa (residuo vs retorno de mercado, beta_fundo trailing) ----
mercado <- rf[, .(ret_mercado_fundos = mean(retorno_fundo, na.rm=TRUE)), by = ymk]
rf <- merge(rf, mercado, by = "ymk")
calc_beta_alfa <- function(ret, ret_mkt) {
  n <- length(ret); beta_out <- rep(NA_real_,n); alfa_out <- rep(NA_real_,n)
  if (n<12) return(list(beta=beta_out, alfa=alfa_out))
  for (i in 12:n) {
    jan <- (i-11):i
    fit <- tryCatch(lm(ret[jan]~ret_mkt[jan]), error=function(e) NULL)
    if (!is.null(fit)) { beta_out[i] <- coef(fit)[2]; alfa_out[i] <- coef(fit)[1] }
  }
  list(beta=beta_out, alfa=alfa_out)
}
setorder(rf, cod_fundo, ymk)
rf[, c("beta_trail","alfa_trail") := calc_beta_alfa(retorno_fundo, ret_mercado_fundos), by = cod_fundo]
# alfa_trail e' o intercepto da regressao trailing -- interpretamos como "skill" mensal medio nos ultimos 12m
rf[, alfa_acum_12m := alfa_trail * 12]  # aproximacao simples, alfa mensal medio * 12

# ---- tamanho do fundo (AUM, do painel principal) ----
aum <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ym","aum_prev"))
aum[, cod_fundo := as.character(cod_fundo)]
aum <- unique(aum, by = c("cod_fundo","ym"))
aum <- aum[is.finite(aum_prev) & aum_prev > 0]
aum[, tamanho_rank := frank(aum_prev)/.N, by = ym]

ret_fut <- rf[, .(cod_fundo, ym_fut = ymk, ret_futuro = retorno_fundo)]

# ---- (B) ASSIMETRIA: decis extremos separados, h=1 e h=3 ----
cat("===== (B) ASSIMETRIA (decil 1 sozinho vs decil 10 sozinho, cada vs. media do resto) =====\n")
for (h in c(1,3,6)) {
  m <- copy(rf); m[, ym_fut := addm(ymk,h)]
  m <- merge(m, ret_fut, by = c("cod_fundo","ym_fut"))
  m[, ym := ymk]
  m <- m[is.finite(ret_acum_12m) & is.finite(ret_futuro)]
  treino <- m[ym<CORTE]; teste <- copy(m[ym>=CORTE])
  breaks <- unique(quantile(treino$ret_acum_12m, seq(0,1,0.1), na.rm=TRUE))
  if (length(breaks)<5) next
  breaks[1]<--Inf; breaks[length(breaks)]<-Inf
  teste[, decil := as.integer(cut(ret_acum_12m, breaks, labels=FALSE, include.lowest=TRUE))]
  nd <- length(breaks)-1
  teste <- teste[!is.na(decil)]
  media_geral_mes <- teste[, .(media_mes = mean(ret_futuro)), by = ym]
  d1 <- merge(teste[decil==1, .(ret_d1=mean(ret_futuro)), by=ym], media_geral_mes, by="ym")
  d1[, excesso := ret_d1 - media_mes]
  dN <- merge(teste[decil==nd, .(ret_dN=mean(ret_futuro)), by=ym], media_geral_mes, by="ym")
  dN[, excesso := ret_dN - media_mes]
  if (nrow(d1)>=6) {
    t1 <- mean(d1$excesso)/(sd(d1$excesso)/sqrt(nrow(d1))); p1 <- 2*pt(-abs(t1),df=nrow(d1)-1)
    cat(sprintf("  h=%d DECIL 1 (piores) excesso medio=%+.2fpp/mes t=%.2f p=%.4f (n=%d meses)\n", h, 100*mean(d1$excesso), t1, p1, nrow(d1)))
  }
  if (nrow(dN)>=6) {
    tN <- mean(dN$excesso)/(sd(dN$excesso)/sqrt(nrow(dN))); pN <- 2*pt(-abs(tN),df=nrow(dN)-1)
    cat(sprintf("  h=%d DECIL %d (melhores) excesso medio=%+.2fpp/mes t=%.2f p=%.4f (n=%d meses)\n", h, nd, 100*mean(dN$excesso), tN, pN, nrow(dN)))
  }
}

# ---- (C) ALFA ao inves de retorno bruto ----
cat("\n===== (C) PERSISTENCIA DE ALFA (ajustado por risco) =====\n")
fama_macbeth_fundo <- function(m, var_x, var_y, nome, h, n_grupos=10) {
  treino <- m[ym<CORTE]; teste <- copy(m[ym>=CORTE])
  if (nrow(treino)<100||nrow(teste)<50) return(NULL)
  probs <- seq(0,1,length.out=n_grupos+1)
  breaks <- unique(quantile(treino[[var_x]], probs, na.rm=TRUE))
  if (length(breaks)<3) return(NULL)
  breaks[1]<--Inf; breaks[length(breaks)]<-Inf
  teste[, grupo := as.integer(cut(get(var_x), breaks, labels=FALSE, include.lowest=TRUE))]
  ng <- length(breaks)-1; teste <- teste[!is.na(grupo)]
  por_mes <- teste[grupo %in% c(1,ng), .(rm=mean(get(var_y))), by=.(ym,grupo)]
  sm <- dcast(por_mes, ym~grupo, value.var="rm")
  cp <- as.character(c(1,ng)); if (!all(cp %in% names(sm))) return(NULL)
  setnames(sm,cp,c("q1","qN")); sm <- sm[is.finite(q1)&is.finite(qN)]
  sm[, spread:=qN-q1]; nmes<-nrow(sm); if(nmes<6) return(NULL)
  media<-mean(sm$spread); dp<-sd(sm$spread); t_fm<-media/(dp/sqrt(nmes)); p_fm<-2*pt(-abs(t_fm),df=nmes-1)
  cat(sprintf("%-30s h=%d %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.5f\n", nome,h,nmes,100*media,t_fm,p_fm))
  data.table(sinal=nome,horizonte=h,n_meses=nmes,spread_pp=100*media,t_fm=t_fm,p_fm=p_fm)
}
resultados_c <- list()
for (h in c(1,3,6)) {
  m <- copy(rf); m[, ym_fut := addm(ymk,h)]
  m <- merge(m, ret_fut, by=c("cod_fundo","ym_fut")); m[, ym:=ymk]
  m <- m[is.finite(alfa_acum_12m) & is.finite(ret_futuro)]
  r <- fama_macbeth_fundo(m, "alfa_acum_12m", "ret_futuro", "Persist-alfa", h)
  if (!is.null(r)) resultados_c[[length(resultados_c)+1]] <- r
}

# ---- (D) CONDICIONAL EM TAMANHO ----
cat("\n===== (D) PERSISTENCIA CONDICIONAL EM TAMANHO DO FUNDO =====\n")
resultados_d <- list()
for (h in c(1,3)) {
  m <- copy(rf); m[, ym_fut := addm(ymk,h)]
  m <- merge(m, ret_fut, by=c("cod_fundo","ym_fut")); m[, ym:=ymk]
  m <- merge(m, aum[, .(cod_fundo, ym, tamanho_rank)], by=c("cod_fundo","ym"))
  m <- m[is.finite(ret_acum_12m) & is.finite(ret_futuro) & is.finite(tamanho_rank)]
  pequenos <- m[tamanho_rank <= 1/3]; grandes <- m[tamanho_rank >= 2/3]
  r1 <- fama_macbeth_fundo(pequenos, "ret_acum_12m", "ret_futuro", "Persist-PEQUENOS", h)
  r2 <- fama_macbeth_fundo(grandes, "ret_acum_12m", "ret_futuro", "Persist-GRANDES", h)
  if (!is.null(r1)) resultados_d[[length(resultados_d)+1]] <- r1
  if (!is.null(r2)) resultados_d[[length(resultados_d)+1]] <- r2
}

RC <- rbindlist(resultados_c, fill=TRUE); RD <- rbindlist(resultados_d, fill=TRUE)
fwrite(RC, file.path(OUT, "candidatos_27C_alfa.csv"))
fwrite(RD, file.path(OUT, "candidatos_27D_tamanho.csv"))
cat("\nOK\n")
