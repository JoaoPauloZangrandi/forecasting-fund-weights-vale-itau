# =============================================================================
# 26_persistencia_performance_fundos.R  (exploracao_sinais)
#
# NOVO OBJETO: em vez de prever retorno de ACAO a partir de holdings,
# testa se o retorno do PROPRIO FUNDO tem persistencia -- pergunta
# classica (Carhart 1997, "hot hands") e mais tratavel estatisticamente
# (mais fundos que acoes no nosso painel: ~2500 vs ~500).
#
# Bateria:
#  (A) Persistencia RAW: retorno acumulado passado (varias janelas de
#      formacao) prediz retorno futuro (varios horizontes) -- OOS linear
#      + Fama-MacBeth por decil.
#  (B) ASSIMETRIA (achado classico de Carhart): a persistencia e' mais
#      forte no decil INFERIOR (fundo ruim continua ruim) do que no
#      SUPERIOR (fundo bom nao necessariamente continua bom)?
#  (C) AJUSTADO POR RISCO: alfa vs beta_fundo (nao so' retorno bruto) --
#      sera que e' skill genuino ou so' persistencia de tomada de risco?
#  (D) Condicional em TAMANHO do fundo (AUM): fundos pequenos tem mais
#      ou menos persistencia?
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
cat("Fundo-meses de retorno disponiveis:", nrow(rf), "| fundos distintos:", uniqueN(rf$cod_fundo), "\n")
rf[, log_ret := log(1 + retorno_fundo)]

# ---- (A) janelas de formacao variadas ----
for (janela in c(3,6,12,24,36)) {
  rf[[paste0("ret_acum_",janela,"m")]] <- exp(frollsum(rf$log_ret, janela, align="right")) - 1
}

fama_macbeth_fundo <- function(m, var_x, var_y, nome, h, n_grupos = 10) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)

  fit_lm <- lm(as.formula(paste(var_y,"~",var_x)), data = treino)
  pred <- predict(fit_lm, newdata = teste)
  sse_m <- sum((teste[[var_y]]-pred)^2); sse_n <- sum((teste[[var_y]]-mean(treino[[var_y]]))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(as.formula(paste(var_y,"~",var_x)), data=m))$coefficients

  probs <- seq(0,1,length.out=n_grupos+1)
  breaks <- unique(quantile(treino[[var_x]], probs, na.rm=TRUE))
  if (length(breaks) < 3) return(data.table(sinal=nome,horizonte=h,r2_oos_pct=100*r2_oos,p_full=s_full[2,4],
                                             n_meses=NA,spread_pp=NA,t_fm=NA,p_fm=NA))
  breaks[1]<--Inf; breaks[length(breaks)]<-Inf
  teste[, grupo := as.integer(cut(get(var_x), breaks, labels=FALSE, include.lowest=TRUE))]
  ng <- length(breaks)-1
  teste <- teste[!is.na(grupo)]
  por_mes <- teste[grupo %in% c(1,ng), .(rm=mean(get(var_y)),n=.N), by=.(ym,grupo)]
  sm <- dcast(por_mes, ym~grupo, value.var="rm")
  cols_precisa <- as.character(c(1,ng))
  if (!all(cols_precisa %in% names(sm))) return(data.table(sinal=nome,horizonte=h,r2_oos_pct=100*r2_oos,p_full=s_full[2,4],
                                                             n_meses=NA,spread_pp=NA,t_fm=NA,p_fm=NA))
  setnames(sm, cols_precisa, c("q1","qN")); sm <- sm[is.finite(q1)&is.finite(qN)]
  sm[, spread:=qN-q1]; nmes <- nrow(sm)
  if (nmes<6) return(data.table(sinal=nome,horizonte=h,r2_oos_pct=100*r2_oos,p_full=s_full[2,4],
                                 n_meses=nmes,spread_pp=NA,t_fm=NA,p_fm=NA))
  media<-mean(sm$spread); dp<-sd(sm$spread)
  t_fm<-media/(dp/sqrt(nmes)); p_fm<-2*pt(-abs(t_fm),df=nmes-1)
  cat(sprintf("%-30s h=%2d [%2dgrp] n_tr=%6d n_te=%5d R2_OOS=%7.3f%% | %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.5f\n",
              nome,h,n_grupos,nrow(treino),nrow(teste),100*r2_oos,nmes,100*media,t_fm,p_fm))
  data.table(sinal=nome,horizonte=h,r2_oos_pct=100*r2_oos,p_full=s_full[2,4],
             n_meses=nmes,spread_pp=100*media,t_fm=t_fm,p_fm=p_fm)
}

resultados <- list()

cat("===== (A) PERSISTENCIA RAW: varias janelas de formacao x varios horizontes =====\n")
for (janela in c(3,6,12,24,36)) {
  var_form <- paste0("ret_acum_",janela,"m")
  for (h in c(1,3,6,12)) {
    m <- copy(rf); m[, ym_fut := addm(ymk, h)]
    ret_fut <- rf[, .(cod_fundo, ym_fut = ymk, ret_futuro = retorno_fundo)]
    m <- merge(m, ret_fut, by = c("cod_fundo","ym_fut"))
    m[, ym := ymk]
    m <- m[is.finite(get(var_form)) & is.finite(ret_futuro)]
    r <- fama_macbeth_fundo(m, var_form, "ret_futuro", paste0("Persist-",janela,"m"), h)
    if (!is.null(r)) resultados[[length(resultados)+1]] <- r
  }
}

RA <- rbindlist(resultados, fill=TRUE)
fwrite(RA, file.path(OUT, "candidatos_26A_persistencia_raw.csv"))
cat("\n\n===== RESUMO (A): ordenado por p_fm =====\n")
print(RA[order(p_fm)][1:20, .(sinal,horizonte,n_meses,spread_pp=round(spread_pp,2),t_fm=round(t_fm,2),p_fm=round(p_fm,5))])

cat("\nOK -- parte (A) completa\n")
