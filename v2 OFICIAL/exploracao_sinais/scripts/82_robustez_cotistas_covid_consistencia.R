# =============================================================================
# 82_robustez_cotistas_covid_consistencia.R  (exploracao_sinais, agente eventos)
#
# Duas checagens finais no achado de "N. medio de cotistas dos fundos donos"
# -> volatilidade futura (h=6 p=0.00006, h=12 p=0.00022, sobrevive controle
# de HHI/tamanho/vol.passada no script 81):
#  (1) o spread e consistente mes a mes (nao dominado por 1-2 meses
#      extremos, mesmo problema que quase gerou o falso-positivo do
#      Comomentum)?
#  (2) o resultado sobrevive excluindo os meses do crash/recuperacao da
#      COVID (fev-dez/2020), que dominam boa parte do periodo de teste
#      (2020-2021)?
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

frag <- fread(file.path(OUT, "candidatos_79_fragilidade_donos.csv"))
winsor <- function(x) { q <- quantile(x, c(0.01,0.99), na.rm=TRUE); pmin(pmax(x,q[1]),q[2]) }
frag[, frag_cotistas_w := winsor(frag_cotistas_medio)]

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
precos <- precos[is.finite(retorno)]
setorder(precos, ticker, ymk)

calc_vol_futura <- function(dt_precos, h) {
  pw <- copy(dt_precos); cols <- list()
  for (k in 1:h) {
    aux <- pw[, .(ticker, ym_base = addm(ymk, -k), ret_k = retorno)]
    setnames(aux, "ret_k", paste0("r",k)); cols[[k]] <- aux
  }
  bb <- Reduce(function(a,b) merge(a,b,by=c("ticker","ym_base"), all=TRUE), cols)
  ret_cols <- paste0("r",1:h)
  bb[, vol_futura := apply(.SD, 1, sd, na.rm=TRUE), .SDcols = ret_cols]
  bb[, n_obs := apply(.SD, 1, function(x) sum(is.finite(x))), .SDcols = ret_cols]
  bb <- bb[n_obs >= max(2,round(h*0.7))]
  bb[, .(ticker, ym = ym_base, vol_futura)]
}

spread_mensal <- function(m, h) {
  mm <- copy(m)
  mm <- mm[is.finite(frag_cotistas_w) & is.finite(vol_futura)]
  mm[, quintil := as.integer(cut(frag_cotistas_w, quantile(frag_cotistas_w, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by = ym]
  mm <- mm[!is.na(quintil) & ym >= CORTE]
  n_por_grupo <- mm[quintil %in% c(1,5), .N, by=.(ym,quintil)]
  por_mes <- mm[quintil %in% c(1,5), .(m = mean(vol_futura), n=.N), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "m")
  setnames(sm, c("1","5"), c("q1","q5"))
  sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]
  sm
}

for (h in c(6,12)) {
  vol_h <- calc_vol_futura(precos, h)
  m <- merge(frag, vol_h, by = c("ticker","ym"))
  sm <- spread_mensal(m, h)
  cat(sprintf("\n\n===== h=%d: spread mensal completo (quintil cotistas 5 - 1, sobre vol futura) =====\n", h))
  print(sm[, .(ym, q1=round(q1,4), q5=round(q5,4), spread=round(spread,5))])
  cat(sprintf("N meses: %d | %% meses com spread negativo (direcao esperada): %.1f%%\n",
              nrow(sm), 100*mean(sm$spread < 0)))

  cat("\n-- (1) Consistencia: estatistica sem os 2 meses mais extremos --\n")
  sm_ord <- sm[order(abs(spread))]
  sm_sem_extremos <- sm_ord[1:(nrow(sm_ord)-2)]
  media <- mean(sm_sem_extremos$spread); dp <- sd(sm_sem_extremos$spread); n <- nrow(sm_sem_extremos)
  t <- media/(dp/sqrt(n)); p <- 2*pt(-abs(t), df=n-1)
  cat(sprintf("Excluindo os 2 meses de spread mais extremo (absoluto): n=%d media=%+.5f t=%.2f p=%.5f\n", n, media, t, p))

  cat("\n-- (2) Excluindo meses do crash/recuperacao COVID (fev-dez/2020) --\n")
  sm_sem_covid <- sm[!(ym %in% 202002:202012)]
  if (nrow(sm_sem_covid) >= 6) {
    media2 <- mean(sm_sem_covid$spread); dp2 <- sd(sm_sem_covid$spread); n2 <- nrow(sm_sem_covid)
    t2 <- media2/(dp2/sqrt(n2)); p2 <- 2*pt(-abs(t2), df=n2-1)
    cat(sprintf("Sem COVID (fev-dez/2020): n=%d media=%+.5f t=%.2f p=%.5f\n", n2, media2, t2, p2))
  } else {
    cat(sprintf("Poucos meses fora da COVID no periodo de teste (n=%d) para esse horizonte -- nao testavel isoladamente.\n", nrow(sm_sem_covid)))
  }
}
cat("\nOK\n")
