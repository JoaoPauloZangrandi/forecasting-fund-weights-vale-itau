# =============================================================================
# 10_diagnostico_comomentum_h6.R  (exploracao_sinais)
#
# O candidato Comomentum h=6 passou de 5% no Fama-MacBeth (p=0,041), mas
# com magnitude implausivel (+6pp/mes) e amostra MUITO fina (~37
# acoes/mes no decil de momentum, ~7 por quintil). Investiga antes de
# reportar como achado: (1) o spread mes-a-mes esta concentrado em 1-2
# meses extremos (provavelmente COVID) ou e' distribuido? (2) quantas
# acoes distintas entram no quintil extremo, de fato? (3) o resultado
# sobrevive excluindo os meses de crash (fev-jun/2020)?
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

mom <- precos[, .(ticker, ymk, retorno)]
setorder(mom, ticker, ymk)
mom[, log_ret := log(1+retorno)]
mom[, mom_12m := exp(frollsum(log_ret, 12, align="right")) - 1, by = ticker]
mom <- mom[!is.na(mom_12m)]

mercado <- precos[, .(ret_mercado = mean(retorno, na.rm=TRUE)), by = ymk]
precos_resid <- merge(precos, mercado, by = "ymk")
precos_resid[, retorno_resid := retorno - ret_mercado]
resid_wide <- dcast(precos_resid, ymk ~ ticker, value.var = "retorno_resid")
setorder(resid_wide, ymk)

meses_teste <- sort(unique(mom[ymk >= 201801]$ymk))

calc_comomentum_mes <- function(mes_atual) {
  m_mes <- mom[ymk == mes_atual]
  if (nrow(m_mes) < 20) return(NULL)
  corte_top <- quantile(m_mes$mom_12m, 0.9, na.rm=TRUE)
  top_mom <- m_mes[mom_12m >= corte_top]$ticker
  if (length(top_mom) < 8) return(NULL)
  janela <- resid_wide[ymk <= mes_atual][ymk > addm(mes_atual, -12)]
  cols_disp <- intersect(top_mom, names(janela))
  if (length(cols_disp) < 8) return(NULL)
  mat <- as.matrix(janela[, ..cols_disp])
  mat <- mat[, colSums(!is.na(mat)) >= 8, drop = FALSE]
  if (ncol(mat) < 8) return(NULL)
  cor_mat <- suppressWarnings(cor(mat, use = "pairwise.complete.obs"))
  cor_mat[!is.finite(cor_mat)] <- NA
  diag(cor_mat) <- NA
  data.table(ticker = colnames(cor_mat), como = rowMeans(cor_mat, na.rm=TRUE), ym = mes_atual)[is.finite(como)]
}
como_dt <- rbindlist(Filter(Negate(is.null), lapply(meses_teste, calc_comomentum_mes)))

h <- 6
m <- copy(como_dt); m[, ym_ret := addm(ym, h)]
m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
m <- m[is.finite(retorno) & is.finite(como)]
treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
cat("n distintos ATIVOS no teste (h=6, top-momentum):", uniqueN(teste$ticker), "\n")
cat("meses de teste distintos:", uniqueN(teste$ym), "\n")

breaks <- unique(quantile(treino$como, seq(0,1,0.2), na.rm = TRUE))
breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
teste[, quintil := as.integer(cut(como, breaks, labels = FALSE, include.lowest = TRUE))]
n_grupos <- length(breaks) - 1
if (n_grupos < 5) teste[, quintil := ifelse(quintil == 1, 1, ifelse(quintil == n_grupos, 5, quintil))]

cat("\n===== Composicao por mes (quintil 1 vs 5), n de acoes =====\n")
comp <- teste[quintil %in% c(1,5), .(n = .N), by = .(ym, quintil)]
print(dcast(comp, ym ~ quintil, value.var = "n"))

cat("\n===== Spread MES A MES (Q5 - Q1), pra ver se e' concentrado =====\n")
por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(retorno), n=.N), by = .(ym, quintil)]
sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
setnames(sm, c("1","5"), c("q1","q5"))
sm[, spread := q5 - q1]
print(sm[order(ym)])

cat("\n===== Excluindo meses de retorno que caem no periodo de crash (fev-jun/2020) =====\n")
sm_sem_crash <- sm[!ym %in% c(202002,202003,202004,202005,202006)]
cat("Meses restantes:", nrow(sm_sem_crash), "\n")
if (nrow(sm_sem_crash) >= 6) {
  media2 <- mean(sm_sem_crash$spread, na.rm=TRUE); dp2 <- sd(sm_sem_crash$spread, na.rm=TRUE); n2 <- nrow(sm_sem_crash)
  t2 <- media2/(dp2/sqrt(n2)); p2 <- 2*pt(-abs(t2), df=n2-1)
  cat(sprintf("Spread medio SEM crash: %.4f (%.2fpp/mes) | t=%.2f | p=%.4f\n", media2, 100*media2, t2, p2))
} else cat("Poucos meses sobrando sem o periodo de crash\n")

cat("\nOK\n")
