# =============================================================================
# 36_reconciliacao_metodologia_cio.R  (exploracao_sinais)
#
# O candidato #35 achou Sharpe=1,53 (24m) e Sharpe=2,57 em 2021-sozinho
# (t=2,57, p=0,026) pro long-short CIO Peer Momentum -- OPOSTO do que o
# candidato #15/16 registrou (enfraquece pra p=0,057 sem 2020). As duas
# analises usam o MESMO cio_peer_return.csv, mas metodologias de corte em
# quintil DIFERENTES: script 11 usa breaks FIXOS calculados no TREINO
# (<2020) e aplicados no teste; scripts 34/35 re-cortam quintil A CADA MES
# (padrao mais comum em sorts de carteira, Fama-French style). Preciso
# separar: a diferenca vem do metodo de corte, ou de outra coisa (ex:
# universo, tratamento de NA)? Roda as DUAS metodologias, lado a lado, na
# MESMISSIMA base, como serie de retorno de portfolio (nao so' t-stat).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
cio <- fread(file.path(OUT, "cio_peer_return.csv"))

base <- copy(cio)
ret_prox <- precos[, .(ticker, ym_alvo = addm(ymk, -1), retorno_prox = retorno)]
base <- merge(base, ret_prox, by.x = c("ticker","ym"), by.y = c("ticker","ym_alvo"))
base <- base[is.finite(peer_ret) & is.finite(retorno_prox)]

sharpe <- function(r) mean(r,na.rm=TRUE)/sd(r,na.rm=TRUE)*sqrt(12)

# ---- METODO A: breaks FIXOS do treino (igual script 11 original) ----
treino <- base[ym < CORTE]
breaks_treino <- quantile(treino$peer_ret, 0:5/5, na.rm=TRUE)
breaks_treino[1] <- -Inf; breaks_treino[6] <- Inf
teste_A <- copy(base[ym >= CORTE])
teste_A[, quintil := as.integer(cut(peer_ret, breaks_treino, labels=FALSE, include.lowest=TRUE))]
sm_A <- teste_A[quintil %in% c(1,5), .(ret=mean(retorno_prox), n=.N), by=.(ym,quintil)]
smw_A <- dcast(sm_A, ym~quintil, value.var=c("ret","n"))
setnames(smw_A, c("ret_1","ret_5"), c("short","long"))
smw_A[, ret_ls := long - short]

# ---- METODO B: re-corte de quintil A CADA MES (script 34/35) ----
teste_B <- copy(base[ym >= CORTE])
teste_B[, quintil := as.integer(cut(peer_ret, quantile(peer_ret, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by=ym]
sm_B <- teste_B[quintil %in% c(1,5), .(ret=mean(retorno_prox), n=.N), by=.(ym,quintil)]
smw_B <- dcast(sm_B, ym~quintil, value.var=c("ret","n"))
setnames(smw_B, c("ret_1","ret_5"), c("short","long"))
smw_B[, ret_ls := long - short]

comp <- merge(smw_A[,.(ym, ret_ls_A=ret_ls, n_1_A=n_1, n_5_A=n_5)],
              smw_B[,.(ym, ret_ls_B=ret_ls, n_1_B=n_1, n_5_B=n_5)], by="ym")
cat("===== Comparacao mes a mes: Metodo A (breaks fixos do treino) vs Metodo B (re-corte mensal) =====\n")
print(comp)

cat("\n===== Resumo =====\n")
for (nome in c("A (breaks fixos do treino)","B (re-corte mensal)")) {
  col <- if (grepl("^A", nome)) smw_A$ret_ls else smw_B$ret_ls
  t_full <- t.test(col)
  col_2021 <- if (grepl("^A", nome)) smw_A[ym>=202101]$ret_ls else smw_B[ym>=202101]$ret_ls
  t_2021 <- t.test(col_2021)
  cat(sprintf("Metodo %s:\n", nome))
  cat(sprintf("  24m completo : Sharpe=%.3f | media=%.3f%%/mes | t=%.2f | p=%.4f | N=%d\n",
              sharpe(col), 100*mean(col), t_full$statistic, t_full$p.value, length(col)))
  cat(sprintf("  So' 2021 (12m): Sharpe=%.3f | media=%.3f%%/mes | t=%.2f | p=%.4f | N=%d\n",
              sharpe(col_2021), 100*mean(col_2021), t_2021$statistic, t_2021$p.value, length(col_2021)))
}

cat("\n===== N por perna, os dois metodos (media/mediana/min) =====\n")
cat(sprintf("Metodo A: perna curta med=%.0f min=%d | perna longa med=%.0f min=%d\n",
            median(smw_A$n_1,na.rm=TRUE), min(smw_A$n_1,na.rm=TRUE), median(smw_A$n_5,na.rm=TRUE), min(smw_A$n_5,na.rm=TRUE)))
cat(sprintf("Metodo B: perna curta med=%.0f min=%d | perna longa med=%.0f min=%d\n",
            median(smw_B$n_1,na.rm=TRUE), min(smw_B$n_1,na.rm=TRUE), median(smw_B$n_5,na.rm=TRUE), min(smw_B$n_5,na.rm=TRUE)))

fwrite(comp, file.path(OUT, "candidatos_36_reconciliacao_metodo.csv"))
cat("\nOK\n")
