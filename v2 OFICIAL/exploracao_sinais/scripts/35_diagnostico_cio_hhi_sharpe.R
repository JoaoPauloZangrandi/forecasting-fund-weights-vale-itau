# =============================================================================
# 35_diagnostico_cio_hhi_sharpe.R  (exploracao_sinais)
#
# O candidato #34 (long-short CIO Peer Momentum puro, equal-weight dentro
# de cada perna) deu Sharpe=1,035, retorno anualizado ~10,5%/ano -- MUITO
# mais modesto que os 45-63%/ano ja flagados como implausiveis no
# candidato #15 (mesma variavel peer_ret, mas Fama-MacBeth spread bruto).
# Antes de acreditar nisso, aplicar TODAS as checagens que ja derrubaram
# candidatos anteriores (Comomentum, principalmente):
#   1) N de acoes por perna por mes (grupo degenerado?)
#   2) Robustez excluindo 2020 (crash/recuperacao COVID)
#   3) Quanto dos 24 meses sao positivos vs negativos (nao dominado por
#      1-2 meses extremos)
#   4) Reconciliar com a magnitude do candidato #15 (por que diferente?)
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
base <- base[is.finite(peer_ret) & is.finite(retorno_prox) & ym >= CORTE]
base[, quintil_peer := as.integer(cut(peer_ret, quantile(peer_ret, 0:5/5, na.rm=TRUE), include.lowest=TRUE)), by=ym]

por_mes <- base[quintil_peer %in% c(1,5), .(ret=mean(retorno_prox), n=.N), by=.(ym,quintil_peer)]
sm <- dcast(por_mes, ym~quintil_peer, value.var=c("ret","n"))
setnames(sm, c("ret_1","ret_5","n_1","n_5"), c("ret_short","ret_long","n_short","n_long"))
sm[, ret_ls := ret_long - ret_short]
setorder(sm, ym)

cat("===== 1) N de acoes por perna por mes =====\n")
cat(sprintf("N perna curta (Q1): min=%d mediana=%.0f max=%d\n", min(sm$n_short), median(sm$n_short), max(sm$n_short)))
cat(sprintf("N perna longa (Q5): min=%d mediana=%.0f max=%d\n", min(sm$n_long), median(sm$n_long), max(sm$n_long)))

cat("\n===== 2) Serie mensal completa do spread =====\n")
print(sm[, .(ym, n_short, n_long, ret_short=round(100*ret_short,2), ret_long=round(100*ret_long,2), ret_ls=round(100*ret_ls,2))])

cat("\n===== 3) Quantos meses positivos vs negativos, e quanto os 2 maiores contribuem =====\n")
n_pos <- sum(sm$ret_ls > 0); n_neg <- sum(sm$ret_ls <= 0)
cat(sprintf("Meses positivos: %d de %d (%.0f%%)\n", n_pos, nrow(sm), 100*n_pos/nrow(sm)))
top2 <- sm[order(-ret_ls)][1:2]
cat(sprintf("2 maiores meses: %s (%.2f%%) e %s (%.2f%%)\n", top2$ym[1], 100*top2$ret_ls[1], top2$ym[2], 100*top2$ret_ls[2]))
soma_total <- sum(sm$ret_ls); soma_sem_top2 <- soma_total - sum(top2$ret_ls)
cat(sprintf("Soma total do spread (24m): %.2f%% | Soma excluindo os 2 maiores meses: %.2f%%\n", 100*soma_total, 100*soma_sem_top2))

cat("\n===== 4) Robustez excluindo 2020 (so' 2021) =====\n")
sm_2021 <- sm[ym >= 202101]
sharpe <- function(r) mean(r,na.rm=TRUE)/sd(r,na.rm=TRUE)*sqrt(12)
t_2021 <- t.test(sm_2021$ret_ls)
cat(sprintf("2020+2021 (24m): Sharpe=%.3f | media=%.3f%%/mes | ret_anual=%.2f%%\n",
            sharpe(sm$ret_ls), 100*mean(sm$ret_ls), 100*mean(sm$ret_ls)*12))
cat(sprintf("So' 2021 (%dm)  : Sharpe=%.3f | media=%.3f%%/mes | ret_anual=%.2f%% | t=%.2f | p=%.4f\n",
            nrow(sm_2021), sharpe(sm_2021$ret_ls), 100*mean(sm_2021$ret_ls), 100*mean(sm_2021$ret_ls)*12,
            t_2021$statistic, t_2021$p.value))

cat("\n===== 5) Reconciliacao com candidato #15 (Fama-MacBeth spread bruto) =====\n")
t_completo <- t.test(sm$ret_ls)
cat(sprintf("Spread medio aqui (quintil 20%%, equal-weight dentro da perna): %.3f pp/mes | t=%.2f | p=%.4f\n",
            100*mean(sm$ret_ls), t_completo$statistic, t_completo$p.value))
cat("(candidato #15 reportou 4,15pp/mes -- diferenca pode vir de definicao de quintil,\n")
cat(" piso de conexao de peers, ou filtro de amostra distinto -- INVESTIGAR antes de conflar os dois)\n")

fwrite(sm, file.path(OUT, "candidatos_35_diagnostico_cio_hhi.csv"))
cat("\nOK\n")
