# Diagnostico da "inversao" jun/set -- so le CSVs ja gravados
suppressPackageStartupMessages({ library(data.table); library(fixest) })
DD <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau/v2 OFICIAL/data"
setFixest_notes(FALSE)

D <- fread(file.path(DD, "torneio_q4_fluxo.csv"))
D[, cod_fundo := as.character(cod_fundo)]
cat("Fundo-ano:", nrow(D), "| fundos:", uniqueN(D$cod_fundo), "\n\n")

cat("=========== (1) A INVERSAO E' ARITMETICA? ===========\n")
cat("max |exc_set - (exc_jun + exc_q3)| =", max(abs(D$exc_set - (D$exc_jun + D$exc_q3))), "\n")
cat("(se ~0, exc_set e' a SOMA das duas pecas, nao uma medida independente)\n\n")

cat("--- dispersao e correlacao ---\n")
print(D[, .(sd_jun = sd(exc_jun), sd_q3 = sd(exc_q3), sd_set = sd(exc_set))])
print(round(cor(D[, .(exc_jun, exc_q3, exc_set)]), 3))

cat("\n--- as tres regressoes univariadas (EF de ano, cluster fundo) ---\n")
f_jun <- feols(l_realoc_q4 ~ exc_jun | ano, D, cluster = ~cod_fundo)
f_q3  <- feols(l_realoc_q4 ~ exc_q3  | ano, D, cluster = ~cod_fundo)
f_set <- feols(l_realoc_q4 ~ exc_set | ano, D, cluster = ~cod_fundo)
f_2   <- feols(l_realoc_q4 ~ exc_jun + exc_q3 | ano, D, cluster = ~cod_fundo)
cat(sprintf("so exc_jun : %+.3f (p=%.2e)\n", coef(f_jun)[1], pvalue(f_jun)[1]))
cat(sprintf("so exc_q3  : %+.3f (p=%.2e)\n", coef(f_q3)[1],  pvalue(f_q3)[1]))
cat(sprintf("so exc_set : %+.3f (p=%.2e)\n", coef(f_set)[1], pvalue(f_set)[1]))
cat(sprintf("juntos     : jun %+.3f | q3 %+.3f\n", coef(f_2)[1], coef(f_2)[2]))

cat("\n--- decomposicao: o beta de exc_set previsto pelos dois betas ---\n")
Dm <- copy(D)
for (v in c("exc_jun","exc_q3","exc_set")) Dm[, (v) := get(v) - mean(get(v)), by = ano]
b1 <- coef(f_2)[1]; b2 <- coef(f_2)[2]
cs <- Dm[, .(c_jun_set = cov(exc_jun, exc_set), c_q3_set = cov(exc_q3, exc_set), v_set = var(exc_set))]
pred <- (b1*cs$c_jun_set + b2*cs$c_q3_set) / cs$v_set
cat(sprintf("beta(exc_set) previsto pela algebra = %+.3f | observado = %+.3f\n", pred, coef(f_set)[1]))
cat(sprintf("  parcela vinda de jun: %+.3f | parcela vinda de q3: %+.3f\n",
            b1*cs$c_jun_set/cs$v_set, b2*cs$c_q3_set/cs$v_set))

cat("\n\n=========== (2) ANO A ANO ===========\n")
out <- rbindlist(lapply(sort(unique(D$ano)), function(a) {
  d <- D[ano == a]
  f <- feols(l_realoc_q4 ~ exc_jun + exc_q3, d, cluster = ~cod_fundo)
  data.table(ano = a, n = nrow(d),
             b_jun = coef(f)["exc_jun"], p_jun = pvalue(f)["exc_jun"],
             b_q3  = coef(f)["exc_q3"],  p_q3  = pvalue(f)["exc_q3"],
             sd_q3 = sd(d$exc_q3))
}))
print(out[, .(ano, n, b_jun = round(b_jun,3), p_jun = signif(p_jun,2),
              b_q3 = round(b_q3,3), p_q3 = signif(p_q3,2), sd_q3 = round(sd_q3,3))])

cat("\n--- deixando um ano de fora por vez (EF de ano) ---\n")
for (a in sort(unique(D$ano))) {
  f <- feols(l_realoc_q4 ~ exc_jun + exc_q3 | ano, D[ano != a], cluster = ~cod_fundo)
  cat(sprintf("sem %d: jun %+.3f (p=%.1e) | q3 %+.3f (p=%.1e) | n=%d\n",
              a, coef(f)[1], pvalue(f)[1], coef(f)[2], pvalue(f)[2], f$nobs))
}

cat("\n\n=========== (3) OUTLIERS E ESCALA ===========\n")
print(D[, .(q = c("1%","5%","50%","95%","99%"),
            exc_jun = round(quantile(exc_jun, c(.01,.05,.5,.95,.99)),3),
            exc_q3  = round(quantile(exc_q3,  c(.01,.05,.5,.95,.99)),3),
            dep     = round(quantile(l_realoc_q4, c(.01,.05,.5,.95,.99)),3))])
W <- copy(D)
wins <- function(x, p = .01) { q <- quantile(x, c(p, 1-p)); pmin(pmax(x, q[1]), q[2]) }
W[, `:=`(exc_jun = wins(exc_jun), exc_q3 = wins(exc_q3), l_realoc_q4 = wins(l_realoc_q4))]
fw <- feols(l_realoc_q4 ~ exc_jun + exc_q3 | ano, W, cluster = ~cod_fundo)
cat(sprintf("winsorizado 1/99: jun %+.3f (p=%.1e) | q3 %+.3f (p=%.1e)\n",
            coef(fw)[1], pvalue(fw)[1], coef(fw)[2], pvalue(fw)[2]))
D[, r_jun := frank(exc_jun)/.N, by = ano]
D[, r_q3  := frank(exc_q3)/.N,  by = ano]
fr <- feols(l_realoc_q4 ~ r_jun + r_q3 | ano, D, cluster = ~cod_fundo)
cat(sprintf("em PERCENTIL (imune a cauda): jun %+.3f (p=%.1e) | q3 %+.3f (p=%.1e)\n",
            coef(fr)[1], pvalue(fr)[1], coef(fr)[2], pvalue(fr)[2]))
cat("  (interpretacao: efeito de ir do fundo mais fraco ao mais forte do ano)\n")
