suppressPackageStartupMessages({ library(data.table); library(fixest) })
DD <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau/v2 OFICIAL/data"
SC <- DD
setFixest_notes(FALSE)

# ---- (D) refeito com nomes, no recorte de tempo de negociacao --------------
D <- fread(file.path(SC, "bateria_q4.csv")); D[, cod_fundo := as.character(cod_fundo)]
cat("=========== (D) ANO A ANO, CALENDARIO CORRIGIDO (coefs por NOME) ===========\n")
for (a in sort(unique(D$ano))) {
  f <- feols(l_q4 ~ exc_h1 + exc_q3 + exc_q4, D[ano==a], cluster=~cod_fundo)
  cat(sprintf("%d (n=%4d): h1 %+.3f (p=%.3f) | q3 %+.3f (p=%.3f) | q4 %+.3f (p=%.3f)\n", a, sum(D$ano==a),
      coef(f)["exc_h1"], pvalue(f)["exc_h1"], coef(f)["exc_q3"], pvalue(f)["exc_q3"],
      coef(f)["exc_q4"], pvalue(f)["exc_q4"]))
}

# ---- painel TRIMESTRAL em tempo de negociacao ------------------------------
Q <- fread(file.path(SC, "realoc_fundo_mes.csv")); Q[, cod_fundo := as.character(cod_fundo)]
Q[, tri := (mes_negoc - 1L) %/% 3L + 1L]
TR <- Q[, .(realoc = weighted.mean(ativa, n), passiva = weighted.mean(passiva, n),
            n = sum(n), nm = uniqueN(mes_negoc)),
        by = .(cod_fundo, ano = ano_negoc, tri)]
TR <- TR[nm == 3L & n >= 5]

P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"),
           select = c("cod_fundo","ano","mes","retorno_fundo","retorno_ibov"))
P[, cod_fundo := as.character(cod_fundo)]
P <- P[is.finite(retorno_fundo) & is.finite(retorno_ibov)]
P[, tri := (mes - 1L) %/% 3L + 1L]
P[, nm := .N, by = .(cod_fundo, ano, tri)]
EX <- P[nm == 3L, .(exc = prod(1+retorno_fundo) - prod(1+retorno_ibov),
                    ret = prod(1+retorno_fundo) - 1), by = .(cod_fundo, ano, tri)]
EX[, t := ano*4L + tri]
setorder(EX, cod_fundo, t)
EX[, `:=`(exc_ant = shift(exc), t_ant = shift(t)), by = cod_fundo]
EX <- EX[t_ant == t - 1L]                      # exige o trimestre anterior contiguo

TR[, t := ano*4L + tri]
D2 <- merge(TR, EX[, .(cod_fundo, ano, tri, exc, exc_ant)], by = c("cod_fundo","ano","tri"))
D2[, l_realoc := log(realoc)]
D2 <- D2[is.finite(l_realoc) & is.finite(exc_ant)]
D2[, anotri := ano*10L + tri]
D2[, t4 := as.integer(tri == 4L)]
cat("\n\nPainel trimestral:", nrow(D2), "fundo-ano-tri |", uniqueN(D2$cod_fundo), "fundos\n")
print(D2[, .N, by = tri][order(tri)])

sh <- function(f, lbl) { cf <- coef(f); pv <- pvalue(f)
  cat(sprintf("%-40s", lbl)); for (v in names(cf)) cat(sprintf(" | %s %+.3f (p=%.1e)", v, cf[v], pv[v]))
  cat(sprintf(" | n=%d\n", f$nobs)) }

cat("\n=========== O EFEITO DE RECENCIA E' SO' DO 4o TRIMESTRE? ===========\n")
cat("Dependente: log(realocacao ativa do trimestre), NIVEL (sem razao, sem denominador)\n")
cat("EF de fundo (nivel de atividade da casa) + EF de ano-trimestre (mercado)\n\n")
sh(feols(l_realoc ~ exc_ant | cod_fundo + anotri, D2, cluster=~cod_fundo), "todos os trimestres juntos")
sh(feols(l_realoc ~ exc_ant + exc_ant:t4 | cod_fundo + anotri, D2, cluster=~cod_fundo), "com efeito extra no 4o tri")
sh(feols(l_realoc ~ exc_ant + exc | cod_fundo + anotri, D2, cluster=~cod_fundo), "+ contemporaneo")
sh(feols(l_realoc ~ exc_ant + exc + exc_ant:t4 + exc:t4 | cod_fundo + anotri, D2, cluster=~cod_fundo), "tudo interagido com 4o tri")

cat("\n--- um coeficiente POR TRIMESTRE (o placebo honesto) ---\n")
f <- feols(l_realoc ~ i(tri, exc_ant) | cod_fundo + anotri, D2, cluster=~cod_fundo)
print(summary(f))

cat("\n--- o mesmo, sem EF de fundo (so' EF de ano-trimestre) ---\n")
f2 <- feols(l_realoc ~ i(tri, exc_ant) | anotri, D2, cluster=~cod_fundo)
print(coeftable(f2))

cat("\n--- e na DERIVA PASSIVA? (nao e' decisao; serve de contraprova) ---\n")
D2[, l_passiva := log(passiva)]
f3 <- feols(l_passiva ~ i(tri, exc_ant) | cod_fundo + anotri,
            D2[is.finite(l_passiva)], cluster=~cod_fundo)
print(coeftable(f3))
fwrite(D2, file.path(SC, "painel_tri.csv"))
cat("\nOK\n")
