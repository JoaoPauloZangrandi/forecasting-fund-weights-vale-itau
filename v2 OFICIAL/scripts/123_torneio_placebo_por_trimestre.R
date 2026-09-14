suppressPackageStartupMessages({ library(data.table); library(fixest) })
DD <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau/v2 OFICIAL/data"
SC <- DD
setFixest_notes(FALSE)
D2 <- fread(file.path(SC, "painel_tri.csv")); D2[, cod_fundo := as.character(cod_fundo)]
D  <- fread(file.path(SC, "bateria_q4.csv"));  D[, cod_fundo := as.character(cod_fundo)]

cat("=========== (D2) ano a ano SEM o contemporaneo (compara com o 119) ===========\n")
for (a in sort(unique(D$ano))) {
  f <- feols(l_q4 ~ exc_h1 + exc_q3, D[ano==a], cluster=~cod_fundo)
  cat(sprintf("%d (n=%4d): h1 %+.3f (p=%.3f) | q3 %+.3f (p=%.3f)\n", a, sum(D$ano==a),
      coef(f)["exc_h1"], pvalue(f)["exc_h1"], coef(f)["exc_q3"], pvalue(f)["exc_q3"]))
}

cat("\n=========== UM COEFICIENTE POR TRIMESTRE ===========\n")
cat("Dependente: log(realocacao ativa), EF de fundo + EF de ano-trimestre\n")
cat("\n--- (a) so' o trimestre ANTERIOR (o que a hipotese do torneio preve) ---\n")
print(coeftable(feols(l_realoc ~ i(tri, exc_ant) | cod_fundo + anotri, D2, cluster=~cod_fundo)))
cat("\n--- (b) anterior E contemporaneo, ambos livres por trimestre ---\n")
print(coeftable(feols(l_realoc ~ i(tri, exc_ant) + i(tri, exc) | cod_fundo + anotri, D2, cluster=~cod_fundo)))
cat("\n--- (c) contraprova: a mesma coisa na DERIVA PASSIVA (nao e' decisao) ---\n")
D2[, l_passiva := log(passiva)]
print(coeftable(feols(l_passiva ~ i(tri, exc_ant) + i(tri, exc) | cod_fundo + anotri,
                      D2[is.finite(l_passiva)], cluster=~cod_fundo)))

cat("\n=========== O 4o TRIMESTRE, ANO A ANO ===========\n")
cat("coeficiente do trimestre anterior DENTRO do 4o tri, por ano\n")
for (a in sort(unique(D2$ano))) {
  d <- D2[ano == a & tri == 4L]
  if (nrow(d) < 100) next
  f <- feols(l_realoc ~ exc_ant + exc, d, cluster=~cod_fundo)
  cat(sprintf("%d (n=%4d): exc_ant %+.3f (p=%.3f) | exc %+.3f (p=%.3f)\n", a, nrow(d),
      coef(f)["exc_ant"], pvalue(f)["exc_ant"], coef(f)["exc"], pvalue(f)["exc"]))
}
cat("\nmesma coisa no 2o trimestre (placebo), por ano\n")
for (a in sort(unique(D2$ano))) {
  d <- D2[ano == a & tri == 2L]
  if (nrow(d) < 100) next
  f <- feols(l_realoc ~ exc_ant + exc, d, cluster=~cod_fundo)
  cat(sprintf("%d (n=%4d): exc_ant %+.3f (p=%.3f) | exc %+.3f (p=%.3f)\n", a, nrow(d),
      coef(f)["exc_ant"], pvalue(f)["exc_ant"], coef(f)["exc"], pvalue(f)["exc"]))
}
cat("\nOK\n")
