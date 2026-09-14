suppressPackageStartupMessages({ library(data.table); library(fixest) })
DD <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau/v2 OFICIAL/data"
SC <- DD
setFixest_notes(FALSE)
D2 <- fread(file.path(SC, "painel_tri.csv")); D2[, cod_fundo := as.character(cod_fundo)]

# remove o nivel de atividade proprio do fundo (equivalente ao EF de fundo)
D2[, l_dm := l_realoc - mean(l_realoc), by = cod_fundo]
D2[, nq := .N, by = cod_fundo]
D2s <- D2[nq >= 4]                       # fundo precisa de historia p/ a media fazer sentido

cat("=========== EFEITO POR TRIMESTRE E POR ANO (dentro do fundo) ===========\n")
cat("dependente: log(realoc) - media do fundo; regressores padronizados (1 d.p.)\n")
cat("celula = efeito de 1 d.p. a MAIS de desempenho, em log-pontos de realocacao\n\n")
res <- rbindlist(lapply(sort(unique(D2s$ano)), function(a) rbindlist(lapply(1:4, function(q) {
  d <- D2s[ano == a & tri == q]
  if (nrow(d) < 150) return(NULL)
  d[, `:=`(z_ant = exc_ant/sd(exc_ant), z_con = exc/sd(exc))]
  f <- feols(l_dm ~ z_ant + z_con, d, vcov = "hetero")
  data.table(ano = a, tri = q, n = nrow(d),
             b_ant = coef(f)["z_ant"], p_ant = pvalue(f)["z_ant"],
             b_con = coef(f)["z_con"], p_con = pvalue(f)["z_con"])
}))))
print(res[, .(ano, tri, n, b_ant = round(b_ant,3), p_ant = round(p_ant,3),
              b_con = round(b_con,3), p_con = round(p_con,3))])

cat("\n--- so' o 4o trimestre, lado a lado com a media dos outros tres ---\n")
print(res[, .(media_b_ant = round(mean(b_ant),3)), by = .(tri)][order(tri)])

cat("\n=========== POOLED PADRONIZADO, POR TRIMESTRE ===========\n")
D2[, `:=`(z_ant = exc_ant/sd(exc_ant), z_con = exc/sd(exc)), by = anotri]
print(coeftable(feols(l_realoc ~ i(tri, z_ant) + i(tri, z_con) | cod_fundo + anotri,
                      D2, cluster = ~cod_fundo)))

cat("\n=========== TESTE FORMAL: 4o TRI E' DIFERENTE DO 1o? ===========\n")
f <- feols(l_realoc ~ i(tri, z_ant) + i(tri, z_con) | cod_fundo + anotri, D2, cluster = ~cod_fundo)
print(wald(f, "tri::4:z_ant"))
cat("\nH0: coef do anterior no 4o tri == coef no 1o tri\n")
print(car::linearHypothesis(f, "tri::4:z_ant = tri::1:z_ant"))
cat("\nOK\n")
