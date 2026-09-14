suppressPackageStartupMessages({ library(data.table); library(fixest) })
DD <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau/v2 OFICIAL/data"
SC <- DD
setFixest_notes(FALSE)

Q <- fread(file.path(SC, "realoc_fundo_mes.csv")); Q[, cod_fundo := as.character(cod_fundo)]

# ---- janelas em TEMPO DE NEGOCIACAO (o dw do mes m e' negociado no mes m+1) ----
w <- function(meses, nm, ativa = TRUE) {
  col <- if (ativa) "ativa" else "passiva"
  z <- Q[mes_negoc %in% meses, .(v = weighted.mean(get(col), n), n = sum(n), nm_ = uniqueN(mes_negoc)),
         by = .(cod_fundo, ano = ano_negoc)]
  setnames(z, c("v","n","nm_"), paste0(c("v_","n_","m_"), nm)); z
}
# ---- desempenho mensal ----
P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"),
           select = c("cod_fundo","ym","ano","mes","retorno_fundo","retorno_ibov"))
P[, cod_fundo := as.character(cod_fundo)]
P <- P[is.finite(retorno_fundo) & is.finite(retorno_ibov)]
P[, nm_ano := .N, by = .(cod_fundo, ano)]
P <- P[nm_ano == 12L]                      # exige o ano civil completo
exc_w <- function(meses, nm) {
  z <- P[mes %in% meses, .(e = prod(1+retorno_fundo) - prod(1+retorno_ibov),
                           vol = sd(retorno_fundo), absr = mean(abs(retorno_fundo))),
         by = .(cod_fundo, ano)]
  setnames(z, c("e","vol","absr"), paste0(c("exc_","vol_","absr_"), nm)); z
}
D <- Reduce(function(a,b) merge(a,b,by=c("cod_fundo","ano")),
            list(w(1:6,"h1"), w(7:9,"q3"), w(10:12,"q4"),
                 exc_w(1:6,"h1"), exc_w(7:9,"q3"), exc_w(10:12,"q4"), exc_w(4:6,"q2")))
D <- D[m_h1 == 6L & m_q3 == 3L & m_q4 == 3L & n_h1 >= 10 & n_q4 >= 5]
D[, l_q4 := log(v_q4 / v_h1)]
D[, l_q3 := log(v_q3 / v_h1)]
D <- D[is.finite(l_q4) & is.finite(l_q3)]
cat("Fundo-ano (janela em tempo de negociacao):", nrow(D), "| fundos:", uniqueN(D$cod_fundo), "\n")
print(D[, .N, by = ano][order(ano)])

sh <- function(f, lbl) {
  cf <- coef(f); pv <- pvalue(f)
  cat(sprintf("%-46s", lbl))
  for (v in names(cf)) cat(sprintf(" | %s %+.3f (p=%.1e)", v, cf[v], pv[v]))
  cat(sprintf(" | n=%d\n", f$nobs))
}

cat("\n=========== (A) CALENDARIO CORRIGIDO ===========\n")
cat("Q4 = negociacao de OUT/NOV/DEZ (o desenho antigo pegava NOV/DEZ/JAN)\n")
cat("exc_q3 = retorno composto do 3o tri - Ibov no 3o tri (o antigo era diferenca de acumulados)\n\n")
sh(feols(l_q4 ~ exc_h1 | ano, D, cluster=~cod_fundo), "so' acumulado ate junho")
sh(feols(l_q4 ~ exc_q3 | ano, D, cluster=~cod_fundo), "so' 3o trimestre")
sh(feols(l_q4 ~ exc_h1 + exc_q3 | ano, D, cluster=~cod_fundo), "os dois")
sh(feols(l_q4 ~ exc_h1 + exc_q3 | ano + cod_fundo, D, cluster=~cod_fundo), "os dois + EF de fundo")

cat("\n=========== (B) O TESTE MECANICO: E O 4o TRI CONTEMPORANEO? ===========\n")
cat("Torneio = decisao tomada em outubro com a informacao de setembro.\n")
cat("Se o retorno do PROPRIO 4o tri explicar tanto quanto o do 3o, a medida\n")
cat("de realocacao esta' reagindo ao mercado, nao a uma decisao.\n\n")
sh(feols(l_q4 ~ exc_q4 | ano, D, cluster=~cod_fundo), "so' o 4o tri (contemporaneo)")
sh(feols(l_q4 ~ exc_q3 + exc_q4 | ano, D, cluster=~cod_fundo), "3o vs 4o, lado a lado")
sh(feols(l_q4 ~ exc_h1 + exc_q3 + exc_q4 | ano, D, cluster=~cod_fundo), "os tres")
sh(feols(l_q4 ~ exc_h1 + exc_q3 + exc_q4 + vol_q4 + absr_q4 | ano, D, cluster=~cod_fundo),
   "+ volatilidade do fundo no 4o tri")

cat("\n=========== (C) PLACEBO: O MESMO DESENHO NO 3o TRIMESTRE ===========\n")
cat("Nao ha' torneio em julho. Se o 2o tri prever a realocacao do 3o tri do\n")
cat("mesmo jeito, o padrao e' recencia geral, nao incentivo de fim de ano.\n\n")
sh(feols(l_q3 ~ exc_q2 | ano, D, cluster=~cod_fundo), "3o tri ~ 2o tri (placebo)")
sh(feols(l_q4 ~ exc_q3 | ano, D, cluster=~cod_fundo), "4o tri ~ 3o tri (o achado)")
sh(feols(l_q3 ~ exc_q2 + exc_q3 | ano, D, cluster=~cod_fundo), "placebo + contemporaneo")

cat("\n=========== (D) ANO A ANO, CALENDARIO CORRIGIDO ===========\n")
for (a in sort(unique(D$ano))) {
  f <- feols(l_q4 ~ exc_h1 + exc_q3 + exc_q4, D[ano==a], cluster=~cod_fundo)
  # os coeficientes tem de ser lidos por NOME: sem efeito fixo, coef(f)[1] e o
  # intercepto, e a leitura posicional desloca a tabela inteira em uma casa.
  cat(sprintf("%d (n=%4d): h1 %+.3f (p=%.2f) | q3 %+.3f (p=%.2f) | q4 %+.3f (p=%.2f)\n",
      a, sum(D$ano==a),
      coef(f)["exc_h1"], pvalue(f)["exc_h1"], coef(f)["exc_q3"], pvalue(f)["exc_q3"],
      coef(f)["exc_q4"], pvalue(f)["exc_q4"]))
}
fwrite(D, file.path(SC, "bateria_q4.csv"))
cat("\nOK\n")
