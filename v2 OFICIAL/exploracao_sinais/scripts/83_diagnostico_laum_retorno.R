# =============================================================================
# 83_diagnostico_laum_retorno.R  (exploracao_sinais, agente eventos)
#
# Diagnostico rapido do achado secundario de RETORNO do script 80:
# "tamanho medio dos fundos donos" (log AUM, frag_laum) prediz retorno
# NEGATIVO em h=1 (p=0.043) e h=3 (p=0.016) -- acoes com donos-fundos
# maiores tem retorno futuro MENOR. Suspeita: pode ser so o efeito-tamanho
# classico (small caps tem premio de retorno maior) disfarcado, ja que
# fundos grandes tendem a concentrar em blue chips. Testa controlando por
# log_valor_total (proxy de tamanho/liquidez da propria acao).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

frag <- fread(file.path(OUT, "candidatos_79_fragilidade_donos.csv"))
winsor <- function(x) { q <- quantile(x, c(0.01,0.99), na.rm=TRUE); pmin(pmax(x,q[1]),q[2]) }
frag[, frag_laum_w := winsor(frag_laum_medio)]

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(log_valor_total = log(sum(valor_posicao)), n_fundos_c=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos_c >= 10]

base <- merge(frag, crowd, by = c("ticker","ym"))
cat("Correlacao frag_laum_w x log_valor_total:", cor(base$frag_laum_w, base$log_valor_total, use="pairwise.complete.obs"), "\n\n")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
precos <- precos[is.finite(retorno)]

fm_multivariado <- function(m, alvo, vars_x, nome) {
  mm <- copy(m)
  for (v in vars_x) mm[, (paste0(v,"_z")) := scale(get(v)), by = ym]
  cols_z <- paste0(vars_x, "_z")
  mm <- mm[Reduce(`&`, lapply(cols_z, function(c) is.finite(mm[[c]]))) & is.finite(mm[[alvo]])]
  teste <- mm[ym >= CORTE]
  form <- as.formula(paste(alvo, "~", paste(cols_z, collapse=" + ")))
  meses <- sort(unique(teste$ym))
  coef_list <- list()
  for (mes in meses) {
    d <- teste[ym == mes]
    if (nrow(d) < 30) next
    fit <- tryCatch(lm(form, data = d), error = function(e) NULL)
    if (is.null(fit)) next
    coef_list[[length(coef_list)+1]] <- as.list(coef(fit))
  }
  if (length(coef_list) < 6) { cat(sprintf("%-30s: poucos meses (%d)\n", nome, length(coef_list))); return(invisible(NULL)) }
  CM <- rbindlist(coef_list, fill = TRUE)
  cat(sprintf("\n--- %s (alvo=%s, %d meses) ---\n", nome, alvo, nrow(CM)))
  for (cn in names(CM)) {
    x <- CM[[cn]]; x <- x[is.finite(x)]
    if (length(x) < 6) next
    med <- mean(x); dp <- sd(x); n <- length(x); t <- med/(dp/sqrt(n)); p <- 2*pt(-abs(t), df=n-1)
    cat(sprintf("  %-20s coef_medio=%+8.5f  t=%6.2f  p=%.5f\n", cn, med, t, p))
  }
}

for (h in c(1,3)) {
  m <- copy(base); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"))
  m <- m[is.finite(retorno)]
  fm_multivariado(m, "retorno", c("frag_laum_w","log_valor_total"), sprintf("laum + tam.posicao -> retorno h=%d", h))
  fm_multivariado(m, "retorno", c("frag_laum_w"), sprintf("laum sozinho -> retorno h=%d", h))
}
cat("\nOK\n")
