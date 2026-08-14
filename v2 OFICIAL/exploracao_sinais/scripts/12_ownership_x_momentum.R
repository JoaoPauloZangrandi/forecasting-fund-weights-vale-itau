# =============================================================================
# 12_ownership_x_momentum.R  (exploracao_sinais)
#
# (O) OWNERSHIP x MOMENTUM (interacao condicional, evidencia de China
#     A-shares 2023): em acoes com ALTA propriedade institucional, o efeito
#     momentum e' mais forte (menos ruido de retail distorcendo o sinal);
#     em BAIXA propriedade institucional, momentum mais fraco/ruidoso.
#     Diferente de tudo testado ate agora: nao e' "ownership prediz
#     retorno" isolado, e' "ownership MODULA a forca do momentum".
#
# Testa: retorno(t+h) ~ momentum_12m(t) + ownership_institucional(t) +
#        momentum_12m(t) * ownership_institucional(t)
# Hipotese: o termo de INTERACAO e' positivo (momentum funciona melhor
# quando ownership institucional e' alta).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

# momentum 12m por ativo
mom <- copy(precos); setorder(mom, ticker, ymk)
mom[, log_ret := log(1+retorno)]
mom[, mom_12m := exp(frollsum(log_ret, 12, align="right")) - 1, by = ticker]
mom <- mom[!is.na(mom_12m)]

# "ownership institucional": aqui, valor total (R$) mantido pelo nosso universo de fundos,
# normalizado (rank percentil por mes) -- proxy do quanto o ativo e' "institucionalizado"
# dentro do universo CVM de fundos de acoes (nao temos ownership TOTAL da empresa, so' o
# que os fundos do nosso painel carregam)
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
valor_total <- pp[, .(valor_total_mil = sum(valor_mil), n_fundos = uniqueN(ym)), by = .(ativo, ym)]
valor_total[, ticker := trimws(sub(".*- ", "", ativo))]
valor_total[, own_rank := frank(valor_total_mil) / .N, by = ym]  # 0 a 1, rank dentro do mes

d <- merge(mom[, .(ticker, ym = ymk, mom_12m)], valor_total[, .(ticker, ym, own_rank)], by = c("ticker","ym"))
cat("Ativo-mes com momentum + ownership rank disponiveis:", nrow(d), "\n")

d[, mom_x_own := mom_12m * own_rank]

teste_interacao <- function(m, h) {
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
  fit_lm <- lm(retorno ~ mom_12m + own_rank + mom_x_own, data = treino)
  s <- summary(fit_lm)$coefficients
  pred <- predict(fit_lm, newdata = teste)
  sse_m <- sum((teste$retorno - pred)^2); sse_n <- sum((teste$retorno - mean(treino$retorno))^2)
  r2_oos <- 1 - sse_m/sse_n
  cat(sprintf("h=%2d: n_tr=%5d n_te=%4d | R2_OOS=%7.4f%% | coef_interacao(treino)=%.5f\n",
              h, nrow(treino), nrow(teste), 100*r2_oos, coef(fit_lm)["mom_x_own"]))
  cat("  [amostra inteira] coeficientes:\n"); print(round(s, 5))
  data.table(horizonte = h, r2_oos_pct = 100*r2_oos, coef_interacao = coef(fit_lm)["mom_x_own"],
             p_interacao_full = s["mom_x_own","Pr(>|t|)"])
}

cat("===== (O) Momentum x Ownership institucional, interacao =====\n")
resultados <- list()
for (h in c(1,3,6,12)) {
  m <- copy(d); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(mom_12m) & is.finite(own_rank)]
  r <- teste_interacao(m, h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

# tambem: long-short SO' no terco de alta ownership vs SO' no terco de baixa ownership,
# comparando se momentum funciona melhor num grupo que no outro (Fama-MacBeth em cada grupo)
cat("\n===== Momentum (Q5-Q1) SEPARADO por tercil de ownership =====\n")
fama_macbeth_grupo <- function(m, grupo_label, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  breaks <- unique(quantile(treino$mom_12m, seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) return(NULL)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(mom_12m, breaks, labels = FALSE, include.lowest = TRUE))]
  n_grupos <- length(breaks) - 1
  if (n_grupos < 5) teste[, quintil := ifelse(quintil == 1, 1, ifelse(quintil == n_grupos, 5, quintil))]
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(retorno), n=.N), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5-q1]; n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df=n_meses-1)
  cat(sprintf("Momentum spread, ownership=%-6s h=%2d | %2d meses | spread=%+6.2fpp/mes | t=%5.2f | p=%.4f\n",
              grupo_label, h, n_meses, 100*media, t_fm, p_fm))
  data.table(grupo = grupo_label, horizonte = h, n_meses = n_meses, spread_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

resultados_grupo <- list()
for (h in c(1,3,6)) {
  m <- copy(d); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(mom_12m) & is.finite(own_rank)]
  alta <- m[own_rank >= 2/3]; baixa <- m[own_rank <= 1/3]
  r1 <- fama_macbeth_grupo(alta, "ALTA", h)
  r2 <- fama_macbeth_grupo(baixa, "BAIXA", h)
  if (!is.null(r1)) resultados_grupo[[length(resultados_grupo)+1]] <- r1
  if (!is.null(r2)) resultados_grupo[[length(resultados_grupo)+1]] <- r2
}

R1 <- rbindlist(resultados); R2 <- rbindlist(resultados_grupo)
fwrite(R1, file.path(OUT, "candidatos_12_interacao.csv"))
fwrite(R2, file.path(OUT, "candidatos_12_grupos.csv"))
cat("\nOK\n")
