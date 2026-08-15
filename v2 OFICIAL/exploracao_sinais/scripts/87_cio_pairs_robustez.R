# =============================================================================
# 87_cio_pairs_robustez.R  (exploracao_sinais, agente ML/pairs)
#
# Robustez do candidato de pairs trading (script 84). O script 84 achou,
# em 24 especificacoes (2 cortes de CIO x 2 janelas K x 2 horizontes h x 3
# testes), NENHUMA evidencia de REVERSAO -- o unico resultado perto de
# significativo (p=0,034, K=6 h=1 top5pct) tinha SINAL NEGATIVO pro
# "pair-trade de reversao" (comprar quem ficou pra tras, vender quem
# subiu), ou seja, sugeria o OPOSTO (continuacao/momentum entre gemeos),
# nao reversao -- o que e' teoricamente coerente com o CIO Peer Momentum
# (candidato #15, Ying 2024): a teoria e' de DIFUSAO/continuacao de
# informacao entre acoes com ownership em comum, nao de reversao.
#
# Duas checagens adicionais:
#   (R1) Restringir a pares "gemeos de verdade": CIO >= 0,7 (limiar
#        absoluto, nao so' top-percentil relativo) E ambos os tickers com
#        cobertura institucional decente (n_fundos >= 20 em fit_ativo_mes,
#        licao do script 38 -- evita que o resultado seja dominado por
#        pares envolvendo 1-2 fundos so').
#   (R2) Reportar explicitamente a hipotese OPOSTA (continuacao: comprar
#        quem JA subiu mais dentro do par, vender quem ficou pra tras) --
#        e' matematicamente o espelho do pair-trade de reversao (mesma
#        |t|, sinal oposto), mas reportado aqui de forma direta e nomeada,
#        porque e' a leitura mais provavel dos dados do script 84.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setorder(precos, ticker, ymk)
precos[, log_ret := log(1 + retorno)]
for (K in c(3, 6)) precos[, paste0("cumret_", K) := exp(frollsum(log_ret, K, align = "right")) - 1, by = ticker]

# cobertura institucional (n_fundos) por ticker-mes, ja calculado na Etapa 1
fit_ativo <- fread(file.path(DATA, "fit_ativo_mes.csv"), select = c("ativo","ym","n_fundos"))
fit_ativo[, ticker := trimws(sub(".*- ", "", ativo))]
cobertura <- unique(fit_ativo[, .(ticker, ym, n_fundos)])

pares_top5 <- fread(file.path(OUT, "pairs_cio_top5pct.csv"))
cat("Pares brutos (top5pct, do script 84):", nrow(pares_top5), "\n")

# ---- filtro (R1): CIO absoluto >= 0,7 E ambos os tickers com n_fundos>=20 --
pares_filtrado <- merge(pares_top5, cobertura, by.x = c("i","ym"), by.y = c("ticker","ym"))
setnames(pares_filtrado, "n_fundos", "n_fundos_i")
pares_filtrado <- merge(pares_filtrado, cobertura, by.x = c("j","ym"), by.y = c("ticker","ym"))
setnames(pares_filtrado, "n_fundos", "n_fundos_j")
pares_filtrado <- pares_filtrado[cio >= 0.7 & n_fundos_i >= 20 & n_fundos_j >= 20]
cat("Pares depois do filtro (CIO>=0.7 absoluto, n_fundos>=20 ambos os lados):", nrow(pares_filtrado), "\n")
n_pares_mes_filt <- pares_filtrado[, .N, by = ym]
cat("Pares/mes (filtrado): min=", min(n_pares_mes_filt$N), " mediana=", median(n_pares_mes_filt$N),
    " max=", max(n_pares_mes_filt$N), "\n")
fwrite(pares_filtrado, file.path(OUT, "pairs_cio_liquido_absoluto.csv"))

montar_base <- function(pares_base, K, h) {
  cr_col <- paste0("cumret_", K)
  cr <- precos[, c("ticker","ymk",cr_col), with = FALSE]; setnames(cr, cr_col, "cumret")
  base <- copy(pares_base)
  base <- merge(base, cr, by.x = c("i","ym"), by.y = c("ticker","ymk")); setnames(base, "cumret", "cumret_i")
  base <- merge(base, cr, by.x = c("j","ym"), by.y = c("ticker","ymk")); setnames(base, "cumret", "cumret_j")
  base <- base[is.finite(cumret_i) & is.finite(cumret_j)]
  base[, spread := cumret_i - cumret_j]
  ret_fwd <- precos[, .(ticker, ym_alvo = addm(ymk, -h), ret_fwd = retorno)]
  base <- merge(base, ret_fwd, by.x = c("i","ym"), by.y = c("ticker","ym_alvo")); setnames(base, "ret_fwd", "ret_i_fwd")
  base <- merge(base, ret_fwd, by.x = c("j","ym"), by.y = c("ticker","ym_alvo")); setnames(base, "ret_fwd", "ret_j_fwd")
  base <- base[is.finite(ret_i_fwd) & is.finite(ret_j_fwd)]
  base[, diff_fwd := ret_i_fwd - ret_j_fwd]
  base[, pair_trade_reversao   := -sign(spread) * diff_fwd]   # long perdedor, short vencedor
  base[, pair_trade_continuacao := sign(spread) * diff_fwd]   # long vencedor, short perdedor (espelho)
  base
}

fm_portfolio <- function(base, coluna, nome, K, h) {
  teste <- base[ym >= CORTE]
  if (nrow(teste) < 30) return(NULL)
  por_mes <- teste[, .(ret_medio = mean(get(coluna)), n = .N), by = ym]
  por_mes <- por_mes[n >= 5]
  n_meses <- nrow(por_mes)
  if (n_meses < 6) return(NULL)
  media <- mean(por_mes$ret_medio); dp <- sd(por_mes$ret_medio)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  cat(sprintf("[%s] K=%d h=%d | %2d meses | ret_medio=%+7.3f%%/mes (anualizado~%+6.1f%%) | t=%6.2f | p=%.4f | N_par_mes(min/mediana)=%d/%.0f\n",
              nome, K, h, n_meses, 100*media, 100*((1+media)^12-1), t_fm, p_fm, min(por_mes$n), median(por_mes$n)))
  data.table(spec = nome, K = K, h = h, n_meses = n_meses, ret_medio_pct = 100*media, t_fm = t_fm, p_fm = p_fm,
             n_par_mes_min = min(por_mes$n), n_par_mes_mediana = median(por_mes$n))
}

resultados <- list()
cat("\n===== (R1) Universo liquido, CIO>=0,7 absoluto =====\n")
for (K in c(3, 6)) {
  for (h in c(1, 3)) {
    base <- montar_base(pares_filtrado, K, h)
    r_rev <- fm_portfolio(base, "pair_trade_reversao", "R1_liquido_reversao", K, h)
    r_con <- fm_portfolio(base, "pair_trade_continuacao", "R1_liquido_continuacao", K, h)
    resultados <- c(resultados, list(r_rev, r_con))
  }
}

# ---- (R2) hipotese de continuacao no universo COMPLETO (top5pct, sem filtro
#      de liquidez) -- espelho exato do resultado do script 84, reportado
#      aqui de forma nomeada e direta -----------------------------------------
cat("\n===== (R2) Hipotese de CONTINUACAO (espelho do script 84), universo completo top5pct =====\n")
for (K in c(3, 6)) {
  for (h in c(1, 3)) {
    base <- montar_base(pares_top5, K, h)
    r_con <- fm_portfolio(base, "pair_trade_continuacao", "R2_completo_continuacao", K, h)
    resultados <- c(resultados, list(r_con))
  }
}

RES <- rbindlist(Filter(Negate(is.null), resultados))
fwrite(RES, file.path(OUT, "candidatos_87_cio_pairs_robustez.csv"))
cat("\n\n===== RESUMO ORDENADO POR p_fm =====\n")
print(RES[order(p_fm)])
cat("\nOK\n")
