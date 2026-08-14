# =============================================================================
# 04_long_short_fama_macbeth.R  (exploracao_sinais)
#
# O script 03 testou long-short por quintil com t.test padrao, tratando
# cada par ativo-mes como observacao independente -- ERRO: retornos de
# ativos no MESMO mes sao correlacionados (fator de mercado comum), entao
# o erro-padrao fica artificialmente pequeno e a significancia, inflada.
# Prova de que algo estava errado: FIT h=0/h=1 deram p=1e-20/1e-22 no
# long-short, mas R2~0% na regressao linear (script 01) -- inconsistente
# demais pra ser sinal real.
#
# Este script refaz com Fama-MacBeth de verdade: pra cada MES do periodo de
# teste, calcula o spread Q5-Q1 (UM numero por mes, breakpoints fixos do
# treino). Testa se a MEDIA da serie temporal de spreads mensais e' > 0,
# usando o erro-padrao da propria serie temporal (T meses, nao N ativo-mes)
# -- e' o jeito certo de fazer isso, corrige a dependencia cross-sectional
# automaticamente.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

fama_macbeth <- function(sinal_dt, var_sinal, nome, h) {
  m <- copy(sinal_dt)
  m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(get(var_sinal))]
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 100 || nrow(teste) < 50) { cat(sprintf("%s h=%d: amostra pequena demais\n", nome, h)); return(NULL) }

  breaks <- quantile(treino[[var_sinal]], seq(0,1,0.2), na.rm = TRUE)
  breaks[1] <- -Inf; breaks[6] <- Inf
  teste[, quintil := cut(get(var_sinal), breaks, labels = FALSE, include.lowest = TRUE)]
  teste <- teste[!is.na(quintil)]

  # UM spread por MES (nao por ativo-mes)
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(retorno)), by = .(ym, quintil)]
  spreads_mensais <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  setnames(spreads_mensais, c("1","5"), c("q1","q5"))
  spreads_mensais <- spreads_mensais[is.finite(q1) & is.finite(q5)]
  spreads_mensais[, spread := q5 - q1]

  n_meses <- nrow(spreads_mensais)
  if (n_meses < 6) { cat(sprintf("%s h=%d: poucos meses de teste (%d), pulando\n", nome, h, n_meses)); return(NULL) }
  media <- mean(spreads_mensais$spread)
  dp <- sd(spreads_mensais$spread)
  t_fm <- media / (dp / sqrt(n_meses))
  p_fm <- 2 * pt(-abs(t_fm), df = n_meses - 1)
  pct_meses_positivo <- 100*mean(spreads_mensais$spread > 0)

  cat(sprintf("%-30s h=%2d | %2d meses | spread medio=%+7.4f (%.2f p.p./mes) | t(FM)=%6.2f | p(FM)=%.4f | %% meses positivo=%.0f%%\n",
              nome, h, n_meses, media, 100*media, t_fm, p_fm, pct_meses_positivo))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media,
             t_fm = t_fm, p_fm = p_fm, pct_meses_positivo = pct_meses_positivo,
             spread_medio_anualizado_pp = 100*((1+media)^12 - 1))
}

resultados <- list()

# FIT
fit_completo <- fread(file.path(DATA, "fit_ativo_mes.csv"))
fit_completo[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(0,1,3,6,12)) resultados[[length(resultados)+1]] <- fama_macbeth(fit_completo, "FIT", "FIT (Lou)", h)

# Demanda agregada revelada
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
valor_total <- pp[, .(valor_total_mil = sum(valor_mil)), by = .(ativo, ym)]
vt_prev <- valor_total[, .(ativo, ym = addm(ym, 1), valor_total_prev = valor_total_mil)]
dem <- merge(valor_total, vt_prev, by = c("ativo","ym"))
dem <- dem[valor_total_prev > 0]
dem[, dem_pct := (valor_total_mil - valor_total_prev) / valor_total_prev]
q <- quantile(dem$dem_pct, c(0.01,0.99), na.rm=TRUE)
dem[, dem_pct_w := pmin(pmax(dem_pct, q[1]), q[2])]
dem[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(1,3,6,12)) resultados[[length(resultados)+1]] <- fama_macbeth(dem, "dem_pct_w", "Demanda agregada revelada", h)

# Herding
pp2 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso"))
peso_prev2b <- pp2[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d2 <- merge(pp2, peso_prev2b, by = c("cod_fundo","ativo","ym"))
d2 <- d2[is.finite(peso_prev)]
d2[, compra := peso > peso_prev]; d2[, venda := peso < peso_prev]
por_am <- d2[, .(n_fundos=.N, n_compra=sum(compra), n_venda=sum(venda)), by=.(ativo,ym)]
por_am <- por_am[n_fundos >= 10]
por_am[, p_compra := n_compra/(n_compra+n_venda)]
media_mes2 <- por_am[, .(p_compra_medio_mes = mean(p_compra, na.rm=TRUE)), by=ym]
por_am <- merge(por_am, media_mes2, by="ym")
por_am[, HM_sinal := p_compra - p_compra_medio_mes]
por_am[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(1,3,6)) resultados[[length(resultados)+1]] <- fama_macbeth(por_am, "HM_sinal", "Herding (LSV)", h)

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_04_fama_macbeth.csv"))
cat("\n\n===== RESUMO FAMA-MACBETH (ordenado por |t|) =====\n")
setorder(R, -abs(t_fm))
print(R)
cat("\nOK\n")
