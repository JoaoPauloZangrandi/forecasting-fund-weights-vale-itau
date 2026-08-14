# =============================================================================
# 03_lideres_e_long_short.R  (exploracao_sinais)
#
# (D) SINAL DOS LIDERES: a analise de replicantes ja identifica pares
#     lider->seguidor (script 66/67). Testa uma pergunta DIFERENTE da que o
#     TCC ja rejeitou: em vez de "o fluxo do SEGUIDOR (com eco do lider)
#     prediz retorno", testa "o que os LIDERES fazem primeiro prediz
#     retorno?" -- ideia de smart-money: se esses fundos sao 'lideres' e'
#     porque outros os seguem, talvez eles tenham informacao real.
#
# (E) LONG-SHORT POR QUINTIL, FORA DA AMOSTRA: os candidatos ja testados
#     (demanda revelada h=6, herding h=6, FIT h=0) tiveram R2 linear
#     baixo, mas R2 linear pode esconder uma relacao monotonica que so'
#     aparece comprando o quintil top e vendendo o quintil bottom. Testa
#     isso de verdade: breakpoints de quintil calculados SO no treino,
#     aplicados ao teste (sem vazamento), estrategia long-short.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

# =============================================================================
# (D) Sinal dos lideres
# =============================================================================
cat("===== (D) O QUE OS LIDERES FAZEM PREDIZ RETORNO? =====\n")
beta_tab <- fread(file.path(DATA, "pares_lideranca_beta.csv"))
lideres <- unique(beta_tab$lider)
cat("Fundos-lider distintos:", length(lideres), "\n")

M <- fread(file.path(DATA, "ajuste_parcial_universo_completo_h1.csv"))
M[, cod_fundo := as.character(cod_fundo)]
M_lideres <- M[cod_fundo %in% lideres]
cat("Obs (fundo,ativo,mes) dos lideres:", nrow(M_lideres), "\n")

# agrega por ativo-mes: quantos lideres estao comprando vs vendendo (d = peso_pred - peso, positivo = deveria comprar mais)
lam <- 0.0690
M_lideres[, u := dw_corrigido - lam * d]
agg_lid <- M_lideres[, .(u_medio_lideres = mean(u, na.rm=TRUE), n_lideres = .N), by = .(ativo, ym)]
agg_lid <- agg_lid[n_lideres >= 5]
agg_lid[, ticker := trimws(sub(".*- ", "", ativo))]

for (h in c(1,3,6)) {
  m <- copy(agg_lid)
  m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(u_medio_lideres)]
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 50 || nrow(teste) < 30) { cat(sprintf("h=%d: amostra pequena demais (treino=%d teste=%d)\n", h, nrow(treino), nrow(teste))); next }
  fit_lm <- lm(retorno ~ u_medio_lideres, data = treino)
  alpha <- coef(fit_lm)[1]; beta <- coef(fit_lm)[2]
  pred <- alpha + beta*teste$u_medio_lideres
  sse_m <- sum((teste$retorno - pred)^2); sse_n <- sum((teste$retorno - mean(treino$retorno))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(retorno ~ u_medio_lideres, data = m))$coefficients
  cat(sprintf("Sinal lideres, h=%d: n_treino=%d n_teste=%d | beta=%.5f | R2_OOS=%.4f%% | [full: coef=%.5f t=%.3f p=%.4f]\n",
              h, nrow(treino), nrow(teste), beta, 100*r2_oos, s_full[2,1], s_full[2,3], s_full[2,4]))
}

# =============================================================================
# (E) LONG-SHORT POR QUINTIL, fora da amostra, pros 3 melhores candidatos ja vistos
# =============================================================================
cat("\n\n===== (E) LONG-SHORT POR QUINTIL, FORA DA AMOSTRA =====\n")

testa_long_short <- function(sinal_dt, var_sinal, nome, h) {
  m <- copy(sinal_dt)
  m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(get(var_sinal))]
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 100 || nrow(teste) < 50) { cat(sprintf("%s h=%d: amostra pequena demais\n", nome, h)); return(NULL) }

  # breakpoints de quintil SO do treino
  breaks <- quantile(treino[[var_sinal]], seq(0,1,0.2), na.rm = TRUE)
  breaks[1] <- -Inf; breaks[6] <- Inf
  teste[, quintil := cut(get(var_sinal), breaks, labels = FALSE, include.lowest = TRUE)]
  teste <- teste[!is.na(quintil)]

  por_q <- teste[, .(retorno_medio = mean(retorno), n = .N), by = quintil]
  setorder(por_q, quintil)
  q5 <- por_q[quintil == 5]$retorno_medio; q1 <- por_q[quintil == 1]$retorno_medio
  if (length(q5) == 0 || length(q1) == 0) return(NULL)
  spread <- q5 - q1
  t_test <- t.test(teste[quintil==5]$retorno, teste[quintil==1]$retorno)
  cat(sprintf("%-35s h=%2d | long-short (Q5-Q1) OOS = %+7.4f (%.2f p.p.) | p=%.4f | n_teste=%d\n",
              nome, h, spread, 100*spread, t_test$p.value, nrow(teste)))
  data.table(sinal = nome, horizonte = h, spread_oos = spread, spread_pp = 100*spread, p_valor = t_test$p.value, n_teste = nrow(teste))
}

resultados_ls <- list()

# demanda agregada revelada
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("ativo","ym","valor_mil"))
valor_total <- pp[, .(valor_total_mil = sum(valor_mil)), by = .(ativo, ym)]
vt_prev <- valor_total[, .(ativo, ym = addm(ym, 1), valor_total_prev = valor_total_mil)]
dem <- merge(valor_total, vt_prev, by = c("ativo","ym"))
dem <- dem[valor_total_prev > 0]
dem[, dem_pct := (valor_total_mil - valor_total_prev) / valor_total_prev]
q <- quantile(dem$dem_pct, c(0.01,0.99), na.rm=TRUE)
dem[, dem_pct_w := pmin(pmax(dem_pct, q[1]), q[2])]
dem[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(1,3,6,12)) resultados_ls[[length(resultados_ls)+1]] <- testa_long_short(dem, "dem_pct_w", "Demanda agregada revelada", h)

# FIT (Lou)
fit_completo <- fread(file.path(DATA, "fit_ativo_mes.csv"))
fit_completo[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(0,1,6,12)) resultados_ls[[length(resultados_ls)+1]] <- testa_long_short(fit_completo, "FIT", "FIT (Lou)", h)

# herding
peso_prev2 <- pp2 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso"))
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
for (h in c(1,3,6)) resultados_ls[[length(resultados_ls)+1]] <- testa_long_short(por_am, "HM_sinal", "Herding (LSV)", h)

R <- rbindlist(resultados_ls)
fwrite(R, file.path(OUT, "candidatos_03_long_short_resumo.csv"))
cat("\n\n===== RESUMO LONG-SHORT (ordenado por spread) =====\n")
setorder(R, -spread_pp)
print(R)
cat("\nOK\n")
