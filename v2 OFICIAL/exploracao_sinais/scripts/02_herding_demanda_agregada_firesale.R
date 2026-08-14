# =============================================================================
# 02_herding_demanda_agregada_firesale.R  (exploracao_sinais)
#
# 3 candidatos testados juntos, TODOS com teste fora da amostra desde o
# inicio (treino <2020-01, teste >=2020-01, beta estimado so no treino):
#
# (A) HERDING (Lakonishok-Shleifer-Vishny 1992 / Sias 2004): quando muitos
#     fundos compram (ou vendem) o mesmo ativo no mesmo mes, alem do que se
#     esperaria por acaso, isso prediz retorno FUTURO -- literatura sugere
#     efeito de curto prazo (continuacao) que REVERTE no medio prazo.
#
# (B) DEMANDA AGREGADA REVELADA: diferente do FIT (script 105, que e fluxo
#     MECANICO/induzido por captacao-resgate), aqui e o que os fundos
#     REALMENTE fizeram -- variacao do valor total (R$) mantido pelo
#     conjunto de fundos num ativo, mes a mes. Testa se "demanda revelada"
#     tem mais sinal que "pressao mecanica".
#
# (C) FIRE SALE ESPECIFICO (Coval-Stafford 2007): a literatura de fire sale
#     enfatiza que o efeito se concentra em EVENTOS EXTREMOS de resgate, nao
#     no fluxo medio (que ja foi testado e rejeitado via FIT). Testa so os
#     ativo-mes com pressao de venda mecanica extrema (fundo com resgate
#     forte E posicao concentrada no ativo).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"))
pp[, cod_fundo := as.character(cod_fundo)]
precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
rmse <- function(x) sqrt(mean(x^2))

teste_oos_generico <- function(m, var_x, var_y, nome) {
  # m precisa ter colunas: ym, [var_x], [var_y]
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 100 || nrow(teste) < 50) { cat(sprintf("%s: amostra pequena demais\n", nome)); return(NULL) }
  form <- as.formula(paste(var_y, "~", var_x))
  fit_lm <- lm(form, data = treino)
  alpha <- coef(fit_lm)[1]; beta <- coef(fit_lm)[2]
  pred <- alpha + beta * teste[[var_x]]
  erro_modelo <- teste[[var_y]] - pred
  erro_naive <- teste[[var_y]] - mean(treino[[var_y]])
  sse_m <- sum(erro_modelo^2); sse_n <- sum(erro_naive^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(form, data = m))$coefficients
  cat(sprintf("%-40s | n_treino=%6d n_teste=%5d | beta(treino)=%9.5f | R2_OOS=%8.4f%% | [amostra inteira: coef=%.5f t=%.3f p=%.4f]\n",
              nome, nrow(treino), nrow(teste), beta, 100*r2_oos, s_full[2,1], s_full[2,3], s_full[2,4]))
  data.table(sinal = nome, n_treino = nrow(treino), n_teste = nrow(teste), beta_treino = beta, r2_oos_pct = 100*r2_oos,
             coef_full = s_full[2,1], t_full = s_full[2,3], p_full = s_full[2,4])
}

resultados <- list()

# =============================================================================
# (A) HERDING -- LSV measure
# =============================================================================
cat("\n===== (A) HERDING (LSV) =====\n")
# peso defasado, pra saber se o fundo AUMENTOU ou DIMINUIU posicao no mes
peso_prev <- pp[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d <- merge(pp[, .(cod_fundo, ativo, ym, peso, gestora_grupo)], peso_prev, by = c("cod_fundo","ativo","ym"))
d <- d[is.finite(peso_prev)]
d[, compra := peso > peso_prev]
d[, venda  := peso < peso_prev]

por_ativo_mes <- d[, .(n_fundos = .N, n_compra = sum(compra), n_venda = sum(venda)), by = .(ativo, ym)]
por_ativo_mes <- por_ativo_mes[n_fundos >= 10]  # minimo de fundos pra medida fazer sentido
por_ativo_mes[, p_compra := n_compra / (n_compra + n_venda)]

# proporcao esperada de compradores naquele MES (media entre ativos), pra tirar o "clima" geral do mercado
media_mes <- por_ativo_mes[, .(p_compra_medio_mes = mean(p_compra, na.rm=TRUE)), by = ym]
por_ativo_mes <- merge(por_ativo_mes, media_mes, by = "ym")
por_ativo_mes[, HM_sinal := p_compra - p_compra_medio_mes]  # >0: mais comprado que a media do mercado naquele mes; <0: mais vendido

por_ativo_mes[, ticker := trimws(sub(".*- ", "", ativo))]
for (h in c(1,3,6)) {
  m <- copy(por_ativo_mes)
  m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  r <- teste_oos_generico(m, "HM_sinal", "retorno", sprintf("Herding (LSV), h=%d", h))
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

# =============================================================================
# (B) DEMANDA AGREGADA REVELADA -- variacao do valor total mantido
# =============================================================================
cat("\n===== (B) DEMANDA AGREGADA REVELADA (delta valor_total) =====\n")
valor_total <- pp[, .(valor_total_mil = sum(valor_mil)), by = .(ativo, ym)]
vt_prev <- valor_total[, .(ativo, ym = addm(ym, 1), valor_total_prev = valor_total_mil)]
dem <- merge(valor_total, vt_prev, by = c("ativo","ym"))
dem <- dem[valor_total_prev > 0]
dem[, dem_pct := (valor_total_mil - valor_total_prev) / valor_total_prev]
dem[, ticker := trimws(sub(".*- ", "", ativo))]
# winsorizar dem_pct (denominador pequeno em ativo pouco carregado gera outlier extremo)
q <- quantile(dem$dem_pct, c(0.01,0.99), na.rm=TRUE)
dem[, dem_pct_w := pmin(pmax(dem_pct, q[1]), q[2])]

for (h in c(1,3,6)) {
  m <- copy(dem)
  m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno)]
  r <- teste_oos_generico(m, "dem_pct_w", "retorno", sprintf("Demanda agregada revelada, h=%d", h))
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

# =============================================================================
# (C) FIRE SALE ESPECIFICO -- so' pressao de venda MECANICA extrema
# =============================================================================
cat("\n===== (C) FIRE SALE ESPECIFICO (resgate extremo + posicao concentrada) =====\n")
fit_completo <- fread(file.path(DATA, "fit_ativo_mes.csv"))
fit_completo[, ticker := trimws(sub(".*- ", "", ativo))]
# fire sale classico: FIT muito NEGATIVO (pressao de venda) -- olhar so' o decil mais vendedor
corte_venda <- quantile(fit_completo$FIT, 0.05, na.rm = TRUE)  # 5% mais vendedor
extremos_venda <- fit_completo[FIT <= corte_venda]
cat("Ativo-mes com pressao de venda extrema (FIT <= P5 =", round(corte_venda,4), "):", nrow(extremos_venda), "\n")

for (h in c(1,3,6,12)) {
  m <- copy(extremos_venda)
  m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(FIT)]
  r <- teste_oos_generico(m, "FIT", "retorno", sprintf("Fire sale extremo (P5), h=%d", h))
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
  # tambem reporta o retorno MEDIO desses casos (fire sale preveria recuperacao = retorno medio positivo)
  cat(sprintf("   retorno medio apos venda extrema, h=%d: %.4f (n=%d)\n", h, mean(m$retorno, na.rm=TRUE), nrow(m)))
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_02_resumo.csv"))
cat("\n\n===== RESUMO GERAL =====\n")
print(R[, .(sinal, n_teste, r2_oos_pct = round(r2_oos_pct,3), p_full = round(p_full,4))])
cat("\nOK\n")
