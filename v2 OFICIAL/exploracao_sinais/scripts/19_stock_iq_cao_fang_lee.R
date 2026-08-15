# =============================================================================
# 19_stock_iq_cao_fang_lee.R  (exploracao_sinais)
#
# (V) STOCK IQ (Cao Fang & Lee 2026, Accounting & Finance) -- adaptacao
#     fiel com os dados disponiveis. DIFERENTE do candidato "lideres
#     ponderados por skill" ja testado (candidato #9): aqui o sinal e' o
#     DESVIO de cada fundo em relacao ao que ELE MESMO normalmente faz em
#     acoes do mesmo "segmento de estilo" (nao correlacao de residuo entre
#     PARES de fundos) -- isola stock-picking especifico de tilt de estilo
#     amplo. So' gestores habeis que concordam entre si (mesma decisao de
#     overweight) geram sinal; gestores nao-habeis so' adicionam ruido.
#
# Segmentos de estilo: tamanho (tercil, log valor_total_mil) x momentum
# (tercil, retorno 12m) -- 9 segmentos. NAO temos book-to-market (dado
# indisponivel no painel de holdings CVM) -- limitacao explicita registrada,
# igual ao proprio paper reconhece que usa 3 dimensoes (nos usamos 2).
#
# Stock_IQ_{i,t} = soma sobre fundos f que carregam i de
#   [ (peso_{f,i,t} - peso_medio_{f,segmento(i),t}) * skill_f ]
# ponderado por AUM do fundo (fundos maiores pesam mais na soma).
#
# skill_f = retorno acumulado 12m do fundo (skill_12m, mesma proxy ja
# usada no candidato #9 -- nao temos GVA/alfa formal, limitacao registrada).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

# ---- 1. segmentos de estilo: tamanho x momentum, por mes ----
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[ticker %in% unique(precos$ticker)]

tamanho <- pp[, .(valor_total_mil = sum(peso*aum_prev)), by = .(ticker, ym)]
tamanho[, tam_tercil := as.integer(cut(valor_total_mil, quantile(valor_total_mil, c(0,1/3,2/3,1)),
                                        labels = FALSE, include.lowest = TRUE)), by = ym]

mom <- copy(precos); setorder(mom, ticker, ymk)
mom[, log_ret := log(1+retorno)]
mom[, mom_12m := exp(frollsum(log_ret, 12, align="right")) - 1, by = ticker]
mom <- mom[!is.na(mom_12m), .(ticker, ym = ymk, mom_12m)]
mom[, mom_tercil := as.integer(cut(mom_12m, quantile(mom_12m, c(0,1/3,2/3,1), na.rm=TRUE),
                                    labels = FALSE, include.lowest = TRUE)), by = ym]

segmentos <- merge(tamanho[, .(ticker, ym, tam_tercil)], mom[, .(ticker, ym, mom_tercil)], by = c("ticker","ym"))
segmentos <- segmentos[is.finite(tam_tercil) & is.finite(mom_tercil)]
segmentos[, segmento := tam_tercil*10L + mom_tercil]  # 9 segmentos (11,12,13,21,...,33)
cat("Ativo-mes com segmento definido:", nrow(segmentos), "| segmentos distintos:", uniqueN(segmentos$segmento), "\n")

# ---- 2. skill do fundo (retorno acumulado 12m, ja calculado no candidato #9) ----
rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, skill_12m := exp(frollsum(log_ret, 12, align = "right")) - 1]
rf_skill <- rf[!is.na(skill_12m), .(cod_fundo, ym = ymk, skill_12m)]

# ---- 3. junta painel + segmento + skill ----
pp[, cod_fundo := as.character(cod_fundo)]
d <- merge(pp[, .(cod_fundo, ticker, ym, peso, aum_prev)], segmentos, by = c("ticker","ym"))
d <- merge(d, rf_skill, by = c("cod_fundo","ym"))
cat("Obs (fundo,acao,mes) com segmento+skill disponiveis:", nrow(d), "\n")

# ---- 4. peso medio do fundo NO SEGMENTO (excluindo a propria acao-alvo, pra evitar
#         circularidade -- mesma logica ja usada no HHI_resto do TCC) ----
d[, peso_soma_segmento := sum(peso), by = .(cod_fundo, ym, segmento)]
d[, n_segmento := .N, by = .(cod_fundo, ym, segmento)]
d <- d[n_segmento >= 2]  # precisa de pelo menos 1 outra acao no segmento pra "media excluindo a propria" fazer sentido
d[, peso_medio_resto_segmento := (peso_soma_segmento - peso) / (n_segmento - 1)]
d[, desvio := peso - peso_medio_resto_segmento]

cat("Obs com desvio calculavel (>=2 acoes do fundo no segmento):", nrow(d), "\n")

# ---- 5. Stock IQ: soma ponderada por AUM do fundo, do desvio*skill ----
d[, contrib := aum_prev * desvio * skill_12m]
stock_iq <- d[, .(stock_iq = sum(contrib) / sum(aum_prev), n_fundos = .N), by = .(ticker, ym)]
stock_iq <- stock_iq[n_fundos >= 10]
cat("Ativo-mes com Stock IQ calculavel (>=10 fundos):", nrow(stock_iq), "\n")
fwrite(stock_iq, file.path(OUT, "stock_iq.csv"))

# =============================================================================
# TESTE: Stock IQ prediz retorno futuro, h=1 a h=4 (igual ao paper original)
# =============================================================================
teste_oos_linear <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- m[ym >= CORTE]
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  form <- as.formula(paste(var_y, "~", var_x))
  fit_lm <- lm(form, data = treino)
  alpha <- coef(fit_lm)[1]; beta <- coef(fit_lm)[2]
  pred <- alpha + beta * teste[[var_x]]
  sse_m <- sum((teste[[var_y]] - pred)^2); sse_n <- sum((teste[[var_y]] - mean(treino[[var_y]]))^2)
  r2_oos <- 1 - sse_m/sse_n
  s_full <- summary(lm(form, data = m))$coefficients
  cat(sprintf("%-15s h=%d [linear] n_tr=%5d n_te=%4d beta=%9.5f R2_OOS=%8.4f%% p_full=%.4f\n",
              nome, h, nrow(treino), nrow(teste), beta, 100*r2_oos, s_full[2,4]))
  data.table(sinal = nome, horizonte = h, r2_oos_pct = 100*r2_oos, p_full = s_full[2,4])
}
fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 50 || nrow(teste) < 30) return(NULL)
  breaks <- unique(quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) return(NULL)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE))]
  n_grupos <- length(breaks) - 1
  if (n_grupos < 5) teste[, quintil := ifelse(quintil == 1, 1, ifelse(quintil == n_grupos, 5, quintil))]
  teste <- teste[!is.na(quintil)]
  n_por_mes <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  cat(sprintf("   [N por quintil-mes] min=%d mediana=%.0f max=%d\n", min(n_por_mes$N), median(n_por_mes$N), max(n_por_mes$N)))
  por_mes <- teste[quintil %in% c(1,5), .(retorno_medio = mean(get(var_y))), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "retorno_medio")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5-q1]; n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df=n_meses-1)
  cat(sprintf("%-15s h=%d [FM] %2d meses spread=%+7.2fpp/mes t=%6.2f p=%.4f\n", nome, h, n_meses, 100*media, t_fm, p_fm))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_pp = 100*media, t_fm = t_fm, p_fm = p_fm)
}

cat("\n===== STOCK IQ: teste fora da amostra, h=1 a h=4 (igual ao paper original) =====\n")
resultados_lin <- list(); resultados_fm <- list()
for (h in c(1,2,3,4,6,12)) {
  m <- copy(stock_iq); m[, ym_ret := addm(ym, h)]
  m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(stock_iq)]
  r1 <- teste_oos_linear(m, "stock_iq", "retorno", "Stock IQ", h)
  r2 <- fama_macbeth(m, "stock_iq", "retorno", "Stock IQ", h)
  if (!is.null(r1)) resultados_lin[[length(resultados_lin)+1]] <- r1
  if (!is.null(r2)) resultados_fm[[length(resultados_fm)+1]] <- r2
}

RL <- rbindlist(resultados_lin); RF <- rbindlist(resultados_fm)
fwrite(RL, file.path(OUT, "candidatos_19_linear.csv"))
fwrite(RF, file.path(OUT, "candidatos_19_fama_macbeth.csv"))
cat("\nOK\n")
