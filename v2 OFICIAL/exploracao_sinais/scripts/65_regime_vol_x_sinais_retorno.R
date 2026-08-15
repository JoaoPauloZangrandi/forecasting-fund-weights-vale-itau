# =============================================================================
# 65_regime_vol_x_sinais_retorno.R  (exploracao_sinais, agente "regime")
#
# FRENTE (A) do angulo deste agente: interacao entre regime de volatilidade
# PREVISTA (HHI de posse -- candidatos #26-31, achado mais solido de toda a
# exploracao: HHI prediz vol futura, sobretudo h=12) e sinais de RETORNO ja
# testados na exploracao. O script 41 (de outro agente paralelo) ja testou
# essa interacao para o CIO Peer Momentum especificamente (resultado nulo:
# melhor caso p=0.148 baseline h=1, nenhum split por HHI significativo).
# Este script cobre os sinais de retorno que AINDA NAO foram cruzados com
# HHI: Breadth of ownership, E[FIT] (fluxo esperado), Herding (LSV), e
# Reversao de curto prazo -- 4 sinais x [baseline, tercil-baixo-HHI,
# tercil-alto-HHI, mediana-baixo, mediana-alto] x h=1,3,6.
#
# Hipotese: em acoes muito voláteis (HHI alto -> vol futura prevista alta),
# ruido idiossincratico pode afogar sinais de retorno fracos; em acoes mais
# "calmas" (HHI baixo), o sinal pode aparecer mais limpo -- mesma logica
# testada pelo agente do script 41 para CIO, aqui estendida aos outros
# sinais de retorno da familia de holdings.
#
# METODOLOGIA (as 3 licoes inegociaveis, aplicadas identicamente ao script
# 41 para manter comparabilidade):
#   1) Fama-MacBeth de verdade: spread Q5-Q1 (ou extremo) calculado MES A
#      MES, testando a media da serie temporal de spreads mensais com o
#      erro-padrao da propria serie (nao pooled).
#   2) Quintil RE-CORTADO A CADA MES dentro do subgrupo de HHI (nunca
#      breaks fixos do treino aplicados no teste).
#   3) HHI (variavel de condicionamento) tambem re-cortada a cada mes
#      (by=ym), e o teste roda so em ym>=202001.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500  # mesma contagem corrida de ~450-500 especificacoes ja testadas

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# =============================================================================
# HHI de posse (crowding), identico ao script 41 -- variavel de CONDICIONAMENTO
# =============================================================================
pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"),
             select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos = .N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]
cat("Ticker-mes com HHI calculavel (n_fundos>=10):", nrow(crowd), "\n")

# =============================================================================
# SINAIS DE RETORNO (reconstrucao fiel dos scripts 02/05/08, isolada aqui
# para nao tocar nos arquivos originais)
# =============================================================================
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","ym","peso","aum_prev","flow_aum"))
pp[, cod_fundo := as.character(cod_fundo)]
pp[, ticker := trimws(sub(".*- ", "", ativo))]

# ---- (1) Breadth of ownership: variacao % do numero de fundos com posicao ----
cat("\n===== Construindo Breadth of ownership =====\n")
breadth <- pp[peso > 0, .(breadth = uniqueN(cod_fundo)), by = .(ativo, ym)]
br_prev <- breadth[, .(ativo, ym = addm(ym, 1), breadth_prev = breadth)]
br <- merge(breadth, br_prev, by = c("ativo","ym"))
br[, delta_breadth_pct := (breadth - breadth_prev) / breadth_prev]
q <- quantile(br$delta_breadth_pct, c(0.01,0.99), na.rm=TRUE)
br[, delta_breadth_w := pmin(pmax(delta_breadth_pct, q[1]), q[2])]
br[, ticker := trimws(sub(".*- ", "", ativo))]
sinal_breadth <- br[, .(ticker, ym, valor_sinal = delta_breadth_w)]

# ---- (2) Herding (LSV): proporcao de compradores relativa a media do mercado ----
cat("===== Construindo Herding (LSV) =====\n")
peso_prev <- pp[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d <- merge(pp[, .(cod_fundo, ativo, ym, peso)], peso_prev, by = c("cod_fundo","ativo","ym"))
d <- d[is.finite(peso_prev)]
d[, compra := peso > peso_prev]; d[, venda := peso < peso_prev]
por_ativo_mes <- d[, .(n_fundos_h = .N, n_compra = sum(compra), n_venda = sum(venda)), by = .(ativo, ym)]
por_ativo_mes <- por_ativo_mes[n_fundos_h >= 10]
por_ativo_mes[, p_compra := n_compra / (n_compra + n_venda)]
media_mes <- por_ativo_mes[, .(p_compra_medio_mes = mean(p_compra, na.rm=TRUE)), by = ym]
por_ativo_mes <- merge(por_ativo_mes, media_mes, by = "ym")
por_ativo_mes[, HM_sinal := p_compra - p_compra_medio_mes]
por_ativo_mes[, ticker := trimws(sub(".*- ", "", ativo))]
sinal_herding <- por_ativo_mes[, .(ticker, ym, valor_sinal = HM_sinal)]

# ---- (3) E[FIT]: fluxo esperado (previsto por skill_12m do fundo, so treino) ----
cat("===== Construindo E[FIT] (fluxo esperado) =====\n")
rf <- fread(file.path(DATA, "retorno_fundo_mensal.csv"))
rf[, cod_fundo := as.character(cod_fundo)]
setorder(rf, cod_fundo, ymk)
rf[, log_ret := log(1 + retorno_fundo)]
rf[, skill_12m := exp(frollsum(log_ret, 12, align = "right")) - 1]
rf_skill <- rf[!is.na(skill_12m), .(cod_fundo, ym = ymk, skill_12m)]

fluxo_fundo <- unique(pp[, .(cod_fundo, ym, flow_aum)])
fluxo_fundo <- fluxo_fundo[is.finite(flow_aum)]
skill_lag <- copy(rf_skill); skill_lag[, ym := addm(ym, 1)]
fluxo_fundo <- merge(fluxo_fundo, skill_lag, by = c("cod_fundo","ym"), all.x = TRUE)
fluxo_fundo <- fluxo_fundo[is.finite(skill_12m)]

treino_1a <- fluxo_fundo[ym < CORTE]
fit_1a <- lm(flow_aum ~ skill_12m, data = treino_1a)
cat("1o estagio (so treino): coef=", round(coef(fit_1a)[2],5), " R2=", round(summary(fit_1a)$r.squared,5), "\n")
fluxo_fundo[, flow_aum_esperado := predict(fit_1a, newdata = fluxo_fundo)]

peso_prev2 <- pp[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d2 <- merge(pp[, .(cod_fundo, ativo, ym, aum_prev)], peso_prev2, by = c("cod_fundo","ativo","ym"))
d2 <- merge(d2, fluxo_fundo[, .(cod_fundo, ym, flow_aum_esperado)], by = c("cod_fundo","ym"))
d2 <- d2[is.finite(peso_prev) & peso_prev > 0 & is.finite(aum_prev) & is.finite(flow_aum_esperado)]
d2[, peso_valor := peso_prev * aum_prev]
efit <- d2[, .(EFIT = sum(peso_valor * flow_aum_esperado) / sum(peso_valor), n_fundos_e = .N), by = .(ativo, ym)]
efit <- efit[n_fundos_e >= 10]
efit[, ticker := trimws(sub(".*- ", "", ativo))]
sinal_efit <- efit[, .(ticker, ym, valor_sinal = EFIT)]

# ---- (4) Reversao de curto prazo: retorno do PROPRIO mes, sinal invertido ----
cat("===== Construindo Reversao de curto prazo =====\n")
sinal_reversao <- precos[, .(ticker, ym = ymk, valor_sinal = -retorno)]
sinal_reversao <- sinal_reversao[is.finite(valor_sinal)]

sinais <- list(
  "Breadth of ownership"       = sinal_breadth,
  "Herding (LSV)"               = sinal_herding,
  "E[FIT] (fluxo esperado)"     = sinal_efit,
  "Reversao curto prazo"        = sinal_reversao
)

# =============================================================================
# TESTE: Fama-MacBeth com re-corte de quintil MES A MES (metodo B, lecao #36)
# =============================================================================
fama_macbeth_recut <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[is.finite(get(var_x)) & is.finite(get(var_y))])
  if (nrow(teste) < 30) return(NULL)
  teste[, quintil := {
    qs <- quantile(get(var_x), 0:5/5, na.rm = TRUE)
    if (length(unique(qs)) < 6) as.integer(NA) else as.integer(cut(get(var_x), qs, include.lowest = TRUE))
  }, by = ym]
  teste <- teste[!is.na(quintil)]
  chk <- teste[quintil %in% c(1,5), .N, by = .(ym, quintil)]
  n_min <- if (nrow(chk) > 0) min(chk$N) else 0
  por_mes <- teste[quintil %in% c(1,5), .(rm = mean(get(var_y)), n = .N), by = .(ym, quintil)]
  sm <- dcast(por_mes, ym ~ quintil, value.var = "rm")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1) & is.finite(q5)]
  sm[, spread := q5 - q1]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
  sh <- media/dp*sqrt(12)
  cat(sprintf("%-42s h=%d %2d meses n_min=%3d spread=%+7.3fpp/mes (%+7.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()

for (nome_sinal in names(sinais)) {
  base <- merge(sinais[[nome_sinal]], crowd[, .(ticker, ym, hhi_posse)], by = c("ticker","ym"))
  cat(sprintf("\n\n########## SINAL: %s (n=%d ticker-mes com HHI) ##########\n", nome_sinal, nrow(base)))

  for (h in c(1,3,6)) {
    m <- copy(base); m[, ym_ret := addm(ym, h)]
    m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
    m <- m[ym >= CORTE & is.finite(retorno)]
    if (nrow(m) < 30) next

    # baseline sem condicionar
    r0 <- fama_macbeth_recut(m, "valor_sinal", "retorno", paste0(nome_sinal, "|baseline"), h)
    if (!is.null(r0)) resultados[[length(resultados)+1]] <- r0

    # tercil de HHI re-cortado por mes
    m[, tercil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:3/3, na.rm = TRUE), include.lowest = TRUE)), by = ym]
    baixo_t <- m[tercil_hhi == 1]; alto_t <- m[tercil_hhi == 3]
    r1 <- fama_macbeth_recut(baixo_t, "valor_sinal", "retorno", paste0(nome_sinal, "|BAIXO-HHI(tercil)"), h)
    r2 <- fama_macbeth_recut(alto_t,  "valor_sinal", "retorno", paste0(nome_sinal, "|ALTO-HHI(tercil)"), h)
    if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
    if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2

    # mediana de HHI re-cortada por mes
    m[, mediana_hhi := median(hhi_posse), by = ym]
    baixo_m <- m[hhi_posse < mediana_hhi]; alto_m <- m[hhi_posse >= mediana_hhi]
    r3 <- fama_macbeth_recut(baixo_m, "valor_sinal", "retorno", paste0(nome_sinal, "|BAIXO-HHI(mediana)"), h)
    r4 <- fama_macbeth_recut(alto_m,  "valor_sinal", "retorno", paste0(nome_sinal, "|ALTO-HHI(mediana)"), h)
    if (!is.null(r3)) resultados[[length(resultados)+1]] <- r3
    if (!is.null(r4)) resultados[[length(resultados)+1]] <- r4
  }
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_65_regime_vol_x_sinais.csv"))
cat("\n\n===== RESUMO GERAL (ordenado por p, top 20) =====\n")
print(head(R[order(p_fm)], 20))
cat("\nOK\n")
