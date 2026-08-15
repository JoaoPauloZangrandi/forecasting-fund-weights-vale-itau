# =============================================================================
# 67_choque_posse_wardlaw_corrigido.R  (exploracao_sinais, agente "regime")
#
# FRENTE (B) do angulo deste agente: fluxo/posicao EXTREMA -- nao o NIVEL de
# posse institucional (ja testado varias vezes: HHI, breadth, ownership),
# mas a MUDANCA abrupta, com uma correcao metodologica que, ate onde a
# leitura do log mostra, NUNCA foi aplicada nesta exploracao: o candidato
# #2B ("demanda agregada revelada", script 02) testou a variacao percentual
# BRUTA do valor total (R$) detido pelos fundos numa acao --
#   dem_pct_t = (ValorTotal_t - ValorTotal_{t-1}) / ValorTotal_{t-1}
# -- mas essa variavel tem EXATAMENTE a contaminacao mecanica que Wardlaw
# (2020, JF) documenta na literatura padrao de flow-induced trading: se o
# preco da acao sobe 10% num mes e nenhum fundo compra ou vende 1 cota
# sequer, ValorTotal (que e' soma de peso*AUM, ou de valor_mil) sobe ~10%
# so' pela reavaliacao de preco -- nao e' "demanda revelada" nenhuma, e'
# so' o retorno do mes proprio disfarcado de sinal de holdings (o mesmo
# problema estrutural que a nota de Wardlaw no LOG_CANDIDATOS.md ja
# atribui, com razoavel confianca, como explicacao do porque o FIT/robo do
# TCC nao acha nada).
#
# CORRECAO (Flow_t, formula pedida explicitamente pelo Joao no prompt):
#   Flow_ativo_t = (ValorTotal_t - ValorTotal_{t-1}*(1+Retorno_ativo_t)) / ValorTotal_{t-1}
# Isola a parte do crescimento do valor total que NAO e' explicada pela
# valorizacao/desvalorizacao do proprio preco -- ou seja, aproxima
# "quantas cotas a mais/a menos os fundos, em conjunto, estao segurando",
# nao "quanto o valor de mercado da posicao mudou". Testamos as DUAS
# versoes lado a lado (bruta/contaminada vs corrigida) para deixar
# explicito o efeito da correcao -- diagnostico direto do mecanismo que
# Wardlaw descreve, nao so' uma alegacao teorica.
#
# Teste do sinal em PERCENTIS EXTREMOS (top 10% / bottom 10%, decis, como
# pedido explicitamente -- alem do quintil, ja padrao no resto da
# exploracao, para checar se o efeito e mais forte nos extremos mais
# extremos, como a hipotese de "smart money accumulation/distribution"
# sugeriria).
#
# METODOLOGIA (as 3 licoes inegociaveis):
#   1) Fama-MacBeth de verdade (media da serie de spreads mensais, erro-
#      padrao da propria serie).
#   2) Decis/quintis RE-CORTADOS A CADA MES (nunca breaks fixos do treino).
#   3) Teste roda em ym>=202001 (treino nao entra em nenhum corte usado
#      no teste).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/500

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# =============================================================================
# Construcao: valor total (R$) detido pelos fundos, por ticker-mes
# =============================================================================
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","valor_mil"))
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[peso > 0 & is.finite(valor_mil) & valor_mil > 0]

valor_tk <- pp[, .(valor_total = sum(valor_mil), n_fundos = uniqueN(cod_fundo)), by = .(ticker, ym)]
valor_tk <- valor_tk[n_fundos >= 10]
setorder(valor_tk, ticker, ym)

# valor_total_prev e retorno do PROPRIO mes ym (contemporaneo ao valor_total
# marcado no fim do mes ym -- mesma convencao usada em todo o resto da
# exploracao: retorno em ymk=X e' o retorno REALIZADO durante o mes X)
valor_tk[, valor_total_prev := shift(valor_total), by = ticker]
valor_tk <- merge(valor_tk, precos, by.x = c("ticker","ym"), by.y = c("ticker","ymk"), all.x = TRUE)
valor_tk <- valor_tk[is.finite(valor_total_prev) & valor_total_prev > 0 & is.finite(retorno)]

# --- versao BRUTA (contaminada, replica candidato #2B com filtro/janela identicos) ---
valor_tk[, choque_bruto := (valor_total - valor_total_prev) / valor_total_prev]

# --- versao CORRIGIDA (Wardlaw-safe): remove o componente mecanico do retorno ---
valor_tk[, choque_corrigido := (valor_total - valor_total_prev*(1+retorno)) / valor_total_prev]

# winsoriza 1%/99% as duas (mesma pratica do resto da exploracao, evita
# outlier de denominador pequeno dominar o resultado)
for (v in c("choque_bruto","choque_corrigido")) {
  q <- quantile(valor_tk[[v]], c(0.01,0.99), na.rm = TRUE)
  valor_tk[, (paste0(v,"_w")) := pmin(pmax(get(v), q[1]), q[2])]
}

cat("Ticker-mes com choque de posse calculavel (n_fundos>=10):", nrow(valor_tk), "\n")
cat("Correlacao choque_bruto x choque_corrigido:", round(cor(valor_tk$choque_bruto_w, valor_tk$choque_corrigido_w, use="complete.obs"),3), "\n")
cat("Correlacao choque_bruto x retorno proprio (deveria ser alta se Wardlaw estiver certo):",
    round(cor(valor_tk$choque_bruto_w, valor_tk$retorno, use="complete.obs"),3), "\n")
cat("Correlacao choque_corrigido x retorno proprio (deveria ser bem menor apos a correcao):",
    round(cor(valor_tk$choque_corrigido_w, valor_tk$retorno, use="complete.obs"),3), "\n")

# =============================================================================
# TESTE: Fama-MacBeth, re-corte MES A MES, quintil E decil (top10%/bottom10%)
# =============================================================================
fama_macbeth_grupo <- function(m, var_x, var_y, nome, h, n_grupos = 5) {
  teste <- copy(m[is.finite(get(var_x)) & is.finite(get(var_y))])
  if (nrow(teste) < 30) return(NULL)
  probs <- seq(0, 1, length.out = n_grupos + 1)
  teste[, grupo := {
    qs <- quantile(get(var_x), probs, na.rm = TRUE)
    if (length(unique(qs)) < (n_grupos + 1)) as.integer(NA) else as.integer(cut(get(var_x), qs, include.lowest = TRUE))
  }, by = ym]
  teste <- teste[!is.na(grupo)]
  g_baixo <- 1L; g_alto <- n_grupos
  chk <- teste[grupo %in% c(g_baixo, g_alto), .N, by = .(ym, grupo)]
  n_min <- if (nrow(chk) > 0) min(chk$N) else 0
  por_mes <- teste[grupo %in% c(g_baixo, g_alto), .(rm = mean(get(var_y)), n = .N), by = .(ym, grupo)]
  sm <- dcast(por_mes, ym ~ grupo, value.var = "rm")
  nomes_col <- as.character(c(g_baixo, g_alto))
  if (!all(nomes_col %in% names(sm))) return(NULL)
  setnames(sm, nomes_col, c("q_baixo","q_alto")); sm <- sm[is.finite(q_baixo) & is.finite(q_alto)]
  sm[, spread := q_alto - q_baixo]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df = nmes-1)
  sh <- media/dp*sqrt(12)
  cat(sprintf("%-38s h=%d [%dgrp] %2d meses n_min=%3d spread=%+7.3fpp/mes (%+7.1f%%/ano) Sharpe=%5.2f t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, n_grupos, nmes, n_min, 100*media, 100*((1+media)^12-1), sh, t_fm, p_fm,
              ifelse(p_fm < LIMIAR_BONFERRONI, "SIM", "nao")))
  data.table(sinal = nome, horizonte = h, n_grupos = n_grupos, n_meses = nmes, n_min_grupo_mes = n_min,
             spread_pp = 100*media, sharpe = sh, t_fm = t_fm, p_fm = p_fm, sig_bonferroni = p_fm < LIMIAR_BONFERRONI)
}

resultados <- list()
variaveis <- c("choque_bruto_w" = "Choque posse|BRUTO(contaminado)",
                "choque_corrigido_w" = "Choque posse|CORRIGIDO(Wardlaw-safe)")

for (var_x in names(variaveis)) {
  nome_base <- variaveis[[var_x]]
  cat(sprintf("\n\n########## %s ##########\n", nome_base))
  for (h in c(1,3,6)) {
    m <- copy(valor_tk); m[, ym_ret := addm(ym, h)]
    m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"),
               all.x = TRUE, suffixes = c("","_fut"))
    m <- m[ym >= CORTE & is.finite(retorno_fut)]

    r5 <- fama_macbeth_grupo(m, var_x, "retorno_fut", nome_base, h, n_grupos = 5)
    r10 <- fama_macbeth_grupo(m, var_x, "retorno_fut", nome_base, h, n_grupos = 10)
    if (!is.null(r5)) resultados[[length(resultados)+1]] <- r5
    if (!is.null(r10)) resultados[[length(resultados)+1]] <- r10
  }
}

# =============================================================================
# Diagnostico adicional: sinal 1 mes ANTES do crash de COVID (dez/2019) prediz
# queda desproporcional no crash (mesmo desenho do candidato #40, mas aqui
# testando o choque de posse recem-construido em vez de HHI)
# =============================================================================
cat("\n\n===== Diagnostico extra: choque de posse (dez/2019) x retorno no crash (fev+mar/2020) =====\n")
pre_crash <- valor_tk[ym == 201912, .(ticker, choque_bruto_w, choque_corrigido_w)]
crash_ret <- precos[ymk %in% c(202002, 202003), .(ret_crash = sum(log(1+retorno))), by = ticker]
crash_ret[, ret_crash := exp(ret_crash) - 1]
diag <- merge(pre_crash, crash_ret, by = "ticker")
cat("N ações com choque dez/2019 + retorno no crash:", nrow(diag), "\n")
for (v in c("choque_bruto_w","choque_corrigido_w")) {
  ct <- cor.test(diag[[v]], diag$ret_crash)
  cat(sprintf("  %-22s: corr=%.3f (p=%.4f, n=%d)\n", v, ct$estimate, ct$p.value, nrow(diag)))
}

R <- rbindlist(resultados, fill = TRUE)
fwrite(R, file.path(OUT, "candidatos_67_choque_posse_wardlaw.csv"))
cat("\n\n===== RESUMO GERAL (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
