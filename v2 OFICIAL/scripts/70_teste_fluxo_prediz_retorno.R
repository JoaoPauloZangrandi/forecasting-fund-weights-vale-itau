# =============================================================================
# 70_teste_fluxo_prediz_retorno.R  (v2 OFICIAL -- teste_estrategia)
#
# TESTE CRITICO pedido pelo Joao antes de falar em long/short: o fluxo
# liquido previsto (Fase 5, "eco do replicante") em ATIVO-MES t prediz o
# RETORNO do ativo entre t e t+1? Ate agora so validamos que o modelo
# preve MUDANCA DE PESO dos fundos -- nunca testamos se isso se traduz em
# retorno de preco (a premissa implicita de qualquer estrategia de trade
# em cima disso).
#
# Duas versoes do teste, lado a lado:
#  (a) regressao simples: retorno(t+1) ~ fluxo(t)
#  (b) quintis: separa ativo-mes em 5 grupos pelo tamanho do fluxo previsto,
#      olha retorno medio por grupo (mais robusto a outlier que so' a
#      regressao, padrao classico de teste de sinal em financas)
# Fluxo normalizado por VALOR TOTAL da posicao naquele ativo-mes no nosso
# universo (nao R$ bruto) -- senao o teste so' pega "ativo grande tem fluxo
# grande", nao poder preditivo de verdade.
#
# Compara tambem contra o sinal SEM o eco do lider (so' lambda*d) -- pra
# saber se o "eco do replicante" adiciona algo, ou se e' so ruido.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

nm <- fread(file.path(REPO, "v2 OFICIAL/data/sinal_robo_net_mercado.csv"))
nm[, ticker := trimws(sub(".*- ", "", ativo))]
cat("Ativo-mes com sinal (fluxo liquido):", nrow(nm), "\n")

# ---- valor total da posicao naquele ativo-mes, pra normalizar o fluxo -----
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"))
valor_total <- pp[, .(valor_total_mil = sum(valor_mil)), by = .(ativo, ano, mes)]
valor_total[, ym := ano*100L + mes]
nm <- merge(nm, valor_total[, .(ativo, ym, valor_total_mil)], by = c("ativo","ym"), all.x = TRUE)
nm[, fluxo_pct := fluxo_liquido_reais / (valor_total_mil/1000)]  # valor_mil em milhares -> R$; fluxo em R$
nm[, fluxo_normal_pct := fluxo_liquido_normal / (valor_total_mil/1000)]
cat("Com valor_total disponivel:", nm[!is.na(fluxo_pct) & is.finite(fluxo_pct), .N], "de", nrow(nm), "\n")

# ---- retorno em t+1 -----------------------------------------------------
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
nm[, ym_fut := addm(ym, 1L)]
precos <- fread(file.path(REPO, "v2 OFICIAL/data/precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setnames(precos, c("ymk","retorno"), c("ym_fut","retorno_fut"))
M <- merge(nm, precos, by = c("ticker","ym_fut"), all.x = TRUE)
M <- M[is.finite(fluxo_pct) & !is.na(retorno_fut)]
cat("\nObs finais (fluxo,retorno_fut) validas:", nrow(M), "\n")

cat("\n===== (a) Regressao simples: retorno(t+1) ~ fluxo(t) =====\n")
fit_eco <- lm(retorno_fut ~ fluxo_pct, data = M)
print(summary(fit_eco)$coefficients)
cat("R2:", round(summary(fit_eco)$r.squared, 5), "\n")

fit_normal <- lm(retorno_fut ~ fluxo_normal_pct, data = M)
cat("\n--- comparacao: sinal SEM eco do lider (so' lambda*d) ---\n")
print(summary(fit_normal)$coefficients)
cat("R2:", round(summary(fit_normal)$r.squared, 5), "\n")

cat("\n===== (b) Quintis por fluxo_pct, retorno medio subsequente =====\n")
M[, quintil := cut(fluxo_pct, quantile(fluxo_pct, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE)]
por_quintil <- M[, .(n = .N, fluxo_pct_medio = mean(fluxo_pct), retorno_medio = mean(retorno_fut),
                      retorno_mediano = median(retorno_fut)), by = quintil]
setorder(por_quintil, quintil)
print(por_quintil)

cat("\nDiferenca quintil 5 (mais comprado) - quintil 1 (mais vendido), retorno medio:",
    round(por_quintil[quintil==5]$retorno_medio - por_quintil[quintil==1]$retorno_medio, 5), "\n")

t_q5_q1 <- t.test(M[quintil==5]$retorno_fut, M[quintil==1]$retorno_fut)
cat("Teste t (quintil5 vs quintil1): diferenca =", round(diff(-t_q5_q1$estimate),5),
    "| p-valor =", signif(t_q5_q1$p.value,3), "\n")

fwrite(M, file.path(REPO, "v2 OFICIAL/data/teste_fluxo_prediz_retorno.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/teste_fluxo_prediz_retorno.csv'\n")
