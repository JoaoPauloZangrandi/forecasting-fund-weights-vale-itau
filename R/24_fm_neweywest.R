# =============================================================================
# 24_fm_neweywest.R   — PASSO 1 da agenda do orientador
#
# (a) Padroniza as caracteristicas de ESCALA ln(PL) e ln(cotistas) (z-score
#     global: centradas e divididas pelo desvio-padrao). FIC e o intercepto
#     ficam em escala original (o intercepto passa a recuperar o NIVEL MEDIO
#     do peso, como descreve o orientador).
# (b) Roda a cross-section MES A MES (72 meses) e resume Fama-MacBeth.
# (c) Erro-padrao em DUAS versoes:
#       - classico (sd/sqrt(T)), que pressupoe coefs serialmente independentes;
#       - Newey-West (HAC), que corrige a autocorrelacao dos coefs (0,7-0,8).
#
# Imprime tres blocos: (1) regressao ORIGINAL nao-padronizada + NW;
# (2) regressao PADRONIZADA (coefs por desvio-padrao) + NW; (3) o L usado.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num  := as.numeric(aum)]
d[, l_aum    := log(aum_num)]
d[, l_cot    := log(n_cotistas)]
d[, flow_aum := fluxo_liq / aum_num]
d[, ym := ano * 100L + mes]
d <- d[!is.na(flow_aum)]
cat("Obs (com fluxo):", nrow(d), "| meses:", uniqueN(d$ym), "| fundos:", uniqueN(d$cod_fundo), "\n")

# --- padronizacao GLOBAL (z-score) das caracteristicas de escala ---
d[, z_aum := (l_aum - mean(l_aum)) / sd(l_aum)]
d[, z_cot := (l_cot - mean(l_cot)) / sd(l_cot)]

# ---- roda as 72 cross-sections e devolve a matriz de coeficientes ----
run_fm <- function(formula, vars) {
  meses <- sort(unique(d$ym))
  M <- rbindlist(lapply(meses, function(mm) {
    dm <- d[ym == mm]
    cf <- coef(lm(formula, data = dm))
    as.data.table(as.list(cf))
  }))
  setnames(M, names(M), vars)
  M
}

# Newey-West (HAC) para a MEDIA de uma serie temporal de coeficientes.
# Var_NW(media) = (1/T^2)[ sum u_t^2 + 2 sum_{l=1}^L (1-l/(L+1)) sum_t u_t u_{t-l} ]
nw_se <- function(x, L) {
  T <- length(x); u <- x - mean(x)
  g0 <- sum(u^2)
  acc <- 0
  for (l in 1:L) {
    w <- 1 - l/(L+1)
    acc <- acc + w * sum(u[(l+1):T] * u[1:(T-l)])
  }
  sqrt((g0 + 2*acc) / T^2)
}

L <- floor(4 * (nrow(unique(d[, .(ym)])) / 100)^(2/9))   # regra automatica
Tm <- uniqueN(d$ym)
L <- floor(4 * (Tm/100)^(2/9))
cat("T =", Tm, "meses | defasagem Newey-West L =", L, "\n")

resumo <- function(M, rotulos) {
  rbindlist(lapply(seq_along(M), function(j) {
    x <- M[[j]]; T <- length(x); m <- mean(x)
    se_cl <- sd(x)/sqrt(T); se_nw <- nw_se(x, L)
    data.table(variavel = rotulos[j], media = m,
               se_classico = se_cl, t_classico = m/se_cl,
               se_nw = se_nw, t_nw = m/se_nw,
               p_nw = 2*pt(-abs(m/se_nw), df = T-1),
               meses_pos = sum(x > 0))
  }))
}

vars <- c("intercepto","l_aum","l_cot","is_fic","flow_aum")
rot  <- c("Intercepto","ln(patrimonio)","ln(cotistas)","Fundo de cotas","Captacao/patrimonio")

cat("\n========== (1) NAO-PADRONIZADA (coefs por unidade de log) ==========\n")
M1 <- run_fm(peso_vale3 ~ l_aum + l_cot + is_fic + flow_aum, vars)
r1 <- resumo(M1, rot)
print(r1[, lapply(.SD, function(z) if (is.numeric(z)) round(z,6) else z)])

cat("\n========== (2) PADRONIZADA: z(ln PL), z(ln cot) (coefs por DESVIO-PADRAO) ==========\n")
M2 <- run_fm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum, vars)
r2 <- resumo(M2, c("Intercepto","ln(patrimonio) [por dp]","ln(cotistas) [por dp]",
                   "Fundo de cotas","Captacao/patrimonio"))
print(r2[, lapply(.SD, function(z) if (is.numeric(z)) round(z,6) else z)])

cat("\nNota: razao se_nw/se_classico por variavel (efeito da correcao):\n")
print(data.table(variavel = rot, razao = round(r1$se_nw / r1$se_classico, 2)))

fwrite(r1, file.path(REPO, "data/processed/reg24_fm_nw_naopadr.csv"))
fwrite(r2, file.path(REPO, "data/processed/reg24_fm_nw_padr.csv"))
cat("\nOK - salvo em data/processed/reg24_fm_nw_*.csv\n")
