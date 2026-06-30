# =============================================================================
# 25_fractional_logit.R   — PASSO 2 da agenda do orientador
#
# Reestima a cross-section do PESO com a LIGACAO LOGISTICA (resposta fracionaria,
# Papke-Wooldridge 1996) e compara com a versao LINEAR (OLS) do passo 1.
#   w_{i,t} = g(x' theta) + e ,   g(z) = 1/(1+e^-z)   (em R: quasibinomial/logit)
#
# Saidas:
#   (A) coefs Fama-MacBeth do LOGIT (escala log-odds) + Newey-West -> sinais/signif
#   (B) EFEITOS PARCIAIS MEDIOS (APE) do logit, em pontos de peso, vs coefs lineares
#   (C) quantas previsoes do LINEAR caem FORA de [0,1] (motivo de usar o logit)
# As caracteristicas de escala entram padronizadas (z), como no passo 1.
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
d[, z_aum := (l_aum - mean(l_aum)) / sd(l_aum)]
d[, z_cot := (l_cot - mean(l_cot)) / sd(l_cot)]
cat("Obs:", nrow(d), "| meses:", uniqueN(d$ym), "| fundos:", uniqueN(d$cod_fundo), "\n")
cat("Peso: min", round(min(d$peso_vale3),4), "| max", round(max(d$peso_vale3),4),
    "| zeros:", sum(d$peso_vale3 == 0), "\n\n")

meses <- sort(unique(d$ym))
vars  <- c("(Intercept)","z_aum","z_cot","is_fic","flow_aum")
rot   <- c("Intercepto","ln(patrimonio)[dp]","ln(cotistas)[dp]","Fundo de cotas","Captacao/patrim")

lin_c <- logit_c <- ape_m <- vector("list", length(meses))
n_oob_lin <- 0L; n_tot <- 0L

for (k in seq_along(meses)) {
  dm <- d[ym == meses[k]]
  # --- LINEAR (OLS) ---
  fl  <- lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum, data = dm)
  lin_c[[k]] <- as.data.table(as.list(coef(fl)))
  pl  <- fitted(fl); n_oob_lin <- n_oob_lin + sum(pl < 0 | pl > 1); n_tot <- n_tot + length(pl)
  # --- LOGIT fracionario (quasibinomial) ---
  fg  <- suppressWarnings(glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum,
                              family = quasibinomial(link = "logit"), data = dm))
  cg  <- coef(fg); logit_c[[k]] <- as.data.table(as.list(cg))
  # efeito parcial medio: APE_k = beta_k * mean(p*(1-p))
  p   <- fitted(fg); s <- mean(p * (1 - p))
  ape <- cg[c("z_aum","z_cot","is_fic","flow_aum")] * s
  ape_m[[k]] <- as.data.table(as.list(ape))
}
lin_c <- rbindlist(lin_c); setnames(lin_c, vars)
logit_c <- rbindlist(logit_c); setnames(logit_c, vars)
ape_m <- rbindlist(ape_m)

# Newey-West (HAC) para a media de uma serie de 72 coeficientes
nw_se <- function(x, L = 3) {
  T <- length(x); u <- x - mean(x); g0 <- sum(u^2); acc <- 0
  for (l in 1:L) acc <- acc + (1 - l/(L+1)) * sum(u[(l+1):T]*u[1:(T-l)])
  sqrt((g0 + 2*acc) / T^2)
}
fm <- function(M, labels) rbindlist(lapply(seq_along(M), function(j){
  x <- M[[j]]; m <- mean(x); se <- nw_se(x); tt <- m/se
  data.table(variavel = labels[j], media = m, se_nw = se, t_nw = tt,
             p_nw = 2*pt(-abs(tt), df = length(x)-1), meses_pos = sum(x > 0))
}))

cat("===== (A) LOGIT fracionario — coefs Fama-MacBeth (escala log-odds) + Newey-West =====\n")
print(fm(logit_c, rot)[, lapply(.SD, function(z) if(is.numeric(z)) round(z,5) else z)])

cat("\n===== (B) COMPARACAO em pontos de peso: coef LINEAR vs EFEITO PARCIAL MEDIO do LOGIT =====\n")
lin_fm <- sapply(lin_c[, .(z_aum,z_cot,is_fic,flow_aum)], mean)
ape_fm <- sapply(ape_m, mean)
cmp <- data.table(variavel = c("ln(patrimonio)[dp]","ln(cotistas)[dp]","Fundo de cotas","Captacao/patrim"),
                  linear = as.numeric(lin_fm), ape_logit = as.numeric(ape_fm))
cmp[, dif := ape_logit - linear]
print(cmp[, lapply(.SD, function(z) if(is.numeric(z)) round(z,6) else z)])

cat("\n===== (C) PREVISOES FORA DE [0,1] =====\n")
cat("Linear (OLS): ", n_oob_lin, "de", n_tot, "previsoes fora de [0,1] (",
    round(100*n_oob_lin/n_tot,2), "% )\n", sep="")
cat("Logit       : 0 por construcao (g(.) sempre em (0,1))\n")

cat("\n===== (D) AJUSTE: pseudo-R2 medio =====\n")
# R2 linear medio (ja sabido ~0.152); pseudo-R2 do logit via correlacao^2 obs-prev por mes
r2lin <- mean(sapply(meses, function(mm){ dm<-d[ym==mm]; summary(lm(peso_vale3~z_aum+z_cot+is_fic+flow_aum,dm))$r.squared }))
r2log <- mean(sapply(meses, function(mm){ dm<-d[ym==mm]
  fg<-suppressWarnings(glm(peso_vale3~z_aum+z_cot+is_fic+flow_aum,family=quasibinomial(),data=dm))
  cor(dm$peso_vale3, fitted(fg))^2 }))
cat("Linear  R2 medio/mes:", round(r2lin,4), "\n")
cat("Logit   pseudo-R2 (cor^2 obs-prev) medio/mes:", round(r2log,4), "\n")

fwrite(fm(logit_c, rot), file.path(REPO, "data/processed/reg25_logit_fm.csv"))
fwrite(cmp, file.path(REPO, "data/processed/reg25_ape_vs_linear.csv"))
cat("\nOK - salvo em data/processed/reg25_*.csv\n")
