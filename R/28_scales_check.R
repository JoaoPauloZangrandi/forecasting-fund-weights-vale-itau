# =============================================================================
# 28_scales_check.R — confere, com precisao, os numeros em ESCALA PADRONIZADA
# para deixar tabelas e texto batendo (Tabelas 2, 3 e 4 do tcc_novo).
#   (1) FM linear PADRONIZADA (Tabela 2) + Newey-West
#   (2) POOLED PADRONIZADA (Tabela 3): l_aum,l_cot,preco,beta por desvio-padrao
#   (3) LOGIT: efeito parcial medio (APE) por desvio-padrao (coluna da Tabela 4)
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq/aum_num][, ym := ano*100L+mes]
d <- d[!is.na(flow_aum)]
z <- function(x) (x-mean(x))/sd(x)
d[, z_aum := z(l_aum)][, z_cot := z(l_cot)]

nw_se <- function(x, L=3){ T<-length(x); u<-x-mean(x); a<-0
  for(l in 1:L) a<-a+(1-l/(L+1))*sum(u[(l+1):T]*u[1:(T-l)]); sqrt((sum(u^2)+2*a)/T^2) }

# (1) FM linear padronizada
meses <- sort(unique(d$ym))
M <- rbindlist(lapply(meses, function(m) as.data.table(as.list(coef(
  lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum, data=d[ym==m]))))))
setnames(M, c("int","z_aum","z_cot","is_fic","flow"))
cat("===== (1) TABELA 2 — FM LINEAR PADRONIZADA + Newey-West =====\n")
for(j in seq_along(M)){ x<-M[[j]]; m<-mean(x); se<-nw_se(x);
  cat(sprintf("%-10s coef=% .5f  se_nw=%.5f  t=% .1f\n", names(M)[j], m, se, m/se)) }

# (2) Pooled padronizada
d[, z_preco := z(preco_mes)][, z_beta := z(beta_mes)]
fp <- lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_preco + z_beta, data=d)
cat("\n===== (2) TABELA 3 — POOLED PADRONIZADA =====\n")
print(round(summary(fp)$coefficients, 6))
cat("R2:", round(summary(fp)$r.squared,4), "\n")

# (3) Logit: efeito parcial medio (APE) por desvio-padrao
ape <- rbindlist(lapply(meses, function(m){ dm<-d[ym==m]
  fg<-suppressWarnings(glm(peso_vale3~z_aum+z_cot+is_fic+flow_aum,family=quasibinomial(),data=dm))
  cg<-coef(fg); p<-fitted(fg); s<-mean(p*(1-p))
  as.data.table(as.list(cg[c("z_aum","z_cot","is_fic","flow_aum")]*s)) }))
cat("\n===== (3) TABELA 4 — EFEITO PARCIAL MEDIO do LOGIT (p.p. por d.p.) =====\n")
print(round(sapply(ape, mean)*100, 3))
