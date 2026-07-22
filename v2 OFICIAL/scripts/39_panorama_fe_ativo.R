# =============================================================================
# 39_panorama_fe_ativo.R  (v2 OFICIAL)
#
# Panorama completo da Secao 6.2 (FE pooled ativo+mes, R/31): efeito-fixo de
# CADA ativo (fixef(f)$ativo), nao so os 5 coeficientes pooled. Mesmo filtro
# de elegibilidade da Secao 6.1/6.3 (>=24 meses de presenca), MAIS um filtro
# de peso mediano > 0,1% -- necessario porque ativos com peso essencialmente
# zero (posicoes-poeira, presentes em muitos meses mas nunca efetivamente
# alocados) dominam os extremos do FE de forma espuria (achado replicado
# tambem na dispersao do erro por ativo, R/38 -- ver 40_panorama_erro_ativo.R).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)
d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]

dd <- d[is.finite(flow_aum)]
f <- feglm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf | ativo + ym,
           data = dd, family = quasibinomial(), cluster = ~cod_fundo)

fe_ativo <- data.table(ativo = names(fixef(f)$ativo), fe = as.numeric(fixef(f)$ativo))
cat("Ativos com FE estimado:", nrow(fe_ativo), "\n")

cobertura <- d[, .(peso_mediano = median(peso), n_meses = uniqueN(ym)), by = ativo]
fe_ativo <- merge(fe_ativo, cobertura, by = "ativo")

elig <- fe_ativo[n_meses >= 24 & peso_mediano > 0.001]
cat("Elegiveis (>=24 meses E peso mediano > 0,1%):", nrow(elig), "de", nrow(fe_ativo), "\n\n")

cat("Quartis do FE (log-odds) entre elegiveis:\n")
print(round(quantile(elig$fe, c(.25,.5,.75)), 3))

vale <- elig[ativo == "VALE ON N1 - VALE3"]
cat("\nVALE3 FE:", round(vale$fe,3), "| rank:",
    which(elig[order(-fe)]$ativo == "VALE ON N1 - VALE3"), "de", nrow(elig), "\n")

setorder(elig, -fe)
cat("\nTop5 (FE mais positivo -- sobrepeso alem do explicado por tamanho/cotistas/fic/fluxo/beta):\n")
print(head(elig[,.(ativo, fe=round(fe,3), n_meses, peso_mediano=round(peso_mediano,4))], 5))
cat("\nBottom5 (FE mais negativo -- subpeso):\n")
print(tail(elig[,.(ativo, fe=round(fe,3), n_meses, peso_mediano=round(peso_mediano,4))], 5))

fwrite(elig, file.path(REPO, "v2 OFICIAL/data/pooled_fe2way_ativo_filtrado.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/pooled_fe2way_ativo_filtrado.csv'\n")
