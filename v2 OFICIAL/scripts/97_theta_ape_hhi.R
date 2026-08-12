# =============================================================================
# 97_theta_ape_hhi.R  (v2 OFICIAL)
#
# Completa o theta_multiativo_comHHI.csv do script 96 com o APE (efeito
# marginal medio, pontos percentuais) de cada caracteristica, incl. HHI --
# mesmo metodo do script 30 (dg = dlogis(z_pred); ape_x = coef_x*mean(dg);
# ape_fic via diferenca discreta plogis(z1)-plogis(z0)), so que agora com
# 6 caracteristicas. Reajusta a celula (nao reaproveita coeficientes do
# script 96) porque precisa do vetor de derivadas por observacao (dg), que
# nao foi salvo antes.
#
# Decisao de escopo (nao estava no plano original, adicionada aqui):
# TCC_finalV2 vai usar a base MULTIATIVO (nao mais VALE3-Itau 250 fundos)
# tambem pra Etapa 1/2 dos Resultados, unificando toda a Secao 5 numa unica
# base -- evita duplicar o esforco de HHI numa segunda pipeline (VALE3-Itau,
# scripts 13-16) so pra essa parte, e remove a troca de base confusa que o
# documento atual tinha (5.1/5.2 = VALE3-Itau; 5.3 = multiativo).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
hhi_total <- d[, .(hhi_total = sum(peso^2)), by = .(cod_fundo, ym)]
d <- merge(d, hhi_total, by = c("cod_fundo","ym"))
d[, hhi_resto := hhi_total - peso^2]

fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)
mu_hhi <- mean(d$hhi_resto); sd_hhi <- sd(d$hhi_resto)

d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]
d[, z_hhi := (hhi_resto-mu_hhi)/sd_hhi]

cnt <- d[, .N, by = .(ativo, ym)]
celulas <- cnt[N >= 15]
cat("Celulas (ativo,mes) com >=15 holders:", nrow(celulas), "\n")

setkey(d, ativo, ym)
n_nao_convergiu <- 0L
run_cell <- function(av, mm) {
  dm <- d[.(av, mm)]
  fit <- tryCatch(suppressWarnings(glm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + z_hhi,
                     family = quasibinomial(link="logit"), data = dm)), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 7) return(NULL)
  if (!fit$converged) { n_nao_convergiu <<- n_nao_convergiu + 1L; return(NULL) }
  cf <- coef(fit)
  s <- summary(fit)$coefficients
  if (!"z_hhi" %in% rownames(s)) return(NULL)
  pr2 <- max(1 - fit$deviance/fit$null.deviance, -1, na.rm = TRUE)
  z_pred <- predict(fit, type = "link"); dg <- dlogis(z_pred)
  ape_aum <- cf[["z_aum"]]*mean(dg); ape_cot <- cf[["z_cot"]]*mean(dg)
  ape_flow <- cf[["flow_aum"]]*mean(dg); ape_betaf <- cf[["z_betaf"]]*mean(dg)
  ape_hhi <- cf[["z_hhi"]]*mean(dg)
  z0 <- z_pred - cf[["is_fic"]]*dm$is_fic; z1 <- z0 + cf[["is_fic"]]
  ape_fic <- mean(plogis(z1) - plogis(z0))
  data.table(ativo = av, ym = mm, n = nrow(dm), pr2 = pr2,
             intercepto = cf[[1]], b_aum = cf[[2]], b_cot = cf[[3]], b_fic = cf[[4]],
             b_flow = cf[[5]], b_betaf = cf[[6]], b_hhi = cf[[7]],
             t_hhi = s["z_hhi","t value"], sig_hhi = abs(s["z_hhi","t value"]) > 1.96,
             ape_aum = ape_aum, ape_cot = ape_cot, ape_fic = ape_fic,
             ape_flow = ape_flow, ape_betaf = ape_betaf, ape_hhi = ape_hhi)
}
t0 <- Sys.time()
theta <- rbindlist(mapply(run_cell, celulas$ativo, celulas$ym, SIMPLIFY = FALSE))
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |", nrow(theta),
    "celulas estimadas |", n_nao_convergiu, "celulas nao convergiram (excluidas)\n")
fwrite(theta, file.path(REPO, "v2 OFICIAL/data/theta_multiativo_comHHI.csv"))

cat("\n==== Resumo entre", nrow(theta), "celulas (ativo,mes) ====\n")
cat("--- coeficiente padronizado (log-odds), mediana e % positivo ---\n")
for (v in c("b_aum","b_cot","b_fic","b_flow","b_betaf","b_hhi")) {
  cat(sprintf("%-8s mediana=%9.5f | %% positivo=%.1f%%\n", v, median(theta[[v]]), 100*mean(theta[[v]]>0)))
}
cat("--- APE (pontos percentuais), mediana e % positivo ---\n")
for (v in c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf","ape_hhi")) {
  cat(sprintf("%-8s mediana=%9.6f | %% positivo=%.1f%%\n", v, median(theta[[v]]), 100*mean(theta[[v]]>0)))
}
cat(sprintf("\nSignificancia HHI: %d de %d (%.1f%%) | pseudo-R2 medio (com piso -1)=%.4f\n",
            sum(theta$sig_hhi), nrow(theta), 100*mean(theta$sig_hhi), mean(theta$pr2)))

# ---- media temporal por ativo (>=24 meses estimados), pra comparar com VALE3 ----
avg <- theta[, .(n_meses = .N, b_aum=mean(b_aum), b_cot=mean(b_cot), b_fic=mean(b_fic),
                 b_flow=mean(b_flow,na.rm=TRUE), b_betaf=mean(b_betaf,na.rm=TRUE), b_hhi=mean(b_hhi),
                 ape_aum=mean(ape_aum), ape_cot=mean(ape_cot), ape_fic=mean(ape_fic),
                 ape_flow=mean(ape_flow,na.rm=TRUE), ape_betaf=mean(ape_betaf,na.rm=TRUE), ape_hhi=mean(ape_hhi),
                 pr2=mean(pr2)), by = ativo][n_meses >= 24]
cat("\nAtivos com >=24 meses estimados:", nrow(avg), "\n")
fwrite(avg, file.path(REPO, "v2 OFICIAL/data/theta_media_ativo_comHHI.csv"))
cat("OK\n")
