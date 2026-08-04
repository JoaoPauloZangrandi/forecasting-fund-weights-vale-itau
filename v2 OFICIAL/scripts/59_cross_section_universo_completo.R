# =============================================================================
# 59_cross_section_universo_completo.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 1, passo 2 (Equacao 1 / Regressao A): logit peso ~ 5 caracteristicas,
# POR ATIVO E POR MES (>=15 fundos holders), no universo COMPLETO (nao
# ancorado em VALE3). Mesma especificacao exata do R/30 -- so muda a base
# de entrada (painel_universo_completo_final.csv, R/58).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_universo_completo_final.csv"))
cat("Painel:", nrow(d), "linhas |", uniqueN(d$cod_fundo), "fundos |", uniqueN(d$ativo), "ativos\n")

fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)
cat("Fundo-meses unicos p/ padronizacao:", nrow(fm_unico), "\n")

d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]

cnt <- d[, .N, by = .(ativo, ym)]
celulas <- cnt[N >= 15]
cat("\nCelulas (ativo,mes) com >=15 holders:", nrow(celulas), "|", uniqueN(celulas$ativo),
    "ativos distintos |", sum(celulas$N), "obs\n")

setkey(d, ativo, ym)
n_nao_convergiu <- 0L
run_cell <- function(av, mm) {
  dm <- d[.(av, mm)]
  fit <- tryCatch(suppressWarnings(glm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf,
                     family = quasibinomial(link="logit"), data = dm)), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 6) return(NULL)
  if (!fit$converged) { n_nao_convergiu <<- n_nao_convergiu + 1L; return(NULL) }
  cf <- coef(fit)
  pr2 <- max(1 - fit$deviance/fit$null.deviance, -1, na.rm = TRUE)
  z_pred <- predict(fit, type = "link"); dg <- dlogis(z_pred)
  ape_aum <- cf[["z_aum"]]*mean(dg); ape_cot <- cf[["z_cot"]]*mean(dg)
  ape_flow <- cf[["flow_aum"]]*mean(dg); ape_betaf <- cf[["z_betaf"]]*mean(dg)
  z0 <- z_pred - cf[["is_fic"]]*dm$is_fic; z1 <- z0 + cf[["is_fic"]]
  ape_fic <- mean(plogis(z1) - plogis(z0))
  peso_pred <- plogis(z_pred)
  list(theta = data.table(ativo = av, ym = mm, n = nrow(dm), pr2 = pr2,
             intercepto = cf[[1]], b_aum = cf[[2]], b_cot = cf[[3]], b_fic = cf[[4]],
             b_flow = cf[[5]], b_betaf = cf[[6]],
             ape_aum = ape_aum, ape_cot = ape_cot, ape_fic = ape_fic,
             ape_flow = ape_flow, ape_betaf = ape_betaf),
       pred = data.table(cod_fundo = dm$cod_fundo, ativo = av, ym = mm, peso = dm$peso, peso_pred = peso_pred))
}
t0 <- Sys.time()
res_list <- mapply(run_cell, celulas$ativo, celulas$ym, SIMPLIFY = FALSE)
res_list <- Filter(Negate(is.null), res_list)
theta <- rbindlist(lapply(res_list, `[[`, "theta"))
peso_pred_full <- rbindlist(lapply(res_list, `[[`, "pred"))
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |", nrow(theta),
    "celulas estimadas |", n_nao_convergiu, "celulas nao convergiram (excluidas)\n")
fwrite(theta, file.path(REPO, "v2 OFICIAL/data/theta_universo_completo.csv"))
fwrite(peso_pred_full, file.path(REPO, "v2 OFICIAL/data/peso_pred_universo_completo.csv"))

# ---- media temporal por ativo (so ativos com >=24 meses estimados) --------
avg <- theta[, .(n_meses = .N, b_aum = mean(b_aum), b_cot = mean(b_cot), b_fic = mean(b_fic),
                 b_flow = mean(b_flow, na.rm = TRUE), b_betaf = mean(b_betaf, na.rm = TRUE),
                 ape_aum = mean(ape_aum), ape_cot = mean(ape_cot), ape_fic = mean(ape_fic),
                 ape_flow = mean(ape_flow, na.rm = TRUE), ape_betaf = mean(ape_betaf, na.rm = TRUE),
                 pr2 = mean(pr2)), by = ativo][n_meses >= 24]
cat("\n==== Ativos com >=24 meses estimados:", nrow(avg), "====\n")
cat("--- por sinal do coeficiente (log-odds) ---\n")
for (v in c("b_aum","b_cot","b_fic","b_flow","b_betaf")) {
  sinal_vale <- sign(avg[ativo=="VALE ON N1 - VALE3"][[v]])
  pct_igual <- 100*mean(sign(avg[[v]]) == sinal_vale)
  cat(sprintf("%-8s mediana=%8.5f | %% mesmo sinal que VALE3=%.1f%%\n", v, median(avg[[v]]), pct_igual))
}
cat("Pseudo-R2 medio:", round(mean(avg$pr2),4), "\n")

fwrite(avg, file.path(REPO, "v2 OFICIAL/data/theta_media_ativo_universo_completo.csv"))
cat("\nOK - salvo em 'theta_universo_completo.csv', 'peso_pred_universo_completo.csv' e 'theta_media_ativo_universo_completo.csv'\n")
