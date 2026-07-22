# =============================================================================
# 30_cross_section_multiativo.R  (v2 OFICIAL)
#
# LOGIT (nao mais linear -- decisao do Joao: logit e o modelo mae em todo o
# documento). Regressao logistica peso ~ 5 caracteristicas, POR ATIVO E POR
# MES (>=15 fundos holders), com beta do fundo incluido, universo de 41
# gestoras (nao so Itau). Alem do coeficiente (log-odds), calcula o APE
# (efeito marginal medio) de cada celula -- usado depois pra comparar sinal
# e magnitude com VALE3 em pontos percentuais, escala mais robusta a
# extremos do que o log-odds bruto (ver Adendo de processo na Secao 5).
#
# Padronizacao (z-score) de AUM/cotistas/beta_fundo usa a distribuicao entre
# (fundo,mes) UNICOS, nao entre linhas (fundo,ativo,mes) -- senao um fundo
# que detem muitos ativos pesaria mais na media/dp do que um fundo com 1 so.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
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
  # suppressWarnings: "fitted probabilities numerically 0 or 1" e comum em celulas
  # pequenas, nao invalida o coeficiente, so infla o EP (que aqui nao usamos, so o
  # ponto estimado). Nao-convergencia de verdade e checada por fit$converged abaixo.
  fit <- tryCatch(suppressWarnings(glm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf,
                     family = quasibinomial(link="logit"), data = dm)), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 6) return(NULL)
  if (!fit$converged) { n_nao_convergiu <<- n_nao_convergiu + 1L; return(NULL) }
  cf <- coef(fit)
  # pseudo-R2 de McFadden: em celulas com peso ~0 p/ todos os fundos (posicao residual/
  # legado), o null.deviance colapsa a ~0 e o pseudo-R2 explode p/ -Inf -- nao e erro de
  # ajuste, e degenerescencia numerica de uma variavel-resposta sem variancia. Piso em -1
  # (ja comunica "muito pior que o nulo" sem corromper medias/medianas depois).
  pr2 <- max(1 - fit$deviance/fit$null.deviance, -1, na.rm = TRUE)
  z_pred <- predict(fit, type = "link"); dg <- dlogis(z_pred)
  ape_aum <- cf[["z_aum"]]*mean(dg); ape_cot <- cf[["z_cot"]]*mean(dg)
  ape_flow <- cf[["flow_aum"]]*mean(dg); ape_betaf <- cf[["z_betaf"]]*mean(dg)
  z0 <- z_pred - cf[["is_fic"]]*dm$is_fic; z1 <- z0 + cf[["is_fic"]]
  ape_fic <- mean(plogis(z1) - plogis(z0))
  data.table(ativo = av, ym = mm, n = nrow(dm), pr2 = pr2,
             intercepto = cf[[1]], b_aum = cf[[2]], b_cot = cf[[3]], b_fic = cf[[4]],
             b_flow = cf[[5]], b_betaf = cf[[6]],
             ape_aum = ape_aum, ape_cot = ape_cot, ape_fic = ape_fic,
             ape_flow = ape_flow, ape_betaf = ape_betaf)
}
t0 <- Sys.time()
theta <- rbindlist(mapply(run_cell, celulas$ativo, celulas$ym, SIMPLIFY = FALSE))
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |", nrow(theta),
    "celulas estimadas |", n_nao_convergiu, "celulas nao convergiram (excluidas)\n")
fwrite(theta, file.path(REPO, "v2 OFICIAL/data/theta_multiativo.csv"))

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
cat("--- por sinal do APE (pontos percentuais, mais robusto) ---\n")
for (v in c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf")) {
  sinal_vale <- sign(avg[ativo=="VALE ON N1 - VALE3"][[v]])
  pct_igual <- 100*mean(sign(avg[[v]]) == sinal_vale)
  cat(sprintf("%-8s mediana=%8.5f | %% mesmo sinal que VALE3=%.1f%%\n", v, median(avg[[v]]), pct_igual))
}

vale_row <- avg[ativo == "VALE ON N1 - VALE3"]
cat("\n==== VALE3 nesta amostra (percentil entre os", nrow(avg), "ativos, pelo APE) ====\n")
for (v in c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf")) {
  pct <- 100*mean(avg[[v]] <= vale_row[[v]])
  cat(sprintf("%-8s = %8.5f | percentil %.1f\n", v, vale_row[[v]], pct))
}
cat("Pseudo-R2 medio:", round(mean(avg$pr2),4), "\n")

fwrite(avg, file.path(REPO, "v2 OFICIAL/data/theta_media_ativo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/theta_multiativo.csv' e 'theta_media_ativo.csv'\n")
