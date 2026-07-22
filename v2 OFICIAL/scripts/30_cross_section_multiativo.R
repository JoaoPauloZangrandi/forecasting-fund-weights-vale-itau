# =============================================================================
# 30_cross_section_multiativo.R  (v2 OFICIAL)
#
# Regressao peso ~ 5 caracteristicas, POR ATIVO E POR MES (>=15 fundos
# holders), igual v1 (R/32), agora com beta do fundo incluido e usando o
# universo de 41 gestoras (nao so Itau).
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
run_cell <- function(av, mm) {
  dm <- d[.(av, mm)]
  fit <- tryCatch(lm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf, data = dm), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 6) return(NULL)
  cf <- coef(fit)
  data.table(ativo = av, ym = mm, n = nrow(dm), r2 = summary(fit)$r.squared,
             intercepto = cf[[1]], b_aum = cf[[2]], b_cot = cf[[3]], b_fic = cf[[4]],
             b_flow = cf[[5]], b_betaf = cf[[6]])
}
t0 <- Sys.time()
theta <- rbindlist(mapply(run_cell, celulas$ativo, celulas$ym, SIMPLIFY = FALSE))
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |", nrow(theta), "celulas estimadas\n")
fwrite(theta, file.path(REPO, "v2 OFICIAL/data/theta_multiativo.csv"))

# ---- media temporal por ativo (so ativos com >=24 meses estimados) --------
avg <- theta[, .(n_meses = .N, b_aum = mean(b_aum), b_cot = mean(b_cot), b_fic = mean(b_fic),
                 b_flow = mean(b_flow, na.rm = TRUE), b_betaf = mean(b_betaf, na.rm = TRUE),
                 r2 = mean(r2)), by = ativo][n_meses >= 24]
cat("\n==== Ativos com >=24 meses estimados:", nrow(avg), "====\n")
for (v in c("b_aum","b_cot","b_fic","b_flow","b_betaf")) {
  sinal_vale <- sign(avg[ativo=="VALE ON N1 - VALE3"][[v]])
  pct_igual <- 100*mean(sign(avg[[v]]) == sinal_vale)
  cat(sprintf("%-8s mediana=%8.5f | %% mesmo sinal que VALE3=%.1f%%\n", v, median(avg[[v]]), pct_igual))
}

vale_row <- avg[ativo == "VALE ON N1 - VALE3"]
cat("\n==== VALE3 nesta amostra (percentil entre os", nrow(avg), "ativos) ====\n")
for (v in c("b_aum","b_cot","b_fic","b_flow","b_betaf")) {
  pct <- 100*mean(avg[[v]] <= vale_row[[v]])
  cat(sprintf("%-8s = %8.5f | percentil %.1f\n", v, vale_row[[v]], pct))
}

fwrite(avg, file.path(REPO, "v2 OFICIAL/data/theta_media_ativo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/theta_multiativo.csv' e 'theta_media_ativo.csv'\n")
