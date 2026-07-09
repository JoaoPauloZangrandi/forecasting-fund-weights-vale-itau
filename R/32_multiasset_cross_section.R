# =============================================================================
# 32_multiasset_cross_section.R
#
# Generaliza o Passo 1 (regressao transversal, R/14/R/24) para TODOS os ativos
# do painel multiativo (R/31): estima theta_{n,t} = coeficientes da regressao
# peso ~ caracteristicas do fundo, POR ATIVO E POR MES (so celulas com >=15
# fundos Itau holders, para a regressao ser bem identificada).
#
# Cuidado metodologico (novo nesta rodada): a padronizacao (z-score) de
# ln(PL) e ln(cotistas) usa a distribuicao entre (fundo,mes) UNICOS, nao entre
# linhas (fundo,ativo,mes) -- senao um fundo que detem 50 ativos pesaria 50x
# mais na media/desvio-padrao do que um fundo que detem 1.
#
# Saidas:
#   (A) reg32_theta_multiativo.csv    -- um theta_{n,t} por (ativo,mes)
#   (B) reg32_theta_media_ativo.csv   -- media temporal de theta_n por ativo
#   (C) reg32_pooled_fe2way.csv       -- 1 regressao pooled com efeito-fixo de
#                                         ativo E de mes (Frisch-Waugh, via
#                                         demeaning iterativo), EP cluster fundo
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_multiativo_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq/aum_num][, ym := ano*100L+mes]

# --- padronizacao pela distribuicao em (fundo,mes) UNICO (nao repetida por ativo) ---
fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
d[, z_aum := (l_aum-mu_aum)/sd_aum][, z_cot := (l_cot-mu_cot)/sd_cot]
cat("Fundo-meses unicos p/ padronizacao:", nrow(fm_unico), "| media/dp l_aum:",
    round(mu_aum,2), round(sd_aum,2), "| l_cot:", round(mu_cot,2), round(sd_cot,2), "\n")

# =============================================================================
# (A) theta_{n,t}: uma regressao por (ativo, mes), min 15 holders
# =============================================================================
cnt <- d[, .N, by = .(ativo, ym)]
celulas <- cnt[N >= 15]
cat("\nCelulas (ativo,mes) com >=15 holders:", nrow(celulas), "|", uniqueN(celulas$ativo),
    "ativos distintos |", sum(celulas$N), "obs\n")

setkey(d, ativo, ym)
run_cell <- function(av, mm) {
  dm <- d[.(av, mm)]
  fit <- tryCatch(lm(peso ~ z_aum + z_cot + is_fic + flow_aum, data = dm), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 5) return(NULL)
  cf <- coef(fit)
  data.table(ativo = av, ym = mm, n = nrow(dm), r2 = summary(fit)$r.squared,
             intercepto = cf[[1]], b_aum = cf[[2]], b_cot = cf[[3]], b_fic = cf[[4]], b_flow = cf[[5]])
}
t0 <- Sys.time()
theta <- rbindlist(mapply(run_cell, celulas$ativo, celulas$ym, SIMPLIFY = FALSE))
cat("tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |", nrow(theta), "celulas estimadas\n")
fwrite(theta, file.path(REPO, "data/processed/reg32_theta_multiativo.csv"))

# =============================================================================
# (B) media temporal de theta_n por ativo (so ativos com >=24 meses estimados)
#     -> testa se o padrao de VALE3 e TIPICO ou IDIOSSINCRATICO
# =============================================================================
avg <- theta[, .(n_meses = .N, b_aum = mean(b_aum), b_cot = mean(b_cot),
                 b_fic = mean(b_fic), b_flow = mean(b_flow, na.rm = TRUE), r2 = mean(r2)),
             by = ativo][n_meses >= 24]
cat("(nota: 2 de 23.704 celulas tem b_flow NA por colinearidade isolada em nov/2021; excluidas via na.rm)\n")
cat("\n==== (B) Ativos com >=24 meses estimados:", nrow(avg), "====\n")
cat("b_aum:  mediana=", round(median(avg$b_aum),5), " | % negativo=",
    round(100*mean(avg$b_aum < 0),1), "% (VALE3: 100% dos 72 meses, R/14)\n", sep="")
cat("b_cot:  mediana=", round(median(avg$b_cot),5), " | % positivo=",
    round(100*mean(avg$b_cot > 0),1), "% (VALE3: 100% dos 72 meses, R/14)\n", sep="")
cat("b_fic:  mediana=", round(median(avg$b_fic),5), " | % negativo=",
    round(100*mean(avg$b_fic < 0),1), "% (VALE3: 100% dos 72 meses, R/14)\n", sep="")
cat("b_flow: mediana=", round(median(avg$b_flow),5), " | % negativo=",
    round(100*mean(avg$b_flow < 0),1), "%\n", sep="")
vale_row <- avg[ativo == "VALE ON N1 - VALE3"]
cat("\nVALE3 nesta amostra: b_aum=", round(vale_row$b_aum,5), " (percentil ",
    round(100*mean(avg$b_aum <= vale_row$b_aum),0), " da distribuicao entre ativos)\n", sep="")
fwrite(avg, file.path(REPO, "data/processed/reg32_theta_media_ativo.csv"))

# =============================================================================
# (C) pooled com efeito-fixo de ATIVO e de MES (Frisch-Waugh: demeaning
#     iterativo em duas vias), EP cluster por fundo
# =============================================================================
dd <- d[is.finite(flow_aum)]
demean2way <- function(x, g1, g2, iter = 15) {
  y <- x
  for (i in seq_len(iter)) {
    y <- y - ave(y, g1, FUN = function(z) mean(z, na.rm = TRUE))
    y <- y - ave(y, g2, FUN = function(z) mean(z, na.rm = TRUE))
  }
  y
}
cat("\n==== (C) Pooled com efeito-fixo de ativo e de mes (Frisch-Waugh) ====\n")
vars <- c("peso","z_aum","z_cot","is_fic","flow_aum")
Dm <- dd[, lapply(.SD, function(v) demean2way(v, dd$ativo, dd$ym)), .SDcols = vars]
setnames(Dm, paste0(vars, "_d"))
Xm <- cbind(1, as.matrix(Dm[, .(z_aum_d, z_cot_d, is_fic_d, flow_aum_d)]))
y  <- Dm$peso_d
b  <- solve(crossprod(Xm), crossprod(Xm, y))
u  <- as.numeric(y - Xm %*% b)
cl_meat <- function(Z, u, cl) {
  k <- ncol(Z); Mt <- matrix(0,k,k)
  for (g in unique(cl)) { i <- cl==g; s <- crossprod(Z[i,,drop=FALSE], u[i]); Mt <- Mt + s%*%t(s) }
  (length(unique(cl))/(length(unique(cl))-1)) * Mt
}
V <- solve(crossprod(Xm)) %*% cl_meat(Xm, u, dd$cod_fundo) %*% solve(crossprod(Xm))
se <- sqrt(diag(V)); tt <- b/se
res_fe2 <- data.table(variavel = c("intercepto(omitido p/FE)","z_aum","z_cot","is_fic","flow_aum"),
                      coef = as.numeric(b), se_cluster_fundo = se, t = as.numeric(tt))
print(res_fe2)
cat("\nObs:", nrow(dd), "| ativos:", uniqueN(dd$ativo), "| meses:", uniqueN(dd$ym), "| fundos:", uniqueN(dd$cod_fundo), "\n")
cat("(o intercepto/efeitos-fixos de ativo e mes sao absorvidos pelo demeaning; coefs sao 'dentro' de ativo-mes)\n")
fwrite(res_fe2, file.path(REPO, "data/processed/reg32_pooled_fe2way.csv"))
cat("\nOK - salvo em data/processed/reg32_*.csv\n")
