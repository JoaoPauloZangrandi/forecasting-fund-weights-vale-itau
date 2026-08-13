# =============================================================================
# 99_pipeline_hhi_lag_etapa1.R  (v2 OFICIAL)
#
# Motor oficial de Etapa 1 + Etapa 3 do TCC: 6 caracteristicas (inclui
# HHI_resto defasado em t-1), por celula (ativo,mes), h=1,3,6,12.
#
# HHI_resto_lag(fundo,ativo,t) = HHI_total(fundo,t-1) - peso(fundo,ativo,t-1)^2
#   Fundos sem dado de carteira em t-1 (fundo novo, por exemplo) ficam sem
#   essa caracteristica -- excluidos da amostra final, mesma logica ja
#   aplicada as outras 5 caracteristicas (ex. beta exige 252 pregoes).
#
# Atualizado em 12/08/2026: painel_multiativo_final.csv passou a conter o
# universo COMPLETO (todo fundo do universo CVM com posicao em acoes
# 2016-2021, todas as gestoras, todas as acoes -- nao mais ancorado em
# VALE3). Este script nao mudou de logica, so o painel de entrada mudou de
# conteudo (script 101_cross_section_universo_completo_hhi.R validou que os
# dois paineis tem exatamente o mesmo schema). Grava direto nos nomes
# canonicos usados no TCC (theta_multiativo.csv, erro_e_multiativo.csv,
# etapa3_multiativo_*.csv) -- nao ha mais versao "5 caracteristicas" ou
# "HHI contemporaneo" pra comparar, e o bloco de comparacao foi removido.
# RODAR COM CAMINHO ABSOLUTO. Demorado -- rodar em background.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("Painel (ja limpo de soma_peso>105%):", nrow(d), "linhas |", uniqueN(d$cod_fundo), "fundos\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# =============================================================================
# HHI_resto DEFASADO
# =============================================================================
hhi_total <- d[, .(hhi_total = sum(peso^2)), by = .(cod_fundo, ym)]
hhi_total_lag <- hhi_total[, .(cod_fundo, ym = addm(ym, 1), hhi_total_lag1 = hhi_total)]
d <- merge(d, hhi_total_lag, by = c("cod_fundo","ym"), all.x = TRUE)

peso_prev <- d[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev_asset = peso)]
d <- merge(d, peso_prev, by = c("cod_fundo","ativo","ym"), all.x = TRUE)
d[is.na(peso_prev_asset), peso_prev_asset := 0]
d[, hhi_resto_lag := hhi_total_lag1 - peso_prev_asset^2]

n_antes <- nrow(d)
n_fundos_antes <- uniqueN(d$cod_fundo)
d <- d[is.finite(hhi_resto_lag)]
cat(sprintf("Linhas excluidas por falta de HHI defasado (sem dado em t-1): %d de %d (%.2f%%)\n",
            n_antes - nrow(d), n_antes, 100*(n_antes-nrow(d))/n_antes))
cat(sprintf("Fundos: %d -> %d\n", n_fundos_antes, uniqueN(d$cod_fundo)))
stopifnot(all(d$hhi_resto_lag >= -1e-9))

# =============================================================================
# Etapa 1 + erro + theta/APE, 6 caracteristicas (HHI defasado), uma passada
# =============================================================================
fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)
mu_hhi <- mean(d$hhi_resto_lag); sd_hhi <- sd(d$hhi_resto_lag)

d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]
d[, z_hhi := (hhi_resto_lag-mu_hhi)/sd_hhi]

cnt <- d[, .N, by = .(ativo, ym)]
celulas <- cnt[N >= 15]
cat("Celulas (ativo,mes) com >=15 holders:", nrow(celulas), "\n")

setkey(d, ativo, ym)
n_nao_convergiu <- 0L
resultados_erro <- vector("list", nrow(celulas))
resultados_theta <- vector("list", nrow(celulas))
run_cell <- function(i) {
  av <- celulas$ativo[i]; mm <- celulas$ym[i]
  dm <- d[.(av, mm)]
  fit <- tryCatch(suppressWarnings(glm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + z_hhi,
                     family = quasibinomial(link="logit"), data = dm)), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 7) return(NULL)
  if (!fit$converged) { n_nao_convergiu <<- n_nao_convergiu + 1L; return(NULL) }
  s <- summary(fit)$coefficients
  if (!"z_hhi" %in% rownames(s)) return(NULL)
  cf <- coef(fit)
  resultados_erro[[i]] <<- data.table(cod_fundo = dm$cod_fundo, gestora_grupo = dm$gestora_grupo,
             ativo = av, ym = mm, peso = dm$peso, peso_pred = fitted(fit), erro = residuals(fit, type = "response"))
  pr2 <- max(1 - fit$deviance/fit$null.deviance, -1, na.rm = TRUE)
  z_pred <- predict(fit, type = "link"); dg <- dlogis(z_pred)
  ape_aum <- cf[["z_aum"]]*mean(dg); ape_cot <- cf[["z_cot"]]*mean(dg)
  ape_flow <- cf[["flow_aum"]]*mean(dg); ape_betaf <- cf[["z_betaf"]]*mean(dg); ape_hhi <- cf[["z_hhi"]]*mean(dg)
  z0 <- z_pred - cf[["is_fic"]]*dm$is_fic; z1 <- z0 + cf[["is_fic"]]
  ape_fic <- mean(plogis(z1) - plogis(z0))
  resultados_theta[[i]] <<- data.table(ativo = av, ym = mm, n = nrow(dm), pr2 = pr2,
             intercepto = cf[[1]], b_aum = cf[["z_aum"]], b_cot = cf[["z_cot"]], b_fic = cf[["is_fic"]],
             b_flow = cf[["flow_aum"]], b_betaf = cf[["z_betaf"]], b_hhi = cf[["z_hhi"]],
             t_hhi = s["z_hhi","t value"], sig_hhi = abs(s["z_hhi","t value"]) > 1.96,
             ape_aum = ape_aum, ape_cot = ape_cot, ape_fic = ape_fic,
             ape_flow = ape_flow, ape_betaf = ape_betaf, ape_hhi = ape_hhi)
  NULL
}
t0 <- Sys.time()
invisible(lapply(seq_len(nrow(celulas)), run_cell))
E <- rbindlist(resultados_erro)
THETA <- rbindlist(resultados_theta)
cat("Tempo Etapa 1/erro/theta (HHI defasado):", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |",
    n_nao_convergiu, "celulas nao convergiram | n_erro =", nrow(E), "| n_theta =", nrow(THETA), "\n")
fwrite(E, file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
fwrite(THETA, file.path(REPO, "v2 OFICIAL/data/theta_multiativo.csv"))

cat("\n==== Resumo entre", nrow(THETA), "celulas ====\n")
cat(sprintf("Significancia HHI_lag: %d de %d (%.1f%%) | %% positivo (sig): %d/%d\n",
            sum(THETA$sig_hhi), nrow(THETA), 100*mean(THETA$sig_hhi),
            sum(THETA$sig_hhi & THETA$b_hhi>0), sum(THETA$sig_hhi)))
cat(sprintf("pseudo-R2 medio (piso -1): %.4f\n", mean(THETA$pr2)))

# ---- media por ativo (>=24 meses), pra tabela auxiliar se precisar --------
avg <- THETA[, .(n_meses = .N, b_aum=mean(b_aum), b_cot=mean(b_cot), b_fic=mean(b_fic),
                 b_flow=mean(b_flow,na.rm=TRUE), b_betaf=mean(b_betaf,na.rm=TRUE), b_hhi=mean(b_hhi),
                 ape_aum=mean(ape_aum), ape_cot=mean(ape_cot), ape_fic=mean(ape_fic),
                 ape_flow=mean(ape_flow,na.rm=TRUE), ape_betaf=mean(ape_betaf,na.rm=TRUE), ape_hhi=mean(ape_hhi),
                 pr2=mean(pr2)), by = ativo][n_meses >= 24]
fwrite(avg, file.path(REPO, "v2 OFICIAL/data/theta_media_ativo.csv"))

# =============================================================================
# Etapa 3, h=1,3,6,12
# =============================================================================
E[, d := peso_pred - peso]
CORTE <- 202001L
rmse <- function(x) sqrt(mean(x^2))

roda_horizonte <- function(h) {
  fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
  atual <- E[, .(cod_fundo, gestora_grupo, ativo, ym, d, peso, ym_fut = addm(ym, h))]
  M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"), by.y = c("cod_fundo","ativo","ym_fut_key"))
  M[, dw := peso_fut - peso]
  treino <- M[ym < CORTE]; teste <- M[ym >= CORTE & ym_fut <= 202112L]
  fit <- lm(dw ~ 0 + d, data = treino); lam <- unname(coef(fit)["d"])
  teste[, erro_oos := dw - lam * d]; teste[, erro_naive := dw]; teste[, horizonte := h]
  cat(sprintf("h=%d: treino %d | teste %d | lambda=%.4f | RMSE ajuste=%.6f | RMSE naive=%.6f\n",
              h, nrow(treino), nrow(teste), lam, rmse(teste$erro_oos), rmse(teste$erro_naive)))
  teste
}
R1 <- roda_horizonte(1); R3 <- roda_horizonte(3); R6 <- roda_horizonte(6); R12 <- roda_horizonte(12)
fwrite(R1, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
fwrite(R3, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))

por_gestora <- function(dt, h) {
  g <- dt[, .(n_obs = .N, n_fundos = uniqueN(cod_fundo), rmse_oos = rmse(erro_oos),
              rmse_naive = rmse(erro_naive)), by = gestora_grupo]
  g[, margem_pct := 100*(rmse_naive - rmse_oos)/rmse_naive]; g[, horizonte := h]; g
}
G <- rbindlist(list(por_gestora(R1,1), por_gestora(R3,3), por_gestora(R6,6), por_gestora(R12,12)))
W <- dcast(G, gestora_grupo ~ horizonte, value.var = "rmse_oos", fun.aggregate = mean)
setnames(W, as.character(c(1,3,6,12)), paste0("rmse_h", c(1,3,6,12)))
Wm <- dcast(G, gestora_grupo ~ horizonte, value.var = "margem_pct", fun.aggregate = mean)
setnames(Wm, as.character(c(1,3,6,12)), paste0("margem_h", c(1,3,6,12)))
W <- merge(W, Wm, by = "gestora_grupo")
n_fundos_h1 <- G[horizonte == 1, .(gestora_grupo, n_fundos_h1 = n_fundos)]
W <- merge(W, n_fundos_h1, by = "gestora_grupo", all.x = TRUE)
setorder(W, -rmse_h1)
fwrite(G, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte_longo.csv"))
fwrite(W, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))

cat(sprintf("\nRMSE h=1: %.6f | h=3: %.6f | h=6: %.6f | h=12: %.6f\n",
            rmse(R1$erro_oos), rmse(R3$erro_oos), rmse(R6$erro_oos), rmse(R12$erro_oos)))
cat(sprintf("RMSE ingenua h=1: %.6f | h=3: %.6f | h=6: %.6f | h=12: %.6f\n",
            rmse(R1$erro_naive), rmse(R3$erro_naive), rmse(R6$erro_naive), rmse(R12$erro_naive)))

cat("\nOK - pipeline Etapa 1 + Etapa 3 (universo completo, 6 caracteristicas) completa\n")
