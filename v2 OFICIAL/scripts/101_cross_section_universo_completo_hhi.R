# =============================================================================
# 101_cross_section_universo_completo_hhi.R  (v2 OFICIAL -- teste_estrategia)
#
# Corrige uma inconsistencia real, apontada pelo Joao em 12/08/2026: a trilha
# "universo completo" (scripts 58-73, alimenta a Secao 6 / teste_estrategia.tex)
# nunca recebeu o HHI_resto (6a caracteristica) que o script 99 adicionou na
# trilha principal (Secao 5). Mesma logica exata do script 99 (HHI_resto
# DEFASADO, self-join em t-1), so muda a base de entrada:
# painel_universo_completo_final.csv (script 58) em vez de
# painel_multiativo_final.csv.
#
# HHI_resto_lag(fundo,ativo,t) = HHI_total(fundo,t-1) - peso(fundo,ativo,t-1)^2
#
# Sobrescreve os nomes canonicos (theta_universo_completo.csv,
# peso_pred_universo_completo.csv, theta_media_ativo_universo_completo.csv)
# apos fazer backup dos originais -- os scripts 60-73 consomem so d/peso/
# peso_pred, sem mudanca de logica neles.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DATA <- file.path(REPO, "v2 OFICIAL/data")

d <- fread(file.path(DATA, "painel_universo_completo_final.csv"))
d[, cod_fundo := as.character(cod_fundo)]
cat("Painel (universo completo, 5 caracteristicas):", nrow(d), "linhas |",
    uniqueN(d$cod_fundo), "fundos |", uniqueN(d$ativo), "ativos\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# =============================================================================
# HHI_resto DEFASADO (mesma logica do script 99)
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
cat(sprintf("Fundos: %d -> %d | Ativos: %d\n", n_fundos_antes, uniqueN(d$cod_fundo), uniqueN(d$ativo)))
stopifnot(all(d$hhi_resto_lag >= -1e-9))

# =============================================================================
# Etapa 1, 6 caracteristicas, por celula (ativo, mes)
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
cat("\nCelulas (ativo,mes) com >=15 holders:", nrow(celulas), "|", uniqueN(celulas$ativo),
    "ativos distintos |", sum(celulas$N), "obs\n")

setkey(d, ativo, ym)
n_nao_convergiu <- 0L
run_cell <- function(av, mm) {
  dm <- d[.(av, mm)]
  fit <- tryCatch(suppressWarnings(glm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + z_hhi,
                     family = quasibinomial(link="logit"), data = dm)), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 7) return(NULL)
  if (!fit$converged) { n_nao_convergiu <<- n_nao_convergiu + 1L; return(NULL) }
  s <- summary(fit)$coefficients
  if (!"z_hhi" %in% rownames(s)) return(NULL)
  cf <- coef(fit)
  pr2 <- max(1 - fit$deviance/fit$null.deviance, -1, na.rm = TRUE)
  z_pred <- predict(fit, type = "link"); dg <- dlogis(z_pred)
  ape_aum <- cf[["z_aum"]]*mean(dg); ape_cot <- cf[["z_cot"]]*mean(dg)
  ape_flow <- cf[["flow_aum"]]*mean(dg); ape_betaf <- cf[["z_betaf"]]*mean(dg); ape_hhi <- cf[["z_hhi"]]*mean(dg)
  z0 <- z_pred - cf[["is_fic"]]*dm$is_fic; z1 <- z0 + cf[["is_fic"]]
  ape_fic <- mean(plogis(z1) - plogis(z0))
  peso_pred <- plogis(z_pred)
  list(theta = data.table(ativo = av, ym = mm, n = nrow(dm), pr2 = pr2,
             intercepto = cf[[1]], b_aum = cf[["z_aum"]], b_cot = cf[["z_cot"]], b_fic = cf[["is_fic"]],
             b_flow = cf[["flow_aum"]], b_betaf = cf[["z_betaf"]], b_hhi = cf[["z_hhi"]],
             t_hhi = s["z_hhi","t value"], sig_hhi = abs(s["z_hhi","t value"]) > 1.96,
             ape_aum = ape_aum, ape_cot = ape_cot, ape_fic = ape_fic,
             ape_flow = ape_flow, ape_betaf = ape_betaf, ape_hhi = ape_hhi),
       pred = data.table(cod_fundo = dm$cod_fundo, ativo = av, ym = mm, peso = dm$peso, peso_pred = peso_pred))
}
t0 <- Sys.time()
res_list <- mapply(run_cell, celulas$ativo, celulas$ym, SIMPLIFY = FALSE)
res_list <- Filter(Negate(is.null), res_list)
theta <- rbindlist(lapply(res_list, `[[`, "theta"))
peso_pred_full <- rbindlist(lapply(res_list, `[[`, "pred"))
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |", nrow(theta),
    "celulas estimadas |", n_nao_convergiu, "celulas nao convergiram (excluidas)\n")

cat("\n==== Resumo entre", nrow(theta), "celulas ====\n")
cat(sprintf("Significancia HHI_lag: %d de %d (%.1f%%) | %% positivo (sig): %d/%d\n",
            sum(theta$sig_hhi), nrow(theta), 100*mean(theta$sig_hhi),
            sum(theta$sig_hhi & theta$b_hhi>0), sum(theta$sig_hhi)))
cat(sprintf("pseudo-R2 medio (piso -1): %.4f\n", mean(theta$pr2)))

# ---- media temporal por ativo (so ativos com >=24 meses estimados) --------
avg <- theta[, .(n_meses = .N, b_aum = mean(b_aum), b_cot = mean(b_cot), b_fic = mean(b_fic),
                 b_flow = mean(b_flow, na.rm = TRUE), b_betaf = mean(b_betaf, na.rm = TRUE), b_hhi = mean(b_hhi),
                 ape_aum = mean(ape_aum), ape_cot = mean(ape_cot), ape_fic = mean(ape_fic),
                 ape_flow = mean(ape_flow, na.rm = TRUE), ape_betaf = mean(ape_betaf, na.rm = TRUE),
                 ape_hhi = mean(ape_hhi), pr2 = mean(pr2)), by = ativo][n_meses >= 24]
cat("\n==== Ativos com >=24 meses estimados:", nrow(avg), "====\n")
cat("--- por sinal do coeficiente (log-odds) ---\n")
for (v in c("b_aum","b_cot","b_fic","b_flow","b_betaf","b_hhi")) {
  sinal_vale <- sign(avg[ativo=="VALE ON N1 - VALE3"][[v]])
  pct_igual <- 100*mean(sign(avg[[v]]) == sinal_vale)
  cat(sprintf("%-8s mediana=%8.5f | %% mesmo sinal que VALE3=%.1f%%\n", v, median(avg[[v]]), pct_igual))
}
cat("Pseudo-R2 medio:", round(mean(avg$pr2),4), "\n")

# =============================================================================
# Comparacao com a versao de 5 caracteristicas (antes de sobrescrever)
# =============================================================================
theta_old <- fread(file.path(DATA, "theta_universo_completo.csv"))
cat("\n\n===== COMPARACAO: 6 caracteristicas (novo) vs. 5 caracteristicas (antigo) =====\n")
cat(sprintf("Celulas: %d (novo) vs %d (antigo)\n", nrow(theta), nrow(theta_old)))
cat(sprintf("pseudo-R2 medio: %.4f (novo) vs %.4f (antigo)\n", mean(theta$pr2), mean(theta_old$pr2)))

# =============================================================================
# Backup e sobrescrita dos nomes canonicos
# =============================================================================
file.copy(file.path(DATA, "theta_universo_completo.csv"),
          file.path(DATA, "theta_universo_completo_ANTES_HHI.csv.bak"), overwrite = TRUE)
file.copy(file.path(DATA, "peso_pred_universo_completo.csv"),
          file.path(DATA, "peso_pred_universo_completo_ANTES_HHI.csv.bak"), overwrite = TRUE)
file.copy(file.path(DATA, "theta_media_ativo_universo_completo.csv"),
          file.path(DATA, "theta_media_ativo_universo_completo_ANTES_HHI.csv.bak"), overwrite = TRUE)

fwrite(theta, file.path(DATA, "theta_universo_completo.csv"))
fwrite(peso_pred_full, file.path(DATA, "peso_pred_universo_completo.csv"))
fwrite(avg, file.path(DATA, "theta_media_ativo_universo_completo.csv"))
cat("\nOK - theta/peso_pred/theta_media_ativo_universo_completo.csv sobrescritos com 6 caracteristicas (backup .bak salvo)\n")
