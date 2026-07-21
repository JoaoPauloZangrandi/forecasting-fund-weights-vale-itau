# =============================================================================
# 15_recompute_downstream.R  (v2 OFICIAL)
#
# Recomputa TUDO que depende de theta (erro e, alvo w*, ajuste parcial,
# in/out-of-sample) usando o novo theta_logit (5 caracteristicas,
# predeterminado, 59 meses) do R/13.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_predeterminado.csv"))
d[, l_aum := log(aum_prev)]
d[, l_cot := log(cotistas_prev)]
d[, flow_aum := fluxo_prev / aum_prev]
d <- d[is.finite(l_aum) & is.finite(l_cot) & is.finite(flow_aum) & is.finite(beta_fundo)]
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
g <- function(zz) 1/(1+exp(-zz))

theta <- fread(file.path(REPO, "v2 OFICIAL/data/theta_logit_mensal_v2.csv"))
meses <- sort(unique(d$ym))

# ---- erro e (Etapa 1, dentro da amostra) ------------------------------------
out <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]; th <- theta[ym == t]
  if (nrow(dt) < 15 || nrow(th) == 0) next
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)]
  idx <- th$alpha + th$b_aum*dt$z_aum + th$b_cot*dt$z_cot + th$b_fic*dt$is_fic +
         th$b_flow*dt$flow_aum + th$b_betaf*dt$z_betaf
  dt[, peso_prev := g(idx)]
  dt[, e := peso_vale3 - peso_prev]
  out[[i]] <- dt[, .(cod_fundo, ym, peso_vale3, peso_prev, e)]
}
E <- rbindlist(out)
cat("Erro e: n =", nrow(E), "| media =", round(mean(E$e),6), "| dp =", round(sd(E$e),6), "\n")
por_mes_e <- E[, .(media_e = mean(e), dp_e = sd(e), n = .N), by = ym]; setorder(por_mes_e, ym)

fwrite(E, file.path(REPO, "v2 OFICIAL/data/v2_erro_e.csv"))
fwrite(por_mes_e, file.path(REPO, "v2 OFICIAL/data/v2_erro_e_por_mes.csv"))

# ---- ajuste parcial: d = w*-w, dw = w_{t+1}-w_t, lambda por MQO -------------
addm <- function(ym,k){ tot<-(ym%/%100L)*12L+(ym%%100L-1L)+k; (tot%/%12L)*100L+(tot%%12L)+1L }
E[, d := peso_prev - peso_vale3]
E[, ym_next := addm(ym,1L)]
nxt <- E[, .(cod_fundo, ym_next_key = ym, peso_next = peso_vale3)]
M <- merge(E, nxt, by.x = c("cod_fundo","ym_next"), by.y = c("cod_fundo","ym_next_key"))
M[, dw := peso_next - peso_vale3]
cat("\nAjuste parcial: pares (t,t+1) disponiveis =", nrow(M), "\n")

fit <- lm(dw ~ 0 + d, data = M)
lam <- coef(fit)["d"]; se <- summary(fit)$coefficients["d","Std. Error"]
cat(sprintf("lambda = %.4f | EP = %.4f | t = %.2f | R2 = %.4f | n = %d\n",
            lam, se, lam/se, summary(fit)$r.squared, nobs(fit)))
M[, u := dw - lam*d]
cat(sprintf("Erro u: media=%.6f dp=%.6f\n", mean(M$u), sd(M$u)))
por_mes_u <- M[, .(media_u = mean(u), dp_u = sd(u), n = .N), by = ym]; setorder(por_mes_u, ym)

fwrite(M, file.path(REPO, "v2 OFICIAL/data/v2_ajuste_parcial.csv"))
fwrite(por_mes_u, file.path(REPO, "v2 OFICIAL/data/v2_ajuste_parcial_por_mes.csv"))

# ---- in/out-of-sample: mesmo corte 2020-01, dentro dos meses cobertos ------
meses_ord <- sort(unique(M$ym))
cat("\nMeses cobertos pelo painel de ajuste parcial:", min(meses_ord), "-", max(meses_ord), "\n")
CORTE <- 202001L
treino <- M[ym < CORTE]; teste <- M[ym >= CORTE & ym < 202112L]
cat("Treino:", nrow(treino), "obs (", min(treino$ym), "-", max(treino$ym), ") | Teste:", nrow(teste),
    "obs (", min(teste$ym), "-", max(teste$ym), ")\n")

fit_tr <- lm(dw ~ 0 + d, data = treino); lam_tr <- coef(fit_tr)["d"]
cat("lambda (treino) =", round(lam_tr,4), "\n")

rmse <- function(x) sqrt(mean(x^2)); mae <- function(x) mean(abs(x))
teste[, erro_ajuste := dw - lam_tr*d]; teste[, erro_naive := dw]
treino[, erro_ajuste := dw - lam_tr*d]

cat(sprintf("\nRMSE treino: ajuste=%.5f naive=%.5f\n", rmse(treino$erro_ajuste), rmse(treino$dw)))
cat(sprintf("RMSE teste : ajuste=%.5f naive=%.5f | MAE ajuste=%.5f naive=%.5f\n",
            rmse(teste$erro_ajuste), rmse(teste$erro_naive), mae(teste$erro_ajuste), mae(teste$erro_naive)))

M[, dw_prev_ajuste := lam_tr * d]
por_mes_completo <- M[, .(rmse_ajuste = rmse(dw - lam_tr*d), rmse_naive = rmse(dw)), by = ym]
setorder(por_mes_completo, ym)

fwrite(por_mes_completo, file.path(REPO, "v2 OFICIAL/data/v2_oos_por_mes.csv"))
saveRDS(list(lam_treino = lam_tr, corte = CORTE), file.path(REPO, "v2 OFICIAL/data/v2_oos_meta.rds"))
cat("\nOK - tudo recomputado e salvo em 'v2 OFICIAL/data/v2_*.csv'\n")
