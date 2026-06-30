# =============================================================================
# 27_pa_robustness.R   — ROBUSTEZ do PASSO 3 (ajuste parcial)
#
# Reestima lambda em dw = lambda*d_t + u, sempre por VI (cluster por fundo),
# em 4 especificacoes:
#   (B)  baseline h=1, instrumento d_{t-1}                  (= R/26)
#   (FE) baseline h=1 + EFEITO-FIXO de fundo (within), instr. d_{t-1}
#   (I2) baseline h=1, instrumento ALTERNATIVO d_{t-2}
#   (H3) horizonte 3 meses: dw3 = w_{t+3}-w_t, instr. d_{t-1}
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq / aum_num][, ym := ano*100L + mes]
d <- d[!is.na(flow_aum)]
d[, z_aum := (l_aum-mean(l_aum))/sd(l_aum)][, z_cot := (l_cot-mean(l_cot))/sd(l_cot)]

# peso desejado (logit por mes) e distancia
d[, w_star := NA_real_]
for (mm in sort(unique(d$ym))) {
  ix <- d$ym == mm
  fg <- suppressWarnings(glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum,
                             family = quasibinomial(link="logit"), data = d[ix]))
  d[ix, w_star := fitted(fg)]
}
d[, dist := w_star - peso_vale3]

# aritmetica de meses (offset em meses-calendario)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

look <- function(col) d[, .(cod_fundo, ymj = ym, val = get(col))]
join <- function(M, key_ym, col, newname) {
  z <- look(col); M2 <- merge(M, z, by.x = c("cod_fundo", key_ym), by.y = c("cod_fundo","ymj"), all.x = TRUE)
  setnames(M2, "val", newname); M2
}
M <- copy(d)
M[, ym_n1 := addm(ym, 1L)][, ym_n3 := addm(ym, 3L)][, ym_p1 := addm(ym, -1L)][, ym_p2 := addm(ym, -2L)]
M <- join(M, "ym_n1", "peso_vale3", "peso_n1")
M <- join(M, "ym_n3", "peso_vale3", "peso_n3")
M <- join(M, "ym_p1", "dist", "dist_l1")
M <- join(M, "ym_p2", "dist", "dist_l2")
M[, dw1 := peso_n1 - peso_vale3][, dw3 := peso_n3 - peso_vale3]

# --- VI manual com erro-padrao agrupado por fundo ---
cl_meat <- function(Z, u, cl) {
  k <- ncol(Z); Mt <- matrix(0,k,k)
  for (g in unique(cl)) { i <- cl==g; s <- crossprod(Z[i,,drop=FALSE], u[i]); Mt <- Mt + s %*% t(s) }
  (length(unique(cl))/(length(unique(cl))-1)) * Mt
}
iv_est <- function(y, xend, z, cl, intercept = TRUE) {
  X <- if (intercept) cbind(1, xend) else cbind(xend)
  Z <- if (intercept) cbind(1, z)    else cbind(z)
  b <- solve(crossprod(Z, X), crossprod(Z, y)); u <- as.numeric(y - X %*% b)
  B <- solve(crossprod(Z, X)); V <- B %*% cl_meat(Z, u, cl) %*% t(B)
  j <- if (intercept) 2L else 1L; se <- sqrt(diag(V))[j]; lam <- b[j]
  data.table(lambda = lam, se = se, t = lam/se, p = 2*pnorm(-abs(lam/se)), n = length(y))
}
demean <- function(x, g) x - ave(x, g, FUN = function(z) mean(z, na.rm=TRUE))

# (B) baseline h=1, instr d_{t-1}
B  <- M[is.finite(dw1) & is.finite(dist) & is.finite(dist_l1)]
rB <- iv_est(B$dw1, B$dist, B$dist_l1, B$cod_fundo)

# (FE) within fundo + instr d_{t-1}
F0 <- B[, .(cod_fundo, dw1, dist, dist_l1)]
F0[, `:=`(dw1d = demean(dw1, cod_fundo), distd = demean(dist, cod_fundo), l1d = demean(dist_l1, cod_fundo))]
rFE <- iv_est(F0$dw1d, F0$distd, F0$l1d, F0$cod_fundo, intercept = FALSE)

# (I2) instrumento alternativo d_{t-2}
A2 <- M[is.finite(dw1) & is.finite(dist) & is.finite(dist_l2)]
rI2 <- iv_est(A2$dw1, A2$dist, A2$dist_l2, A2$cod_fundo)

# (H3) horizonte 3 meses
H3 <- M[is.finite(dw3) & is.finite(dist) & is.finite(dist_l1)]
rH3 <- iv_est(H3$dw3, H3$dist, H3$dist_l1, H3$cod_fundo)

res <- rbindlist(list(
  cbind(spec = "B  h=1, instr d_{t-1} (baseline)", rB),
  cbind(spec = "FE h=1 + efeito-fixo de fundo",    rFE),
  cbind(spec = "I2 h=1, instr alternativo d_{t-2}",rI2),
  cbind(spec = "H3 horizonte 3 meses",             rH3)))

cat("===== ROBUSTEZ do AJUSTE PARCIAL (lambda por VI, EP agrupado por fundo) =====\n")
print(res[, .(spec, lambda = round(lambda,4), se = round(se,4), t = round(t,1),
              p = signif(p,2), n)])
cat("\nReferencia: ajuste acumulado esperado a 3 meses se lambda_1=0,041 e alvo estavel:",
    round(1-(1-0.0413)^3,3), "\n")
cat("Meia-vida implicada (baseline VI):", round(log(0.5)/log(1-rB$lambda),1), "meses\n")
fwrite(res, file.path(REPO, "data/processed/reg27_pa_robustness.csv"))
cat("\nOK - salvo em data/processed/reg27_pa_robustness.csv\n")
