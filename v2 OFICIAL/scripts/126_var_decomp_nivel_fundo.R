# =============================================================================
# 126_var_decomp_nivel_fundo.R  (v2 OFICIAL)
#
# PARADA 2 da extensao. A peca conceitual mais importante do trabalho.
#
# Quatro perguntas:
#   A4) Var(log rmse_i) e' ENTRE gestoras ou DENTRO de gestora? Se for
#       majoritariamente dentro, o TCC atacou a fatia menor do problema.
#   R1) Confiabilidade (rho) de sigma_g e de sigma_i, por dois metodos
#       independentes: split impar/par + Spearman-Brown, e bootstrap.
#   R2) R2_sinal = R2/rho. O R2=0,496 do script 95 esta' ATENUADO por ruido
#       de medida na dependente; o teto alcancavel e' rho, nao 1.
#   L1) LOO-R2 (leave-one-gestora-out) da regressao do script 95. O R2
#       ajustado de 0,405 com n=40 e 6 regressores pode nao sobreviver
#       fora da amostra.
#
# CONTROLE OBRIGATORIO: antes de qualquer numero novo, reproduzir
# R2=0,496 e ajustado=0,405 do script 95. Se nao bater, parar.
#
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
})

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")
CORTE_TREINO <- 202001L
PISO_POEIRA  <- 0.001
set.seed(20260915L)
B_BOOT <- 400L

out <- list()
add <- function(bloco, metrica, valor) {
  out[[length(out) + 1L]] <<- data.table(bloco = bloco, metrica = metrica,
                                         valor = as.character(valor))
}

cat("=============================================================\n")
cat("126 - Decomposicao de variancia e confiabilidade\n")
cat("=============================================================\n\n")

# --- base -------------------------------------------------------------------
pp <- fread(file.path(DD, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ym","ativo","peso","gestora_grupo",
                       "is_fic","beta_fundo","l_aum","l_cot","flow_aum"),
            showProgress = FALSE)
pp[, cod_fundo := as.character(cod_fundo)]

cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
cel_ok <- cel[peso_mediano >= PISO_POEIRA, .(cod_fundo, ativo)]

h1 <- fread(file.path(DD, "etapa3_multiativo_h1.csv"),
            select = c("cod_fundo","ativo","ym","gestora_grupo","erro_oos"),
            showProgress = FALSE)
h1[, cod_fundo := as.character(cod_fundo)]
h1f <- merge(h1, cel_ok, by = c("cod_fundo","ativo"))
rm(h1); invisible(gc())
cat("Base h1 pos-filtro:", nrow(h1f), "obs |", uniqueN(h1f$cod_fundo), "fundos |",
    uniqueN(h1f$gestora_grupo), "gestoras\n\n")

# =============================================================================
# CONTROLE: reproduzir o script 95 exatamente
# =============================================================================
cat("--- CONTROLE: reproduzindo o script 95 ---\n")
dp_gestora <- h1f[, .(dp_erro = sd(erro_oos), n = .N), by = gestora_grupo]
dp_gestora <- dp_gestora[n >= 100]

pp_treino <- pp[ym < CORTE_TREINO]
carac_g <- pp_treino[, .(l_aum = mean(l_aum, na.rm=TRUE), l_cot = mean(l_cot, na.rm=TRUE),
                         beta_fundo = mean(beta_fundo, na.rm=TRUE),
                         flow_aum = mean(flow_aum, na.rm=TRUE),
                         pct_fic = mean(is_fic, na.rm=TRUE)), by = gestora_grupo]
hhi_fm  <- pp_treino[, .(hhi = sum(peso^2)), by = .(cod_fundo, ym, gestora_grupo)]
hhi_g   <- hhi_fm[, .(hhi_medio = mean(hhi, na.rm=TRUE)), by = gestora_grupo]

d95 <- merge(dp_gestora, carac_g, by = "gestora_grupo")
d95 <- merge(d95, hhi_g, by = "gestora_grupo")
d95 <- d95[is.finite(l_aum) & is.finite(l_cot) & is.finite(beta_fundo) &
           is.finite(flow_aum) & is.finite(hhi_medio)]

f95 <- lm(dp_erro ~ beta_fundo + l_aum + l_cot + pct_fic + flow_aum + hhi_medio, data = d95)
r2_95  <- summary(f95)$r.squared
r2a_95 <- summary(f95)$adj.r.squared
cat(sprintf("n gestoras = %d | R2 = %.4f | R2 ajustado = %.4f\n",
            nrow(d95), r2_95, r2a_95))
ok_controle <- abs(r2_95 - 0.496) < 0.01 && abs(r2a_95 - 0.405) < 0.01
cat(sprintf("Bate com o TCC (0,496 / 0,405)? %s\n\n", ifelse(ok_controle, "SIM", "NAO -- PARAR")))
add("controle", "n_gestoras_95", nrow(d95))
add("controle", "r2_95", round(r2_95, 4))
add("controle", "r2_aj_95", round(r2a_95, 4))
add("controle", "reproduz_tcc", ok_controle)

# =============================================================================
# A4: variancia ENTRE vs DENTRO de gestora, no nivel de fundo
# =============================================================================
cat("--- A4: Var(log rmse_i) entre vs. dentro de gestora ---\n")
fundo <- h1f[, .(rmse_i = sqrt(mean(erro_oos^2)),
                 sd_i   = sd(erro_oos),
                 n_obs  = .N,
                 n_mes  = uniqueN(ym)), by = .(cod_fundo, gestora_grupo)]
fundo <- fundo[n_obs >= 24 & n_mes >= 6 & rmse_i > 0]
fundo[, l_rmse := log(rmse_i)]
cat(sprintf("Fundos na amostra do plano: %d | gestoras: %d\n",
            nrow(fundo), uniqueN(fundo$gestora_grupo)))

gm <- fundo[, .(m_g = mean(l_rmse), n_g = .N), by = gestora_grupo]
fundo <- merge(fundo, gm, by = "gestora_grupo")
var_total  <- var(fundo$l_rmse)
media_geral <- mean(fundo$l_rmse)
ss_total   <- sum((fundo$l_rmse - media_geral)^2)
ss_entre   <- sum((fundo$m_g    - media_geral)^2)   # cada fundo carrega o desvio da sua gestora
ss_dentro  <- sum((fundo$l_rmse - fundo$m_g)^2)
stopifnot(abs(ss_total - (ss_entre + ss_dentro)) < 1e-6 * ss_total)
cat(sprintf("Var total de log(rmse_i): %.4f\n", var_total))
cat(sprintf("  ENTRE gestoras : %.1f%%\n", 100 * ss_entre / ss_total))
cat(sprintf("  DENTRO gestora : %.1f%%\n", 100 * ss_dentro / ss_total))
cat(sprintf("Amplitude: rmse min = %.6f | max = %.6f | razao = %.1fx\n",
            min(fundo$rmse_i), max(fundo$rmse_i), max(fundo$rmse_i)/min(fundo$rmse_i)))
add("A4", "n_fundos", nrow(fundo))
add("A4", "var_total_l_rmse", round(var_total, 5))
add("A4", "pct_entre_gestora", round(100 * ss_entre / ss_total, 2))
add("A4", "pct_dentro_gestora", round(100 * ss_dentro / ss_total, 2))
add("A4", "razao_max_min_rmse", round(max(fundo$rmse_i)/min(fundo$rmse_i), 1))

# =============================================================================
# R1: confiabilidade por split impar/par + Spearman-Brown
# =============================================================================
cat("\n--- R1a: confiabilidade por split IMPAR/PAR (Spearman-Brown) ---\n")
h1f[, mes_idx := as.integer(factor(ym))]
h1f[, metade_ip := ifelse(mes_idx %% 2L == 1L, "impar", "par")]
# tambem o split cronologico, para comparar com o script 86
meses <- sort(unique(h1f$ym)); corte_meio <- meses[ceiling(length(meses)/2)]
h1f[, metade_cr := ifelse(ym <= corte_meio, "primeira", "segunda")]

sb <- function(r) 2*r/(1+r)

rho_split <- function(dt, unidade, col_metade, min_n = 30L) {
  s <- dt[, .(dp = sd(erro_oos), n = .N), by = c(unidade, col_metade)]
  w <- dcast(s, as.formula(paste(unidade, "~", col_metade)), value.var = c("dp","n"))
  nm <- names(w); c1 <- nm[2]; c2 <- nm[3]; n1 <- nm[4]; n2 <- nm[5]
  w <- w[is.finite(get(c1)) & is.finite(get(c2)) & get(n1) >= min_n & get(n2) >= min_n]
  r <- cor(w[[c1]], w[[c2]])
  list(r_half = r, rho = sb(r), n = nrow(w))
}

for (u in c("gestora_grupo", "cod_fundo")) {
  for (cm in c("metade_ip", "metade_cr")) {
    z <- rho_split(h1f, u, cm)
    rot <- ifelse(cm == "metade_ip", "impar/par", "cronologico")
    cat(sprintf("  %-14s %-12s: r_half = %.4f | rho = %.4f | n = %d\n",
                u, rot, z$r_half, z$rho, z$n))
    add("R1_split", paste0("rho_", u, "_", cm), round(z$rho, 4))
    add("R1_split", paste0("rhalf_", u, "_", cm), round(z$r_half, 4))
    add("R1_split", paste0("n_", u, "_", cm), z$n)
  }
}

# =============================================================================
# R1b: confiabilidade por BOOTSTRAP em blocos
# =============================================================================
cat("\n--- R1b: confiabilidade por BOOTSTRAP em blocos ---\n")
cat("    (gestora: reamostra FUNDOS dentro da gestora; fundo: reamostra MESES)\n")

# --- gestora: reamostra fundos dentro de cada gestora
gk <- h1f[, .(cod_fundo, gestora_grupo, erro_oos)]
setkey(gk, gestora_grupo, cod_fundo)
fundos_por_g <- gk[, .(fundos = list(unique(cod_fundo))), by = gestora_grupo]
obs_por_fundo <- split(gk$erro_oos, gk$cod_fundo)

boot_sd_gestora <- function(g) {
  fs <- fundos_por_g[gestora_grupo == g, fundos][[1]]
  replicate(B_BOOT, {
    sel <- sample(fs, length(fs), replace = TRUE)
    sd(unlist(obs_por_fundo[sel], use.names = FALSE))
  })
}
gs <- dp_gestora$gestora_grupo
vb <- sapply(gs, function(g) var(boot_sd_gestora(g)))
var_entre_g <- var(dp_gestora$dp_erro)
rho_boot_g  <- 1 - mean(vb) / var_entre_g
cat(sprintf("  gestora: Var_entre = %.3e | Var_boot media = %.3e | rho_boot = %.4f\n",
            var_entre_g, mean(vb), rho_boot_g))
add("R1_boot", "rho_boot_gestora", round(rho_boot_g, 4))

# --- fundo: reamostra meses dentro de cada fundo (bloco = mes)
fk <- h1f[cod_fundo %in% fundo$cod_fundo, .(cod_fundo, ym, erro_oos)]
setkey(fk, cod_fundo, ym)
amostra_f <- sample(unique(fk$cod_fundo), min(400L, uniqueN(fk$cod_fundo)))
vbf <- sapply(amostra_f, function(f) {
  sub <- fk[.(f)]
  ms  <- unique(sub$ym)
  bl  <- split(sub$erro_oos, sub$ym)
  var(replicate(150L, {
    sel <- sample(ms, length(ms), replace = TRUE)
    sd(unlist(bl[as.character(sel)], use.names = FALSE))
  }))
})
var_entre_f <- var(fundo$sd_i)
rho_boot_f  <- 1 - mean(vbf, na.rm = TRUE) / var_entre_f
cat(sprintf("  fundo  : Var_entre = %.3e | Var_boot media = %.3e | rho_boot = %.4f  (subamostra de %d fundos)\n",
            var_entre_f, mean(vbf, na.rm=TRUE), rho_boot_f, length(amostra_f)))
add("R1_boot", "rho_boot_fundo", round(rho_boot_f, 4))

# =============================================================================
# R2: R2_sinal
# =============================================================================
cat("\n--- R2: R2 corrigido por atenuacao (R2_sinal = R2/rho) ---\n")
rho_g_ip <- as.numeric(rho_split(h1f, "gestora_grupo", "metade_ip")$rho)
rho_g_cr <- as.numeric(rho_split(h1f, "gestora_grupo", "metade_cr")$rho)
for (nm in c("impar/par", "cronologico", "bootstrap")) {
  rr <- switch(nm, "impar/par" = rho_g_ip, "cronologico" = rho_g_cr, "bootstrap" = rho_boot_g)
  cat(sprintf("  rho (%-11s) = %.4f  ->  R2_sinal = %.4f | R2aj_sinal = %.4f\n",
              nm, rr, r2_95/rr, r2a_95/rr))
  add("R2_sinal", paste0("r2_sinal_", gsub("[^a-z]", "", nm)), round(r2_95/rr, 4))
}

# =============================================================================
# L1: LOO-R2 da regressao do script 95
# =============================================================================
cat("\n--- L1: LOO-R2 (leave-one-gestora-out) da regressao do script 95 ---\n")
frm <- dp_erro ~ beta_fundo + l_aum + l_cot + pct_fic + flow_aum + hhi_medio
pred_loo <- numeric(nrow(d95))
for (i in seq_len(nrow(d95))) {
  fit <- lm(frm, data = d95[-i])
  pred_loo[i] <- predict(fit, newdata = d95[i])
}
y <- d95$dp_erro
sse_loo <- sum((y - pred_loo)^2)
sst     <- sum((y - mean(y))^2)
r2_loo  <- 1 - sse_loo/sst
cat(sprintf("R2 in-sample = %.4f | R2 ajustado = %.4f | R2 LOO = %.4f\n",
            r2_95, r2a_95, r2_loo))
cat(sprintf("Sobrevive fora da amostra? %s\n", ifelse(r2_loo > 0, "SIM (positivo)", "NAO (negativo)")))
add("L1_loo", "r2_loo_95", round(r2_loo, 4))

# so com a concentracao (o unico coeficiente significativo)
pred_hhi <- numeric(nrow(d95))
for (i in seq_len(nrow(d95))) {
  fit <- lm(dp_erro ~ hhi_medio, data = d95[-i])
  pred_hhi[i] <- predict(fit, newdata = d95[i])
}
r2_loo_hhi <- 1 - sum((y - pred_hhi)^2)/sst
f_hhi <- lm(dp_erro ~ hhi_medio, data = d95)
cat(sprintf("So com concentracao: R2 in-sample = %.4f | R2 LOO = %.4f\n",
            summary(f_hhi)$r.squared, r2_loo_hhi))
add("L1_loo", "r2_insample_so_hhi", round(summary(f_hhi)$r.squared, 4))
add("L1_loo", "r2_loo_so_hhi", round(r2_loo_hhi, 4))

cat("\n>>> LEITURA: se o LOO do modelo de 6 regressores for MENOR que o de 1\n")
cat(">>> regressor, os outros 5 estao custando mais em variancia do que\n")
cat(">>> entregam em ajuste -- e' o custo de n=40.\n")

# --- figura ------------------------------------------------------------------
pdf(file.path(FIG, "fig_ext_entre_dentro_gestora.pdf"), width = 7, height = 5.5)
par(mar = c(4.2, 4.2, 3, 1))
bx <- boxplot(l_rmse ~ reorder(gestora_grupo, l_rmse, median), data = fundo,
              horizontal = FALSE, las = 2, cex.axis = 0.45, outline = FALSE,
              col = "#3B6E9E33", border = "#3B6E9E", xlab = "", ylab = "log(RMSE) do fundo",
              main = sprintf("Erro por fundo, agrupado por gestora\n%.0f%% da variancia e DENTRO da gestora",
                             100 * ss_dentro / ss_total))
dev.off()
cat("\nOK - fig_ext_entre_dentro_gestora.pdf salva\n")

fwrite(rbindlist(out), file.path(DD, "var_fundo_entre_dentro.csv"))
fwrite(fundo, file.path(DD, "erro_por_fundo_teste.csv"))
fwrite(d95, file.path(DD, "replica_dados_script95.csv"))
cat("OK - var_fundo_entre_dentro.csv, erro_por_fundo_teste.csv, replica_dados_script95.csv\n")
