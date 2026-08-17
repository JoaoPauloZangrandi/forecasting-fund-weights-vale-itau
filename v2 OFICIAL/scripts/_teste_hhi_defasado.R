# =============================================================================
# _teste_hhi_defasado.R (temporario, NAO documentado no TCC ainda)
#
# Achado da auditoria (12/08/2026): HHI_resto na Etapa 1 (script 96) e
# calculado com o peso do MESMO mes que ele tenta explicar -- diferente das
# outras 5 caracteristicas, todas defasadas pro ultimo dia do mes anterior.
# Joao pediu pra testar a versao DEFASADA (mes t-1) e comparar a forca do
# efeito contra a versao contemporanea ja em uso.
#
# HHI_resto_lag(fundo,ativo,t) = HHI_total(fundo,t-1) - peso(fundo,ativo,t-1)^2
#   -- concentracao do RESTO da carteira no fechamento do mes anterior,
#   excluindo o peso que o proprio ativo-alvo tinha (tambem no mes anterior).
#   Fundos sem nenhum dado de carteira no mes t-1 (ex.: primeiro mes na
#   amostra) ficam sem HHI_total(t-1) -- essas linhas sao descartadas, mesma
#   logica de "caracteristica ausente = fora da amostra" ja usada pras outras
#   5 caracteristicas.
#
# Roda 3 versoes pra comparar: (A) so contemporaneo (ja em uso, script 96),
# (B) so defasado (novo), (C) as duas juntas na mesma regressao (testa se o
# defasado sobrevive/tem poder proprio depois de controlar pelo contemporaneo).
# RODAR COM CAMINHO ABSOLUTO. NAO sobrescreve nada -- so compara.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("Painel:", nrow(d), "linhas\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# ---- (A) HHI contemporaneo (ja em uso) -------------------------------------
hhi_total <- d[, .(hhi_total = sum(peso^2)), by = .(cod_fundo, ym)]
d <- merge(d, hhi_total, by = c("cod_fundo","ym"))
d[, hhi_resto := hhi_total - peso^2]

# ---- (B) HHI defasado (novo) ------------------------------------------------
# hhi_total do mes anterior, remapeado pro mes seguinte (ym_next) pra virar
# uma caracteristica "conhecida no inicio do mes t"
hhi_total_lag <- hhi_total[, .(cod_fundo, ym = addm(ym, 1), hhi_total_lag1 = hhi_total)]
d <- merge(d, hhi_total_lag, by = c("cod_fundo","ym"), all.x = TRUE)

# peso do MESMO ativo no mes anterior (0 se o fundo nao tinha esse ativo)
peso_prev <- d[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev_asset = peso)]
d <- merge(d, peso_prev, by = c("cod_fundo","ativo","ym"), all.x = TRUE)
d[is.na(peso_prev_asset), peso_prev_asset := 0]

d[, hhi_resto_lag := hhi_total_lag1 - peso_prev_asset^2]
n_sem_lag <- sum(is.na(d$hhi_resto_lag))
cat("Linhas sem HHI defasado disponivel (fundo sem dado no mes t-1, ex. primeiro mes):",
    n_sem_lag, "de", nrow(d), sprintf("(%.2f%%)\n", 100*n_sem_lag/nrow(d)))
cat("HHI_resto_lag: min =", min(d$hhi_resto_lag, na.rm=TRUE), "| max =", max(d$hhi_resto_lag, na.rm=TRUE), "\n\n")

# ---- padronizacao (mesma logica do script 96) -------------------------------
fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)
mu_hhi <- mean(d$hhi_resto); sd_hhi <- sd(d$hhi_resto)
mu_hhi_lag <- mean(d$hhi_resto_lag, na.rm=TRUE); sd_hhi_lag <- sd(d$hhi_resto_lag, na.rm=TRUE)

d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]
d[, z_hhi := (hhi_resto-mu_hhi)/sd_hhi]
d[, z_hhi_lag := (hhi_resto_lag-mu_hhi_lag)/sd_hhi_lag]

d_completo <- d[is.finite(z_hhi_lag)]  # amostra comum as 3 versoes (justo pra comparar)
cat("Amostra comum (com HHI defasado disponivel):", nrow(d_completo), "linhas\n\n")

cnt <- d_completo[, .N, by = .(ativo, ym)]
celulas <- cnt[N >= 15]
cat("Celulas (ativo,mes) com >=15 holders, amostra comum:", nrow(celulas), "\n\n")

setkey(d_completo, ativo, ym)

roda_versao <- function(formula_str, nome) {
  n_ok <- 0L; n_naoconv <- 0L
  resultados <- vector("list", nrow(celulas))
  for (i in seq_len(nrow(celulas))) {
    av <- celulas$ativo[i]; mm <- celulas$ym[i]
    dm <- d_completo[.(av, mm)]
    fit <- tryCatch(suppressWarnings(glm(as.formula(formula_str),
                       family = quasibinomial(link="logit"), data = dm)), error = function(e) NULL)
    if (is.null(fit)) next
    if (!fit$converged) { n_naoconv <- n_naoconv + 1L; next }
    s <- summary(fit)$coefficients
    row <- data.table(ativo = av, ym = mm, n = nrow(dm))
    if ("z_hhi" %in% rownames(s)) {
      row[, `:=`(coef_hhi = s["z_hhi","Estimate"], t_hhi = s["z_hhi","t value"])]
    }
    if ("z_hhi_lag" %in% rownames(s)) {
      row[, `:=`(coef_hhi_lag = s["z_hhi_lag","Estimate"], t_hhi_lag = s["z_hhi_lag","t value"])]
    }
    resultados[[i]] <- row
    n_ok <- n_ok + 1L
  }
  R <- rbindlist(resultados, fill = TRUE)
  cat("=====", nome, "=====\n")
  cat(sprintf("Celulas OK: %d | nao convergiram: %d\n", n_ok, n_naoconv))
  if ("t_hhi" %in% names(R)) {
    sig <- abs(R$t_hhi) > 1.96
    cat(sprintf("z_hhi (contemporaneo): significativo em %d/%d (%.1f%%) | positivo quando sig: %d/%d\n",
                sum(sig,na.rm=TRUE), sum(!is.na(R$t_hhi)), 100*mean(sig,na.rm=TRUE),
                sum(sig & R$coef_hhi>0, na.rm=TRUE), sum(sig,na.rm=TRUE)))
  }
  if ("t_hhi_lag" %in% names(R)) {
    sig <- abs(R$t_hhi_lag) > 1.96
    cat(sprintf("z_hhi_lag (defasado): significativo em %d/%d (%.1f%%) | positivo quando sig: %d/%d\n",
                sum(sig,na.rm=TRUE), sum(!is.na(R$t_hhi_lag)), 100*mean(sig,na.rm=TRUE),
                sum(sig & R$coef_hhi_lag>0, na.rm=TRUE), sum(sig,na.rm=TRUE)))
  }
  cat("\n")
  R
}

R_contemp <- roda_versao("peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + z_hhi", "A) SO CONTEMPORANEO")
R_lag     <- roda_versao("peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + z_hhi_lag", "B) SO DEFASADO")
R_ambos   <- roda_versao("peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + z_hhi + z_hhi_lag", "C) OS DOIS JUNTOS")

fwrite(R_contemp, file.path(REPO, "v2 OFICIAL/data/_teste_hhi_A_contemporaneo.csv"))
fwrite(R_lag, file.path(REPO, "v2 OFICIAL/data/_teste_hhi_B_defasado.csv"))
fwrite(R_ambos, file.path(REPO, "v2 OFICIAL/data/_teste_hhi_C_ambos.csv"))
cat("OK\n")
