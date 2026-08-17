# =============================================================================
# 96_pipeline_hhi_etapa1.R  (v2 OFICIAL)
#
# Inclui HHI_resto (concentracao da carteira do fundo, EXCLUINDO o ativo-
# alvo, pra evitar circularidade) como 6a caracteristica formal da Etapa 1
# -- decisao tomada apos o piloto (_piloto_hhi_etapa1_v2.R, amostra
# aleatoria de 100 celulas ativo-mes, sem vies): 77% das celulas com
# z_hhi significativo, sinal positivo em 75/77, ganho medio de pseudo-R2
# de 0,080.
#
# HHI_resto(fundo,ativo,mes) = HHI_total(fundo,mes) - peso(fundo,ativo,mes)^2
#
# Clonado de 90_pipeline_limpa_soma_peso.R (o script que hoje efetivamente
# produz os arquivos "oficiais" -- ja parte do painel POS-limpeza de
# soma_peso>105%, nao precisa refazer essa parte). Roda a cadeia completa
# (Etapa 1 por celula ativo-mes + Etapa 3 h=1,3,6,12 + agregacao por
# gestora) com a formula de 6 caracteristicas, gravando com sufixo
# "_comHHI" -- nao sobrescreve nada ainda, serve pra comparar antes/depois.
# RODAR COM CAMINHO ABSOLUTO. Demorado (milhares de regressoes logisticas
# por celula ativo-mes) -- rodar em background.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("Painel (ja limpo de soma_peso>105%):", nrow(d), "linhas |", uniqueN(d$cod_fundo), "fundos\n")

# =============================================================================
# HHI_resto
# =============================================================================
hhi_total <- d[, .(hhi_total = sum(peso^2)), by = .(cod_fundo, ym)]
d <- merge(d, hhi_total, by = c("cod_fundo","ym"))
d[, hhi_resto := hhi_total - peso^2]
cat("HHI_resto: min =", min(d$hhi_resto), "| max =", max(d$hhi_resto), "\n")
stopifnot(all(d$hhi_resto >= -1e-9))  # nunca deveria ser negativo

# =============================================================================
# Etapa 1 + erro, 6 caracteristicas (equivalente aos scripts 30/37/90, +HHI)
# =============================================================================
fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)
mu_hhi <- mean(d$hhi_resto); sd_hhi <- sd(d$hhi_resto)  # hhi_resto varia por (fundo,ativo,mes), nao so (fundo,mes)

d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]
d[, z_hhi := (hhi_resto-mu_hhi)/sd_hhi]

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
  pr2 <- 1 - fit$deviance/fit$null.deviance
  resultados_theta[[i]] <<- data.table(ativo = av, ym = mm, n = nrow(dm), pr2 = pr2,
             intercepto = cf[1], b_aum = cf["z_aum"], b_cot = cf["z_cot"], b_fic = cf["is_fic"],
             b_flow = cf["flow_aum"], b_betaf = cf["z_betaf"], b_hhi = cf["z_hhi"],
             t_hhi = s["z_hhi","t value"], sig_hhi = abs(s["z_hhi","t value"]) > 1.96)
  NULL
}
t0 <- Sys.time()
invisible(lapply(seq_len(nrow(celulas)), run_cell))
E <- rbindlist(resultados_erro)
THETA <- rbindlist(resultados_theta)
cat("Tempo Etapa 1/erro/theta (uma passada, 6 caracteristicas):", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |",
    n_nao_convergiu, "celulas nao convergiram | n_erro =", nrow(E), "| n_theta =", nrow(THETA), "\n")
fwrite(E, file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo_comHHI.csv"))
fwrite(THETA, file.path(REPO, "v2 OFICIAL/data/theta_multiativo_comHHI.csv"))

# =============================================================================
# Etapa 3, h=1,3,6,12 (equivalente aos scripts 42/44/90, na base com HHI)
# =============================================================================
E[, d := peso_pred - peso]
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
rmse <- function(x) sqrt(mean(x^2))

roda_horizonte <- function(h) {
  fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
  atual <- E[, .(cod_fundo, gestora_grupo, ativo, ym, d, peso, ym_fut = addm(ym, h))]
  M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"), by.y = c("cod_fundo","ativo","ym_fut_key"))
  M[, dw := peso_fut - peso]
  treino <- M[ym < CORTE]; teste <- M[ym >= CORTE & ym_fut <= 202607L]
  fit <- lm(dw ~ 0 + d, data = treino); lam <- unname(coef(fit)["d"])
  teste[, erro_oos := dw - lam * d]; teste[, erro_naive := dw]; teste[, horizonte := h]
  cat(sprintf("h=%d: treino %d | teste %d | lambda=%.4f | RMSE ajuste=%.6f | RMSE naive=%.6f\n",
              h, nrow(treino), nrow(teste), lam, rmse(teste$erro_oos), rmse(teste$erro_naive)))
  teste
}
R1 <- roda_horizonte(1); R3 <- roda_horizonte(3); R6 <- roda_horizonte(6); R12 <- roda_horizonte(12)
fwrite(R1, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1_comHHI.csv"))
fwrite(R3, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3_comHHI.csv"))

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
fwrite(G, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte_longo_comHHI.csv"))
fwrite(W, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte_comHHI.csv"))

# =============================================================================
# Comparacao: modelo com 5 caracteristicas (atual) vs. com 6 (HHI incluido)
# =============================================================================
cat("\n\n===== COMPARACAO: 5 caracteristicas (atual) vs. 6 (com HHI_resto) =====\n")
R1_orig <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
R3_orig <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))
cat(sprintf("RMSE agregado h=1: 5-carac=%.6f | 6-carac(HHI)=%.6f (dif=%.3f%%)\n",
            rmse(R1_orig$erro_oos), rmse(R1$erro_oos),
            100*(rmse(R1$erro_oos)-rmse(R1_orig$erro_oos))/rmse(R1_orig$erro_oos)))
cat(sprintf("RMSE agregado h=3: 5-carac=%.6f | 6-carac(HHI)=%.6f (dif=%.3f%%)\n",
            rmse(R3_orig$erro_oos), rmse(R3$erro_oos),
            100*(rmse(R3$erro_oos)-rmse(R3_orig$erro_oos))/rmse(R3_orig$erro_oos)))

cat(sprintf("\nCelulas com z_hhi significativo (|t|>1,96): %d de %d (%.1f%%)\n",
            sum(THETA$sig_hhi, na.rm=TRUE), nrow(THETA), 100*mean(THETA$sig_hhi, na.rm=TRUE)))
cat(sprintf("Sinal do coef. de HHI quando significativo: %d positivos, %d negativos\n",
            sum(THETA$sig_hhi & THETA$b_hhi>0, na.rm=TRUE), sum(THETA$sig_hhi & THETA$b_hhi<0, na.rm=TRUE)))
cat(sprintf("Coef. padronizado do HHI (b_hhi): mediana=%.4f | %% positivo (todas celulas)=%.1f%% | pseudo-R2 medio=%.4f\n",
            median(THETA$b_hhi, na.rm=TRUE), 100*mean(THETA$b_hhi>0, na.rm=TRUE), mean(THETA$pr2, na.rm=TRUE)))

W_orig <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))
comp <- merge(W_orig[, .(gestora_grupo, rmse_h1_orig=rmse_h1, rmse_h3_orig=rmse_h3)],
              W[, .(gestora_grupo, rmse_h1_hhi=rmse_h1, rmse_h3_hhi=rmse_h3)], by="gestora_grupo")
comp[, dif_pct_h1 := 100*(rmse_h1_hhi-rmse_h1_orig)/rmse_h1_orig]
comp[, dif_pct_h3 := 100*(rmse_h3_hhi-rmse_h3_orig)/rmse_h3_orig]
setorder(comp, dif_pct_h1)
cat("\nGestoras com MAIOR melhora no RMSE h=1 (6-carac vs 5-carac):\n")
print(comp[1:10, .(gestora_grupo, rmse_h1_orig=round(rmse_h1_orig,5), rmse_h1_hhi=round(rmse_h1_hhi,5),
                    dif_pct_h1=round(dif_pct_h1,2))])
cat("\nGestoras com MAIOR piora (ou menor melhora) no RMSE h=1:\n")
print(tail(comp, 10)[, .(gestora_grupo, rmse_h1_orig=round(rmse_h1_orig,5), rmse_h1_hhi=round(rmse_h1_hhi,5),
                    dif_pct_h1=round(dif_pct_h1,2))])

fwrite(comp, file.path(REPO, "v2 OFICIAL/data/comparacao_hhi_vs_original.csv"))
cat("\nOK - pipeline com HHI completa\n")
