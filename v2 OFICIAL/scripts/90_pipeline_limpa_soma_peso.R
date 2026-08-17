# =============================================================================
# 90_pipeline_limpa_soma_peso.R  (v2 OFICIAL)
#
# Corrige a contaminacao encontrada no script 89: 339 pares fundo-mes
# (0,4% da base) com soma de peso > 1,10 -- ate 498% do patrimonio em um
# caso (fundo 292826, Vinci Partners, exatamente o exemplo usado na
# Figura 11 do TCC_final.tex). Confirmado na fonte bruta da CVM (CDA),
# nao e bug de merge -- provavel erro de reporte do administrador,
# concentrado em fundos em liquidacao acelerada.
#
# Refaz TODA a cadeia relevante (Etapa 1 cross-section por celula, erro
# multiativo, Etapa 3 h=1/3/6/12) excluindo esses fundo-mes ANTES de
# qualquer regressao ou calculo de erro, com sufixo "_limpo" -- os
# arquivos originais NAO sao sobrescritos, pra permitir comparar
# antes/depois e confirmar o quanto (se algo) muda.
# RODAR COM CAMINHO ABSOLUTO. Demorado (refaz milhares de regressoes
# logisticas por celula ativo-mes) -- rodar em background.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
LIM <- 1.05

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("Painel original:", nrow(d), "linhas |", uniqueN(d$cod_fundo), "fundos\n")

soma <- d[, .(soma_peso = sum(peso)), by = .(cod_fundo, ym)]
bad <- soma[soma_peso > LIM, .(cod_fundo, ym)]
cat(sprintf("Fundo-mes excluidos (soma_peso > %.2f): %d de %d (%.3f%%)\n",
            LIM, nrow(bad), nrow(soma), 100*nrow(bad)/nrow(soma)))

d[, chave := paste(cod_fundo, ym)]
bad[, chave := paste(cod_fundo, ym)]
d_limpo <- d[!chave %in% bad$chave]
d_limpo[, chave := NULL]
cat("Painel limpo:", nrow(d_limpo), "linhas (", nrow(d)-nrow(d_limpo), "removidas ) |",
    uniqueN(d_limpo$cod_fundo), "fundos\n")
fwrite(d_limpo, file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final_limpo.csv"))

# =============================================================================
# Etapa 1 + erro (equivalente aos scripts 30/37, na base limpa)
# =============================================================================
fm_unico <- unique(d_limpo[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)

d_limpo[, z_aum := (l_aum-mu_aum)/sd_aum]
d_limpo[, z_cot := (l_cot-mu_cot)/sd_cot]
d_limpo[, z_betaf := (beta_fundo-mu_bf)/sd_bf]

cnt <- d_limpo[, .N, by = .(ativo, ym)]
celulas <- cnt[N >= 15]
cat("Celulas (ativo,mes) com >=15 holders (base limpa):", nrow(celulas), "\n")

setkey(d_limpo, ativo, ym)
n_nao_convergiu <- 0L
run_cell <- function(av, mm) {
  dm <- d_limpo[.(av, mm)]
  fit <- tryCatch(suppressWarnings(glm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf,
                     family = quasibinomial(link="logit"), data = dm)), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 6) return(NULL)
  if (!fit$converged) { n_nao_convergiu <<- n_nao_convergiu + 1L; return(NULL) }
  data.table(cod_fundo = dm$cod_fundo, gestora_grupo = dm$gestora_grupo, ativo = av, ym = mm,
             peso = dm$peso, peso_pred = fitted(fit), erro = residuals(fit, type = "response"))
}
t0 <- Sys.time()
E <- rbindlist(mapply(run_cell, celulas$ativo, celulas$ym, SIMPLIFY = FALSE))
cat("Tempo Etapa 1/erro:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s |",
    n_nao_convergiu, "celulas nao convergiram | n =", nrow(E), "\n")
fwrite(E, file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo_limpo.csv"))

# =============================================================================
# Etapa 3, h=1,3,6,12 (equivalente aos scripts 42/44, na base limpa)
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
fwrite(R1, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1_limpo.csv"))
fwrite(R3, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3_limpo.csv"))

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
fwrite(G, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte_longo_limpo.csv"))
fwrite(W, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte_limpo.csv"))

# =============================================================================
# Comparacao: o que mudou entre a versao original e a limpa?
# =============================================================================
cat("\n\n===== COMPARACAO: original (contaminado) vs. limpo =====\n")
R1_orig <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
cat(sprintf("RMSE agregado h=1: original=%.6f | limpo=%.6f (dif=%.3f%%)\n",
            rmse(R1_orig$erro_oos), rmse(R1$erro_oos),
            100*(rmse(R1$erro_oos)-rmse(R1_orig$erro_oos))/rmse(R1_orig$erro_oos)))

W_orig <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))
comp <- merge(W_orig[, .(gestora_grupo, rmse_h1_orig=rmse_h1, rmse_h3_orig=rmse_h3)],
              W[, .(gestora_grupo, rmse_h1_limpo=rmse_h1, rmse_h3_limpo=rmse_h3)], by="gestora_grupo")
comp[, dif_pct_h1 := 100*(rmse_h1_limpo-rmse_h1_orig)/rmse_h1_orig]
comp[, dif_pct_h3 := 100*(rmse_h3_limpo-rmse_h3_orig)/rmse_h3_orig]
setorder(comp, -abs(dif_pct_h1))
cat("\nGestoras com maior mudanca no RMSE h=1 (limpo vs original):\n")
print(comp[1:10, .(gestora_grupo, rmse_h1_orig=round(rmse_h1_orig,5), rmse_h1_limpo=round(rmse_h1_limpo,5),
                    dif_pct_h1=round(dif_pct_h1,2))])

cat("\n\nRanking do fundo com maior RMSE h=3, base LIMPA (pra substituir a Figura 11):\n")
por_fundo_h3 <- R3[, .(n=.N, rmse_h3 = rmse(erro_oos)), by = .(cod_fundo, gestora_grupo)][n>=6]
setorder(por_fundo_h3, -rmse_h3)
print(head(por_fundo_h3, 10))

fwrite(comp, file.path(REPO, "v2 OFICIAL/data/comparacao_limpo_vs_original.csv"))
cat("\nOK - pipeline limpa completa\n")
