# =============================================================================
# 104_robustez_clustering_teste_decisivo.R  (v2 OFICIAL -- teste_estrategia)
#
# ROBUSTEZ pedida pelo Joao apos auditoria externa (agent de rigor
# econometrico apontou que 70/71 rodam lm() puro, erro-padrao i.i.d., sem
# clusterizar por ativo/mes -- painel empilhado tem correlacao cross-section
# dentro do mes (fator de mercado comum) e serial dentro do ativo).
#
# NAO substitui 70/71 -- roda por cima dos mesmos dados salvos por eles e
# reestima o mesmo teste com 4 familias de erro-padrao, lado a lado:
#   (1) i.i.d. (o que 70/71 ja reportam, aqui so' pra comparacao)
#   (2) cluster por ativo
#   (3) cluster por mes (ym)
#   (4) cluster two-way (ativo + mes)
#   (5) Fama-MacBeth: regressao cross-section mes a mes, t-test na serie de
#       coeficientes (robusto a qualquer estrutura de correlacao dentro do
#       mes, e a heterogeneidade de variancia entre meses)
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table); library(sandwich); library(lmtest)
})
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

roda_todas_versoes <- function(dados, nome_amostra) {
  cat("\n\n=====================================================================\n")
  cat("Amostra:", nome_amostra, "| n =", nrow(dados), "| ativos =", uniqueN(dados$ativo),
      "| meses =", uniqueN(dados$ym), "\n")
  cat("=====================================================================\n")

  for (sinal in c("fluxo_pct", "fluxo_normal_pct")) {
    rotulo <- if (sinal == "fluxo_pct") "COM eco do lider" else "SEM eco (so' lambda*d)"
    cat("\n--- Sinal", rotulo, "---\n")

    fit <- lm(as.formula(paste("retorno_fut ~", sinal)), data = dados)
    b <- coef(fit)[[sinal]]

    p_iid <- coeftest(fit, vcov. = vcovHC(fit, type = "const"))[sinal, "Pr(>|t|)"]
    se_iid <- coeftest(fit, vcov. = vcovHC(fit, type = "const"))[sinal, "Std. Error"]

    p_ativo <- coeftest(fit, vcov. = vcovCL(fit, cluster = dados$ativo, type = "HC1"))[sinal, "Pr(>|t|)"]
    p_mes   <- coeftest(fit, vcov. = vcovCL(fit, cluster = dados$ym,    type = "HC1"))[sinal, "Pr(>|t|)"]
    p_2way  <- coeftest(fit, vcov. = vcovCL(fit, cluster = dados[, .(ativo, ym)], type = "HC1"))[sinal, "Pr(>|t|)"]

    # Fama-MacBeth: 1 regressao por mes, t-test na serie de coeficientes
    fm <- dados[, if (.N >= 10) {
      f <- tryCatch(lm(as.formula(paste("retorno_fut ~", sinal)), data = .SD), error = function(e) NULL)
      if (is.null(f) || is.na(coef(f)[[sinal]])) NULL else list(coef = coef(f)[[sinal]], n = .N)
    }, by = ym]
    n_meses_fm <- nrow(fm)
    tt <- t.test(fm$coef, mu = 0)

    cat(sprintf("  coef = %.4e | n meses c/ regressao valida (FM) = %d\n", b, n_meses_fm))
    cat(sprintf("  p-valor  i.i.d.=%.4f | cluster-ativo=%.4f | cluster-mes=%.4f | two-way=%.4f | Fama-MacBeth=%.4f\n",
                p_iid, p_ativo, p_mes, p_2way, tt$p.value))
  }
}

completa <- fread(file.path(REPO, "v2 OFICIAL/data/teste_fluxo_prediz_retorno.csv"))
restrita <- fread(file.path(REPO, "v2 OFICIAL/data/teste_fluxo_retorno_restrito.csv"))

roda_todas_versoes(completa, "completa (8.383 obs, script 70)")
roda_todas_versoes(restrita, "restrita a margem>0 (script 71)")

cat("\n\nOK - robustez de clustering concluida (nenhum arquivo novo salvo, so' console)\n")
