# =============================================================================
# 35_lambda_arellanobond.R
#
# Estima a dinamica do peso por PAINEL DINAMICO (Arellano-Bond, 1991), terceiro
# estimador ao lado de MQO e VI-1-lag (R/26). A equacao de ajuste parcial
#   w_{i,t} = (1-lambda) w_{i,t-1} + lambda w*_{i,t-1} + u_{i,t}
# e um caso particular de painel dinamico AR(1) com um regressor adicional
# (w*, o peso desejado do logit). Nao impomos a restricao no-linear de que os
# dois coeficientes somem 1 (isso exigiria GMM nao-linear); em vez disso
# reportamos os DOIS coeficientes livres: rho (persistencia, em lag(w)) e
# lambda (resposta ao alvo, em lag(w*)) -- uma versao MAIS FLEXIVEL do modelo
# de ajuste parcial, que permite comparar com o lambda restrito (VI, R/26).
#
# Arellano-Bond: diferencia a equacao (remove efeito-fixo de fundo implicito)
# e instrumenta a variavel dependente defasada diferenciada com NIVEIS mais
# defasados (GMM). Isso ataca DIRETAMENTE o problema de "w aparece nos dois
# lados" (Secao de reversao mecanica do correcoes_fable.pdf) por construcao,
# de forma mais geral que a VI com 1 lag do R/26.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(plm) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq/aum_num][, ym := ano*100L+mes]
d <- d[!is.na(flow_aum)]
z <- function(x) (x-mean(x))/sd(x)
d[, z_aum := z(l_aum)][, z_cot := z(l_cot)]

# peso desejado (logit fracionario por mes, igual R/26)
d[, w_star := NA_real_]
for (mm in sort(unique(d$ym))) { ix <- d$ym==mm
  fg <- suppressWarnings(glm(peso_vale3~z_aum+z_cot+is_fic+flow_aum,
                             family=quasibinomial(link="logit"), data=d[ix]))
  d[ix, w_star := fitted(fg)] }

# indice de mes MONOTONICO (nao usar ym=ano*100+mes diretamente: pula 89 unidades
# de dez->jan e quebraria a nocao de "period" consecutivo do plm/pgmm)
d[, monidx := frank(ym, ties.method = "dense")]
setorder(d, cod_fundo, monidx)

pdat <- pdata.frame(as.data.frame(d[, .(cod_fundo, monidx, peso_vale3, w_star)]),
                     index = c("cod_fundo","monidx"))

cat("Paineis: fundos =", uniqueN(d$cod_fundo), "| meses =", uniqueN(d$monidx), "\n")

# --- Arellano-Bond (diferencas), one-step, instrumentos = niveis defasados ---
ab <- pgmm(peso_vale3 ~ lag(peso_vale3, 1) + lag(w_star, 1) | lag(peso_vale3, 2:99),
           data = pdat, effect = "individual", model = "onestep", transformation = "d")
cat("\n===== Arellano-Bond (diferencas, one-step) =====\n")
print(summary(ab))

s <- summary(ab)$coefficients
rho    <- s["lag(peso_vale3, 1)", "Estimate"]
lam_ab <- s["lag(w_star, 1)", "Estimate"]
cat("\nrho (persistencia, coef de w_{t-1}):", round(rho,4), "\n")
cat("lambda (resposta ao alvo, coef de w*_{t-1}):", round(lam_ab,4), "\n")
cat("Para comparacao: lambda VI (R/26) = 0.0413 | lambda MQO (R/26) = 0.0976\n")
cat("Implied '1-rho' (se fosse o modelo restrito):", round(1-rho,4), "\n")

# --- teste de especificacao: Sargan/Hansen (sobreidentificacao) e AR(2) ---
cat("\n===== Diagnosticos =====\n")
tryCatch({
  st <- sargan(ab)
  cat("Teste de Sargan (H0: instrumentos validos): estat=", round(st$statistic,2),
      " p=", round(st$p.value,4), "\n", sep="")
}, error = function(e) cat("Sargan: nao disponivel (", conditionMessage(e), ")\n"))
tryCatch({
  a2 <- mtest(ab, order = 2L)
  cat("Teste AR(2) nos residuos diferenciados (H0: sem autocorrelacao de ordem 2): z=",
      round(a2$statistic,2), " p=", round(a2$p.value,4), "\n", sep="")
}, error = function(e) cat("AR(2): nao disponivel (", conditionMessage(e), ")\n"))

out <- data.table(estimador = "Arellano-Bond (diferencas, one-step, instrumentos completos)",
                  rho = rho, lambda_w_star = lam_ab,
                  se_rho = s["lag(peso_vale3, 1)","Std. Error"],
                  se_lambda = s["lag(w_star, 1)","Std. Error"])

# =============================================================================
# Robustez: T=72 e' longo p/ o AB padrao -> lag(y,2:99) gera 2.484 instrumentos
# p/ 307 fundos (mais instrumentos que unidades), Sargan fica nao-informativo
# (p=1). Pratica padrao: LIMITAR os instrumentos a poucas defasagens (aqui,
# 2:5) para nao "overfitar" a matriz de projecao de 1o estagio.
# =============================================================================
fwrite(out, file.path(REPO, "data/processed/reg35_lambda_arellanobond.csv"))
cat("\nOK (instrumentos completos) - salvo em data/processed/reg35_lambda_arellanobond.csv\n")

cat("\n===== Robustez: instrumentos LIMITADOS a lag 2:5, SEM colapsar =====\n")
ab2 <- tryCatch(
  pgmm(peso_vale3 ~ lag(peso_vale3, 1) + lag(w_star, 1) | lag(peso_vale3, 2:5),
       data = pdat, effect = "individual", model = "onestep", transformation = "d"),
  error = function(e) { cat("ERRO na estimacao:", conditionMessage(e), "\n"); NULL })
if (!is.null(ab2)) {
  s2 <- tryCatch(summary(ab2)$coefficients, error = function(e) { cat("ERRO no summary (matriz singular):", conditionMessage(e), "\n"); NULL })
  if (!is.null(s2)) {
    rho2 <- s2["lag(peso_vale3, 1)","Estimate"]; lam2 <- s2["lag(w_star, 1)","Estimate"]
    cat("rho:", round(rho2,4), "| lambda_w_star:", round(lam2,4), "\n")
    out <- rbind(out, data.table(estimador = "Arellano-Bond (one-step, instr. lag 2:5, sem colapsar)",
                                 rho = rho2, lambda_w_star = lam2,
                                 se_rho = s2["lag(peso_vale3, 1)","Std. Error"],
                                 se_lambda = s2["lag(w_star, 1)","Std. Error"]))
  } else {
    cat("(matriz de covariancia singular com so 4 instrumentos sem colapsar -- especificacao instavel,\n",
        " registrado como limitacao, nao reportado como numero)\n", sep="")
  }
}

cat("\n===== Robustez: instrumentos COLAPSADOS (collapse=TRUE, Roodman 2009) =====\n")
# 'collapse' e' a forma padrao (Roodman) de reduzir a contagem de instrumentos sem
# descartar informacao de defasagens distantes: uma coluna por defasagem RELATIVA,
# nao uma por (defasagem x periodo). Evita tanto a proliferacao (2.484 instrumentos)
# quanto a singularidade do corte bruto acima.
ab3 <- tryCatch(
  pgmm(peso_vale3 ~ lag(peso_vale3, 1) + lag(w_star, 1) | lag(peso_vale3, 2:99),
       data = pdat, effect = "individual", model = "onestep", transformation = "d", collapse = TRUE),
  error = function(e) { cat("ERRO na estimacao:", conditionMessage(e), "\n"); NULL })
if (!is.null(ab3)) {
  s3 <- tryCatch(summary(ab3)$coefficients, error = function(e) { cat("ERRO no summary:", conditionMessage(e), "\n"); NULL })
  if (!is.null(s3)) {
    print(summary(ab3))
    rho3 <- s3["lag(peso_vale3, 1)","Estimate"]; lam3 <- s3["lag(w_star, 1)","Estimate"]
    cat("rho:", round(rho3,4), "| lambda_w_star:", round(lam3,4), "\n")
    tryCatch({ st3 <- sargan(ab3); cat("Sargan (colapsado): estat=", round(st3$statistic,2),
        " p=", round(st3$p.value,4), " (df bem menor que o caso completo)\n", sep="") },
        error=function(e) cat("Sargan: erro\n"))
    out <- rbind(out, data.table(estimador = "Arellano-Bond (one-step, instrumentos colapsados)",
                                 rho = rho3, lambda_w_star = lam3,
                                 se_rho = s3["lag(peso_vale3, 1)","Std. Error"],
                                 se_lambda = s3["lag(w_star, 1)","Std. Error"]))
  } else {
    cat("(summary tambem falhou no colapsado -- registrar como limitacao)\n")
  }
}

fwrite(out, file.path(REPO, "data/processed/reg35_lambda_arellanobond.csv"))
cat("\nOK (final) - salvo em data/processed/reg35_lambda_arellanobond.csv\n")
