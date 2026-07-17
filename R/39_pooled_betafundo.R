# =============================================================================
# 39_pooled_betafundo.R
#
# Testa o beta do PROPRIO FUNDO (calculado no R/33) como regressor adicional
# na regressao POOLED do peso de VALE3 -- o resultado citado na Secao 4.9 dos
# documentos ("o coeficiente entra positivo e significativo") nunca havia sido
# salvo em nenhum script/CSV. Este script roda e verifica esse numero pela
# primeira vez, com erro-padrao agrupado por fundo (mesmo estilo do resto do
# trabalho, ver R/26, R/36).
#
# Especificacao identica ao pooled baseline do R/34, com beta_fundo (padro-
# nizado como as demais variaveis continuas) adicionado. Para isolar o efeito
# de ADICIONAR o regressor (e nao um efeito de mudar a amostra), a regressao
# "sem beta_fundo" tambem e restrita aos fundo-meses com beta_fundo nao-NA --
# a mesma amostra usada na regressao "com beta_fundo".
#
# Amostra: 73% da amostra principal (fundos com >=252 pregoes de historico de
# cota; fundos novos ficam sem beta_fundo e sao excluidos das duas regressoes
# aqui, para comparacao limpa).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado_betafundo.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq/aum_num][, ym := ano*100L+mes]
d <- d[!is.na(flow_aum)]
z <- function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
d[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_preco := z(preco_mes)][, z_beta := z(beta_mes)]

n_total <- nrow(d)
n_cov   <- d[!is.na(beta_fundo), .N]
cat("Obs totais (amostra principal):", n_total, "\n")
cat("Obs com beta_fundo nao-NA:", n_cov,
    sprintf(" (%.1f%% -- fundos com >=252 pregoes de historico de cota)\n", 100*n_cov/n_total))

d_cov <- d[!is.na(beta_fundo)]
d_cov[, z_betafundo := z(beta_fundo)]

cl_meat <- function(X,u,g){U<-X*u; Ug<-rowsum(U,g); crossprod(Ug)}
fit_cluster <- function(formula, data, cluster_var) {
  f <- lm(formula, data = data)
  X <- model.matrix(f); u <- residuals(f)
  g <- data[[cluster_var]][as.numeric(rownames(X))]
  XtXinv <- solve(crossprod(X))
  V <- XtXinv %*% cl_meat(X, u, g) %*% XtXinv
  se <- sqrt(diag(V))
  data.table(variavel = names(coef(f)), coef = as.numeric(coef(f)), se_cluster = se,
             t_cluster = as.numeric(coef(f))/se, r2 = summary(f)$r.squared, n = nobs(f))
}

cat("\n===== POOLED, amostra com beta_fundo disponivel, SEM beta_fundo (baseline) =====\n")
r0 <- fit_cluster(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_preco + z_beta,
                   d_cov, "cod_fundo")
print(r0[, .(variavel, coef = round(coef,6), se_cluster = round(se_cluster,6), t_cluster = round(t_cluster,2))])
cat("R2:", round(r0$r2[1],4), "| N:", r0$n[1], "\n")

cat("\n===== POOLED, mesma amostra, COM beta_fundo =====\n")
r1 <- fit_cluster(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_preco + z_beta + z_betafundo,
                   d_cov, "cod_fundo")
print(r1[, .(variavel, coef = round(coef,6), se_cluster = round(se_cluster,6), t_cluster = round(t_cluster,2))])
cat("R2:", round(r1$r2[1],4), "| N:", r1$n[1], "\n")

fwrite(r0, file.path(REPO, "data/processed/reg39_pooled_betafundo_baseline.csv"))
fwrite(r1, file.path(REPO, "data/processed/reg39_pooled_betafundo.csv"))
cat("\nOK - salvo em data/processed/reg39_pooled_betafundo*.csv\n")
