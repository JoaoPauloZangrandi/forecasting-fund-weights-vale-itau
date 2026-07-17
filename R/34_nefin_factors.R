# =============================================================================
# 34_nefin_factors.R
#
# Adiciona os premios de fatores de risco do NEFIN-USP (Nucleo de Economia
# Financeira, USP: Rm-Rf, SMB, HML, WML/momento, IML/iliquidez, taxa livre de
# risco -- metodologia Fama-French/Carhart adaptada ao Brasil) como controles
# na regressao POOLED do peso de VALE3 (Tabela 3 dos documentos). Os fatores
# sao DIARIOS; agregamos para MENSAL por retorno composto dentro do mes.
#
# Por que aqui e nao na cross-section mensal: os fatores sao constantes DENTRO
# do mes (iguais p/ todos os fundos), exatamente como preco/beta da VALE3 --
# so sao identificaveis na regressao empilhada, pela variacao ENTRE meses.
#
# Motivacao: o coeficiente do preco/beta de VALE3 no pooled (R/14, R/28) pode
# estar capturando, em parte, premios de risco de mercado que variam no tempo
# (2016-2021 inclui grande recuperacao do Ibovespa). Controlar pelos fatores
# NEFIN testa se o padrao sobrevive a variaveis de mercado ja conhecidas.
# Fonte: https://nefin.com.br/data/risk-factors/ (baixado em 09/07/2026).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

nf <- fread(file.path(REPO, "data/raw/nefin_factors.csv"))
nf[, Date := as.Date(Date)]
nf <- nf[Date >= as.Date("2015-12-01") & Date <= as.Date("2021-12-31")]
nf[, ym := year(Date)*100L + month(Date)]

# retorno composto dentro do mes (nao soma simples) para cada fator
comp <- function(x) prod(1+x) - 1
mret <- nf[, .(Rm_Rf = comp(Rm_minus_Rf), SMB = comp(SMB), HML = comp(HML),
               WML = comp(WML), IML = comp(IML), Rf = comp(Risk_Free)), by = ym]
cat("Fatores NEFIN agregados p/ mensal:", nrow(mret), "meses (", min(mret$ym), "-", max(mret$ym), ")\n")

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq/aum_num][, ym := ano*100L+mes]
d <- d[!is.na(flow_aum)]
z <- function(x) (x-mean(x))/sd(x)
d[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_preco := z(preco_mes)][, z_beta := z(beta_mes)]
d <- merge(d, mret, by = "ym", all.x = TRUE)
cat("Obs com fatores NEFIN casados:", d[!is.na(Rm_Rf), .N], "de", nrow(d), "\n")
for (v in c("Rm_Rf","SMB","HML","WML")) d[[paste0("z_",v)]] <- z(d[[v]])

cat("\n===== POOLED sem fatores NEFIN (baseline, = Tabela 3) =====\n")
f0 <- lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_preco + z_beta, data = d)
print(round(summary(f0)$coefficients, 6)); cat("R2:", round(summary(f0)$r.squared,4), "\n")

cat("\n===== POOLED com fatores NEFIN (Rm-Rf, SMB, HML, WML) como controle =====\n")
f1 <- lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_preco + z_beta +
           z_Rm_Rf + z_SMB + z_HML + z_WML, data = d)
print(round(summary(f1)$coefficients, 6)); cat("R2:", round(summary(f1)$r.squared,4), "\n")

cat("\n===== Comparacao do coeficiente de preco e beta, com/sem fatores NEFIN =====\n")
cmp <- data.table(
  variavel = c("z_preco","z_beta"),
  sem_nefin = c(coef(f0)["z_preco"], coef(f0)["z_beta"]),
  com_nefin = c(coef(f1)["z_preco"], coef(f1)["z_beta"]))
print(cmp)

# -----------------------------------------------------------------------------
# CORRECAO: os t/asteriscos acima usam erro-padrao de MQO comum (nao agrupado),
# inconsistente com o resto do trabalho (Newey-West na Secao 3.3, cluster por
# fundo no ajuste parcial R/26-27, na margem extensiva R/20, e no beta do fundo
# R/39). Observacoes do MESMO fundo em meses diferentes nao sao independentes;
# a correcao correta e agrupar por fundo, como em todo o resto do documento.
# -----------------------------------------------------------------------------
cl_meat <- function(X,u,g){U<-X*u; Ug<-rowsum(U,g); crossprod(Ug)}
fit_cluster <- function(fit, data, cluster_var) {
  X <- model.matrix(fit); u <- residuals(fit)
  g <- data[[cluster_var]][as.numeric(rownames(X))]
  XtXinv <- solve(crossprod(X))
  V <- XtXinv %*% cl_meat(X, u, g) %*% XtXinv
  se <- sqrt(diag(V))
  data.table(variavel = names(coef(fit)), coef = as.numeric(coef(fit)),
             se_ols = summary(fit)$coefficients[,2], t_ols = summary(fit)$coefficients[,3],
             se_cluster = se, t_cluster = as.numeric(coef(fit))/se)
}

cat("\n===== POOLED sem fatores NEFIN: MQO comum vs. cluster por fundo =====\n")
r0c <- fit_cluster(f0, d, "cod_fundo")
print(r0c[, .(variavel, coef=round(coef,6), t_ols=round(t_ols,2), t_cluster=round(t_cluster,2))])

cat("\n===== POOLED com fatores NEFIN: MQO comum vs. cluster por fundo =====\n")
r1c <- fit_cluster(f1, d, "cod_fundo")
print(r1c[, .(variavel, coef=round(coef,6), t_ols=round(t_ols,2), t_cluster=round(t_cluster,2))])

fwrite(as.data.table(round(summary(f1)$coefficients,6), keep.rownames="variavel"),
       file.path(REPO, "data/processed/reg34_pooled_nefin.csv"))
fwrite(mret, file.path(REPO, "data/processed/reg34_nefin_mensal.csv"))
fwrite(r0c, file.path(REPO, "data/processed/reg34_pooled_sem_nefin_cluster.csv"))
fwrite(r1c, file.path(REPO, "data/processed/reg34_pooled_nefin_cluster.csv"))
cat("\nOK - salvo em data/processed/reg34_*.csv\n")
