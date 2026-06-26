# =============================================================================
# 23_descriptives.R
# Estatisticas descritivas dos dados (Tabela 1) p/ os PDFs. RODAR ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
d <- fread(file.path(REPO,"data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num:=as.numeric(aum)][, flow_aum:=fluxo_liq/aum_num]
d <- d[!is.na(flow_aum)]   # amostra de regressao

st <- function(x) c(media=mean(x,na.rm=T), dp=sd(x,na.rm=T), min=min(x,na.rm=T),
  p25=quantile(x,.25,na.rm=T), p50=median(x,na.rm=T), p75=quantile(x,.75,na.rm=T), max=max(x,na.rm=T))
vars <- list(`peso_vale3 (%)`=d$peso_vale3*100, `AUM (R$ mi)`=d$aum_num/1e6,
  `n_cotistas`=d$n_cotistas, `fluxo_liq (R$ mi)`=d$fluxo_liq/1e6,
  `fluxo/AUM (%)`=d$flow_aum*100, `preco VALE3 (R$)`=d$preco_nominal, `beta VALE3`=d$beta_vale)
cat("== Amostra de regressao (intensiva, >3 cotistas):", nrow(d), "obs |", uniqueN(d$cod_fundo), "fundos ==\n")
cat("Proporcao FIC:", round(mean(d$is_fic),3), "\n\n== DESCRITIVAS ==\n")
T1 <- t(sapply(vars, st))
print(round(T1, 3))

cat("\n== Correlacoes (regressores) ==\n")
d[, l_aum:=log(aum_num)][, l_cot:=log(n_cotistas)]
C <- cor(d[, .(l_aum, l_cot, is_fic, flow_aum, preco_nominal, beta_vale)])
print(round(C,2))

cat("\n== Painel extensivo (Acoes) ==\n")
U <- fread(file.path(REPO,"data/processed/painel_extensivo_acoes.csv"))
cat("obs:", nrow(U), "| fundos:", uniqueN(U$cod_fundo), "| %com VALE3:", round(100*mean(U$tem),1), "\n")
