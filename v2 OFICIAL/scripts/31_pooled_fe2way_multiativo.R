# =============================================================================
# 31_pooled_fe2way_multiativo.R  (v2 OFICIAL)
#
# LOGIT (nao mais linear -- decisao do Joao: logit e o modelo mae em todo o
# documento). Checagem complementar: UMA regressao pooled logistica com
# TODAS as observacoes do painel multiativo, com efeito-fixo de ATIVO e de
# MES, erro-padrao agrupado por fundo.
#
# O truque de demeaning (Frisch-Waugh-Lovell) usado na versao linear NAO
# funciona em modelos nao-lineares -- por isso usa o pacote fixest
# (feglm), feito especificamente para efeito-fixo em GLM de forma rapida
# mesmo com muitos niveis de FE (aqui, 501 ativos + 60 meses).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)
d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]

dd <- d[is.finite(flow_aum)]
cat("Obs para o pooled FE-2way logit:", nrow(dd), "| ativos:", uniqueN(dd$ativo),
    "| meses:", uniqueN(dd$ym), "| fundos:", uniqueN(dd$cod_fundo), "\n")

t0 <- Sys.time()
f <- feglm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf | ativo + ym,
           data = dd, family = quasibinomial(), cluster = ~cod_fundo)
cat("Tempo de estimacao:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")
print(summary(f))

cf <- coef(f)
cat("\nColapsou (perfect fit) alguma variavel?\n")
print(f$collin.var)

# ---- APE: usa a media de g'(z) na amostra inteira (mesma logica das outras) --
# CUIDADO: feglm remove observacoes com FE singleton (aqui, algumas celulas de
# ativo com 0 ou so 1 desfecho) -- z_pred tem MENOS linhas que dd. Usar
# f$obs_selection$obsRemoved (indices negativos) pra alinhar dd ao que o
# modelo de fato usou, senao dd$is_fic fica dessincronizado com z_pred
# (bug real encontrado ao rodar: aviso de reciclagem de vetor).
dd_usado <- if (!is.null(f$obs_selection$obsRemoved)) dd[f$obs_selection$obsRemoved] else dd
stopifnot(nrow(dd_usado) == nobs(f))

z_pred <- predict(f, type = "link")
dg <- dlogis(z_pred)
ape_aum <- cf[["z_aum"]]*mean(dg); ape_cot <- cf[["z_cot"]]*mean(dg)
ape_flow <- cf[["flow_aum"]]*mean(dg); ape_betaf <- cf[["z_betaf"]]*mean(dg)
if ("is_fic" %in% names(cf)) {
  z0 <- z_pred - cf[["is_fic"]]*dd_usado$is_fic; z1 <- z0 + cf[["is_fic"]]
  ape_fic <- mean(plogis(z1) - plogis(z0))
} else ape_fic <- NA_real_

se <- se(f); tt <- cf/se
res_fe2 <- data.table(
  variavel = names(cf), coef = as.numeric(cf), se_cluster_fundo = as.numeric(se), t = as.numeric(tt)
)
res_fe2 <- rbind(res_fe2, data.table(
  variavel = c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf"),
  coef = c(ape_aum, ape_cot, ape_fic, ape_flow, ape_betaf), se_cluster_fundo = NA_real_, t = NA_real_
))
print(res_fe2)
cat("\nObs:", nrow(dd), "| ativos:", uniqueN(dd$ativo), "| meses:", uniqueN(dd$ym),
    "| fundos:", uniqueN(dd$cod_fundo), "\n")
fwrite(res_fe2, file.path(REPO, "v2 OFICIAL/data/pooled_fe2way_multiativo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/pooled_fe2way_multiativo.csv'\n")
