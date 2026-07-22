# =============================================================================
# 41_pooled_fe3way_gestora_multiativo.R  (v2 OFICIAL)
#
# Fecha o "panorama geral" pedido pelo Joao: ate aqui, o coeficiente por
# GESTORA (Secao 5, R/20-21) so foi estimado com VALE3; e o painel
# multiativo (Secao 6) nunca quebrou por gestora nos COEFICIENTES -- so no
# ERRO (R/37-40). Esta regressao junta as duas coisas: painel multiativo
# completo (todos os 430 ativos), com efeito-fixo de ativo, de mes E de
# GESTORA (3 dimensoes) -- "controlando por que ativo e que mes, como cada
# gestora se compara as outras, olhando a carteira de acoes inteira dela,
# nao so VALE3?"
#
# NOTA DE PROCESSO: a 1a tentativa usou gestora como DUMMY normal (41
# colunas na matriz do modelo, igual ao R/20-21), nao como efeito-fixo --
# estourou a memoria da maquina (16GB, ficou com <10MB livres, processo
# tive que ser encerrado manualmente). O motivo: o vcov robusto/clusterizado
# do fixest com dummies via matriz densa nao usa o mesmo algoritmo enxuto
# (projecoes alternadas) que ele usa pra efeito-fixo -- por isso, aqui,
# gestora entra como EFEITO-FIXO (3a dimensao, junto de ativo e mes), que
# usa o mesmo algoritmo leve do R/31/R/39 (custo linear em N, nao no
# produto de niveis).
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
cat("Obs:", nrow(dd), "| ativos:", uniqueN(dd$ativo), "| meses:", uniqueN(dd$ym),
    "| gestoras:", uniqueN(dd$gestora_grupo), "\n")

t0 <- Sys.time()
f <- feglm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf | ativo + ym + gestora_grupo,
           data = dd, family = quasibinomial(), cluster = ~cod_fundo)
cat("Tempo de estimacao:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")
print(summary(f))

dd_usado <- if (!is.null(f$obs_selection$obsRemoved)) dd[f$obs_selection$obsRemoved] else dd
stopifnot(nrow(dd_usado) == nobs(f))

cf <- coef(f); se <- se(f); tt <- cf/se
z_pred <- predict(f, type = "link")
dg <- dlogis(z_pred)
ape_aum <- cf[["z_aum"]]*mean(dg); ape_cot <- cf[["z_cot"]]*mean(dg)
ape_flow <- cf[["flow_aum"]]*mean(dg); ape_betaf <- cf[["z_betaf"]]*mean(dg)
z0 <- z_pred - cf[["is_fic"]]*dd_usado$is_fic; z1 <- z0 + cf[["is_fic"]]
ape_fic <- mean(plogis(z1) - plogis(z0))
res_5 <- data.table(variavel = names(cf), coef = as.numeric(cf), t = as.numeric(tt))
res_5 <- rbind(res_5, data.table(
  variavel = c("ape_aum","ape_cot","ape_fic","ape_flow","ape_betaf"),
  coef = c(ape_aum, ape_cot, ape_fic, ape_flow, ape_betaf), t = NA_real_))
cat("\n===== As 5 caracteristicas (com efeito-fixo de ativo+mes+gestora) =====\n")
print(res_5)

# ---- efeito-fixo de gestora, centrado no Itau (interpretacao vs Itau) ----
fe_g <- fixef(f)$gestora_grupo
fe_g <- data.table(gestora = names(fe_g), fe = as.numeric(fe_g))
itau_fe <- fe_g[gestora == "Itau", fe]
fe_g[, fe_vs_itau := fe - itau_fe]
setorder(fe_g, -fe_vs_itau)
cat("\n===== Efeito-fixo de gestora (log-odds), centrado no Itau =====\n")
print(fe_g)

fwrite(res_5, file.path(REPO, "v2 OFICIAL/data/pooled_fe3way_multiativo_chars.csv"))
fwrite(fe_g, file.path(REPO, "v2 OFICIAL/data/pooled_fe3way_multiativo_gestora.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/pooled_fe3way_multiativo_*.csv'\n")
