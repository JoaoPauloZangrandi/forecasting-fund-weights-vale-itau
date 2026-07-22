# =============================================================================
# 37_erro_multiativo.R  (v2 OFICIAL)
#
# Estende a regressao por celula (ativo,mes) do R/30 -- em vez de so guardar
# os coeficientes de cada celula, guarda tambem o ERRO individual de cada
# fundo (peso real - peso previsto pela celula), abrangendo todos os 501
# ativos e as 41 gestoras. Mesma equacao, mesmo criterio de celula (>=15
# holders), mesma padronizacao (R/30) -- so acrescenta a extracao do residuo.
#
# Objetivo: decidir como construir o robo caca-replicantes SOMENTE depois de
# ver o comportamento desse erro no universo completo (nao so VALE3/Itau) --
# pedido explicito do Joao.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("Painel:", nrow(d), "linhas |", uniqueN(d$cod_fundo), "fundos |", uniqueN(d$ativo), "ativos\n")

fm_unico <- unique(d[, .(cod_fundo, ym, l_aum, l_cot, beta_fundo)])
mu_aum <- mean(fm_unico$l_aum); sd_aum <- sd(fm_unico$l_aum)
mu_cot <- mean(fm_unico$l_cot); sd_cot <- sd(fm_unico$l_cot)
mu_bf  <- mean(fm_unico$beta_fundo); sd_bf <- sd(fm_unico$beta_fundo)

d[, z_aum := (l_aum-mu_aum)/sd_aum]
d[, z_cot := (l_cot-mu_cot)/sd_cot]
d[, z_betaf := (beta_fundo-mu_bf)/sd_bf]

cnt <- d[, .N, by = .(ativo, ym)]
celulas <- cnt[N >= 15]
cat("Celulas (ativo,mes) com >=15 holders:", nrow(celulas), "|", uniqueN(celulas$ativo),
    "ativos distintos |", sum(celulas$N), "obs\n")

setkey(d, ativo, ym)
run_cell <- function(av, mm) {
  dm <- d[.(av, mm)]
  fit <- tryCatch(lm(peso ~ z_aum + z_cot + is_fic + flow_aum + z_betaf, data = dm), error = function(e) NULL)
  if (is.null(fit) || length(coef(fit)) < 6) return(NULL)
  data.table(cod_fundo = dm$cod_fundo, gestora_grupo = dm$gestora_grupo, ativo = av, ym = mm,
             peso = dm$peso, peso_pred = fitted(fit), erro = residuals(fit))
}
t0 <- Sys.time()
E <- rbindlist(mapply(run_cell, celulas$ativo, celulas$ym, SIMPLIFY = FALSE))
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")
cat("\nErro multiativo: n =", nrow(E), "| media =", round(mean(E$erro), 8),
    "| dp =", round(sd(E$erro), 6), "\n")
cat("Fundos distintos:", uniqueN(E$cod_fundo), "| ativos distintos:", uniqueN(E$ativo),
    "| gestoras distintas:", uniqueN(E$gestora_grupo), "\n")

fwrite(E, file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/erro_e_multiativo.csv'\n")
