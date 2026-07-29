# =============================================================================
# 43_rmse_multihorizonte_multiativo.R  (v2 OFICIAL)
#
# Passo D do plano: "etapa 4, flexibilizacao de lags" -- generaliza o teste
# multi-horizonte (script 36, ate aqui so VALE3-Itau, h=1,2,3,6,12) para
# TODO o painel multiativo (430 ativos, 41 gestoras, 2.347 fundos).
#
# Mesma logica do script 42 (reaproveita peso_pred do script 37, mesmo
# esquema treino/teste <2020-01 vs >=2020-01, lambda_h unico por MQO so no
# treino), agora repetida para os 5 horizontes de uma vez, produzindo a
# tabela agregada (pooled em todos os ativos/fundos) equivalente a Tabela 6
# (script 36) -- espelha exatamente as mesmas colunas: h, lambda, RMSE
# ajuste/ingenua, margem, % de meses em que o ajuste parcial vence.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
E[, d := peso_pred - peso]
cat("Base (erro_e_multiativo):", nrow(E), "obs |", uniqueN(E$ativo), "ativos\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
rmse <- function(x) sqrt(mean(x^2))

horizontes <- c(1, 2, 3, 6, 12)
res <- vector("list", length(horizontes))
por_mes_h <- vector("list", length(horizontes))

for (i in seq_along(horizontes)) {
  h <- horizontes[i]
  fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
  atual <- E[, .(cod_fundo, ativo, ym, d, peso, ym_fut = addm(ym, h))]
  M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"),
             by.y = c("cod_fundo","ativo","ym_fut_key"))
  M[, dw := peso_fut - peso]

  treino <- M[ym < CORTE]
  teste  <- M[ym >= CORTE & ym_fut <= 202112L]
  cat(sprintf("\nh=%d: treino %d obs | teste %d obs\n", h, nrow(treino), nrow(teste)))

  fit <- lm(dw ~ 0 + d, data = treino)
  lam <- unname(coef(fit)["d"])
  teste[, erro_ajuste := dw - lam * d]

  por_mes <- teste[, .(rmse_ajuste = rmse(erro_ajuste), rmse_naive = rmse(dw), n = .N), by = ym]
  setorder(por_mes, ym); por_mes[, horizonte := h]
  por_mes_h[[i]] <- por_mes

  res[[i]] <- data.table(
    h = h, lambda = lam, n_treino = nrow(treino), n_teste = nrow(teste),
    n_meses_teste = uniqueN(teste$ym),
    rmse_ajuste = rmse(teste$erro_ajuste), rmse_naive = rmse(teste$dw),
    vence_pct = 100*mean(por_mes$rmse_ajuste < por_mes$rmse_naive))
}
tab <- rbindlist(res)
tab[, margem_pct := 100*(rmse_naive - rmse_ajuste)/rmse_naive]
cat("\n===== RMSE fora da amostra, TODOS os ativos/gestoras, por horizonte =====\n")
print(tab[, .(h, lambda = round(lambda,4), n_teste, n_meses_teste,
              rmse_ajuste = round(rmse_ajuste,5), rmse_naive = round(rmse_naive,5),
              margem_pct = round(margem_pct,2), vence_pct = round(vence_pct,1))])

fwrite(tab, file.path(REPO, "v2 OFICIAL/data/rmse_multihorizonte_multiativo.csv"))
POR_MES <- rbindlist(por_mes_h)
fwrite(POR_MES, file.path(REPO, "v2 OFICIAL/data/rmse_multihorizonte_multiativo_por_mes.csv"))

# ---- Figura: RMSE ajuste vs. ingenua, por horizonte (mesmo estilo do script 36) ----
pdf(file.path(FIG, "fig_rmse_multihorizonte_multiativo_v2.pdf"), width = 7, height = 4.8)
par(mar = c(4,4,1.2,1))
plot(tab$h, tab$rmse_naive, type = "o", pch = 16, col = "#8A2E2E", lwd = 1.8,
     ylim = range(c(tab$rmse_ajuste, tab$rmse_naive)), xlab = "horizonte (meses à frente)",
     ylab = "RMSE fora da amostra (2020-2021)", main = "", xaxt="n")
axis(1, at = horizontes)
lines(tab$h, tab$rmse_ajuste, type = "o", pch = 16, col = "#2E5C8A", lwd = 1.8)
legend("topleft", legend = c("ingênua (sem mudança)", "ajuste parcial"),
       col = c("#8A2E2E","#2E5C8A"), lwd = 1.8, pch = 16, bty = "n")
dev.off()

cat("\nOK - salvo em 'v2 OFICIAL/data/rmse_multihorizonte_multiativo*.csv' e figura\n")
