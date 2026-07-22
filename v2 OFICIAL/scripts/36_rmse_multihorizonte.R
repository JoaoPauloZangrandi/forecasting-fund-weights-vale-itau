# =============================================================================
# 36_rmse_multihorizonte.R  (v2 OFICIAL)
#
# Segunda metade do item 4 do plano: flexibiliza o RMSE da Etapa 3 (que so
# olhava h=1 mes a frente) para varios horizontes h. Mesma logica simples de
# sempre -- MQO direto, mesmo alvo w* (peso_prev, da Etapa 1 logit), mesmo
# corte treino/teste (< 2020-01 vs >= 2020-01) -- soh repetida para cada h.
#
# Para cada h: dw_h = peso_{t+h} - peso_t; d = peso_prev_t - peso_t (mesmo "d"
# de sempre, alvo fixado em t porque nao temos caracteristicas futuras);
# lambda_h estimado por MQO SO no treino; RMSE ajuste (lambda_h*d) vs RMSE
# ingenua (prever zero mudanca) SO no teste.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

E <- fread(file.path(REPO, "v2 OFICIAL/data/v2_erro_e.csv"))
E[, d := peso_prev - peso_vale3]
cat("Base (Etapa 1, erro e):", nrow(E), "obs |", uniqueN(E$ym), "meses (",
    min(E$ym), "-", max(E$ym), ")\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
rmse <- function(x) sqrt(mean(x^2)); mae <- function(x) mean(abs(x))

horizontes <- c(1, 2, 3, 6, 12)
res <- vector("list", length(horizontes))
por_mes_h <- vector("list", length(horizontes))

for (i in seq_along(horizontes)) {
  h <- horizontes[i]
  fut <- E[, .(cod_fundo, ym_fut_key = ym, peso_fut = peso_vale3)]
  atual <- E[, .(cod_fundo, ym, d, peso_vale3, ym_fut = addm(ym, h))]
  M <- merge(atual, fut, by.x = c("cod_fundo","ym_fut"), by.y = c("cod_fundo","ym_fut_key"))
  M[, dw := peso_fut - peso_vale3]

  treino <- M[ym < CORTE]
  teste  <- M[ym >= CORTE & ym_fut <= 202112L]
  if (nrow(treino) < 30 || nrow(teste) < 30) { res[[i]] <- NULL; next }

  fit <- lm(dw ~ 0 + d, data = treino)
  lam <- unname(coef(fit)["d"])
  teste[, erro_ajuste := dw - lam * d]

  por_mes <- teste[, .(rmse_ajuste = rmse(dw - lam*d), rmse_naive = rmse(dw), n = .N), by = ym]
  setorder(por_mes, ym); por_mes[, horizonte := h]
  por_mes_h[[i]] <- por_mes

  res[[i]] <- data.table(
    h = h, lambda = lam, n_treino = nrow(treino), n_teste = nrow(teste),
    rmse_ajuste = rmse(teste$erro_ajuste), rmse_naive = rmse(teste$dw),
    mae_ajuste = mae(teste$erro_ajuste), mae_naive = mae(teste$dw),
    vence_pct = 100*mean(por_mes$rmse_ajuste < por_mes$rmse_naive))
}
tab <- rbindlist(res)
tab[, margem_pct := 100*(rmse_naive - rmse_ajuste)/rmse_naive]
cat("\n===== RMSE fora da amostra, por horizonte =====\n")
print(tab[, .(h, lambda = round(lambda,4), n_teste, rmse_ajuste = round(rmse_ajuste,5),
              rmse_naive = round(rmse_naive,5), margem_pct = round(margem_pct,2),
              vence_pct = round(vence_pct,1))])

fwrite(tab, file.path(REPO, "v2 OFICIAL/data/rmse_multihorizonte.csv"))
POR_MES <- rbindlist(por_mes_h)
fwrite(POR_MES, file.path(REPO, "v2 OFICIAL/data/rmse_multihorizonte_por_mes.csv"))

# ---- Figura: RMSE ajuste vs. ingenua, por horizonte -------------------------
pdf(file.path(FIG, "fig_rmse_multihorizonte_v2.pdf"), width = 7, height = 4.8)
par(mar = c(4,4,2.5,1))
plot(tab$h, tab$rmse_naive, type = "o", pch = 16, col = "#8A2E2E", lwd = 1.8,
     ylim = range(c(tab$rmse_ajuste, tab$rmse_naive)), xlab = "horizonte (meses à frente)",
     ylab = "RMSE fora da amostra (2020-2021)", main = "RMSE fora da amostra por horizonte", xaxt="n")
axis(1, at = horizontes)
lines(tab$h, tab$rmse_ajuste, type = "o", pch = 16, col = "#2E5C8A", lwd = 1.8)
legend("topleft", legend = c("ingênua (sem mudança)", "ajuste parcial"),
       col = c("#8A2E2E","#2E5C8A"), lwd = 1.8, pch = 16, bty = "n")
dev.off()

cat("\nOK - salvo em 'v2 OFICIAL/data/rmse_multihorizonte.csv' e figura\n")
