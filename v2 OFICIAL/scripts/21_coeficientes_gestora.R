# =============================================================================
# 21_coeficientes_gestora.R  (v2 OFICIAL)
#
# Extrai os coeficientes individuais das 40 dummies de gestora (Itau =
# referencia) da mesma regressao do R/20, mes a mes, e resume media/dp/t
# entre os 59 meses -- para ver quais gestoras carregam mais/menos peso de
# VALE3 do que o Itau, tudo o mais igual (mesmas 5 caracteristicas
# controladas).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_final.csv"))
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
meses <- sort(unique(d$ym))

out <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  dt[, gestora_grupo := droplevels(factor(gestora_grupo))]
  if (!"Itau" %in% levels(dt$gestora_grupo) || nrow(dt) < 60 || nlevels(dt$gestora_grupo) < 2) next
  dt[, gestora_grupo := relevel(gestora_grupo, ref = "Itau")]
  dt[, z_aum := z(l_aum)][, z_cot := z(l_cot)][, z_betaf := z(beta_fundo)]
  f <- tryCatch(lm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + gestora_grupo, data = dt),
                error = function(e) NULL)
  if (is.null(f)) next
  cf <- coef(f)
  gcf <- cf[grepl("^gestora_grupo", names(cf))]
  if (length(gcf) == 0) next
  nomes <- sub("^gestora_grupo", "", names(gcf))
  out[[i]] <- data.table(ym = t, gestora = nomes, coef = as.numeric(gcf))
}
G <- rbindlist(out)
cat("Observacoes gestora-mes:", nrow(G), "| gestoras distintas (exceto Itau):", uniqueN(G$gestora), "\n\n")

resumo <- G[, .(n_meses = .N, media = mean(coef), dp = sd(coef)), by = gestora]
resumo[, t := media/(dp/sqrt(n_meses))]
resumo[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]
setorder(resumo, -media)

cat("===== Coeficiente de cada gestora vs. Itau (referencia), ordenado do maior pro menor =====\n")
print(resumo[, .(gestora, n_meses, media = round(media,5), t = round(t,2), sig)], nrows = 100)

fwrite(resumo, file.path(REPO, "v2 OFICIAL/data/coeficientes_gestora_resumo.csv"))
fwrite(G, file.path(REPO, "v2 OFICIAL/data/coeficientes_gestora_mensal.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/coeficientes_gestora_resumo.csv'\n")
