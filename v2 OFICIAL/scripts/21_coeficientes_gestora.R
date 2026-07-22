# =============================================================================
# 21_coeficientes_gestora.R  (v2 OFICIAL)
#
# LOGIT (nao mais linear -- decisao do Joao: logit e o modelo mae em todo o
# documento). Extrai os coeficientes individuais das 40 dummies de gestora
# (Itau = referencia) da mesma regressao logistica do R/20, mes a mes, e
# resume media/dp/t entre os meses -- para ver quais gestoras carregam
# mais/menos peso de VALE3 do que o Itau, tudo o mais igual (mesmas 5
# caracteristicas controladas).
#
# Alem do coeficiente em log-odds, calcula o APE discreto de cada gestora:
# para cada fundo da amostra naquele mes (nao so os da gestora g), a
# diferenca media de g(z) forcando gestora=g contra gestora=Itau, mantendo
# as 5 caracteristicas observadas -- mesma logica do APE do FIC (variavel
# categorica/discreta, nao continua).
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
  f <- tryCatch(glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum + z_betaf + gestora_grupo,
                     family = quasibinomial(link="logit"), data = dt), error = function(e) NULL)
  if (is.null(f)) next
  cf <- coef(f)
  gcf <- cf[grepl("^gestora_grupo", names(cf))]
  if (length(gcf) == 0) next
  nomes <- sub("^gestora_grupo", "", names(gcf))
  names(gcf) <- nomes

  z_pred <- predict(f, type = "link")
  shift_i <- ifelse(as.character(dt$gestora_grupo) == "Itau", 0, gcf[as.character(dt$gestora_grupo)])
  shift_i[is.na(shift_i)] <- 0
  z_itau <- z_pred - shift_i  # linear preditor de cada fundo, "como se fosse" Itau

  ape_g <- vapply(nomes, function(g) mean(plogis(z_itau + gcf[g]) - plogis(z_itau)), numeric(1))

  out[[i]] <- data.table(ym = t, gestora = nomes, coef = as.numeric(gcf), ape = as.numeric(ape_g))
}
G <- rbindlist(out)
cat("Observacoes gestora-mes:", nrow(G), "| gestoras distintas (exceto Itau):", uniqueN(G$gestora), "\n\n")

resumo <- G[, .(n_meses = .N, media_coef = mean(coef), dp_coef = sd(coef),
                media_ape = mean(ape), dp_ape = sd(ape)), by = gestora]
resumo[, t_coef := media_coef/(dp_coef/sqrt(n_meses))]
resumo[, sig_coef := ifelse(abs(t_coef)>3.29,"***",ifelse(abs(t_coef)>2.58,"**",ifelse(abs(t_coef)>1.96,"*","n.s.")))]
resumo[, t_ape := media_ape/(dp_ape/sqrt(n_meses))]
resumo[, sig_ape := ifelse(abs(t_ape)>3.29,"***",ifelse(abs(t_ape)>2.58,"**",ifelse(abs(t_ape)>1.96,"*","n.s.")))]
setorder(resumo, -media_ape)

cat("===== Coeficiente (log-odds) e APE (pontos percentuais) de cada gestora vs. Itau =====\n")
print(resumo[, .(gestora, n_meses, media_coef = round(media_coef,4), sig_coef,
                  media_ape = round(media_ape,5), sig_ape)], nrows = 100)

fwrite(resumo, file.path(REPO, "v2 OFICIAL/data/coeficientes_gestora_resumo.csv"))
fwrite(G, file.path(REPO, "v2 OFICIAL/data/coeficientes_gestora_mensal.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/coeficientes_gestora_resumo.csv'\n")
