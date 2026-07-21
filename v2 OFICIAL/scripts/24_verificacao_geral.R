# =============================================================================
# 24_verificacao_geral.R  (v2 OFICIAL)
#
# Bateria de verificacao antes de avancar para a Etapa 3 (todas as acoes):
# checa integridade do painel de todas as gestoras, faixas de valores,
# agrupamento de gestora, e REPRODUZ os numeros reportados no PDF do zero,
# para confirmar que nao ha inconsistencia entre o que foi escrito e o que
# os dados realmente dao.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

cat("========== 1) PAINEL BRUTO (todas as gestoras) ==========\n")
p0 <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv"))
cat("Obs:", nrow(p0), "| fundos:", uniqueN(p0$cod_fundo), "| grupos gestora:", uniqueN(p0$gestora_grupo), "\n")
cat("peso_vale3 fora de [0,1]:", p0[peso_vale3 < 0 | peso_vale3 > 1, .N], "\n")
dups0 <- p0[, .N, by = .(cod_fundo, data)][N > 1]
cat("Pares (fundo,mes) duplicados:", nrow(dups0), "\n")
cat("NAs em gestora_grupo:", p0[is.na(gestora_grupo) | gestora_grupo=="", .N], "\n")

cat("\n========== 2) AGRUPAMENTO DE GESTORA: cross-check raw -> grupo ==========\n")
xt <- p0[, .N, by = .(gestora_raw, gestora_grupo)][order(gestora_grupo, -N)]
# funcoes suspeitas: grupo que reune rotulos raw muito diferentes entre si
grupos_multi <- xt[, .(n_rotulos = uniqueN(gestora_raw)), by = gestora_grupo][n_rotulos > 1]
cat("Grupos com mais de 1 rotulo bruto (conferir se faz sentido):\n")
for (g in grupos_multi$gestora_grupo) {
  cat(sprintf("  %s <- %s\n", g, paste(unique(xt[gestora_grupo==g]$gestora_raw), collapse="; ")))
}

cat("\n========== 3) PAINEL PREDETERMINADO (features) ==========\n")
p1 <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_predeterminado.csv"))
cat("Obs:", nrow(p1), "(era", nrow(p0), "no bruto -- deve ser igual, so ganha colunas)\n")
cat("aum_prev: min=", min(p1$aum_prev,na.rm=TRUE), " max=", max(p1$aum_prev,na.rm=TRUE),
    " | negativos ou zero:", p1[!is.na(aum_prev) & aum_prev<=0, .N], "\n")
cat("cotistas_prev: min=", min(p1$cotistas_prev,na.rm=TRUE), " max=", max(p1$cotistas_prev,na.rm=TRUE),
    " | negativos ou zero:", p1[!is.na(cotistas_prev) & cotistas_prev<=0, .N], "\n")
cat("fluxo_prev/aum_prev (flow_aum-like) extremos:\n")
fa <- (p1$fluxo_prev/p1$aum_prev); fa <- fa[is.finite(fa)]
print(quantile(fa, probs=c(0,0.001,0.01,0.5,0.99,0.999,1)))

cat("\n========== 4) BETA DO FUNDO ==========\n")
bf <- fread(file.path(REPO, "v2 OFICIAL/data/beta_fundo_todas_gestoras.csv"))
cat("Obs:", nrow(bf), "| |beta|>5 remanescentes:", bf[abs(beta_fundo)>5,.N], "\n")
print(quantile(bf$beta_fundo, probs=c(0,0.01,0.5,0.99,1)))

cat("\n========== 5) PAINEL FINAL (usado na regressao) ==========\n")
d <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_final.csv"))
cat("Obs:", nrow(d), "| fundos:", uniqueN(d$cod_fundo), "| meses:", uniqueN(d$ym), "\n")
cat("peso_vale3 fora de [0,1]:", d[peso_vale3<0 | peso_vale3>1, .N], "\n")
cat("l_aum / l_cot / flow_aum / beta_fundo com NA ou infinito:",
    d[!is.finite(l_aum)|!is.finite(l_cot)|!is.finite(flow_aum)|!is.finite(beta_fundo), .N], "\n")
dupsF <- d[, .N, by = .(cod_fundo, ym)][N > 1]
cat("Pares (fundo,mes) duplicados no painel final:", nrow(dupsF), "\n")

cat("\nGestoras por mes (min/max/media) e se 'Itau' esta sempre presente:\n")
por_mes <- d[, .(n_gestoras = uniqueN(gestora_grupo), tem_itau = "Itau" %in% gestora_grupo), by = ym]
cat("min:", min(por_mes$n_gestoras), " max:", max(por_mes$n_gestoras),
    " media:", round(mean(por_mes$n_gestoras),1), "\n")
cat("Meses SEM Itau como nivel:", sum(!por_mes$tem_itau), "\n")

cat("\n========== 6) REPRODUZ OS NUMEROS DO PDF, DO ZERO ==========\n")
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
meses <- sort(unique(d$ym))

# --- sem FE ---
out <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  if (nrow(dt) < 60) next
  dt[, z_aum:=z(l_aum)][, z_cot:=z(l_cot)][, z_betaf:=z(beta_fundo)]
  f <- lm(peso_vale3 ~ z_aum+z_cot+is_fic+flow_aum+z_betaf, data=dt)
  out[[i]] <- data.table(ym=t, r2=summary(f)$r.squared, n=nrow(dt),
                          alpha=coef(f)[1], b_aum=coef(f)[2], b_cot=coef(f)[3],
                          b_fic=coef(f)[4], b_flow=coef(f)[5], b_betaf=coef(f)[6])
}
ts <- rbindlist(out)
cat("SEM FE -- R2 medio:", round(mean(ts$r2),4), "(PDF diz 0,540) | N medio:", round(mean(ts$n),1), "\n")
cat("  b_aum media:", round(mean(ts$b_aum),5), "(PDF diz -0,00178)\n")
cat("  b_betaf media:", round(mean(ts$b_betaf),5), "(PDF diz 0,02536)\n")

# --- com FE ---
out2 <- vector("list", length(meses))
for (i in seq_along(meses)) {
  t <- meses[i]; dt <- d[ym == t]
  dt[, gestora_grupo := droplevels(factor(gestora_grupo))]
  if (nrow(dt) < 60 || !"Itau" %in% levels(dt$gestora_grupo) || nlevels(dt$gestora_grupo) < 2) next
  dt[, gestora_grupo := relevel(gestora_grupo, ref="Itau")]
  dt[, z_aum:=z(l_aum)][, z_cot:=z(l_cot)][, z_betaf:=z(beta_fundo)]
  f <- tryCatch(lm(peso_vale3 ~ z_aum+z_cot+is_fic+flow_aum+z_betaf+gestora_grupo, data=dt), error=function(e) NULL)
  if (is.null(f)) next
  out2[[i]] <- data.table(ym=t, r2=summary(f)$r.squared, n=nrow(dt))
}
tc <- rbindlist(out2)
cat("COM FE -- R2 medio:", round(mean(tc$r2),4), "(PDF diz 0,632) | meses:", nrow(tc), "(PDF diz 59)\n")

cat("\n========== 7) VERIFICA lambda/ajuste-parcial/OOS (Etapas 1-3 originais) ==========\n")
lam <- fread(file.path(REPO, "v2 OFICIAL/data/v2_ajuste_parcial.csv"))
fit <- lm(dw ~ 0 + d, data=lam)
cat("lambda pooled recalculado:", round(coef(fit),4), "(PDF diz 0,1534/0,153)\n")

cat("\nOK - verificacao completa\n")
