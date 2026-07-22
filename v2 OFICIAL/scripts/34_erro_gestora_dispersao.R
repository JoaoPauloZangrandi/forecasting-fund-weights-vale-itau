# =============================================================================
# 34_erro_gestora_dispersao.R  (v2 OFICIAL)
#
# Continuacao do R/33: a media do erro por gestora e ~0 por construcao (efeito
# fixo absorve, normal equations garantem soma-zero dentro de cada grupo com
# dummy propria) -- nao e informativa. O que sobra e a DISPERSAO (dp) do erro
# dentro de cada gestora: quanto maior, mais dificil o modelo explica aquele
# grupo. Gera tabela + 2 histogramas + evolucao mensal (top-5/bottom-5 mais
# dispersas, com cobertura minima de meses).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_por_gestora.csv"))
cat("Erro e (com FE de gestora):", nrow(E), "obs\n")

# ---- tabela: dispersao por gestora ------------------------------------------
agg <- E[, .(n_obs = .N, n_meses = uniqueN(ym), dp = sd(erro)), by = gestora_grupo]
agg <- agg[n_obs > 1]
setorder(agg, -dp)
cat("\n===== Desvio-padrao do erro por gestora (ordenado, maior->menor) =====\n")
print(agg[, .(gestora_grupo, n_obs, n_meses, dp = round(dp, 5))])
dp_itau <- agg[gestora_grupo == "Itau", dp]
cat("\ndp Itau (referencia):", round(dp_itau, 5), "\n")
fwrite(agg, file.path(REPO, "v2 OFICIAL/data/erro_e_gestora_dispersao.csv"))

# ---- Figura A: histograma do erro pooled (todas as gestoras) ---------------
pdf(file.path(FIG, "hist_erro_e_gestora_v2.pdf"), width = 6, height = 4.5)
hist(E$erro, breaks = 60, col = "#B0B8C1", border = "white",
     main = "", xlab = "erro e (peso observado - ajustado)", ylab = "frequência",
     xlim = c(-0.1, 0.1))
abline(v = 0, col = "#8A2E2E", lwd = 2, lty = 2)
dev.off()

# ---- Figura B: histograma da dispersao (dp) entre as gestoras --------------
pdf(file.path(FIG, "hist_dp_erro_gestora_v2.pdf"), width = 6, height = 4.5)
hist(agg$dp, breaks = 15, col = "#B0B8C1", border = "white",
     main = "", xlab = "desvio-padrão do erro dentro da gestora", ylab = "nº de gestoras")
abline(v = dp_itau, col = "#8A2E2E", lwd = 2)
legend("topright", legend = "Itaú", col = "#8A2E2E", lwd = 2, bty = "n")
dev.off()

# ---- Figura C: evolucao mensal do dp, top-5/bottom-5 (n_meses>=45) ---------
elig <- agg[n_meses >= 45]
top5 <- elig[order(-dp)][1:5, gestora_grupo]
bot5 <- elig[order(dp)][1:5, gestora_grupo]
cat("\nTop-5 mais dispersas (>=45 meses):", paste(top5, collapse=", "), "\n")
cat("Bottom-5 menos dispersas (>=45 meses):", paste(bot5, collapse=", "), "\n")

por_mes <- E[gestora_grupo %in% c(top5, bot5), .(dp_mes = sd(erro), n = .N), by = .(gestora_grupo, ym)]
por_mes <- por_mes[n >= 3]

meses_todos <- sort(unique(por_mes$ym))
datas_todas <- as.Date(paste0(substr(meses_todos,1,4),"-",substr(meses_todos,5,6),"-01"))
cores <- c("#2E5C8A","#8A2E2E","#2E8A4E","#8A6D2E","#6B2E8A")

serie_mat <- function(gestoras) {
  m <- matrix(NA_real_, nrow = length(meses_todos), ncol = length(gestoras),
              dimnames = list(as.character(meses_todos), gestoras))
  for (g in gestoras) {
    gs <- por_mes[gestora_grupo == g]
    m[as.character(gs$ym), g] <- gs$dp_mes
  }
  m
}
m_top <- serie_mat(top5); m_bot <- serie_mat(bot5)

pdf(file.path(FIG, "fig_dp_erro_gestora_evolucao.pdf"), width = 8, height = 7.5)
par(mfrow = c(2,1), mar = c(3,4,2.5,1))

plot(datas_todas, m_top[,1], type="n", ylim = range(m_top, na.rm=TRUE),
     xlab="", ylab="dp mensal do erro", main="As 5 gestoras com erro mais DISPERSO")
for (i in seq_along(top5)) lines(datas_todas, m_top[,i], col=cores[i], lwd=1.8, type="o", pch=16, cex=0.5)
legend("topleft", legend=top5, col=cores, lwd=1.8, bty="n", cex=0.75, ncol=2)

plot(datas_todas, m_bot[,1], type="n", ylim = range(m_bot, na.rm=TRUE),
     xlab="", ylab="dp mensal do erro", main="As 5 gestoras com erro menos DISPERSO")
for (i in seq_along(bot5)) lines(datas_todas, m_bot[,i], col=cores[i], lwd=1.8, type="o", pch=16, cex=0.5)
legend("topleft", legend=bot5, col=cores, lwd=1.8, bty="n", cex=0.75, ncol=2)

dev.off()

cat("\nOK - figuras salvas em 'v2 OFICIAL/figuras/'\n")
