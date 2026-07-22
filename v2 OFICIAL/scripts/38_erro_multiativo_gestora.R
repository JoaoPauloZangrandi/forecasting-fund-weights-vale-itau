# =============================================================================
# 38_erro_multiativo_gestora.R  (v2 OFICIAL)
#
# Resume o erro multiativo (R/37, todos os 501 ativos) por gestora: media
# (com teste de significancia baseado na serie MENSAL, mesmo estilo de todo o
# documento) e dispersao (dp pooled). Diferente da Secao 5 (efeito-fixo de
# gestora, media=0 por construcao), aqui cada celula (ativo,mes) tem sua
# propria regressao SEM dummy de gestora -- entao a media NAO e zero por
# construcao, e passa a ser informativa junto com a dispersao.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
cat("Erro multiativo:", nrow(E), "obs |", uniqueN(E$gestora_grupo), "gestoras\n")

# ---- serie mensal por gestora (media do erro naquele mes, todos ativos/fundos) ----
por_mes <- E[, .(media_mes = mean(erro), n = .N), by = .(gestora_grupo, ym)]
resumo <- por_mes[, .(n_meses = .N, media = mean(media_mes), dp_media_mensal = sd(media_mes),
                       n_obs = sum(n)), by = gestora_grupo]
resumo[, t := media/(dp_media_mensal/sqrt(n_meses))]
resumo[, sig := ifelse(abs(t)>3.29,"***",ifelse(abs(t)>2.58,"**",ifelse(abs(t)>1.96,"*","n.s.")))]

# ---- dispersao pooled (dp bruto de todas as obs da gestora, nao so a media mensal) ----
dp_pool <- E[, .(dp = sd(erro)), by = gestora_grupo]
resumo <- merge(resumo, dp_pool, by = "gestora_grupo")
setorder(resumo, -media)

cat("\n===== Erro medio (mensal, testado) e dispersao pooled, por gestora =====\n")
print(resumo[, .(gestora_grupo, n_obs, n_meses, media = round(media,5), t = round(t,2), sig,
                  dp = round(dp,5))])

fwrite(resumo, file.path(REPO, "v2 OFICIAL/data/erro_multiativo_gestora_resumo.csv"))

# ---- Figura A: histograma do erro pooled (todas as gestoras/ativos) --------
pdf(file.path(FIG, "hist_erro_multiativo_v2.pdf"), width = 6, height = 4.5)
hist(E$erro, breaks = 100, col = "#B0B8C1", border = "white",
     main = "", xlab = "erro (peso observado - ajustado, todos os 430 ativos)",
     ylab = "frequência", xlim = c(-0.1, 0.1))
abline(v = 0, col = "#8A2E2E", lwd = 2, lty = 2)
dev.off()

# ---- Figura B: dispersao (y) vs media (x), uma bolha por gestora -----------
pdf(file.path(FIG, "fig_erro_multiativo_media_dp.pdf"), width = 7, height = 6)
par(mar = c(4,4,2,1))
plot(resumo$media, resumo$dp, pch = 16, col = "#2E5C8A", cex = 1.1,
     xlab = "erro médio (viés sistemático)", ylab = "desvio-padrão do erro (dispersão)",
     main = "Erro por gestora: viés vs. dispersão (universo de todas as ações)")
abline(v = 0, col = "grey70", lty = 2)
text(resumo$media, resumo$dp, labels = resumo$gestora_grupo, pos = 3, cex = 0.55, col = "grey30")
dev.off()

cat("\nOK - salvo em 'v2 OFICIAL/data/erro_multiativo_gestora_resumo.csv' e figuras\n")
