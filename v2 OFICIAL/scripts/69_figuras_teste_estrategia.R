# =============================================================================
# 69_figuras_teste_estrategia.R  (v2 OFICIAL -- teste_estrategia)
#
# 3 figuras pro documento teste_estrategia.tex, pedido do Joao (documento
# tinha texto demais, pouco grafico/tabela):
#  (1) margem bruto vs corrigido por ativo, colorido por fonte do preco
#      (Fase 3) -- visualiza o achado central da Secao 4.
#  (2) top 10 pares de gestora por taxa normalizada (Fase 4).
#  (3) top compra/venda liquida em R$ (Fase 5).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras_estrategia")

# ---- Figura 1: margem bruto vs corrigido, por fonte ------------------------
precos <- fread(file.path(REPO, "v2 OFICIAL/data/precos_mensais_final.csv"))
fonte_ticker <- unique(precos[, .(ticker, fonte)])
pa <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_universo_completo_por_ativo_h1.csv"))
pa[, ticker := trimws(sub(".*- ", "", ativo))]
elegivel <- pa[n_obs >= 100]
elegivel <- merge(elegivel, fonte_ticker, by = "ticker", all.x = TRUE)
elegivel <- elegivel[!is.na(fonte)]

pdf(file.path(FIG, "fig1_margem_bruto_vs_corrigido.pdf"), width = 6.5, height = 5.5)
par(mar = c(4.2, 4.2, 1, 1))
cores <- ifelse(elegivel$fonte == "yahoo_ajustado", "#2E5C8A", "#B8452E")
plot(elegivel$margem_bruto, elegivel$margem_corr, pch = 16, col = cores, cex = 0.7,
     xlab = "Margem vs. ingênua, SEM correção mecânica (%)",
     ylab = "Margem vs. ingênua, COM correção mecânica (%)",
     xlim = c(-30, 20), ylim = c(-100, 20))
abline(0, 1, col = "grey60", lty = 2)
abline(h = 0, col = "grey85"); abline(v = 0, col = "grey85")
legend("bottomright", legend = c("Preço ajustado (Yahoo)", "Preço bruto (B3)"),
       col = c("#2E5C8A", "#B8452E"), pch = 16, bty = "o", bg = "white", cex = 0.85)
dev.off()
cat("Fig 1 OK -", elegivel[,.N], "ativos plotados\n")

# ---- Figura 2: top 10 pares de gestora por taxa normalizada ----------------
mapa <- fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
n_fundos <- mapa[, .N, by = gestora_grupo]
pg <- fread(file.path(REPO, "v2 OFICIAL/data/resumo_pares_gestora.csv"))
pg[, gestora_a := trimws(sub(" <->.*", "", par_gestora))]
pg[, gestora_b := trimws(sub(".*<-> ", "", par_gestora))]
pg <- merge(pg, n_fundos, by.x = "gestora_a", by.y = "gestora_grupo")
setnames(pg, "N", "n_fundos_a")
pg <- merge(pg, n_fundos, by.x = "gestora_b", by.y = "gestora_grupo")
setnames(pg, "N", "n_fundos_b")
pg[, taxa := n_pares_fundo / (n_fundos_a * n_fundos_b)]
setorder(pg, -taxa)
top10 <- pg[1:10]

pdf(file.path(FIG, "fig2_top_pares_gestora.pdf"), width = 7, height = 5.5)
par(mar = c(4.2, 13, 1, 1))
bp <- barplot(rev(top10$taxa), horiz = TRUE, names.arg = rev(top10$par_gestora),
              las = 1, cex.names = 0.7, col = "#2E5C8A", border = NA,
              xlab = "Taxa normalizada (pares significativos / pares possíveis)")
dev.off()
cat("Fig 2 OK -", nrow(top10), "pares de gestora plotados\n")

# ---- Figura 3: top compra/venda liquida em R$ -------------------------------
nm <- fread(file.path(REPO, "v2 OFICIAL/data/sinal_robo_net_mercado.csv"))
nm[, rotulo := paste0(sub(" - .*", "", ativo), "\n", ym)]
top_compra <- nm[order(-fluxo_liquido_reais)][1:5]
top_venda  <- nm[order(fluxo_liquido_reais)][1:5]
junto <- rbind(top_compra, top_venda)
junto[, fluxo_bi := fluxo_liquido_reais/1e3]  # ja esta em milhoes, /1e3 = bilhoes
setorder(junto, fluxo_bi)

pdf(file.path(FIG, "fig3_top_compra_venda.pdf"), width = 7.5, height = 5.5)
par(mar = c(4.2, 9, 1, 1))
cores3 <- ifelse(junto$fluxo_bi > 0, "#1E7A4D", "#B8452E")
bp <- barplot(junto$fluxo_bi, horiz = TRUE, names.arg = junto$rotulo,
              las = 1, cex.names = 0.65, col = cores3, border = NA,
              xlab = "Fluxo líquido previsto (R$ bilhões)")
abline(v = 0, col = "grey40")
dev.off()
cat("Fig 3 OK -", nrow(junto), "casos plotados\n")

cat("\nOK - 3 figuras salvas em 'v2 OFICIAL/figuras_estrategia/'\n")
