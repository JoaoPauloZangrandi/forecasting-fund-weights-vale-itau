# =============================================================================
# 35_robo_replicante.R  (v2 OFICIAL)
#
# Item 4 do plano: o "robozinho caca-replicantes". Mesma logica da dispersao
# do erro por gestora (R/33-34), agora descendo pro nivel de FUNDO INDIVIDUAL.
# Diferenca importante: no nivel gestora, a MEDIA do erro e zero por
# construcao (dummy propria, normal equations do MQO). No nivel fundo, isso
# NAO vale -- cada fundo e so uma entre varias observacoes que alimentam a
# dummy da sua gestora, entao a media do erro de um fundo especifico pode (e
# de fato) se afastar bastante de zero.
#
# Escore = MAE (erro medio absoluto) do fundo ao longo do tempo -- combina
# vies sistematico e volatilidade num numero so. MAE baixo = "replicante"
# (peso em VALE3 bem explicado pelas 5 caracteristicas); MAE alto = "humano"
# (alocacao que a formula generica nao explica).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_por_gestora.csv"))
cat("Erro e (com FE de gestora):", nrow(E), "obs |", uniqueN(E$cod_fundo), "fundos\n")

por_fundo <- E[, .(n_meses = .N, mae = mean(abs(erro)), media = mean(erro), dp = sd(erro)),
               by = .(cod_fundo, gestora_grupo)]
cat("\nMedia do |erro medio do fundo| (deveria ser >> 0, diferente do nivel gestora):",
    round(mean(abs(por_fundo$media)), 5), "\n")

# so fundos com historico suficiente (mesmo corte >=24 meses da Secao 6)
elig <- por_fundo[n_meses >= 24]
cat("Fundos elegiveis (>=24 meses):", nrow(elig), "de", nrow(por_fundo), "\n")
fwrite(elig, file.path(REPO, "v2 OFICIAL/data/robo_replicante_por_fundo.csv"))

cat("\nDistribuicao do MAE:\n")
print(quantile(elig$mae, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 1)))

setorder(elig, mae)
cat("\n===== 10 mais REPLICANTES (menor MAE) =====\n")
print(elig[1:10, .(cod_fundo, gestora_grupo, n_meses, mae = round(mae, 5))])

setorder(elig, -mae)
cat("\n===== 10 mais HUMANOS (maior MAE) =====\n")
print(elig[1:10, .(cod_fundo, gestora_grupo, n_meses, mae = round(mae, 5))])

# ---- checagem de convergencia com o achado por gestora (Tabela 7) ----------
por_gestora <- elig[, .(n_fundos = .N, mae_medio = mean(mae)), by = gestora_grupo]
disp <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_gestora_dispersao.csv"))
chk <- merge(por_gestora, disp[, .(gestora_grupo, dp_gestora = dp)], by = "gestora_grupo")
r_conv <- cor(chk$mae_medio, chk$dp_gestora)
cat("\nCorrelacao (MAE medio dos fundos elegiveis da gestora) vs (dp do erro da gestora, Tabela 7):",
    round(r_conv, 3), "\n")

# ---- Figura A: histograma do MAE entre os fundos elegiveis -----------------
pdf(file.path(FIG, "hist_mae_fundo_v2.pdf"), width = 6, height = 4.5)
hist(elig$mae, breaks = 60, col = "#B0B8C1", border = "white",
     main = "", xlab = "MAE do erro e, por fundo (>= 24 meses)", ylab = "nº de fundos",
     xlim = c(0, 0.1))
dev.off()

# ---- Figura B: evolucao do erro do fundo mais extremo ("mais humano") -----
extremo <- elig[1, cod_fundo]  # apos o ultimo setorder(-mae), linha 1 = maior MAE
serie <- E[cod_fundo == extremo][order(ym)]
serie[, data := as.Date(paste0(substr(ym,1,4), "-", substr(ym,5,6), "-01"))]
pdf(file.path(FIG, "fig_erro_fundo_extremo_v2.pdf"), width = 7.5, height = 4.5)
par(mar = c(4,4,2.5,1))
plot(serie$data, serie$peso_vale3, type = "o", pch = 16, cex = 0.6, col = "#8A2E2E",
     ylim = range(c(serie$peso_vale3, serie$peso_pred)), xlab = "mês", ylab = "peso de VALE3",
     main = sprintf("Fundo %d (%s): peso real vs. previsto pela fórmula", extremo,
                     serie$gestora_grupo[1]))
lines(serie$data, serie$peso_pred, type = "o", pch = 16, cex = 0.6, col = "#2E5C8A")
legend("topleft", legend = c("peso real", "peso previsto (5 características)"),
       col = c("#8A2E2E","#2E5C8A"), lwd = 1.5, pch = 16, bty = "n", cex = 0.85)
dev.off()

cat("\nOK - salvo em 'v2 OFICIAL/data/robo_replicante_por_fundo.csv' e figuras\n")
