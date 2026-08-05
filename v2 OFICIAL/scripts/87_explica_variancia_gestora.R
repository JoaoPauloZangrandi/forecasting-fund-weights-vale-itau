# =============================================================================
# 87_explica_variancia_gestora.R  (v2 OFICIAL)
#
# Segue o script 86 (dispersao do erro por gestora e persistente,
# corr=0,78). Pergunta agora: o que EXPLICA essa dispersao? Cruza o
# desvio-padrao do erro fora da amostra de cada gestora (h=1, mesmo
# filtro de posicao-poeira de sempre) contra tres caracteristicas de
# gestora:
#   - beta medio do fundo (ja teoria, Figura 13, mas la era vs RMSE
#     inteiro; aqui isola so a parte de dispersao)
#   - tamanho medio (log AUM)
#   - concentracao media da carteira (indice HHI dos pesos por ativo,
#     dentro de cada fundo-mes, depois medido por gestora)
#
# CORRIGIDO (pedido do Joao): a primeira versao calculava essas tres
# caracteristicas usando os 59 meses inteiros do painel (fev/2017 a
# dez/2021), que INCLUI o proprio periodo de teste (2020-2021) onde
# dp_erro e medido -- nao e look-ahead bias no sentido usual (essa
# regressao nao e um modelo preditivo, e descritiva), mas mistura
# caracteristica "explicativa" com o mesmo periodo do erro que ela
# tenta explicar, o que enfraquece a leitura causal. Agora as
# caracteristicas usam SO o periodo de treino (<2020-01), genuinamente
# predeterminadas em relacao ao dp_erro (so periodo de teste) --
# mesma disciplina usada em todo o resto do modelo (lambda, etc.).
# Resultado: conclusao se mantem (HHI ainda o mais forte).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

# --- dispersao do erro por gestora (mesma base/filtro dos scripts 77/84/86) ---
h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","peso","ym","gestora_grupo","beta_fundo","l_aum"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))

dp_gestora <- h1f[, .(dp_erro = sd(erro_oos), n = .N), by = gestora_grupo]
dp_gestora <- dp_gestora[n >= 100]
cat("Gestoras com dispersao estimavel (n>=100):", nrow(dp_gestora), "\n")

# --- caracteristicas de gestora, SO periodo de treino (<2020-01) ---
pp_treino <- pp[ym < 202001]
carac_fundo <- pp_treino[, .(l_aum = mean(l_aum, na.rm=TRUE), beta_fundo = mean(beta_fundo, na.rm=TRUE)),
                   by = .(gestora_grupo)]

hhi_fundo_mes <- pp_treino[, .(hhi = sum(peso^2)), by = .(cod_fundo, ym, gestora_grupo)]
hhi_gestora <- hhi_fundo_mes[, .(hhi_medio = mean(hhi, na.rm=TRUE)), by = gestora_grupo]

carac <- merge(carac_fundo, hhi_gestora, by = "gestora_grupo")
d <- merge(dp_gestora, carac, by = "gestora_grupo")
d <- d[is.finite(l_aum) & is.finite(beta_fundo) & is.finite(hhi_medio)]
cat("Gestoras com caracteristicas completas:", nrow(d), "\n")

cat("\n===== Correlacoes: dispersao do erro (dp_erro) vs. caracteristicas de gestora =====\n")
cat(sprintf("dp_erro x beta medio do fundo   : corr = %.4f\n", cor(d$dp_erro, d$beta_fundo)))
cat(sprintf("dp_erro x tamanho medio (l_aum)  : corr = %.4f\n", cor(d$dp_erro, d$l_aum)))
cat(sprintf("dp_erro x concentracao (HHI)     : corr = %.4f\n", cor(d$dp_erro, d$hhi_medio)))

fit_beta <- lm(dp_erro ~ beta_fundo, data = d)
fit_aum  <- lm(dp_erro ~ l_aum, data = d)
fit_hhi  <- lm(dp_erro ~ hhi_medio, data = d)
cat(sprintf("\nR2 (beta)  = %.4f | R2 (tamanho) = %.4f | R2 (HHI) = %.4f\n",
            summary(fit_beta)$r.squared, summary(fit_aum)$r.squared, summary(fit_hhi)$r.squared))

fit_full <- lm(dp_erro ~ beta_fundo + l_aum + hhi_medio, data = d)
cat("\n===== Regressao multipla (as 3 caracteristicas juntas) =====\n")
print(summary(fit_full)$coefficients)
cat(sprintf("R2 (multipla) = %.4f\n", summary(fit_full)$r.squared))

pdf(file.path(FIG, "fig_explica_variancia_gestora.pdf"), width = 10, height = 4.3)
par(mfrow = c(1,3), mar = c(4.5,4.5,3,1), oma = c(0,0,2.2,0))
plot(d$beta_fundo, d$dp_erro, pch=19, col=adjustcolor("#3B6E9E",0.65),
     xlab="Beta médio do fundo", ylab="Desvio-padrão do erro fora da amostra (h=1)",
     main=sprintf("vs. Beta\ncorr=%.2f", cor(d$dp_erro,d$beta_fundo)))
abline(fit_beta, col="firebrick", lwd=2)
plot(d$l_aum, d$dp_erro, pch=19, col=adjustcolor("#3B6E9E",0.65),
     xlab="Tamanho médio (log AUM)", ylab="",
     main=sprintf("vs. Tamanho\ncorr=%.2f", cor(d$dp_erro,d$l_aum)))
abline(fit_aum, col="firebrick", lwd=2)
plot(d$hhi_medio, d$dp_erro, pch=19, col=adjustcolor("#3B6E9E",0.65),
     xlab="Concentração média da carteira (HHI)", ylab="",
     main=sprintf("vs. Concentração\ncorr=%.2f", cor(d$dp_erro,d$hhi_medio)))
abline(fit_hhi, col="firebrick", lwd=2)
mtext("O que explica a dispersão do erro por gestora?", outer=TRUE, line=0.6, cex=1.05, font=2)
dev.off()
cat("\nOK - fig_explica_variancia_gestora.pdf salva\n")

fwrite(d, file.path(REPO, "v2 OFICIAL/data/explica_variancia_gestora.csv"))
cat("OK\n")
