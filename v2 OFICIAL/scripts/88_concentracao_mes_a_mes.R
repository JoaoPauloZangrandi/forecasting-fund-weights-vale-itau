# =============================================================================
# 88_concentracao_mes_a_mes.R  (v2 OFICIAL)
#
# Segue o script 87 (concentracao media por gestora explica variancia do
# erro, corr=0,76 -- mas so 40 pontos, um por gestora). Refina para o
# nivel FUNDO-MES (dezenas de milhares de pares): a concentracao da
# carteira de um fundo especifico, num mes especifico, prediz o erro
# fora da amostra daquele fundo naquele mes? Tres versoes:
#
#  A) Contemporanea: HHI do mes t vs erro do mes t.
#  B) Defasada: HHI do mes t-1 (ja conhecido antes do erro) vs erro do
#     mes t -- testa precedencia temporal, nao so coincidencia.
#  C) Dentro do fundo (demeaned): tirando a media de cada fundo dos dois
#     lados, isola se um fundo ficando MAIS concentrado do que o normal
#     dele mesmo coincide com erro MAIOR do que o normal dele mesmo --
#     separa o efeito "tipo de fundo" (ja mostrado no script 87) do
#     efeito temporal dentro do mesmo fundo.
#
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","peso","ym"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))

erro_fm <- h1f[, .(erro_rms = sqrt(mean(erro_oos^2)), n_ativos = .N), by = .(cod_fundo, ym)]
hhi_fm  <- pp[, .(hhi = sum(peso^2)), by = .(cod_fundo, ym)]
cat("Pares fundo-mes com erro:", nrow(erro_fm), "| com HHI:", nrow(hhi_fm), "\n")

# --- A) contemporanea ---
dA <- merge(erro_fm, hhi_fm, by = c("cod_fundo","ym"))
cat("\n===== A) HHI contemporaneo (mes t) vs erro (mes t) =====\n")
cat("Pares fundo-mes:", nrow(dA), "| fundos distintos:", uniqueN(dA$cod_fundo), "\n")
cat(sprintf("Correlacao (Pearson) = %.4f | (Spearman) = %.4f\n",
            cor(dA$hhi, dA$erro_rms), cor(dA$hhi, dA$erro_rms, method="spearman")))

# --- B) defasada: HHI do mes anterior explica erro do mes atual ---
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
hhi_lag <- copy(hhi_fm); hhi_lag[, ym := addm(ym, 1L)]; setnames(hhi_lag, "hhi", "hhi_lag")
dB <- merge(erro_fm, hhi_lag, by = c("cod_fundo","ym"))
cat("\n===== B) HHI DEFASADO (mes t-1) vs erro (mes t) =====\n")
cat("Pares fundo-mes:", nrow(dB), "| fundos distintos:", uniqueN(dB$cod_fundo), "\n")
cat(sprintf("Correlacao (Pearson) = %.4f | (Spearman) = %.4f\n",
            cor(dB$hhi_lag, dB$erro_rms), cor(dB$hhi_lag, dB$erro_rms, method="spearman")))

# --- C) dentro do fundo (demeaned) ---
n_fundo <- dA[, .N, by = cod_fundo]
fundos_ok <- n_fundo[N >= 10]$cod_fundo
dC <- dA[cod_fundo %in% fundos_ok]
dC[, hhi_dm := hhi - mean(hhi), by = cod_fundo]
dC[, erro_dm := erro_rms - mean(erro_rms), by = cod_fundo]
cat("\n===== C) DENTRO DO FUNDO (demeaned): fundos com >=10 meses =====\n")
cat("Fundos:", length(fundos_ok), "| pares fundo-mes:", nrow(dC), "\n")
cat(sprintf("Correlacao (Pearson) = %.4f | (Spearman) = %.4f\n",
            cor(dC$hhi_dm, dC$erro_dm), cor(dC$hhi_dm, dC$erro_dm, method="spearman")))

# --- binned (decis de HHI) pra cada versao, pra visualizar com N grande ---
binned <- function(x, y, nbins = 10) {
  q <- quantile(x, probs = seq(0,1,length.out = nbins+1), na.rm = TRUE, type = 7)
  q[1] <- q[1] - 1e-9
  bin <- cut(x, q, labels = FALSE)
  agg <- data.table(bin = bin, x = x, y = y)[, .(x_medio = mean(x), y_medio = mean(y),
                                                    y_se = sd(y)/sqrt(.N), n = .N), by = bin]
  setorder(agg, bin)
  agg
}
bA <- binned(dA$hhi, dA$erro_rms)
bB <- binned(dB$hhi_lag, dB$erro_rms)
bC <- binned(dC$hhi_dm, dC$erro_dm)

pdf(file.path(FIG, "fig_concentracao_mes_a_mes.pdf"), width = 11, height = 4.3)
par(mfrow = c(1,3), mar = c(4.5,4.5,3,1), oma = c(0,0,2.2,0))

plot(bA$x_medio, bA$y_medio, pch=19, col="#3B6E9E", type="b",
     xlab="HHI da carteira, mês t (decis)", ylab="Erro fora da amostra (RMS), mês t",
     main=sprintf("A) Contemporânea\ncorr=%.2f (N=%s)", cor(dA$hhi,dA$erro_rms), format(nrow(dA),big.mark=".")))
arrows(bA$x_medio, bA$y_medio-bA$y_se, bA$x_medio, bA$y_medio+bA$y_se, angle=90, code=3, length=0.03, col="#3B6E9E")

plot(bB$x_medio, bB$y_medio, pch=19, col="#B8452E", type="b",
     xlab="HHI da carteira, mês t-1 (decis)", ylab="Erro fora da amostra (RMS), mês t",
     main=sprintf("B) Defasada (t-1 → t)\ncorr=%.2f (N=%s)", cor(dB$hhi_lag,dB$erro_rms), format(nrow(dB),big.mark=".")))
arrows(bB$x_medio, bB$y_medio-bB$y_se, bB$x_medio, bB$y_medio+bB$y_se, angle=90, code=3, length=0.03, col="#B8452E")

plot(bC$x_medio, bC$y_medio, pch=19, col="#5B8C3A", type="b",
     xlab="HHI - média do fundo (decis)", ylab="Erro - média do fundo",
     main=sprintf("C) Dentro do fundo\ncorr=%.2f (N=%s)", cor(dC$hhi_dm,dC$erro_dm), format(nrow(dC),big.mark=".")))
abline(h=0,v=0,col="grey80",lty=2)
arrows(bC$x_medio, bC$y_medio-bC$y_se, bC$x_medio, bC$y_medio+bC$y_se, angle=90, code=3, length=0.03, col="#5B8C3A")

mtext("Concentração da carteira (HHI) e erro fora da amostra, nível fundo-mês", outer=TRUE, line=0.6, cex=1.05, font=2)
dev.off()
cat("\nOK - fig_concentracao_mes_a_mes.pdf salva\n")

fwrite(dA, file.path(REPO, "v2 OFICIAL/data/concentracao_mes_a_mes_A.csv"))
fwrite(dB, file.path(REPO, "v2 OFICIAL/data/concentracao_mes_a_mes_B.csv"))
fwrite(dC, file.path(REPO, "v2 OFICIAL/data/concentracao_mes_a_mes_C.csv"))
cat("OK\n")
