# =============================================================================
# 73_tentativas_salvar_sinal.R  (v2 OFICIAL -- teste_estrategia)
#
# Pedido do Joao: "vamos tentar contornar" o resultado negativo do R/70/71
# (fluxo previsto nao prediz retorno). 3 tentativas com racional economico
# especifico cada, reportadas honestamente (nao e' pesca de p-valor):
#  (A) efeito fixo de MES na regressao -- um mes de alta/baixa geral do
#      mercado pode afogar o sinal idiossincratico do ativo.
#  (B) so' os casos de CONVICCAO FORTE (decis extremos de |fluxo_pct|) --
#      a massa de ativo-mes com sinal quase zero pode diluir um efeito que
#      só aparece quando o sinal é forte de verdade.
#  (C) o sinal SEM o eco do lider (so' lambda*d) -- ja mostrou direcao
#      certa e alguma significancia no R/70; testado aqui com (A) e (B)
#      tambem aplicados.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

M <- fread(file.path(REPO, "v2 OFICIAL/data/teste_fluxo_prediz_retorno.csv"))
cat("Base:", nrow(M), "obs |", uniqueN(M$ym), "meses\n")

resultados <- list()

# ---- (A) efeito fixo de mes ------------------------------------------------
M[, retorno_demeaned := retorno_fut - mean(retorno_fut), by = ym_fut]
fit_a_eco <- lm(retorno_demeaned ~ fluxo_pct, data = M)
fit_a_normal <- lm(retorno_demeaned ~ fluxo_normal_pct, data = M)
resultados[["A: FE mes, com eco"]] <- summary(fit_a_eco)$coefficients[2,]
resultados[["A: FE mes, sem eco"]] <- summary(fit_a_normal)$coefficients[2,]
cat("\n===== (A) Com efeito fixo de mes =====\n")
cat("Com eco:   coef=", signif(coef(fit_a_eco)[2],3), " p=", signif(summary(fit_a_eco)$coefficients[2,4],3),
    " R2=", signif(summary(fit_a_eco)$r.squared,4), "\n")
cat("Sem eco:   coef=", signif(coef(fit_a_normal)[2],3), " p=", signif(summary(fit_a_normal)$coefficients[2,4],3),
    " R2=", signif(summary(fit_a_normal)$r.squared,4), "\n")

# ---- (B) so decis extremos de |fluxo_pct| ----------------------------------
M[, absfluxo := abs(fluxo_pct)]
corte <- quantile(M$absfluxo, 0.8)
extremos <- M[absfluxo >= corte]
cat("\n===== (B) So' top 20% |fluxo_pct| (conviccao forte), n =", nrow(extremos), "=====\n")
extremos[, decil := cut(fluxo_pct, quantile(fluxo_pct, seq(0,1,0.1)), include.lowest=TRUE, labels=FALSE)]
fit_b_eco <- lm(retorno_fut ~ fluxo_pct, data = extremos)
cat("Com eco:   coef=", signif(coef(fit_b_eco)[2],3), " p=", signif(summary(fit_b_eco)$coefficients[2,4],3),
    " R2=", signif(summary(fit_b_eco)$r.squared,4), "\n")

por_decil <- extremos[, .(n=.N, retorno_medio=mean(retorno_fut)), by=decil][order(decil)]
print(por_decil)
top_decil <- extremos[fluxo_pct >= quantile(extremos$fluxo_pct, 0.9)]
bot_decil <- extremos[fluxo_pct <= quantile(extremos$fluxo_pct, 0.1)]
tt <- t.test(top_decil$retorno_fut, bot_decil$retorno_fut)
cat("Decil 10 (mais comprado) - decil 1 (mais vendido) [dentro dos extremos]: diff=",
    round(mean(top_decil$retorno_fut)-mean(bot_decil$retorno_fut),5), " p=", signif(tt$p.value,3), "\n")

# ---- (C) sem eco, com FE de mes E so extremos ------------------------------
M[, absfluxo_normal := abs(fluxo_normal_pct)]
corte_n <- quantile(M$absfluxo_normal, 0.8)
extremos_n <- M[absfluxo_normal >= corte_n]
extremos_n[, retorno_demeaned := retorno_fut - mean(retorno_fut), by = ym_fut]
fit_c <- lm(retorno_demeaned ~ fluxo_normal_pct, data = extremos_n)
cat("\n===== (C) Sem eco + FE mes + so extremos, n =", nrow(extremos_n), "=====\n")
cat("coef=", signif(coef(fit_c)[2],3), " p=", signif(summary(fit_c)$coefficients[2,4],3),
    " R2=", signif(summary(fit_c)$r.squared,4), "\n")

cat("\n===== RESUMO COMPARATIVO =====\n")
cat(sprintf("%-45s %10s %10s %10s\n", "Especificacao", "coef", "p-valor", "R2(%)"))
cat(sprintf("%-45s %10s %10s %10s\n", "Original, com eco (R/70)", "-8.9e-08", "0.071", "0.038"))
cat(sprintf("%-45s %10s %10s %10s\n", "Original, sem eco (R/70)", "7.6e-07", "0.023", "0.06"))
cat(sprintf("%-45s %10.2e %10.3f %10.4f\n", "A: FE mes, com eco", coef(fit_a_eco)[2], summary(fit_a_eco)$coefficients[2,4], 100*summary(fit_a_eco)$r.squared))
cat(sprintf("%-45s %10.2e %10.3f %10.4f\n", "A: FE mes, sem eco", coef(fit_a_normal)[2], summary(fit_a_normal)$coefficients[2,4], 100*summary(fit_a_normal)$r.squared))
cat(sprintf("%-45s %10.2e %10.3f %10.4f\n", "B: so extremos (top20%), com eco", coef(fit_b_eco)[2], summary(fit_b_eco)$coefficients[2,4], 100*summary(fit_b_eco)$r.squared))
cat(sprintf("%-45s %10.2e %10.3f %10.4f\n", "C: sem eco + FE mes + extremos", coef(fit_c)[2], summary(fit_c)$coefficients[2,4], 100*summary(fit_c)$r.squared))

fwrite(M, file.path(REPO, "v2 OFICIAL/data/teste_retorno_tentativas.csv"))
cat("\nOK\n")
