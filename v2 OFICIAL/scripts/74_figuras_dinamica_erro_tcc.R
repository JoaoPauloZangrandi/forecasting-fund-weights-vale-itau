# =============================================================================
# 74_figuras_dinamica_erro_tcc.R  (v2 OFICIAL)
#
# Pedido do orientador (30-31/07/2026, via Joao): a Secao de Resultados do
# TCC precisa de MAIS figuras explorando a DINAMICA dos erros (nao so
# tabelas estaticas de RMSE agregado) -- no mesmo espirito das figuras que
# ja existem na parte de estimativas (evolucao mensal de theta, evolucao
# mensal do APE por gestora, Figura 9).
#
# Usa a base JA VALIDADA da Etapa 3 generalizada (scripts/42, arquivo
# etapa3_multiativo_h1.csv/h3.csv -- universo de 2.820 fundos/41 gestoras
# que ja fundamenta as Tabelas 25/26 do v2 OFICIAL.tex), NAO a base do
# teste_estrategia (universo expandido, ignorado por pedido explicito
# desta rodada).
#
# 2 figuras novas:
#  (1) evolucao mensal do erro fora da amostra (h=1), por gestora -- so as
#      3 gestoras mais dificeis e as 3 mais previsiveis em TODOS os
#      horizontes (achado da Tabela 25/26: Guepardo/Ibiuna/AZ Quest vs.
#      Plural/TNA/Gavea) -- mesmo estilo da Figura 9 (evolucao APE gestora).
#  (2) erro fora da amostra agregado (nuvem + media + dp mensal), h=1 vs
#      h=3 lado a lado -- mesmo estilo das Figuras 3/6 (evolucao erro e/u),
#      agora pro erro fora da amostra generalizado.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
FIG <- file.path(REPO, "v2 OFICIAL/figuras")

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
h3 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))
cat("h1:", nrow(h1), "obs |", uniqueN(h1$gestora_grupo), "gestoras\n")
cat("h3:", nrow(h3), "obs |", uniqueN(h3$gestora_grupo), "gestoras\n")

# ---- Figura A: evolucao mensal do erro OOS por gestora (extremos da Tabela 25/26) ----
dificeis  <- c("Ibiuna Investimentos", "Guepardo Investimentos", "AZ Quest")
faceis    <- c("Plural", "TNA Gestão Patrimonial", "Gávea Investimentos")

por_mes_gestora <- h1[gestora_grupo %in% c(dificeis, faceis),
                       .(erro_medio = mean(erro_oos), n = .N), by = .(gestora_grupo, ym)]
por_mes_gestora[, data := as.Date(paste0(substr(ym,1,4),"-",substr(ym,5,6),"-01"))]
setorder(por_mes_gestora, gestora_grupo, data)

pdf(file.path(FIG, "fig_dinamica_erro_gestora_extremos.pdf"), width = 8, height = 5.5)
par(mfrow = c(1,2), mar = c(4,4,2.5,1))
cores_d <- c("#B8452E","#D98A3D","#8A3E9E")
cores_f <- c("#2E5C8A","#1E7A4D","#5DA9C7")

ylim_comum <- range(por_mes_gestora$erro_medio, na.rm=TRUE)
eixo_x_datas <- range(por_mes_gestora$data)
plot(eixo_x_datas, c(0,0), type="n", xaxt="n", xlim=eixo_x_datas, ylim=ylim_comum,
     xlab="Mês (teste, out-of-sample)",
     ylab="Erro médio mensal (h=1)", main="As 3 gestoras mais difíceis\n(Ibiuna, Guepardo, AZ Quest)")
axis.Date(1, at=seq(eixo_x_datas[1], eixo_x_datas[2], by="4 months"), format="%b/%Y")
abline(h=0, col="grey80")
for (i in seq_along(dificeis)) {
  dm <- por_mes_gestora[gestora_grupo==dificeis[i]]
  lines(dm$data, dm$erro_medio, col=cores_d[i], lwd=2)
}
legend("topleft", legend=dificeis, col=cores_d, lwd=2, bty="n", cex=0.65)

plot(eixo_x_datas, c(0,0), type="n", xaxt="n", xlim=eixo_x_datas, ylim=ylim_comum,
     xlab="Mês (teste, out-of-sample)",
     ylab="Erro médio mensal (h=1)", main="As 3 gestoras mais previsíveis\n(Plural, TNA, Gávea)")
axis.Date(1, at=seq(eixo_x_datas[1], eixo_x_datas[2], by="4 months"), format="%b/%Y")
abline(h=0, col="grey80")
for (i in seq_along(faceis)) {
  dm <- por_mes_gestora[gestora_grupo==faceis[i]]
  lines(dm$data, dm$erro_medio, col=cores_f[i], lwd=2)
}
legend("topleft", legend=faceis, col=cores_f, lwd=2, bty="n", cex=0.65)
dev.off()
cat("Figura A OK\n")

# ---- Figura B: erro OOS agregado, nuvem + media + dp mensal, h=1 vs h=3 ----
prep <- function(dt, h) {
  dt[, data := as.Date(paste0(substr(ym,1,4),"-",substr(ym,5,6),"-01"))]
  agg <- dt[, .(media = mean(erro_oos), dp = sd(erro_oos), n = .N), by = data]
  setorder(agg, data)
  list(dt = dt, agg = agg)
}
p1 <- prep(h1, 1); p3 <- prep(h3, 3)

pdf(file.path(FIG, "fig_dinamica_erro_oos_agregado.pdf"), width = 8, height = 5.5)
par(mfrow = c(1,2), mar = c(4,4,2.5,1))
faz_painel <- function(p, titulo, ylim) {
  amostra <- p$dt[sample(.N, min(30000,.N))]
  plot(amostra$data, amostra$erro_oos, pch=".", col=adjustcolor("grey60",0.3),
       ylim=ylim, xlab="Mês (teste, out-of-sample)", ylab="Erro fora da amostra", main=titulo)
  lines(p$agg$data, p$agg$media, col="#2E5C8A", lwd=2)
  lines(p$agg$data, p$agg$dp, col="#B8452E", lwd=2, lty=2)
  abline(h=0, col="grey40")
  legend("topleft", legend=c("Média mensal","Desvio-padrão mensal"), col=c("#2E5C8A","#B8452E"),
         lwd=2, lty=c(1,2), bty="n", cex=0.65)
}
ylim_c <- c(-0.05, 0.08)
faz_painel(p1, "h = 1 mês", ylim_c)
faz_painel(p3, "h = 3 meses", ylim_c)
dev.off()
cat("Figura B OK\n")

cat("\nOK - 2 figuras salvas em 'v2 OFICIAL/figuras/'\n")
