# =============================================================================
# _check_cobertura_mercado_vale3_60meses.R (temporario, NAO documentado no TCC)
#
# Generaliza a verificacao de cobertura de mercado da VALE3 (posicao +
# proxy de fluxo) para os 60 meses do painel (2017-01 a 2021-12).
#
# LIMITACAO conhecida e mais relevante aqui do que nas checagens de mes
# unico: acoes em circulacao fixadas em 5.129.910.942 (31/12/2020, 20-F),
# mas a Vale teve programas de recompra de acoes em 2017-2018 (ex.: ate
# 80 milhoes de acoes autorizadas em jul/2018, ~37mi recompradas ate
# set/2018) e suspendeu recompra/dividendo apos Brumadinho (jan/2019).
# Isso significa que pra 2017-2018 o numero de acoes em circulacao era
# um pouco MAIOR que 5,13bi (antes das recompras reduzirem o total) --
# usar o numero fixo de dez/2020 tende a SUBESTIMAR levemente o valor de
# mercado (e por consequencia SUPERESTIMAR levemente a cobertura) nos
# anos mais antigos. Efeito esperado pequeno (dezenas de milhoes de acoes
# em ~5,1 bilhoes, <2%), mas fica registrado.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
COTADIR <- file.path(REPO, "data/raw/cotahist")
ACOES_CIRCULACAO <- 5129910942  # 31/12/2020, Formulario 20-F Vale

# ---- COTAHIST 2017-2021: preco/volume diario VALE3, mercado a vista -------
dt_list <- list()
for (y in 2017:2021) {
  con <- file(file.path(COTADIR, sprintf("COTAHIST_A%d.TXT", y)), "r")
  lines <- readLines(con, n = -1L, encoding = "latin1"); close(con)
  lines01 <- lines[substr(lines, 1, 2) == "01"]
  codneg <- trimws(substr(lines01, 13, 24))
  tpmerc <- substr(lines01, 25, 27)
  sub <- lines01[codneg == "VALE3" & tpmerc == "010"]
  dt_list[[as.character(y)]] <- data.table(
    data = as.Date(substr(sub,3,10), format="%Y%m%d"),
    preult = as.numeric(substr(sub,109,121))/100,
    voltot = as.numeric(substr(sub,171,188))/100)
  cat(y, ": ", nrow(dt_list[[as.character(y)]]), "pregoes VALE3\n")
}
dt <- rbindlist(dt_list)
dt[, ym := year(data)*100L + month(data)]

preco_mensal <- dt[dt[, .I[which.max(data)], by = ym]$V1, .(ym, preco_fim_mes = preult)]
vol_mensal <- dt[, .(vol_mes = sum(voltot), n_pregoes = .N), by = ym]
mercado <- merge(preco_mensal, vol_mensal, by = "ym")
mercado[, valor_mercado := preco_fim_mes * ACOES_CIRCULACAO]

# ---- posicao VALE3 na nossa base, por mes -----------------------------------
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
vale <- pp[ativo == "VALE ON N1 - VALE3"]
vale[, valor_posicao := peso * aum_prev]
pos_mensal <- vale[, .(n_fundos = .N, valor_total = sum(valor_posicao, na.rm=TRUE)), by = ym]

# ---- junta tudo, calcula cobertura e proxy de fluxo -------------------------
res <- merge(mercado, pos_mensal, by = "ym", all.x = TRUE)
setorder(res, ym)
res[, cobertura_pct := 100 * valor_total / valor_mercado]
res[, mes_idx := (ym %/% 100L) * 12L + (ym %% 100L)]
res[, delta_posicao := valor_total - shift(valor_total)]
res[, consecutivo := mes_idx - shift(mes_idx) == 1L]
res[is.na(consecutivo), consecutivo := FALSE]
res[consecutivo == FALSE, delta_posicao := NA_real_]  # nao calcula "fluxo" entre meses nao consecutivos
res[, fluxo_proxy_pct := 100 * abs(delta_posicao) / vol_mes]

cat("\n===== Tabela completa (60 meses) =====\n")
print(res[, .(ym, n_fundos, valor_total_bi = round(valor_total/1e9,2),
              valor_mercado_bi = round(valor_mercado/1e9,1),
              cobertura_pct = round(cobertura_pct,2),
              fluxo_proxy_pct = round(fluxo_proxy_pct,2))])

cat("\n===== Resumo da cobertura de posicao (60 meses) =====\n")
print(summary(res$cobertura_pct))
cat("\n===== Resumo do proxy de fluxo (meses com par consecutivo) =====\n")
print(summary(res$fluxo_proxy_pct))

fwrite(res, file.path(REPO, "v2 OFICIAL/data/_cobertura_mercado_vale3_60meses.csv"))

# ---- grafico -----------------------------------------------------------------
FIG <- file.path(REPO, "v2 OFICIAL/figuras/_temp_cobertura_mercado_vale3.png")
res[, data_ref := as.Date(paste0(substr(ym,1,4),"-",substr(ym,5,6),"-01"))]

png(FIG, width = 1100, height = 750, res = 120)
par(mfrow = c(2,1), mar = c(4,4.5,3,1))

plot(res$data_ref, res$cobertura_pct, type = "l", lwd = 2, col = "#3B6E9E",
     xlab = "", ylab = "Cobertura de posição (%)",
     main = "Cobertura de posição: % do valor de mercado da VALE3 detido pelos fundos da base",
     ylim = c(0, max(res$cobertura_pct, na.rm=TRUE)*1.1))
abline(h = mean(res$cobertura_pct, na.rm=TRUE), col = "grey50", lty = 2)
legend("topleft", legend = sprintf("média = %.2f%%", mean(res$cobertura_pct, na.rm=TRUE)),
       lty = 2, col = "grey50", bty = "n")

plot(res$data_ref, res$fluxo_proxy_pct, type = "l", lwd = 2, col = "#C1633B",
     xlab = "", ylab = "Proxy de fluxo (%)",
     main = "Proxy de fluxo: |Δ posição mês a mês| / volume total negociado no mês")
abline(h = median(res$fluxo_proxy_pct, na.rm=TRUE), col = "grey50", lty = 2)
legend("topleft", legend = sprintf("mediana = %.2f%%", median(res$fluxo_proxy_pct, na.rm=TRUE)),
       lty = 2, col = "grey50", bty = "n")

dev.off()
cat("\nOK - grafico salvo em", FIG, "\n")
