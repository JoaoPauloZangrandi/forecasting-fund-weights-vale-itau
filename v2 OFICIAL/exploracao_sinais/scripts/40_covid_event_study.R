# =============================================================================
# 40_covid_event_study.R  (exploracao_sinais)
#
# Teste de MAIOR PODER ESTATISTICO: em vez de serie temporal de 24 meses
# (N=24 observacoes), usa 1 UNICO corte transversal com N~400-500 acoes --
# HHI de posse ANTES do choque (dez/2019 ou jan/2020) prevendo: (a) queda
# de preco durante o crash (fev-mar/2020), (b) volatilidade realizada nos
# meses seguintes, (c) magnitude da recuperacao (abr-dez/2020). Design mais
# fiel ao mecanismo original de Coval-Stafford/Greenwood-Thesmar (CHOQUE de
# liquidacao, nao efeito medio incondicional) e ao que a pesquisa de
# literatura recomendou (Falato-Goldstein-Hortacsu 2021 fizeram exatamente
# isso pra fundos de bonds nos EUA).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
meses <- sort(unique(precos$ymk))
cat("Meses disponiveis 2019-10 a 2021-01:\n")
print(meses[meses >= 201910 & meses <= 202101])

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

MES_PRE <- 201912L
hhi_pre <- crowd[ym == MES_PRE, .(ticker, hhi_posse)]
cat(sprintf("\nAcoes com HHI valido em %d: %d\n", MES_PRE, nrow(hhi_pre)))

# retorno acumulado durante o crash (fev+mar/2020) e na recuperacao (abr-dez/2020)
crash <- precos[ymk %in% c(202002,202003), .(ret_crash = prod(1+retorno)-1), by=ticker]
recuperacao <- precos[ymk %in% 202004:202012, .(ret_recup = prod(1+retorno)-1), by=ticker]
vol_pos_crash <- precos[ymk %in% 202004:202012, .(vol_pos_crash = sd(retorno)), by=ticker]

ev <- merge(hhi_pre, crash, by="ticker")
ev <- merge(ev, recuperacao, by="ticker")
ev <- merge(ev, vol_pos_crash, by="ticker")
ev <- ev[is.finite(hhi_posse) & is.finite(ret_crash) & is.finite(ret_recup) & is.finite(vol_pos_crash)]
cat(sprintf("N final no evento: %d acoes\n\n", nrow(ev)))

cat("===== HHI (dez/2019) prevendo queda durante o crash (fev+mar/2020) =====\n")
fit1 <- lm(ret_crash ~ hhi_posse, data = ev)
print(summary(fit1))

cat("\n===== HHI (dez/2019) prevendo retorno na recuperacao (abr-dez/2020) =====\n")
fit2 <- lm(ret_recup ~ hhi_posse, data = ev)
print(summary(fit2))

cat("\n===== HHI (dez/2019) prevendo volatilidade pos-crash (abr-dez/2020) =====\n")
fit3 <- lm(vol_pos_crash ~ hhi_posse, data = ev)
print(summary(fit3))

cat("\n===== Por quintil de HHI: media do retorno no crash e na recuperacao =====\n")
ev[, quintil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:5/5), include.lowest=TRUE))]
tab <- ev[, .(n=.N, ret_crash_medio=mean(ret_crash), ret_recup_medio=mean(ret_recup),
              vol_pos_crash_media=mean(vol_pos_crash)), by=quintil_hhi][order(quintil_hhi)]
print(tab)

fwrite(ev, file.path(OUT, "candidatos_40_covid_evento.csv"))
cat("\nOK\n")
