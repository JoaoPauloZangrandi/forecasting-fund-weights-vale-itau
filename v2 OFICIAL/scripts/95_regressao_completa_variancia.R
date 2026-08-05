# =============================================================================
# 95_regressao_completa_variancia.R  (v2 OFICIAL)
#
# Pedido do Joao: a equacao 4 (script 87) so testava beta/tamanho/HHI
# contra a dispersao do erro por gestora -- por que nao as 5
# caracteristicas da Etapa 1 inteiras? Resposta: a comparacao nasceu de
# uma linha de investigacao especifica (beta vs concentracao), nao de um
# teste exaustivo. Este script expande para as 5 caracteristicas
# (beta, tamanho, cotistas, FIC, fluxo) + concentracao.
#
# CORRIGIDO (pedido do Joao, mesma correcao do script 87): as
# caracteristicas agora usam SO o periodo de treino (<2020-01), nao os
# 59 meses inteiros -- genuinamente predeterminadas em relacao ao
# dp_erro (medido so no periodo de teste). Com a correcao, HHI continua
# o unico claramente significativo (t=4,18, p=0,0002); fluxo/AUM, que
# antes chegava perto (p=0,07) usando dado contemporaneo, fica bem mais
# fraco com dado so do treino (t=-1,42, p=0,16) -- sugere que aquele
# quase-sinal era em parte um artefato da sobreposicao de periodo, nao
# um efeito real. R2 = 0,54 (era 0,58 na versao com vazamento).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
h1f <- merge(h1, cel[peso_mediano >= 0.001], by = c("cod_fundo","ativo"))

dp_gestora <- h1f[, .(dp_erro = sd(erro_oos), n = .N), by = gestora_grupo]
dp_gestora <- dp_gestora[n >= 100]

pp_treino <- pp[ym < 202001]
carac_fundo <- pp_treino[, .(l_aum = mean(l_aum, na.rm=TRUE), l_cot = mean(l_cot, na.rm=TRUE),
                       beta_fundo = mean(beta_fundo, na.rm=TRUE),
                       flow_aum = mean(flow_aum, na.rm=TRUE),
                       pct_fic = mean(is_fic, na.rm=TRUE)),
                   by = gestora_grupo]

hhi_fundo_mes <- pp_treino[, .(hhi = sum(peso^2)), by = .(cod_fundo, ym, gestora_grupo)]
hhi_gestora <- hhi_fundo_mes[, .(hhi_medio = mean(hhi, na.rm=TRUE)), by = gestora_grupo]

d <- merge(dp_gestora, carac_fundo, by = "gestora_grupo")
d <- merge(d, hhi_gestora, by = "gestora_grupo")
d <- d[is.finite(l_aum) & is.finite(l_cot) & is.finite(beta_fundo) & is.finite(flow_aum) & is.finite(hhi_medio)]
cat("Gestoras na regressao completa:", nrow(d), "\n\n")

cat("===== Regressao com as 5 caracteristicas da Etapa 1 + concentracao (versao final, no TCC) =====\n")
cat("Caracteristicas calculadas SO com dado de treino (<2020-01), predeterminadas.\n")
fit_full <- lm(dp_erro ~ beta_fundo + l_aum + l_cot + pct_fic + flow_aum + hhi_medio, data = d)
print(summary(fit_full)$coefficients)
cat("R2:", summary(fit_full)$r.squared, "\n")

fwrite(d, file.path(REPO, "v2 OFICIAL/data/regressao_completa_variancia_dados.csv"))
cat("\nOK\n")
