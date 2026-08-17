# =============================================================================
# 105_fluxo_induzido_agregado.R  (v2 OFICIAL)
#
# Pedido do Joao (12/08/2026): a Secao 6 precisa de um achado operavel de
# verdade. O "robo caca-replicantes" (pares lider->seguidor, scripts 63-73)
# ja foi testado e rejeitado (fluxo do sinal nao prediz retorno, R2~0).
# Pesquisa de literatura (Coval & Stafford 2007 "Asset Fire Sales (and
# Purchases) in Equity Markets"; Lou 2012 "A Flow-Based Explanation for
# Return Predictability") sugere um desenho DIFERENTE: em vez de correlacao
# de residuos entre pares de fundos, agregar a PRESSAO DE FLUXO MECANICA de
# TODOS os fundos que carregam um ativo, ponderada pelo tamanho da posicao
# (nao residuo de nenhum modelo -- fluxo bruto de captacao/resgate, que ja
# temos predeterminado no painel).
#
# Flow-Induced Trading (FIT), formula de Lou (2012), adaptada ao nosso
# painel:
#   FIT_{n,t} = sum_i [ peso_{i,n,t-1} * AUM_{i,t-1} * fluxo_aum_{i,t} ]
#             / sum_i [ peso_{i,n,t-1} * AUM_{i,t-1} ]
# = media do fluxo/AUM dos fundos que carregam o ativo n, ponderada pelo
# tamanho (em R$) da posicao de cada um em n. Interpretacao: "os fundos que
# mais carregam o ativo n estao vendo X% de captacao/resgate liquido -- se
# ajustarem posicoes proporcionalmente (Lou assume isso, nao residuo de
# nenhum modelo de skill), isso empurra o preco de n mecanicamente".
#
# Testa: FIT prediz retorno CONTEMPORANEO (pressao mecanica, mes t) e depois
# REVERTE (t+3, t+6, t+12) -- assinatura classica de fire sale / demanda
# nao-informada (Coval-Stafford), diferente de informacao genuina (que nao
# reverteria). Tambem condiciona em fluxo EXTREMO (decis) e em concentracao
# de posse (poucos fundos = mais "crowded" = mais impacto esperado), ambos
# achados centrais da literatura.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
pp[, cod_fundo := as.character(cod_fundo)]
cat("Painel:", nrow(pp), "linhas |", uniqueN(pp$cod_fundo), "fundos |", uniqueN(pp$ativo), "ativos\n")

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# ---- peso defasado (t-1), mesmo padrao de self-join do HHI_resto -----------
peso_prev <- pp[, .(cod_fundo, ativo, ym = addm(ym, 1), peso_prev = peso)]
d <- merge(pp[, .(cod_fundo, ativo, ym, aum_prev, flow_aum)], peso_prev,
           by = c("cod_fundo","ativo","ym"))
d <- d[is.finite(peso_prev) & peso_prev > 0 & is.finite(aum_prev) & is.finite(flow_aum)]
d[, peso_valor := peso_prev * aum_prev]
cat("Obs com peso_prev/aum/fluxo validos:", nrow(d), "\n")

# ---- FIT por ativo-mes: media ponderada do fluxo, peso = posicao em R$ ----
fit <- d[, .(FIT = sum(peso_valor * flow_aum) / sum(peso_valor),
             valor_total = sum(peso_valor),
             n_fundos = uniqueN(cod_fundo)), by = .(ativo, ym)]
cat("Celulas ativo-mes com FIT calculavel:", nrow(fit), "\n")
cat("Distribuicao de FIT:\n"); print(summary(fit$FIT))
cat("Distribuicao de n_fundos por celula:\n"); print(summary(fit$n_fundos))

# ---- concentracao de posse: HHI da posse entre os fundos que carregam n ---
hhi_posse <- d[, .(hhi_posse = sum((peso_valor/sum(peso_valor))^2)), by = .(ativo, ym)]
fit <- merge(fit, hhi_posse, by = c("ativo","ym"))

fwrite(fit, file.path(REPO, "v2 OFICIAL/data/fit_ativo_mes.csv"))
cat("\nOK - salvo em 'fit_ativo_mes.csv'\n")

# =============================================================================
# Merge com retorno, horizontes 0 (contemporaneo), +1, +3, +6, +12
# =============================================================================
precos <- fread(file.path(REPO, "v2 OFICIAL/data/precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
fit[, ticker := trimws(sub(".*- ", "", ativo))]

roda_h <- function(h) {
  f2 <- copy(fit)
  f2[, ym_ret := addm(ym, h)]
  m <- merge(f2, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
  m <- m[is.finite(retorno) & is.finite(FIT)]
  m[, horizonte := h]
  m
}
R0 <- roda_h(0); R1 <- roda_h(1); R3 <- roda_h(3); R6 <- roda_h(6); R12 <- roda_h(12)

cat("\n===== Retorno ~ FIT, por horizonte (amostra completa) =====\n")
resumo <- list()
for (h in c(0,1,3,6,12)) {
  Rh <- get(paste0("R", h))
  fit_lm <- lm(retorno ~ FIT, data = Rh)
  s <- summary(fit_lm)$coefficients
  r2 <- summary(fit_lm)$r.squared
  cat(sprintf("h=%2d: n=%7d | coef=%12.6f | t=%7.3f | p=%.4f | R2=%.5f%%\n",
              h, nrow(Rh), s[2,1], s[2,3], s[2,4], 100*r2))
  resumo[[as.character(h)]] <- data.table(horizonte=h, n=nrow(Rh), coef=s[2,1], t=s[2,3], p=s[2,4], r2_pct=100*r2)
}
fwrite(rbindlist(resumo), file.path(REPO, "v2 OFICIAL/data/fit_retorno_resumo.csv"))

# ---- condicionar em FLUXO EXTREMO (top/bottom decil de |FIT|) -------------
cat("\n===== Restrito a FLUXO EXTREMO (top/bottom decil de |FIT|, h=0 e h=3) =====\n")
for (h in c(0,3)) {
  Rh <- get(paste0("R", h))
  corte <- quantile(abs(Rh$FIT), 0.8, na.rm=TRUE)
  ext <- Rh[abs(FIT) >= corte]
  fit_lm <- lm(retorno ~ FIT, data = ext)
  s <- summary(fit_lm)$coefficients
  cat(sprintf("h=%d, extremos (n=%d, %.0f%% da amostra): coef=%.6f | t=%.3f | p=%.4f | R2=%.4f%%\n",
              h, nrow(ext), 100*nrow(ext)/nrow(Rh), s[2,1], s[2,3], s[2,4], 100*summary(fit_lm)$r.squared))
}

# ---- condicionar em CROWDING (poucos fundos = posse concentrada) ----------
cat("\n===== Restrito a posse CONCENTRADA (top tercil de hhi_posse, h=0 e h=3) =====\n")
for (h in c(0,3)) {
  Rh <- get(paste0("R", h))
  corte <- quantile(Rh$hhi_posse, 2/3, na.rm=TRUE)
  conc <- Rh[hhi_posse >= corte]
  fit_lm <- lm(retorno ~ FIT, data = conc)
  s <- summary(fit_lm)$coefficients
  cat(sprintf("h=%d, concentrados (n=%d): coef=%.6f | t=%.3f | p=%.4f | R2=%.4f%%\n",
              h, nrow(conc), s[2,1], s[2,3], s[2,4], 100*summary(fit_lm)$r.squared))
}

# ---- quintis de FIT, retorno medio por horizonte (visualizar reversao) ----
cat("\n===== Retorno medio por quintil de FIT, todos os horizontes =====\n")
for (h in c(0,1,3,6,12)) {
  Rh <- get(paste0("R", h))
  Rh[, quintil := cut(FIT, quantile(FIT, seq(0,1,0.2), na.rm=TRUE), include.lowest=TRUE, labels=FALSE)]
  q <- Rh[, .(retorno_medio = mean(retorno), n=.N), by=quintil][order(quintil)]
  cat(sprintf("h=%d: ", h)); print(q)
}

cat("\nOK\n")
