# =============================================================================
# !!! SUPERADO (12/08/2026) -- NAO RODAR SEM QUERER !!!
# Versao de 5 caracteristicas (sem HHI_resto) -- depende do R/42, tambem
# superado. O script 99_pipeline_hhi_lag_etapa1.R e quem produz oficialmente
# etapa3_multiativo_gestora_multihorizonte.csv/_longo.csv hoje (6 caracte-
# risticas, HHI_resto defasado). Este script escreve NOS MESMOS nomes de
# arquivo -- rodar por engano (mesmo que so pra regerar as Tabelas 9/10 via
# script 93) sobrescreve silenciosamente o resultado atual com a versao
# antiga, sem erro nem aviso. Mantido so por referencia historica/
# comparacao. Se precisar rerodar de verdade, rode 99, nao este.
# =============================================================================
# 44_etapa3_multiativo_gestora_multihorizonte.R  (v2 OFICIAL)
#
# Fecha uma lacuna apontada pelo Joao: a Tabela 22 (erro fora da amostra por
# gestora) so cobria VALE3, h=3. Este script generaliza para TODOS os ativos
# (nao so VALE3) E varios horizontes (h=1,3,6,12) -- o material mais completo
# possivel pra orientar o desenho do robo caca-replicantes: qual gestora e
# mais dificil de prever fora da amostra, olhando a carteira de acoes
# inteira, e se esse ranking muda dependendo de quantos meses a frente se
# tenta prever.
#
# Reaproveita h=1 e h=3 ja calculados no R/42 (etapa3_multiativo_h1.csv,
# etapa3_multiativo_h3.csv) -- nao precisa rodar de novo. So h=6 e h=12 sao
# novos aqui (mesmo esquema de sempre: lambda_h unico por MQO so no treino,
# alvo peso_pred da Secao 6.1/R37, corte <2020-01 vs >=2020-01).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
E[, d := peso_pred - peso]

addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L
rmse <- function(x) sqrt(mean(x^2))

roda_horizonte <- function(h) {
  fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
  atual <- E[, .(cod_fundo, gestora_grupo, ativo, ym, d, peso, ym_fut = addm(ym, h))]
  M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"),
             by.y = c("cod_fundo","ativo","ym_fut_key"))
  M[, dw := peso_fut - peso]
  treino <- M[ym < CORTE]; teste  <- M[ym >= CORTE & ym_fut <= 202112L]
  fit <- lm(dw ~ 0 + d, data = treino); lam <- unname(coef(fit)["d"])
  teste[, erro_oos := dw - lam * d]
  teste[, erro_naive := dw]
  cat(sprintf("h=%d: treino %d obs | teste %d obs | lambda=%.4f\n", h, nrow(treino), nrow(teste), lam))
  teste
}

# ---- reaproveita h=1 e h=3 do R/42, calcula h=6 e h=12 novos --------------
h1 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h1.csv"))
h3 <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_h3.csv"))
h6  <- roda_horizonte(6)
h12 <- roda_horizonte(12)

# ---- Joao perguntou: RMSE eh so do ajuste parcial, cade a ingenua pra
# comparar (padrao do resto do documento: Tabelas 6/23/24 sempre mostram os
# dois lado a lado)? Corrigido: agora tambem agrega rmse_naive (erro da
# previsao ingenua = nenhuma mudanca, dw puro) e calcula a margem % de
# reducao do ajuste sobre a ingenua, por gestora e horizonte.
por_gestora <- function(dt, h) {
  g <- dt[, .(n_obs = .N, n_fundos = uniqueN(cod_fundo), rmse_oos = rmse(erro_oos),
              rmse_naive = rmse(erro_naive)), by = gestora_grupo]
  g[, margem_pct := 100*(rmse_naive - rmse_oos)/rmse_naive]
  g[, horizonte := h]; g
}
G <- rbindlist(list(por_gestora(h1,1), por_gestora(h3,3), por_gestora(h6,6), por_gestora(h12,12)))

# ---- tabela larga: gestora x horizonte, pra ver se o ranking muda --------
# CUIDADO: n_fundos varia por horizonte (pares (t,t+h) validos mudam com h) --
# NAO pode entrar na chave do dcast (bug real encontrado: fragmentava cada
# gestora em varias linhas, uma por valor distinto de n_fundos). Guarda
# n_fundos_h1 a parte, so como referencia de cobertura.
W <- dcast(G, gestora_grupo ~ horizonte, value.var = "rmse_oos", fun.aggregate = mean)
setnames(W, as.character(c(1,3,6,12)), paste0("rmse_h", c(1,3,6,12)))
Wm <- dcast(G, gestora_grupo ~ horizonte, value.var = "margem_pct", fun.aggregate = mean)
setnames(Wm, as.character(c(1,3,6,12)), paste0("margem_h", c(1,3,6,12)))
W <- merge(W, Wm, by = "gestora_grupo")
n_fundos_h1 <- G[horizonte == 1, .(gestora_grupo, n_fundos_h1 = n_fundos)]
W <- merge(W, n_fundos_h1, by = "gestora_grupo", all.x = TRUE)
setorder(W, -rmse_h1)
cat("\n===== RMSE (ajuste) fora da amostra por gestora, todos os ativos, 4 horizontes =====\n")
print(W[, .(gestora_grupo, n_fundos_h1, rmse_h1, rmse_h3, rmse_h6, rmse_h12)], nrows = 50)
cat("\n===== Margem %% (ajuste vs ingenua) por gestora, 4 horizontes =====\n")
print(W[, .(gestora_grupo, margem_h1, margem_h3, margem_h6, margem_h12)], nrows = 50)

# ---- estabilidade do ranking entre horizontes (correlacao de Spearman) ---
cat("\n===== Correlacao de Spearman do ranking entre horizontes =====\n")
combos <- combn(c("rmse_h1","rmse_h3","rmse_h6","rmse_h12"), 2, simplify = FALSE)
for (cb in combos) {
  ok <- complete.cases(W[, ..cb])
  r <- cor(W[[cb[1]]][ok], W[[cb[2]]][ok], method = "spearman")
  cat(sprintf("%s vs %s: rho = %.3f (n=%d)\n", cb[1], cb[2], r, sum(ok)))
}

fwrite(G, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte_longo.csv"))
fwrite(W, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/etapa3_multiativo_gestora_multihorizonte*.csv'\n")
