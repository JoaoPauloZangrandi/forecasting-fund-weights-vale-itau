# =============================================================================
# 84_cio_pairs_trading.R  (exploracao_sinais, agente ML/pairs)
#
# FRENTE (A): pairs trading / valor relativo via overlap de posse (CIO).
# Reaproveita EXATAMENTE a mesma construcao de CIO (cosine similarity de
# posicao em R$, ponderada por AUM) do script 11_cio_peer_momentum.R, mas
# em vez de agregar retorno de TODOS os peers conectados numa media
# ponderada (peer_ret), aqui extraimos os PARES individuais com CIO mais
# alto ("gemeos" institucionais) e testamos reversao do spread de retorno
# acumulado entre os dois membros do par -- pairs trading classico, so que
# usando similaridade de POSSE em vez de similaridade de setor/cointegracao
# de preco (o padrao usual na literatura).
#
# Mecanismo testado: se dois ativos tem ownership quase identica (CIO alto)
# mas um deles subiu MUITO mais que o outro numa janela de formacao (K
# meses), a divergencia deveria reverter -- comprar quem ficou pra tras,
# vender quem subiu mais, dentro do par. Isso e' testado de duas formas:
#   (1) Fama-MacBeth de regressao: diff_retorno_futuro ~ spread_formacao,
#       1 coeficiente por mes, esperando beta NEGATIVO (reversao).
#   (2) Fama-MacBeth de portfolio: retorno do "pair-trade" (long perdedor,
#       short vencedor, dentro do par) medio por mes, testando se a media
#       da serie temporal e' > 0.
#
# LICAO DO COMOMENTUM/CANDIDATO#22(h=3): reportar SEMPRE o N de pares por
# mes (nunca deixar virar um artefato de amostra degenerada). Disciplina
# fora-da-amostra: teste formal em ym >= 202001 (mesmo corte do resto do
# TCC) -- este teste nao precisa de "treino" no sentido de ajustar
# parametros (e' um teste de hipotese direto, como os testes de evento e
# a versao corrigida do CIO peer momentum), entao reporto tambem a amostra
# inteira como descritivo, mas o veredito formal usa so' o periodo de teste.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

set.seed(1)

# ---- precos: retorno + retorno acumulado (janela K meses, terminando em t) --
precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setorder(precos, ticker, ymk)
precos[, log_ret := log(1 + retorno)]
for (K in c(3, 6)) {
  precos[, paste0("cumret_", K) := exp(frollsum(log_ret, K, align = "right")) - 1, by = ticker]
}

# ---- painel de holdings, igual ao script 11 -------------------------------
pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp[, valor := peso * aum_prev]
pp[, ticker := trimws(sub(".*- ", "", ativo))]
tickers_com_preco <- unique(precos$ticker)
pp <- pp[ticker %in% tickers_com_preco]
meses <- sort(unique(pp$ym))
cat("Meses no painel:", length(meses), "\n")

# =============================================================================
# PASSO 1: extrair, mes a mes, os pares com CIO no topo da distribuicao
#          (mesma matriz CIO do script 11, mas guardamos os PARES, nao a
#          media agregada por acao)
# =============================================================================
calc_pares_mes <- function(mes_atual, top_pct = 0.01, cio_min = 0.3) {
  d_mes <- pp[ym == mes_atual]
  if (uniqueN(d_mes$ticker) < 15 || uniqueN(d_mes$cod_fundo) < 15) return(NULL)
  d_mes[, cod_fundo := as.character(cod_fundo)]
  wide <- dcast(d_mes, cod_fundo ~ ticker, value.var = "valor", fun.aggregate = sum, fill = 0)
  X <- as.matrix(wide[, -1, with = FALSE])
  tickers_mes <- colnames(X)
  if (ncol(X) < 15) return(NULL)

  O <- crossprod(X)
  diag_O <- diag(O); diag_O[diag_O <= 0] <- NA
  denom <- sqrt(outer(diag_O, diag_O))
  CIO <- O / denom
  diag(CIO) <- NA
  CIO[!is.finite(CIO)] <- NA

  ut <- which(upper.tri(CIO), arr.ind = TRUE)
  pares <- data.table(i = tickers_mes[ut[,1]], j = tickers_mes[ut[,2]], cio = CIO[ut])
  pares <- pares[is.finite(cio) & cio > 0]
  if (nrow(pares) < 30) return(NULL)

  limiar <- quantile(pares$cio, 1 - top_pct, na.rm = TRUE)
  pares_top <- pares[cio >= limiar & cio >= cio_min]
  if (nrow(pares_top) < 5) return(NULL)
  pares_top[, ym := mes_atual]
  pares_top
}

cat("Extraindo pares CIO top 1%/mes (pode demorar, matriz por mes)...\n")
t0 <- Sys.time()
res_pares <- lapply(meses, calc_pares_mes, top_pct = 0.01, cio_min = 0.3)
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")
pares_dt <- rbindlist(Filter(Negate(is.null), res_pares))
cat("Pares extraidos (top 1%, CIO>=0.3):", nrow(pares_dt),
    "| CIO medio:", round(mean(pares_dt$cio),3),
    "| CIO mediano:", round(median(pares_dt$cio),3), "\n")
n_pares_mes <- pares_dt[, .N, by = ym]
cat("Pares por mes: min=", min(n_pares_mes$N), " mediana=", median(n_pares_mes$N),
    " max=", max(n_pares_mes$N), "\n")
fwrite(pares_dt, file.path(OUT, "pairs_cio_top1pct.csv"))

# tambem guardar um corte mais frouxo (top 5%, cio_min mais baixo), para
# checar robustez do numero de pares mais adiante
cat("\nExtraindo tambem corte mais frouxo (top 5%, CIO>=0.15) para robustez...\n")
res_pares5 <- lapply(meses, calc_pares_mes, top_pct = 0.05, cio_min = 0.15)
pares_dt5 <- rbindlist(Filter(Negate(is.null), res_pares5))
cat("Pares extraidos (top 5%, CIO>=0.15):", nrow(pares_dt5), "\n")
fwrite(pares_dt5, file.path(OUT, "pairs_cio_top5pct.csv"))

# =============================================================================
# PASSO 2: montar spread de formacao (janela K) e retorno futuro (horizonte h)
#          para cada par-mes, nos dois cortes (top1%/top5%)
# =============================================================================
montar_base_pares <- function(pares_base, K, h) {
  cr_col <- paste0("cumret_", K)
  cr <- precos[, c("ticker","ymk",cr_col), with = FALSE]
  setnames(cr, cr_col, "cumret")
  base <- copy(pares_base)
  base <- merge(base, cr, by.x = c("i","ym"), by.y = c("ticker","ymk"))
  setnames(base, "cumret", "cumret_i")
  base <- merge(base, cr, by.x = c("j","ym"), by.y = c("ticker","ymk"))
  setnames(base, "cumret", "cumret_j")
  base <- base[is.finite(cumret_i) & is.finite(cumret_j)]
  base[, spread := cumret_i - cumret_j]

  ret_fwd <- precos[, .(ticker, ym_alvo = addm(ymk, -h), ret_fwd = retorno)]
  base <- merge(base, ret_fwd, by.x = c("i","ym"), by.y = c("ticker","ym_alvo"))
  setnames(base, "ret_fwd", "ret_i_fwd")
  base <- merge(base, ret_fwd, by.x = c("j","ym"), by.y = c("ticker","ym_alvo"))
  setnames(base, "ret_fwd", "ret_j_fwd")
  base <- base[is.finite(ret_i_fwd) & is.finite(ret_j_fwd)]

  base[, diff_fwd := ret_i_fwd - ret_j_fwd]
  # pair-trade: long o "perdedor" da formacao (menor cumret), short o "vencedor"
  base[, pair_trade_ret := -sign(spread) * diff_fwd]
  base[, abs_spread := abs(spread)]
  base
}

# =============================================================================
# PASSO 3: Fama-MacBeth (1): regressao diff_fwd ~ spread, mes a mes
# =============================================================================
fm_regressao_pares <- function(base, nome, K, h) {
  teste <- base[ym >= CORTE]
  if (nrow(teste) < 50) return(NULL)
  n_por_mes <- teste[, .N, by = ym]
  if (min(n_por_mes$N) < 10) {
    cat(sprintf("   [aviso] %s K=%d h=%d: N minimo por mes = %d (< 10, checar degenerescencia)\n",
                nome, K, h, min(n_por_mes$N)))
  }
  coefs <- teste[, {
    if (.N < 10 || sd(spread) < 1e-8) {
      list(beta = NA_real_, n = .N)
    } else {
      fit <- lm(diff_fwd ~ spread)
      list(beta = coef(fit)[2], n = .N)
    }
  }, by = ym]
  coefs <- coefs[is.finite(beta)]
  n_meses <- nrow(coefs)
  if (n_meses < 6) return(NULL)
  media <- mean(coefs$beta); dp <- sd(coefs$beta)
  t_fm <- media / (dp / sqrt(n_meses)); p_fm <- 2 * pt(-abs(t_fm), df = n_meses - 1)
  cat(sprintf("[FM-regressao] %-10s K=%d h=%d | %2d meses | beta_medio=%9.4f | t=%6.2f | p=%.4f | N_par_mes(min/mediana)=%d/%.0f\n",
              nome, K, h, n_meses, media, t_fm, p_fm, min(n_por_mes$N), median(n_por_mes$N)))
  data.table(corte = nome, K = K, h = h, tipo = "regressao_beta", n_meses = n_meses,
             estatistica = media, t_fm = t_fm, p_fm = p_fm,
             n_par_mes_min = min(n_por_mes$N), n_par_mes_mediana = median(n_por_mes$N))
}

# =============================================================================
# PASSO 4: Fama-MacBeth (2): retorno medio do pair-trade por mes
#          (a) todos os pares acima do limiar CIO
#          (b) so' o TERCO com maior |spread| dentro do mes (recorte
#              MENSAL, nao break fixo do treino -- licao das armadilhas
#              anteriores) -- testa se reversao e' mais forte quando a
#              divergencia e' grande, que e' a pergunta do Joao
# =============================================================================
fm_portfolio_pares <- function(base, nome, K, h, so_alta_divergencia = FALSE) {
  d <- copy(base)
  if (so_alta_divergencia) {
    d[, tercil := as.integer(cut(abs_spread, quantile(abs_spread, 0:3/3, na.rm = TRUE),
                                   include.lowest = TRUE)), by = ym]
    d <- d[tercil == 3]
  }
  teste <- d[ym >= CORTE]
  if (nrow(teste) < 30) return(NULL)
  por_mes <- teste[, .(ret_medio = mean(pair_trade_ret), n = .N), by = ym]
  por_mes <- por_mes[n >= 5]
  n_meses <- nrow(por_mes)
  if (n_meses < 6) return(NULL)
  media <- mean(por_mes$ret_medio); dp <- sd(por_mes$ret_medio)
  t_fm <- media / (dp / sqrt(n_meses)); p_fm <- 2 * pt(-abs(t_fm), df = n_meses - 1)
  tag <- if (so_alta_divergencia) "top-tercil |spread|" else "todos os pares"
  cat(sprintf("[FM-portfolio,%s] %-10s K=%d h=%d | %2d meses | ret_medio=%+7.3f%%/mes (anualizado~%+6.1f%%) | t=%6.2f | p=%.4f | N_par_mes(min/mediana)=%d/%.0f\n",
              tag, nome, K, h, n_meses, 100*media, 100*((1+media)^12-1), t_fm, p_fm,
              min(por_mes$n), median(por_mes$n)))
  data.table(corte = nome, K = K, h = h, tipo = paste0("portfolio_", ifelse(so_alta_divergencia,"top_tercil_div","todos")),
             n_meses = n_meses, estatistica = 100*media, t_fm = t_fm, p_fm = p_fm,
             n_par_mes_min = min(por_mes$n), n_par_mes_mediana = median(por_mes$n))
}

# =============================================================================
# RODAR TUDO: 2 cortes de CIO x 2 janelas K x 2 horizontes h = 8 combinacoes,
# cada uma com 3 testes (regressao, portfolio-todos, portfolio-top-div)
# =============================================================================
resultados <- list()
cortes <- list(top1pct = pares_dt, top5pct = pares_dt5)

for (nome_corte in names(cortes)) {
  for (K in c(3, 6)) {
    for (h in c(1, 3)) {
      base <- montar_base_pares(cortes[[nome_corte]], K, h)
      r1 <- fm_regressao_pares(base, nome_corte, K, h)
      r2 <- fm_portfolio_pares(base, nome_corte, K, h, so_alta_divergencia = FALSE)
      r3 <- fm_portfolio_pares(base, nome_corte, K, h, so_alta_divergencia = TRUE)
      resultados <- c(resultados, list(r1, r2, r3))
    }
  }
}

RES <- rbindlist(Filter(Negate(is.null), resultados))
fwrite(RES, file.path(OUT, "candidatos_84_cio_pairs_trading.csv"))
cat("\n\n===== RESUMO ORDENADO POR p_fm =====\n")
print(RES[order(p_fm)])
cat("\nOK\n")
