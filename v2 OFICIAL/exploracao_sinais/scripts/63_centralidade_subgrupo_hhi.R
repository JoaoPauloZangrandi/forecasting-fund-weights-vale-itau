# =============================================================================
# 63_centralidade_subgrupo_hhi.R  (exploracao_sinais / agente_rede)
#
# ANGULO (d): a rede de centralidade calculada no script 60 usa o universo
# INTEIRO de acoes todo mes. Aqui restrinjo a rede a um SUBCONJUNTO --
# acoes de alta concentracao de posse (HHI alto, terco superior, mesma
# proxy de "crowding" ja usada e validada no candidato #26/28 do log
# principal) -- e recalculo grau/autovetor de centralidade SO' DENTRO desse
# subgrupo. Pergunta: entre acoes ja "crowded", ser mais central NESSA rede
# reduzida carrega informacao adicional sobre retorno futuro? Comparo com o
# mesmo exercicio no terco de HHI BAIXO (menos crowded), como contraste.
#
# NOTA ALGEBRICA: CIO_{i,j,t} depende so' das colunas i,j de X (posicoes dos
# fundos em i e em j) -- entao restringir o universo NAO muda o valor de
# CIO_{i,j} para pares i,j que sobrevivem ao corte. O que MUDA e' (a) o grau
# ponderado, que vira uma soma PARCIAL (so' sobre os pares dentro do
# subgrupo) e (b) a centralidade de autovetor, que e' genuinamente diferente
# numa rede menor (autovetores nao sao invariantes a sub-matrizes).
#
# HHI reaproveita a MESMA construcao do candidato de crowding do log
# principal (`valor_posicao`, HHI = soma dos pesos^2 entre fundos donos).
# Recorte de tercil e' RE-FEITO A CADA MES (nao usa corte fixo do treino).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp[, valor := peso * aum_prev]
pp[, ticker := trimws(sub(".*- ", "", ativo))]
pp <- pp[ticker %in% unique(precos$ticker)]

# HHI de posse por ticker-mes (mesma proxy de crowding do log principal)
hhi <- pp[, .(hhi_posse = sum((valor/sum(valor))^2), n_fundos = .N), by = .(ticker, ym)]
hhi <- hhi[n_fundos >= 10]
hhi[, tercil := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:3/3, na.rm = TRUE), include.lowest = TRUE, labels = FALSE)), by = ym]

meses <- sort(unique(pp$ym))
cat("Meses no painel:", length(meses), "\n")

calc_centralidade_subgrupo_mes <- function(mes_atual, tercil_alvo) {
  tickers_grupo <- hhi[ym == mes_atual & tercil == tercil_alvo, ticker]
  d_mes <- pp[ym == mes_atual]
  if (uniqueN(d_mes$ticker) < 15 || uniqueN(d_mes$cod_fundo) < 15) return(NULL)
  d_mes[, cod_fundo := as.character(cod_fundo)]
  wide <- dcast(d_mes, cod_fundo ~ ticker, value.var = "valor", fun.aggregate = sum, fill = 0)
  X <- as.matrix(wide[, -1, with = FALSE])
  # restringe as COLUNAS ao subgrupo (nao muda O_ij para i,j dentro do subgrupo -- ver nota algebrica acima)
  cols_ok <- intersect(colnames(X), tickers_grupo)
  if (length(cols_ok) < 15) return(NULL)
  Xs <- X[, cols_ok, drop = FALSE]
  n <- ncol(Xs)

  O <- crossprod(Xs)
  diag_O <- diag(O); diag_O[diag_O <= 0] <- NA
  denom <- sqrt(outer(diag_O, diag_O))
  CIO <- O / denom
  diag(CIO) <- 0
  CIO[!is.finite(CIO)] <- 0

  deg_raw <- rowSums(CIO); deg_avg <- deg_raw / (n - 1)
  eig <- tryCatch(eigen(CIO, symmetric = TRUE), error = function(e) NULL)
  if (is.null(eig)) { eigen_cent <- rep(NA_real_, n) } else {
    vec <- eig$vectors[, 1]; if (sum(vec) < 0) vec <- -vec; vec <- abs(vec)
    mx <- max(vec); eigen_cent <- if (is.finite(mx) && mx > 0) vec / mx else rep(NA_real_, n)
  }
  data.table(ticker = cols_ok, ym = mes_atual, tercil_hhi = tercil_alvo,
             deg_avg_sub = deg_avg, eigen_cent_sub = eigen_cent, n_tickers_subgrupo = n)
}

cat("Calculando centralidade DENTRO do subgrupo HHI-alto (tercil 3)...\n")
t0 <- Sys.time()
res_alto <- rbindlist(Filter(Negate(is.null), lapply(meses, calc_centralidade_subgrupo_mes, tercil_alvo = 3L)))
cat("Calculando centralidade DENTRO do subgrupo HHI-baixo (tercil 1)...\n")
res_baixo <- rbindlist(Filter(Negate(is.null), lapply(meses, calc_centralidade_subgrupo_mes, tercil_alvo = 1L)))
cat("Tempo total:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")

painel_sub <- rbindlist(list(res_alto, res_baixo))
cat("Ativo-mes (HHI alto):", nrow(res_alto), "| (HHI baixo):", nrow(res_baixo), "\n")
cat("n_tickers_subgrupo mediano -- alto:", median(res_alto$n_tickers_subgrupo),
    "| baixo:", median(res_baixo$n_tickers_subgrupo), "\n")
fwrite(painel_sub, file.path(OUT, "centralidade_subgrupo_hhi.csv"))

# -----------------------------------------------------------------------------
# TESTES: mesma disciplina do script 61 (OOS + Fama-MacBeth com re-corte
# mensal), agora DENTRO de cada subgrupo separadamente
# -----------------------------------------------------------------------------
fama_macbeth_recorte_mensal <- function(m, var_x, var_y, nome, h) {
  teste <- copy(m[ym >= CORTE])
  teste <- teste[is.finite(get(var_x)) & is.finite(get(var_y))]
  if (nrow(teste) < 30) return(NULL)
  teste[, quintil := {
    qs <- unique(quantile(get(var_x), 0:5/5, na.rm = TRUE, type = 7))
    if (length(qs) < 3) rep(NA_integer_, .N) else as.integer(cut(get(var_x), qs, include.lowest = TRUE, labels = FALSE))
  }, by = ym]
  teste <- teste[!is.na(quintil)]
  teste[, extremo := ifelse(quintil == min(quintil), "Q1", ifelse(quintil == max(quintil), "Q5", NA_character_)), by = ym]
  n_por_mes <- teste[!is.na(extremo), .N, by = .(ym, extremo)]
  if (nrow(n_por_mes) == 0) return(NULL)
  por_mes <- teste[!is.na(extremo), .(retorno_medio = mean(get(var_y))), by = .(ym, extremo)]
  sm <- dcast(por_mes, ym ~ extremo, value.var = "retorno_medio")
  if (!all(c("Q1","Q5") %in% names(sm))) return(NULL)
  sm <- sm[is.finite(Q1) & is.finite(Q5)]; sm[, spread := Q5 - Q1]
  n_meses <- nrow(sm)
  if (n_meses < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(n_meses)); p_fm <- 2*pt(-abs(t_fm), df = n_meses-1)
  n_min <- min(n_por_mes$N); n_mediana <- median(n_por_mes$N)
  cat(sprintf("%-38s h=%2d [FM] %2d meses spread=%+7.3fpp/mes t=%6.2f p=%.4f | N min/mediana=%d/%.0f\n",
              nome, h, n_meses, 100*media, t_fm, p_fm, n_min, n_mediana))
  data.table(sinal = nome, horizonte = h, n_meses = n_meses, spread_medio_pp = 100*media,
             t_fm = t_fm, p_fm = p_fm, n_min_grupo_mes = n_min, n_mediana_grupo_mes = n_mediana)
}

resultados <- list()
for (grupo_nome in c("HHI alto (tercil 3)", "HHI baixo (tercil 1)")) {
  base_grupo <- if (grupo_nome == "HHI alto (tercil 3)") res_alto else res_baixo
  for (h in c(1,3,6,12)) {
    m <- copy(base_grupo); m[, ym_ret := addm(ym, h)]
    m <- merge(m, precos, by.x = c("ticker","ym_ret"), by.y = c("ticker","ymk"), all.x = TRUE)
    for (var_x in c("deg_avg_sub","eigen_cent_sub")) {
      mm <- m[is.finite(retorno) & is.finite(get(var_x))]
      nome <- sprintf("Centralidade-subgrupo [%s]: %s", grupo_nome, var_x)
      r <- fama_macbeth_recorte_mensal(mm, var_x, "retorno", nome, h)
      if (!is.null(r)) { r[, grupo := grupo_nome]; r[, variavel := var_x]; resultados[[length(resultados)+1]] <- r }
    }
  }
}
RF <- rbindlist(resultados)
fwrite(RF, file.path(OUT, "candidatos_63_subgrupo_hhi.csv"))
cat("\n===== Resumo ordenado por p_fm =====\n")
print(RF[order(p_fm)])
cat("\nOK\n")
