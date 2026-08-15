# =============================================================================
# 60_centralidade_construcao.R  (exploracao_sinais / agente_rede)
#
# ANGULO: medidas de CENTRALIDADE na rede de posse comum institucional (CIO).
# A matriz CIO ja e' literalmente um grafo ponderado ticker-a-ticker (script
# 11_cio_peer_momentum.R construiu ela pra calcular peer_ret; aqui reusamos a
# MESMA construcao e extraimos propriedades do GRAFO em si, nao o retorno dos
# vizinhos):
#   X_{f,i,t} = AUM_{f,t} * peso_{f,i,t}            (posicao em R$ do fundo f na acao i)
#   O_{i,j,t} = sum_f X_{f,i,t} * X_{f,j,t}          (overlap bruto)
#   CIO_{i,j,t} = O_{i,j,t} / sqrt(O_{i,i,t} * O_{j,j,t})   (cosine, em [0,1])
#
# Medidas construidas por mes:
#   (a) grau ponderado (degree centrality): deg_raw = soma da linha CIO_{i,.}
#       (soma de conexao com TODAS as outras acoes, sem piso de conexao --
#       diferente do peer_ret que usava piso p75, aqui queremos o grau bruto
#       do grafo completo); deg_avg = deg_raw / (n_tickers_mes - 1) (media,
#       comparavel entre meses com universo de tamanho diferente).
#   (b) autovetor de centralidade (eigenvector centrality): autovetor
#       associado ao MAIOR autovalor de CIO (Perron-Frobenius, matriz
#       simetrica nao-negativa -> autovetor de sinal unico), normalizado
#       para max=1 dentro do mes (convencao Bonacich).
#   (c) densidade da rede por mes: media de TODAS as entradas fora da
#       diagonal de CIO (proxy de "quao concentrado/crowded" esta o mercado
#       inteiro naquele mes).
#
# LICAO DO COMOMENTUM/CIO (scripts 09/10/36): sempre checar tamanho efetivo
# da amostra (n_tickers_mes) antes de acreditar em qualquer resultado.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

pp <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp <- pp[is.finite(aum_prev) & aum_prev > 0 & peso > 0]
pp[, valor := peso * aum_prev]
pp[, ticker := trimws(sub(".*- ", "", ativo))]

tickers_com_preco <- unique(precos$ticker)
pp <- pp[ticker %in% tickers_com_preco]

meses <- sort(unique(pp$ym))
cat("Meses no painel:", length(meses), "\n")

calc_centralidade_mes <- function(mes_atual) {
  d_mes <- pp[ym == mes_atual]
  if (uniqueN(d_mes$ticker) < 15 || uniqueN(d_mes$cod_fundo) < 15) return(NULL)

  d_mes[, cod_fundo := as.character(cod_fundo)]
  wide <- dcast(d_mes, cod_fundo ~ ticker, value.var = "valor", fun.aggregate = sum, fill = 0)
  X <- as.matrix(wide[, -1, with = FALSE])
  tickers_mes <- colnames(X)
  n <- ncol(X)
  if (n < 15) return(NULL)

  O <- crossprod(X)  # ticker x ticker, overlap bruto
  diag_O <- diag(O)
  diag_O[diag_O <= 0] <- NA
  denom <- sqrt(outer(diag_O, diag_O))
  CIO <- O / denom
  diag(CIO) <- 0
  CIO[!is.finite(CIO)] <- 0

  # (a) grau ponderado -- soma da linha, SEM piso de conexao (grafo completo)
  deg_raw <- rowSums(CIO)
  deg_avg <- deg_raw / (n - 1)

  # (b) autovetor de centralidade
  eig <- tryCatch(eigen(CIO, symmetric = TRUE), error = function(e) NULL)
  if (is.null(eig)) {
    eigen_cent <- rep(NA_real_, n)
  } else {
    vec <- eig$vectors[, 1]
    if (sum(vec) < 0) vec <- -vec
    vec <- abs(vec)  # matriz nao-negativa simetrica -> Perron-Frobenius, sinal unico (abs so' protege de ruido numerico)
    mx <- max(vec)
    eigen_cent <- if (is.finite(mx) && mx > 0) vec / mx else rep(NA_real_, n)
  }

  # (c) densidade da rede inteira nesse mes (media fora da diagonal)
  offdiag <- CIO[upper.tri(CIO)]
  densidade <- mean(offdiag)

  painel <- data.table(ticker = tickers_mes, ym = mes_atual,
                        deg_raw = deg_raw, deg_avg = deg_avg,
                        eigen_cent = eigen_cent, n_tickers_mes = n)
  list(painel = painel, densidade = data.table(ym = mes_atual, densidade = densidade, n_tickers_mes = n))
}

cat("Calculando centralidade mes a mes (matriz por mes, pode demorar)...\n")
t0 <- Sys.time()
resultado_mensal <- lapply(meses, calc_centralidade_mes)
cat("Tempo:", round(as.numeric(Sys.time()-t0, units="secs"),1), "s\n")
resultado_mensal <- Filter(Negate(is.null), resultado_mensal)

painel_dt <- rbindlist(lapply(resultado_mensal, `[[`, "painel"))
dens_dt   <- rbindlist(lapply(resultado_mensal, `[[`, "densidade"))

cat("Ativo-mes com centralidade calculavel:", nrow(painel_dt),
    "| n_tickers_mes mediano:", median(painel_dt$n_tickers_mes), "\n")

# variacao mes-a-mes (por ticker), SO' entre meses CALENDARIO consecutivos (evita costurar buracos)
setorder(painel_dt, ticker, ym)
painel_dt[, ym_ant_esperado := addm(ym, -1)]
painel_dt[, `:=`(deg_avg_ant = shift(deg_avg), eigen_cent_ant = shift(eigen_cent), ym_ant_real = shift(ym)), by = ticker]
painel_dt[, consecutivo := !is.na(ym_ant_real) & ym_ant_real == ym_ant_esperado]
painel_dt[, deg_avg_delta := ifelse(consecutivo, deg_avg - deg_avg_ant, NA_real_)]
painel_dt[, eigen_cent_delta := ifelse(consecutivo, eigen_cent - eigen_cent_ant, NA_real_)]
painel_dt[, c("ym_ant_esperado","deg_avg_ant","eigen_cent_ant","ym_ant_real","consecutivo") := NULL]

cat("Ativo-mes com delta calculavel (mes calendario consecutivo):", sum(is.finite(painel_dt$deg_avg_delta)), "\n")

# retorno agregado do mercado por mes (equal-weighted, mesma convencao do script 37)
setorder(precos, ticker, ymk)
mercado <- precos[, .(ret_mercado = mean(retorno, na.rm = TRUE)), by = ymk]
setnames(mercado, "ymk", "ym")
dens_dt <- merge(dens_dt, mercado, by = "ym", all.x = TRUE)
setorder(dens_dt, ym)
dens_dt[, ret_mercado_prox := shift(ret_mercado, type = "lead")]  # retorno do mes SEGUINTE, alinhado por linha

fwrite(painel_dt, file.path(OUT, "centralidade_panel.csv"))
fwrite(dens_dt, file.path(OUT, "centralidade_densidade_mercado.csv"))

cat("\nResumo deg_avg:\n"); print(summary(painel_dt$deg_avg))
cat("\nResumo eigen_cent:\n"); print(summary(painel_dt$eigen_cent))
cat("\nResumo densidade (por mes):\n"); print(summary(dens_dt$densidade))
cat("\nCorrelacao deg_avg x eigen_cent (nivel, pooled):",
    round(cor(painel_dt$deg_avg, painel_dt$eigen_cent, use = "complete.obs"), 3), "\n")
cat("\nOK\n")
