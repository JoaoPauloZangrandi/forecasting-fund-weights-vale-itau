# =============================================================================
# 33_52wh_x_hhi_double_sort.R  (exploracao_sinais)
#
# Extensao direta do candidato #24: la, "52-week high" condicionado a ALTA
# posse institucional (own_rank = valor total detido por fundos, medida de
# TAMANHO) deu o resultado mais forte de toda a familia de anomalias
# classicas (h=1, spread=3,04pp/mes, t=3,13, p=0,0048 -- nao bateu
# Bonferroni mas foi o "quase" mais proximo). Aqui trocamos own_rank por
# HHI de posse (medida de CONCENTRACAO/crowding, nossa variavel real de
# fragilidade) -- desenho double-sort a la Chincarini-Lazo-Paz-Moneta
# (2026): sera que o retorno da anomalia se concentra especificamente nas
# acoes mais "crowded"?
#
# Multiplas versoes (preferencia do usuario na duvida metodologica):
# tercil e mediana de corte, h=1/3/6.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
CORTE <- 202001L
LIMIAR_BONFERRONI <- 0.05/450

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","preco","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
setorder(precos, ticker, ymk)
precos[, preco_max_12m := frollapply(preco, 12, max, align="right"), by = ticker]
precos[, prox_52w_high := preco / preco_max_12m]

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

base <- merge(precos[, .(ticker, ym=ymk, prox_52w_high)], crowd[, .(ticker, ym, hhi_posse)], by=c("ticker","ym"))
base <- base[is.finite(prox_52w_high) & is.finite(hhi_posse)]
base[, tercil_hhi := as.integer(cut(hhi_posse, quantile(hhi_posse, 0:3/3, na.rm=TRUE), include.lowest=TRUE)), by=ym]
base[, mediana_hhi := median(hhi_posse), by = ym]

fama_macbeth <- function(m, var_x, var_y, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 30 || nrow(teste) < 20) return(NULL)
  breaks <- unique(quantile(treino[[var_x]], seq(0,1,0.2), na.rm = TRUE))
  if (length(breaks) < 4) return(NULL)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  teste[, quintil := as.integer(cut(get(var_x), breaks, labels = FALSE, include.lowest = TRUE))]
  ng <- length(breaks)-1
  if (ng<5) teste[, quintil := ifelse(quintil==1,1,ifelse(quintil==ng,5,quintil))]
  teste <- teste[!is.na(quintil)]
  por_mes <- teste[quintil %in% c(1,5), .(rm=mean(get(var_y)), n=.N), by=.(ym,quintil)]
  if (nrow(por_mes)==0) return(NULL)
  sm <- dcast(por_mes, ym~quintil, value.var="rm")
  if (!all(c("1","5") %in% names(sm))) return(NULL)
  setnames(sm, c("1","5"), c("q1","q5")); sm <- sm[is.finite(q1)&is.finite(q5)]
  sm[, spread:=q5-q1]; nmes <- nrow(sm)
  if (nmes < 6) return(NULL)
  media <- mean(sm$spread); dp <- sd(sm$spread)
  t_fm <- media/(dp/sqrt(nmes)); p_fm <- 2*pt(-abs(t_fm), df=nmes-1)
  n_medio <- round(mean(por_mes$n))
  cat(sprintf("%-28s h=%d %2d meses n~%3d spread=%+7.2fpp/mes t=%6.2f p=%.5f | Bonf:%s\n",
              nome, h, nmes, n_medio, 100*media, t_fm, p_fm, ifelse(p_fm<LIMIAR_BONFERRONI,"SIM","nao")))
  data.table(sinal=nome, horizonte=h, n_meses=nmes, n_medio_grupo=n_medio, spread_pp=100*media,
             t_fm=t_fm, p_fm=p_fm, sig_bonferroni = p_fm<LIMIAR_BONFERRONI)
}

resultados <- list()

cat("===== 52-week-high -> retorno, condicionado a HHI (tercil, ALTO vs BAIXO crowding) =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno)]
  alto  <- m[tercil_hhi == 3]
  baixo <- m[tercil_hhi == 1]
  r1 <- fama_macbeth(alto,  "prox_52w_high", "retorno", "52wh|ALTO-HHI(tercil)", h)
  r2 <- fama_macbeth(baixo, "prox_52w_high", "retorno", "52wh|BAIXO-HHI(tercil)", h)
  if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
  if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
}

cat("\n===== 52-week-high -> retorno, condicionado a HHI (mediana, ALTO vs BAIXO crowding) =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno)]
  alto  <- m[hhi_posse >= mediana_hhi]
  baixo <- m[hhi_posse <  mediana_hhi]
  r1 <- fama_macbeth(alto,  "prox_52w_high", "retorno", "52wh|ALTO-HHI(mediana)", h)
  r2 <- fama_macbeth(baixo, "prox_52w_high", "retorno", "52wh|BAIXO-HHI(mediana)", h)
  if (!is.null(r1)) resultados[[length(resultados)+1]] <- r1
  if (!is.null(r2)) resultados[[length(resultados)+1]] <- r2
}

cat("\n===== baseline (sem condicionar) pra referencia =====\n")
for (h in c(1,3,6)) {
  m <- copy(base); m[, ym_ret := addm(ym,h)]
  m <- merge(m, precos[,.(ticker,ymk,retorno)], by.x=c("ticker","ym_ret"), by.y=c("ticker","ymk"))
  m <- m[is.finite(retorno)]
  r <- fama_macbeth(m, "prox_52w_high", "retorno", "52wh|SEM condicionar", h)
  if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_33_52wh_x_hhi.csv"))
cat("\n\n===== RESUMO (ordenado por p) =====\n")
print(R[order(p_fm)])
cat("\nOK\n")
