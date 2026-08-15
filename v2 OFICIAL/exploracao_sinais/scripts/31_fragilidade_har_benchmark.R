# =============================================================================
# 31_fragilidade_har_benchmark.R  (exploracao_sinais)
#
# Design rigoroso sugerido pela pesquisa: benchmark estilo HAR-RV (varias
# janelas de volatilidade passada, nao so' uma) + testa se CROWDING
# ("fragilidade" de posse, Greenwood-Thesmar 2011 -- MESMA base teorica
# ja usada no outro TCC do Joao, forecasting-exposure-itub4) adiciona
# R2 FORA DA AMOSTRA incremental. Foco em R2_OOS, nao so' significancia
# (dado o historico de ~400 especificacoes ja testadas).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

setorder(precos, ticker, ymk)
# componentes estilo HAR: vol de curtissimo prazo (1-3m), medio (6m), longo (12m)
precos[, vol_1_3m := frollapply(retorno, 3, sd), by = ticker]
precos[, vol_6m := frollapply(retorno, 6, sd), by = ticker]
precos[, vol_12m := frollapply(retorno, 12, sd), by = ticker]

calc_vol_futura <- function(dt_precos, h) {
  pw <- copy(dt_precos)
  cols <- list()
  for (k in 1:h) {
    aux <- pw[, .(ticker, ym_base = addm(ymk, -k), ret_k = retorno)]
    setnames(aux, "ret_k", paste0("r",k))
    cols[[k]] <- aux
  }
  base <- Reduce(function(a,b) merge(a,b,by=c("ticker","ym_base"), all=TRUE), cols)
  ret_cols <- paste0("r",1:h)
  base[, vol_futura := apply(.SD, 1, sd, na.rm=TRUE), .SDcols = ret_cols]
  base[, n_obs_vol := apply(.SD, 1, function(x) sum(is.finite(x))), .SDcols = ret_cols]
  base <- base[n_obs_vol >= max(2,round(h*0.7))]
  base[, .(ticker, ym = ym_base, vol_futura)]
}

pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2), n_fundos=.N), by = .(ticker, ym)]
crowd <- crowd[n_fundos >= 10]

testa_r2_oos <- function(m, vars_x, nome, h) {
  treino <- m[ym < CORTE]; teste <- copy(m[ym >= CORTE])
  if (nrow(treino) < 100 || nrow(teste) < 50) return(NULL)
  form <- as.formula(paste("vol_futura ~", paste(vars_x, collapse=" + ")))
  fit_lm <- lm(form, data = treino)
  pred <- predict(fit_lm, newdata = teste)
  sse_m <- sum((teste$vol_futura-pred)^2); sse_n <- sum((teste$vol_futura-mean(treino$vol_futura))^2)
  r2_oos <- 1 - sse_m/sse_n
  cat(sprintf("%-35s h=%2d n_tr=%5d n_te=%4d R2_OOS=%8.4f%%\n", nome, h, nrow(treino), nrow(teste), 100*r2_oos))
  data.table(modelo=nome, horizonte=h, n_treino=nrow(treino), n_teste=nrow(teste), r2_oos_pct=100*r2_oos)
}

resultados <- list()
for (h in c(3,6,12)) {
  vol_fut <- calc_vol_futura(precos, h)
  m <- merge(crowd, precos[,.(ticker,ym=ymk,vol_1_3m,vol_6m,vol_12m)], by=c("ticker","ym"))
  m <- merge(m, vol_fut, by=c("ticker","ym"))
  m <- m[is.finite(hhi_posse) & is.finite(vol_1_3m) & is.finite(vol_6m) & is.finite(vol_12m) & is.finite(vol_futura)]
  cat(sprintf("\n========== h=%d (n=%d) ==========\n", h, nrow(m)))

  r1 <- testa_r2_oos(m, "vol_12m", "Benchmark: so vol_12m (ja testado)", h)
  r2 <- testa_r2_oos(m, c("vol_1_3m","vol_6m","vol_12m"), "Benchmark HAR (3 janelas de vol)", h)
  r3 <- testa_r2_oos(m, c("vol_1_3m","vol_6m","vol_12m","hhi_posse"), "HAR + CROWDING (fragilidade)", h)
  for (r in list(r1,r2,r3)) if (!is.null(r)) resultados[[length(resultados)+1]] <- r
}

R <- rbindlist(resultados)
fwrite(R, file.path(OUT, "candidatos_31_har_fragilidade.csv"))
cat("\n\n===== GANHO DE R2_OOS ao adicionar crowding sobre o benchmark HAR =====\n")
RW <- dcast(R, horizonte ~ modelo, value.var = "r2_oos_pct")
print(RW)
cat("\nOK\n")
