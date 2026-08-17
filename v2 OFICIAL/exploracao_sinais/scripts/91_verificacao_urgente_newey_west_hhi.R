# =============================================================================
# 91_verificacao_urgente_newey_west_hhi.R  (exploracao_sinais)
#
# VERIFICACAO URGENTE (15/08/2026, mega-auditoria): um dos agentes de
# auditoria achou que o teste de Fama-MacBeth do candidato #30 (HHI->vol
# futura, h=12 -- O ACHADO PRINCIPAL, base da estrategia do Desafio Quant
# AI) pode estar com o mesmo problema que ja achou no achado de cotistas
# (E3): vol_futura usa janelas de h meses SOBREPOSTAS entre observacoes
# consecutivas, o que autocorrelaciona os coeficientes mensais do FM e
# infla o t-estatistico se nao corrigido (Newey-West). Precisa saber AGORA
# se o achado principal sobrevive a essa correcao.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(sandwich); library(lmtest) })
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
CORTE <- 202001L

precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
setorder(precos, ticker, ymk)
precos[, vol_passada_12m := frollapply(retorno, 12, sd), by = ticker]

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

fama_macbeth_nw <- function(m, vars_x, nome, h) {
  teste <- m[ym >= CORTE]
  meses <- sort(unique(teste$ym))
  coefs <- list()
  for (mm in meses) {
    dm <- teste[ym == mm]
    if (nrow(dm) < 30) next
    form <- as.formula(paste("vol_futura ~", paste(vars_x, collapse=" + ")))
    fit <- tryCatch(lm(form, data = dm), error=function(e) NULL)
    if (is.null(fit)) next
    coefs[[as.character(mm)]] <- data.table(ym=mm, t(coef(fit)))
  }
  CM <- rbindlist(coefs, fill=TRUE)
  setorder(CM, ym)
  cat(sprintf("\n%s, h=%d -- %d meses de regressao cross-sectional valida\n", nome, h, nrow(CM)))

  for (v in vars_x) {
    x <- CM[[v]]; x <- x[is.finite(x)]
    n <- length(x); media <- mean(x); dp <- sd(x)

    # ---- metodo NAIVE (o que ja usamos ate agora, assume i.i.d.) ----
    t_naive <- media/(dp/sqrt(n)); p_naive <- 2*pt(-abs(t_naive), df=n-1)

    # ---- metodo NEWEY-WEST (corrige autocorrelacao induzida pela janela sobreposta de h meses) ----
    # regride a serie de coeficientes mensais contra uma constante, erro-padrao HAC com lag = h-1
    fit_nw <- lm(x ~ 1)
    nw_se <- tryCatch(sqrt(NeweyWest(fit_nw, lag = max(1,h-1), prewhite = FALSE)[1,1]), error=function(e) NA)
    t_nw <- media / nw_se; p_nw <- 2*pt(-abs(t_nw), df=n-1)

    # autocorrelacao lag-1 da serie de coeficientes, pra documentar a magnitude do problema
    ac1 <- if (n > 2) cor(x[-1], x[-n]) else NA

    cat(sprintf("  %-14s | NAIVE: t=%7.3f p=%.6f  ||  NEWEY-WEST(lag=%d): t=%7.3f p=%.6f  || autocorr.lag1=%.3f\n",
                v, t_naive, p_naive, max(1,h-1), t_nw, p_nw, ac1))
  }
}

cat("=========================================================================\n")
cat("VERIFICACAO: HHI -> vol futura, SO' CROWDING, os 3 horizontes principais\n")
cat("=========================================================================\n")
for (h in c(3,6,12)) {
  vol_fut <- calc_vol_futura(precos, h)
  m <- merge(crowd[,.(ticker,ym,hhi_posse)], precos[,.(ticker,ym=ymk,vol_passada_12m)], by=c("ticker","ym"))
  m <- merge(m, vol_fut, by=c("ticker","ym"))
  m <- m[is.finite(hhi_posse) & is.finite(vol_futura)]
  fama_macbeth_nw(m, "hhi_posse", "SO' crowding", h)
}

cat("\n=========================================================================\n")
cat("VERIFICACAO: HHI + vol passada (controle), os 3 horizontes\n")
cat("=========================================================================\n")
for (h in c(3,6,12)) {
  vol_fut <- calc_vol_futura(precos, h)
  m <- merge(crowd[,.(ticker,ym,hhi_posse)], precos[,.(ticker,ym=ymk,vol_passada_12m)], by=c("ticker","ym"))
  m <- merge(m, vol_fut, by=c("ticker","ym"))
  m <- m[is.finite(hhi_posse) & is.finite(vol_passada_12m) & is.finite(vol_futura)]
  fama_macbeth_nw(m, c("hhi_posse","vol_passada_12m"), "HHI + vol passada", h)
}

cat("\nOK\n")
