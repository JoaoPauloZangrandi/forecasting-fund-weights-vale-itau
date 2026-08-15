# =============================================================================
# 81_diagnostico_cotistas_churn.R  (exploracao_sinais, agente eventos)
#
# Diagnostico do achado mais forte do script 80: "N. medio de cotistas dos
# fundos donos" (log) prediz volatilidade futura -- negativo e consistente
# em h=3 (p=0.006), h=6 (p=0.00006, bate Bonferroni do log principal!),
# h=12 (p=0.00022, quase bate). Mecanismo candidato: fundos com POUCOS
# cotistas (poucos clientes, cada um "grande" relativo ao fundo) tem risco
# de resgate mais "lumpy"/correlacionado (um unico cliente saindo pode ser
# uma fracao grande do fundo) -- e um mecanismo do lado do PASSIVO do fundo,
# diferente da concentracao de posse (HHI, lado do ATIVO) ja exaustivamente
# testada. Testa se sobrevive a controles: HHI, tamanho medio dos fundos
# donos (correlacionado 0.45 com cotistas), tamanho da posicao total, e
# volatilidade passada (mesmo teste que ja "matou" boa parte do efeito de
# HHI->vol em horizontes curtos, candidato #29/30 do log principal).
# Tambem verifica o achado secundario de churn/exit-rate (positivo, mais
# fraco, nao bate Bonferroni sozinho).
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA <- file.path(REPO, "v2 OFICIAL/data")
OUT  <- file.path(REPO, "v2 OFICIAL/exploracao_sinais/data")
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
CORTE <- 202001L

frag <- fread(file.path(OUT, "candidatos_79_fragilidade_donos.csv"))
winsor <- function(x) { q <- quantile(x, c(0.01,0.99), na.rm=TRUE); pmin(pmax(x,q[1]),q[2]) }
frag[, frag_laum_w := winsor(frag_laum_medio)]
frag[, frag_cotistas_w := winsor(frag_cotistas_medio)]
frag[, frag_exit_rate_w := winsor(frag_exit_rate_medio)]

# ---- HHI (crowding) e valor total detido, pra controle ----
pp3 <- fread(file.path(DATA, "painel_multiativo_final.csv"), select = c("cod_fundo","ativo","ym","peso","aum_prev"))
pp3[, ticker := trimws(sub(".*- ", "", ativo))]
pp3 <- pp3[is.finite(aum_prev) & aum_prev > 0]
pp3[, valor_posicao := peso * aum_prev]
crowd <- pp3[, .(hhi_posse = sum((valor_posicao/sum(valor_posicao))^2),
                  log_valor_total = log(sum(valor_posicao)),
                  n_fundos_c = .N), by = .(ticker, ym)]
crowd <- crowd[n_fundos_c >= 10]

base <- merge(frag, crowd, by = c("ticker","ym"))
cat("N ticker-mes na base combinada:", nrow(base), "\n\n")

cat("===== Correlacao entre as variaveis (Pearson) =====\n")
print(cor(base[, .(frag_cotistas_w, frag_laum_w, frag_exit_rate_w, hhi_posse, log_valor_total)],
          use = "pairwise.complete.obs"))

# ---- precos e volatilidade futura/passada ----
precos <- fread(file.path(DATA, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
precos <- precos[is.finite(retorno)]
setorder(precos, ticker, ymk)

calc_vol_futura <- function(dt_precos, h) {
  pw <- copy(dt_precos); cols <- list()
  for (k in 1:h) {
    aux <- pw[, .(ticker, ym_base = addm(ymk, -k), ret_k = retorno)]
    setnames(aux, "ret_k", paste0("r",k)); cols[[k]] <- aux
  }
  bb <- Reduce(function(a,b) merge(a,b,by=c("ticker","ym_base"), all=TRUE), cols)
  ret_cols <- paste0("r",1:h)
  bb[, vol_futura := apply(.SD, 1, sd, na.rm=TRUE), .SDcols = ret_cols]
  bb[, n_obs := apply(.SD, 1, function(x) sum(is.finite(x))), .SDcols = ret_cols]
  bb <- bb[n_obs >= max(2,round(h*0.7))]
  bb[, .(ticker, ym = ym_base, vol_futura)]
}
calc_vol_passada12 <- function(dt_precos) {
  pw <- copy(dt_precos); cols <- list()
  for (k in 0:11) {
    aux <- pw[, .(ticker, ym_base = addm(ymk, k), ret_k = retorno)]
    setnames(aux, "ret_k", paste0("r",k)); cols[[k+1]] <- aux
  }
  bb <- Reduce(function(a,b) merge(a,b,by=c("ticker","ym_base"), all=TRUE), cols)
  ret_cols <- paste0("r",0:11)
  bb[, vol_passada12 := apply(.SD, 1, sd, na.rm=TRUE), .SDcols = ret_cols]
  bb[, n_obs := apply(.SD, 1, function(x) sum(is.finite(x))), .SDcols = ret_cols]
  bb <- bb[n_obs >= 9]
  bb[, .(ticker, ym = ym_base, vol_passada12)]
}
vol_pass <- calc_vol_passada12(precos)

fm_multivariado <- function(m, alvo, vars_x, nome) {
  mm <- copy(m)
  for (v in vars_x) mm[, (paste0(v,"_z")) := scale(get(v)), by = ym]
  mm[, (paste0(alvo,"_ok")) := is.finite(get(alvo))]
  cols_z <- paste0(vars_x, "_z")
  mm <- mm[Reduce(`&`, lapply(cols_z, function(c) is.finite(mm[[c]]))) & is.finite(mm[[alvo]])]
  teste <- mm[ym >= CORTE]
  form <- as.formula(paste(alvo, "~", paste(cols_z, collapse=" + ")))
  meses <- sort(unique(teste$ym))
  coef_list <- list()
  for (mes in meses) {
    d <- teste[ym == mes]
    if (nrow(d) < 30) next
    fit <- tryCatch(lm(form, data = d), error = function(e) NULL)
    if (is.null(fit)) next
    cc <- coef(fit)
    coef_list[[length(coef_list)+1]] <- as.list(cc)
  }
  if (length(coef_list) < 6) { cat(sprintf("%-40s: poucos meses (%d)\n", nome, length(coef_list))); return(invisible(NULL)) }
  CM <- rbindlist(coef_list, fill = TRUE)
  cat(sprintf("\n--- %s (alvo=%s, %d meses de teste) ---\n", nome, alvo, nrow(CM)))
  for (cn in names(CM)) {
    x <- CM[[cn]]; x <- x[is.finite(x)]
    if (length(x) < 6) next
    med <- mean(x); dp <- sd(x); n <- length(x); t <- med/(dp/sqrt(n)); p <- 2*pt(-abs(t), df=n-1)
    cat(sprintf("  %-22s coef_medio=%+8.5f  t=%6.2f  p=%.5f  (n_meses=%d)\n", cn, med, t, p, n))
  }
}

cat("\n\n===== DIAGNOSTICO: frag_cotistas sobrevive controlando por HHI, tamanho dos fundos, tamanho da posicao? =====\n")
for (h in c(3,6,12)) {
  vol_h <- calc_vol_futura(precos, h)
  m <- merge(base, vol_h, by = c("ticker","ym"))
  fm_multivariado(m, "vol_futura", c("frag_cotistas_w","hhi_posse","frag_laum_w","log_valor_total"),
                   sprintf("cotistas + HHI + tam.fundo + tam.posicao -> vol h=%d", h))
}

cat("\n\n===== DIAGNOSTICO: frag_cotistas sobrevive controlando por VOLATILIDADE PASSADA (12m)? =====\n")
for (h in c(3,6,12)) {
  vol_h <- calc_vol_futura(precos, h)
  m <- merge(base, vol_h, by = c("ticker","ym"))
  m <- merge(m, vol_pass, by = c("ticker","ym"))
  fm_multivariado(m, "vol_futura", c("frag_cotistas_w","vol_passada12"),
                   sprintf("cotistas + vol.passada12m -> vol h=%d", h))
}

cat("\n\n===== DIAGNOSTICO: frag_exit_rate (churn) sobrevive aos mesmos controles? =====\n")
for (h in c(3,6,12)) {
  vol_h <- calc_vol_futura(precos, h)
  m <- merge(base, vol_h, by = c("ticker","ym"))
  m <- merge(m, vol_pass, by = c("ticker","ym"))
  fm_multivariado(m, "vol_futura", c("frag_exit_rate_w","hhi_posse","vol_passada12"),
                   sprintf("churn + HHI + vol.passada -> vol h=%d", h))
}

cat("\n\n===== Robustez adicional: correlacao frag_cotistas com log_valor_total por SUBGRUPO de tamanho =====\n")
base[, tercil_tamanho := as.integer(cut(log_valor_total, quantile(log_valor_total, 0:3/3, na.rm=TRUE), include.lowest=TRUE))]
cat("Correlacao frag_cotistas x hhi_posse dentro de cada tercil de tamanho (deve ficar baixa se sao mecanismos distintos):\n")
print(base[, .(cor_cotistas_hhi = cor(frag_cotistas_w, hhi_posse, use="pairwise.complete.obs"),
                cor_cotistas_tamanho = cor(frag_cotistas_w, log_valor_total, use="pairwise.complete.obs"), n=.N), by=tercil_tamanho][order(tercil_tamanho)])

cat("\nOK\n")
