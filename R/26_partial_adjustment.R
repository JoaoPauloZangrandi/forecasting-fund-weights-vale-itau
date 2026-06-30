# =============================================================================
# 26_partial_adjustment.R   — PASSO 3 da agenda do orientador
#
# Modelo de AJUSTE PARCIAL da carteira:
#   w_{i,t+1} - w_{i,t} = lambda * d_{i,t} + u_{i,t+1},   d_{i,t} = w*_{i,t} - w_{i,t}
# com w*_{i,t} = g(x' theta_t) = peso desejado (logit do passo 2, fitted no mes t).
#
# d_{i,t} eh MEDIDO COM ERRO (usa theta_t estimado) e compartilha w_{i,t} com o
# lado esquerdo -> MQO sofre vies (atenuacao + reversao mecanica). Corrigimos por
# VARIAVEL INSTRUMENTAL usando a DISTANCIA DEFASADA d_{i,t-1} como instrumento.
#
# Estima MQO e VI (2SLS) lado a lado, com erro-padrao AGRUPADO POR FUNDO.
# Teste central: lambda != 0. RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

d <- fread(file.path(REPO, "data/processed/painel_vale_itau_2016_2021_filtrado.csv"))
d[, aum_num := as.numeric(aum)][, l_aum := log(aum_num)][, l_cot := log(n_cotistas)]
d[, flow_aum := fluxo_liq / aum_num][, ym := ano*100L + mes]
d <- d[!is.na(flow_aum)]
d[, z_aum := (l_aum-mean(l_aum))/sd(l_aum)][, z_cot := (l_cot-mean(l_cot))/sd(l_cot)]

# --- (1) peso desejado w* (logit por mes, passo 2) e distancia d = w* - w ---
d[, w_star := NA_real_]
for (mm in sort(unique(d$ym))) {
  idx <- d$ym == mm
  fg  <- suppressWarnings(glm(peso_vale3 ~ z_aum + z_cot + is_fic + flow_aum,
                              family = quasibinomial(link = "logit"), data = d[idx]))
  d[idx, w_star := fitted(fg)]
}
d[, dist := w_star - peso_vale3]          # d_{i,t}

# --- (2) montar Delta w_{t+1} e distancia defasada d_{t-1} ---
nextym <- function(y) ifelse(y %% 100L == 12L, (y %/% 100L + 1L)*100L + 1L, y + 1L)
prevym <- function(y) ifelse(y %% 100L == 1L,  (y %/% 100L - 1L)*100L + 12L, y - 1L)
d[, ym_next := nextym(ym)][, ym_prev := prevym(ym)]

wlead <- d[, .(cod_fundo, ymj = ym, peso_next = peso_vale3)]
M <- merge(d, wlead, by.x = c("cod_fundo","ym_next"), by.y = c("cod_fundo","ymj"))
dlag  <- d[, .(cod_fundo, ymj = ym, dist_lag = dist)]
M <- merge(M, dlag, by.x = c("cod_fundo","ym_prev"), by.y = c("cod_fundo","ymj"))
M[, dw := peso_next - peso_vale3]         # Delta w_{t+1}
M <- M[is.finite(dw) & is.finite(dist) & is.finite(dist_lag)]
cat("Obs na estimacao:", nrow(M), "| fundos:", uniqueN(M$cod_fundo),
    "| meses:", uniqueN(M$ym), "\n\n")

# --- erro-padrao agrupado por fundo (sanduiche) ---
cl_meat <- function(Z, u, cl) {
  k <- ncol(Z); Mt <- matrix(0, k, k)
  for (g in unique(cl)) { ix <- cl == g; s <- crossprod(Z[ix,,drop=FALSE], u[ix]); Mt <- Mt + s %*% t(s) }
  G <- length(unique(cl)); (G/(G-1)) * Mt
}
y  <- M$dw; cl <- M$cod_fundo
X  <- cbind(1, M$dist)                       # regressores: intercepto, d_t
Zi <- cbind(1, M$dist_lag)                   # instrumentos: intercepto, d_{t-1}

# (MQO)
bo  <- solve(crossprod(X), crossprod(X, y)); uo <- as.numeric(y - X %*% bo)
Vo  <- solve(crossprod(X)) %*% cl_meat(X, uo, cl) %*% solve(crossprod(X))
# (VI / 2SLS, exatamente identificado: beta = (Z'X)^{-1} Z'y)
bi  <- solve(crossprod(Zi, X), crossprod(Zi, y)); ui <- as.numeric(y - X %*% bi)
B   <- solve(crossprod(Zi, X)); Vi <- B %*% cl_meat(Zi, ui, cl) %*% t(B)

# primeiro estagio (relevancia do instrumento): d_t ~ d_{t-1}
fs  <- lm(dist ~ dist_lag, data = M); fss <- summary(fs)

rep_row <- function(nome, b, V) {
  se <- sqrt(diag(V)); t <- b/se
  data.table(modelo = nome, lambda = b[2], se_lambda = se[2], t = t[2],
             p = 2*pnorm(-abs(t[2])))
}
res <- rbind(rep_row("MQO (atenuado)", bo, Vo), rep_row("VI: instrumento d_{t-1}", bi, Vi))

cat("===== PRIMEIRO ESTAGIO (relevancia): d_t ~ d_{t-1} =====\n")
cat("coef d_{t-1}:", round(coef(fs)[2],4), "| t:", round(fss$coefficients[2,3],1),
    "| R2:", round(fss$r.squared,3), "| F:", round(fss$fstatistic[1],0), "\n\n")

cat("===== AJUSTE PARCIAL: lambda (velocidade de ajuste), EP agrupado por fundo =====\n")
print(res[, lapply(.SD, function(z) if (is.numeric(z)) round(z,5) else z)])

lam <- res[modelo == "VI: instrumento d_{t-1}", lambda]
cat("\nlambda (VI):", round(lam,4))
if (lam > 0 && lam < 1) cat(" | meia-vida do ajuste:",
    round(log(0.5)/log(1-lam),1), "meses")
cat("\n")

fwrite(res, file.path(REPO, "data/processed/reg26_partial_adjustment.csv"))
cat("\nOK - salvo em data/processed/reg26_partial_adjustment.csv\n")
