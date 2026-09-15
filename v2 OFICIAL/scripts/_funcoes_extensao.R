# =============================================================================
# _funcoes_extensao.R  (v2 OFICIAL)
#
# Funcoes compartilhadas entre os scripts 130 (desenvolvimento) e 131
# (confirmacao). Ficam num arquivo so para garantir que as duas amostras
# passem pelo MESMO codigo -- se a confirmacao rodasse por um caminho
# diferente, o hold-out perderia o sentido.
#
# Prefixo "_" = nao e' um passo do pipeline, e' biblioteca.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table); library(sandwich); library(lmtest)
})

# --- definicao dos blocos (pre-registro Secao 4, com emendas E1/E2) ----------
BLOCO0_DIRETA <- c("beta_fundo","l_aum","l_cot","pct_fic","flow_aum","hhi_medio")
BLOCO0_ORTO   <- c("beta_resid","l_aum","l_cot","pct_fic","flow_aum","hhi_medio")
BLOCO1 <- c("sd_flow_resid","sd_log_hhi","amp_beta")            # dispersoes
BLOCO2 <- c("turn_mediano","dsleeve_mediano")                   # turnover
BLOCO3 <- c("G1_multimercado","G3_indexado","G4_estilo","G5_nicho","G0_sem_classe")
BLOCO4 <- c("l_n_ativos","razao_poeira")                        # breadth
CONTROLES <- c("soma_peso_medio","n_meses_treino","fonte_b3_pesada")

NOME_BLOCO <- c(rep("base", 6), rep("dispersoes", length(BLOCO1)),
                rep("turnover", length(BLOCO2)), rep("mandato", length(BLOCO3)),
                rep("breadth", length(BLOCO4)), rep("controle", length(CONTROLES)))

prepara_dummies <- function(D) {
  D <- copy(D)
  for (g in c("G1_multimercado","G3_indexado","G4_estilo","G5_nicho","G0_sem_classe")) {
    D[, (g) := as.integer(grupo_mandato == g)]
  }
  D[]
}

# --- pesos do wild bootstrap -------------------------------------------------
gerar_pesos <- function(G, B, tipo) {
  if (tipo == "webb") {
    v <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
    matrix(sample(v, G * B, replace = TRUE), nrow = G)
  } else {
    matrix(sample(c(-1, 1), G * B, replace = TRUE), nrow = G)
  }
}

# --- wild cluster bootstrap-t com nulo imposto -------------------------------
# Vetorizado: so y muda entre replicas, entao (X'X)^{-1}X' e' calculado uma vez
# e o elemento (j,j) do sanduiche sai de sum_g (sum_{i in g} z_i e_i)^2, com
# z = X (X'X)^{-1} e_j. Isso evita montar a matriz de covariancia por replica.
wcb_p <- function(y, X, g, j, B = 1999L, tipo = "rademacher") {
  n <- length(y)
  XtX <- crossprod(X)
  M <- tryCatch(solve(XtX), error = function(e) NULL)
  if (is.null(M)) return(list(t_obs = NA_real_, se = NA_real_, p = NA_real_))
  A <- M %*% t(X)
  b <- as.vector(A %*% y)
  e <- as.vector(y - X %*% b)
  z <- as.vector(X %*% M[, j])
  gf <- as.integer(factor(g)); G <- max(gf)
  se_obs <- sqrt(sum(rowsum(z * e, gf)^2))
  if (!is.finite(se_obs) || se_obs <= 0) return(list(t_obs = NA_real_, se = NA_real_, p = NA_real_))
  t_obs <- b[j] / se_obs

  Xr <- X[, -j, drop = FALSE]
  br <- tryCatch(qr.solve(Xr, y), error = function(e) NULL)
  if (is.null(br)) return(list(t_obs = t_obs, se = se_obs, p = NA_real_))
  yhat_r <- as.vector(Xr %*% br); er <- y - yhat_r

  cnt <- 0L; feito <- 0L; passo <- 250L
  while (feito < B) {
    bb <- min(passo, B - feito)
    V  <- gerar_pesos(G, bb, tipo)[gf, , drop = FALSE]
    Ys <- yhat_r + er * V
    Bs <- A %*% Ys
    Es <- Ys - X %*% Bs
    SS <- rowsum(z * Es, gf)
    se <- sqrt(colSums(SS^2))
    ts <- Bs[j, ] / se
    cnt <- cnt + sum(is.finite(ts) & abs(ts) >= abs(t_obs) - 1e-12)
    feito <- feito + bb
  }
  list(t_obs = t_obs, se = se_obs, p = (cnt + 1) / (B + 1))
}

# regressor quase constante dentro do cluster -> Rademacher e' fragil (MacKinnon
# & Webb 2018), usa-se Webb
quase_constante <- function(x, g, limite = 0.05) {
  vt <- var(x)
  if (!is.finite(vt) || vt <= 0) return(TRUE)
  m <- ave(x, g, FUN = mean)
  var(x - m) / vt < limite
}

# --- teste F de bloco, com vcov clusterizado ---------------------------------
f_bloco <- function(fit, vars, cl) {
  vars <- intersect(vars, names(coef(fit))[!is.na(coef(fit))])
  if (!length(vars)) return(data.table(k = 0L, F = NA_real_, p = NA_real_))
  V <- sandwich::vcovCL(fit, cluster = cl, type = "HC1")
  b <- coef(fit)[vars]; Vs <- V[vars, vars, drop = FALSE]
  Vi <- tryCatch(solve(Vs), error = function(e) NULL)
  if (is.null(Vi)) return(data.table(k = length(vars), F = NA_real_, p = NA_real_))
  W <- as.numeric(t(b) %*% Vi %*% b)
  G <- length(unique(cl)); q <- length(vars)
  Fst <- W / q
  data.table(k = q, F = Fst, p = pf(Fst, q, G - 1, lower.tail = FALSE))
}

# --- Mundlak: desvio da media da gestora + media da gestora -----------------
mundlak <- function(D, vars) {
  D <- copy(D)
  novos_W <- character(0); novos_B <- character(0)
  for (v in vars) {
    mv <- paste0(v, "_B"); wv <- paste0(v, "_W")
    D[, (mv) := mean(get(v), na.rm = TRUE), by = gestora_grupo]
    D[, (wv) := get(v) - get(mv)]
    novos_W <- c(novos_W, wv); novos_B <- c(novos_B, mv)
  }
  list(D = D, W = novos_W, B = novos_B)
}

# --- uma especificacao -------------------------------------------------------
roda_spec <- function(D, dep, vars, spec, param, amostra, B, fe_gestora = FALSE,
                      verbose = TRUE) {
  vars <- vars[sapply(vars, function(v) v %in% names(D) &&
                        is.finite(var(D[[v]], na.rm = TRUE)) && var(D[[v]], na.rm=TRUE) > 0)]
  cols <- c(dep, vars, "gestora_grupo")
  S <- D[complete.cases(D[, ..cols])]
  if (nrow(S) < 50) return(NULL)
  cl <- S$gestora_grupo

  if (fe_gestora) {
    # within: demedia dependente e regressores pela gestora
    S2 <- copy(S)
    S2[, (dep) := get(dep) - mean(get(dep)), by = gestora_grupo]
    for (v in vars) S2[, (v) := get(v) - mean(get(v)), by = gestora_grupo]
    S_fit <- S2
  } else S_fit <- S

  frm <- as.formula(paste(dep, "~", paste(vars, collapse = " + ")))
  fit <- lm(frm, data = S_fit)
  V   <- sandwich::vcovCL(fit, cluster = cl, type = "HC1")
  ct  <- lmtest::coeftest(fit, vcov. = V)

  # descarta colunas aliased (coef NA): model.matrix as mantem, coeftest nao
  X <- model.matrix(fit); y <- S_fit[[dep]]
  vivos <- names(coef(fit))[!is.na(coef(fit))]
  X <- X[, colnames(X) %in% vivos, drop = FALSE]
  nm <- colnames(X)
  linhas <- list()
  for (j in seq_along(nm)) {
    if (nm[j] == "(Intercept)") next
    if (!nm[j] %in% rownames(ct)) next
    tipo <- if (quase_constante(X[, j], cl)) "webb" else "rademacher"
    w <- wcb_p(y, X, cl, j, B = B, tipo = tipo)
    linhas[[length(linhas)+1L]] <- data.table(
      amostra = amostra, spec = spec, param = param, dep = dep,
      variavel = nm[j],
      coef = unname(coef(fit)[nm[j]]),
      se_cv1 = unname(ct[nm[j], 2]), t_cv1 = unname(ct[nm[j], 3]),
      p_cv1 = unname(ct[nm[j], 4]),
      se_wcb = w$se, t_wcb = w$t_obs, p_wcb = w$p, pesos_wcb = tipo)
  }
  coefs <- rbindlist(linhas)

  blocos <- rbindlist(list(
    cbind(bloco = "dispersoes", f_bloco(fit, c(BLOCO1, paste0(BLOCO1,"_W"), paste0(BLOCO1,"_B")), cl)),
    cbind(bloco = "turnover",   f_bloco(fit, c(BLOCO2, paste0(BLOCO2,"_W"), paste0(BLOCO2,"_B")), cl)),
    cbind(bloco = "mandato",    f_bloco(fit, c(BLOCO3, paste0(BLOCO3,"_W"), paste0(BLOCO3,"_B")), cl)),
    cbind(bloco = "breadth",    f_bloco(fit, c(BLOCO4, paste0(BLOCO4,"_W"), paste0(BLOCO4,"_B")), cl))
  ))
  blocos[, `:=`(amostra = amostra, spec = spec, param = param, dep = dep)]

  sm <- summary(fit)
  ajuste <- data.table(amostra = amostra, spec = spec, param = param, dep = dep,
                       n = nrow(S), n_clusters = uniqueN(cl), k = length(vars),
                       r2 = sm$r.squared, r2_aj = sm$adj.r.squared)
  if (verbose) cat(sprintf("  %-4s %-6s %-8s n=%4d k=%2d  R2=%.4f  R2aj=%.4f\n",
                           spec, dep, param, nrow(S), length(vars),
                           sm$r.squared, sm$adj.r.squared))
  list(coef = coefs, blocos = blocos, ajuste = ajuste)
}

# --- bateria completa --------------------------------------------------------
roda_bateria <- function(D, rotulo_amostra, B = 1999L, verbose = TRUE) {
  CO <- list(); BL <- list(); AJ <- list()
  for (param in c("direta", "orto")) {
    base <- if (param == "direta") BLOCO0_DIRETA else BLOCO0_ORTO
    todos <- c(base, BLOCO1, BLOCO2, BLOCO3, BLOCO4, CONTROLES)
    for (dep in c("D1", "D2")) {
      if (verbose) cat(sprintf("\n[%s / %s]\n", param, dep))
      # A1 pooled
      r <- roda_spec(D, dep, todos, "A1", param, rotulo_amostra, B, FALSE, verbose)
      if (!is.null(r)) { CO[[length(CO)+1]] <- r$coef; BL[[length(BL)+1]] <- r$blocos; AJ[[length(AJ)+1]] <- r$ajuste }
      # A2 Mundlak
      mk <- mundlak(D, todos)
      r <- roda_spec(mk$D, dep, c(mk$W, mk$B), "A2", param, rotulo_amostra, B, FALSE, verbose)
      if (!is.null(r)) { CO[[length(CO)+1]] <- r$coef; BL[[length(BL)+1]] <- r$blocos; AJ[[length(AJ)+1]] <- r$ajuste }
      # A3 within-gestora
      r <- roda_spec(D, dep, todos, "A3", param, rotulo_amostra, B, TRUE, verbose)
      if (!is.null(r)) { CO[[length(CO)+1]] <- r$coef; BL[[length(BL)+1]] <- r$blocos; AJ[[length(AJ)+1]] <- r$ajuste }
    }
  }
  coef <- rbindlist(CO); blocos <- rbindlist(BL, fill = TRUE); ajuste <- rbindlist(AJ)

  # --- correcao multipla ------------------------------------------------------
  blocos <- blocos[is.finite(p)]
  blocos[, p_holm := p.adjust(p, method = "holm"), by = .(spec, param, dep)]
  coef[, bloco := fcase(
    grepl("^sd_|^amp_", variavel), "dispersoes",
    grepl("^turn_|^dsleeve_", variavel), "turnover",
    grepl("^G[0-9]_", variavel), "mandato",
    grepl("^l_n_ativos|^razao_poeira", variavel), "breadth",
    grepl("^soma_peso|^n_meses_treino|^fonte_b3", variavel), "controle",
    default = "base")]
  coef[, p_holm := p.adjust(p_wcb, method = "holm"), by = .(spec, param, dep, bloco)]
  coef[, q_bh   := p.adjust(p_wcb, method = "BH"),   by = .(spec, param, dep)]

  ledger <- ajuste[, .(ts = as.character(Sys.time()), script = "130/131",
                       spec_id = paste(amostra, spec, param, dep, sep = "_"),
                       amostra, dep, spec, param, n, n_clusters, k, r2, r2_aj,
                       pre_registrado = "S")]
  list(coef = coef, blocos = blocos, ajuste = ajuste, ledger = ledger)
}

grava_ledger <- function(dt, DD) {
  f <- file.path(DD, "ledger_trials_extensao.csv")
  if (file.exists(f)) {
    velho <- fread(f)
    fwrite(rbindlist(list(velho, dt), fill = TRUE), f)
  } else fwrite(dt, f)
  cat(sprintf("Ledger: +%d linhas\n", nrow(dt)))
}
