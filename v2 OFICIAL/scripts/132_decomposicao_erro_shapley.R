# =============================================================================
# 132_decomposicao_erro_shapley.R  (v2 OFICIAL)
#
# Bloco C do plano: decompoe a variacao do erro fora da amostra em
# componentes de ATIVO, MES, FUNDO e GESTORA.
#
# Por que Shapley e nao R2 sequencial: os efeitos fixos sao cruzados e
# desbalanceados, e gestora e' ESTRITAMENTE ANINHADA em fundo. Com
# aninhamento, a contribuicao marginal de gestora DADO fundo e' zero por
# construcao, e a contribuicao isolada e' positiva -- o R2 sequencial
# devolve um dos dois conforme a ordem, e os dois parecem "a resposta".
# O valor de Shapley e' a media sobre as 4! = 24 ordens e e' a unica
# atribuicao que soma exatamente ao R2 total.
#
# Dependente primaria: log(erro_oos^2 + c). O objeto de interesse e' a
# variancia, e vies^2/RMSE^2 = 0,000% no pool (log do script 86), entao
# E[e^2] = Var(e). O modelo certo de heterocedasticidade e' MULTIPLICATIVO,
# Var = exp(theta_n + theta_t + theta_i), e o log e' a forma aditiva dele --
# a que permite decompor R2. O plano previa quasi-Poisson como primaria, mas
# Poisson nao minimiza soma de quadrados e o R2 na escala da resposta nao e'
# monotono, o que inviabiliza Shapley; entrou como robustez com pseudo-R2.
#
# Robustez: quasi-Poisson (pseudo-R2 de deviance), |e|, e^2 winsorizado.
# Se as participacoes pularem, o resultado e' sobre a cauda e isso aparece.
#
# Correcao de vies: Var(theta_hat) e' viesado para cima porque theta_hat
# carrega erro de estimacao (limited mobility bias, AKM). Estimador
# split-sample: Var(theta) = Cov(theta_A, theta_B) com metades independentes.
#
# RODAR COM CAMINHO ABSOLUTO. Pesado -- roda em alguns minutos.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")
PISO_POEIRA <- 0.001
set.seed(20260915L)
setFixest_notes(FALSE)

cat("=============================================================\n")
cat("132 - Decomposicao do erro em componentes (Shapley)\n")
cat("=============================================================\n\n")

pp <- fread(file.path(DD, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ativo","peso"), showProgress = FALSE)
pp[, cod_fundo := as.character(cod_fundo)]
cel_ok <- pp[, .(pm = median(peso)), by = .(cod_fundo, ativo)][pm >= PISO_POEIRA, .(cod_fundo, ativo)]
rm(pp); invisible(gc())

H <- fread(file.path(DD, "etapa3_multiativo_h1.csv"),
           select = c("cod_fundo","ativo","ym","gestora_grupo","erro_oos"),
           showProgress = FALSE)
H[, cod_fundo := as.character(cod_fundo)]
H <- merge(H, cel_ok, by = c("cod_fundo","ativo"))
H[, `:=`(e2 = erro_oos^2, ae = abs(erro_oos))]
cat(sprintf("Base: %d obs | %d ativos | %d meses | %d fundos | %d gestoras\n\n",
            nrow(H), uniqueN(H$ativo), uniqueN(H$ym), uniqueN(H$cod_fundo),
            uniqueN(H$gestora_grupo)))

# conferencia do aninhamento
aninha <- H[, uniqueN(gestora_grupo), by = cod_fundo][V1 > 1, .N]
cat(sprintf("Fundos com mais de uma gestora (tem que ser 0): %d\n\n", aninha))

FATORES <- c("ativo","ym","cod_fundo","gestora_grupo")

# --- R2 de um subconjunto de efeitos fixos -----------------------------------
# DUAS ARMADILHAS, ambas encontradas na primeira rodada deste script:
#  (1) fixest remove observacoes singleton por padrao, entao cada subconjunto
#      rodava numa AMOSTRA DIFERENTE e o R2 chegava a CAIR ao acrescentar um
#      fator -- impossivel em OLS. Resolve-se com fixef.rm = "none".
#  (2) Poisson nao minimiza soma de quadrados, entao o R2 na escala da resposta
#      nao e' monotono e o Shapley fica sem sentido. A dependente primaria
#      passa a ser log(e^2 + c), que e' a forma ADITIVA do mesmo modelo
#      multiplicativo Var = exp(theta_n + theta_t + theta_i). Poisson fica como
#      robustez, com pseudo-R2 de deviance, que e' monotono.
r2_subset <- function(dt, yvar, fats, familia) {
  if (!length(fats)) return(0)
  frm <- as.formula(paste(yvar, "~ 1 |", paste(fats, collapse = " + ")))
  fit <- tryCatch({
    if (familia == "pois") fepois(frm, data = dt, fixef.rm = "none", warn = FALSE, notes = FALSE)
    else feols(frm, data = dt, fixef.rm = "none", warn = FALSE, notes = FALSE)
  }, error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  if (familia == "pois") {
    return(as.numeric(fitstat(fit, "pr2")$pr2))   # pseudo-R2 de deviance, monotono
  }
  y  <- dt[[yvar]]; fv <- as.numeric(predict(fit))
  1 - sum((y - fv)^2) / sum((y - mean(y))^2)
}

# --- Shapley sobre os 4 fatores ----------------------------------------------
shapley <- function(dt, yvar, familia, rotulo) {
  cat(sprintf("--- Shapley: %s (%s) ---\n", rotulo, familia))
  subs <- unlist(lapply(0:4, function(k) combn(FATORES, k, simplify = FALSE)),
                 recursive = FALSE)
  # prefixo obrigatorio: em R, indexar vetor nomeado por "" devolve NA, entao o
  # conjunto vazio precisa de uma chave nao vazia
  chave <- function(s) paste0("S:", paste(sort(s), collapse = "|"))
  val <- setNames(numeric(length(subs)), sapply(subs, chave))
  for (i in seq_along(subs)) {
    val[chave(subs[[i]])] <- r2_subset(dt, yvar, subs[[i]], familia)
    cat(sprintf("    [%2d/16] {%s} R2 = %.4f\n", i,
                ifelse(length(subs[[i]]), paste(subs[[i]], collapse=","), "vazio"),
                val[chave(subs[[i]])]))
  }
  n <- 4
  fatorial <- factorial
  sh <- sapply(FATORES, function(f) {
    outros <- setdiff(FATORES, f)
    tot <- 0
    for (k in 0:3) for (S in combn(outros, k, simplify = FALSE)) {
      w <- fatorial(k) * fatorial(n - k - 1) / fatorial(n)
      tot <- tot + w * (val[chave(c(S, f))] - val[chave(S)])
    }
    tot
  })
  isolado  <- sapply(FATORES, function(f) val[chave(f)])
  marginal <- sapply(FATORES, function(f) val[chave(FATORES)] - val[chave(setdiff(FATORES, f))])
  out <- data.table(dependente = rotulo, familia = familia, fator = FATORES,
                    r2_isolado = as.numeric(isolado),
                    r2_marginal = as.numeric(marginal),
                    shapley = as.numeric(sh))
  r2_tot <- val[chave(FATORES)]
  cat(sprintf("  R2 total = %.4f | soma dos Shapley = %.4f | diferenca = %.2e\n",
              r2_tot, sum(sh), r2_tot - sum(sh)))
  out[, r2_total := r2_tot]
  out[, pct_shapley := 100 * shapley / r2_tot]
  print(out[, .(fator, r2_isolado = round(r2_isolado,4), r2_marginal = round(r2_marginal,4),
                shapley = round(shapley,4), pct = round(pct_shapley,1))])
  cat("\n")
  out
}

c_piso <- H[e2 > 0, quantile(e2, 0.01)]
H[, l_e2 := log(e2 + c_piso)]
cat(sprintf("Piso c = p1 dos e^2 positivos = %.3e\n\n", c_piso))

RES <- list()
RES[[1]] <- shapley(H, "l_e2", "ols",  "log(erro^2 + c) [PRIMARIA]")
RES[[2]] <- shapley(H, "e2",   "pois", "erro^2 (quasi-Poisson, pseudo-R2)")
RES[[3]] <- shapley(H, "ae",   "ols",  "|erro|")
lim <- quantile(H$e2, 0.999)
H[, e2_w := pmin(e2, lim)]
H[, l_e2w := log(e2_w + c_piso)]
RES[[4]] <- shapley(H, "l_e2w", "ols", "log(erro^2 winsorizado 99,9% + c)")

# --- versao CONDICIONAL a (ativo x mes) --------------------------------------
# A Etapa 1 e' um GLM por celula (ativo, mes), entao os residuos sao ortogonais
# aos regressores DENTRO da celula. Isso pre-carrega a decomposicao a favor de
# ativo x mes. A versao abaixo remove a media da celula antes de decompor, e e'
# a unica que responde a pergunta sobre GESTORAS.
cat("--- Versao CONDICIONAL a (ativo x mes): remove a media da celula ---\n")
H[, cel := paste(ativo, ym)]
H[, l_e2_cond := l_e2 - mean(l_e2), by = cel]
RES[[5]] <- shapley(H, "l_e2_cond", "ols", "log(erro^2) condicional a ativo x mes")

# --- correcao de vies por split impar/par ------------------------------------
cat("--- Correcao de vies: split-sample impar/par ---\n")
H[, mes_idx := as.integer(factor(ym))]
H[, lado := ifelse(mes_idx %% 2L == 1L, "A", "B")]
comp_fe <- function(fat) {
  fa <- H[lado == "A", .(m = mean(l_e2)), by = fat]
  fb <- H[lado == "B", .(m = mean(l_e2)), by = fat]
  M <- merge(fa, fb, by = fat, suffixes = c("_A","_B"))
  M <- M[is.finite(m_A) & is.finite(m_B)]
  bruto <- var(c(M$m_A, M$m_B))
  corr  <- cov(M$m_A, M$m_B)
  data.table(fator = fat, n_niveis = nrow(M), var_bruta = bruto,
             var_corrigida = corr, confiabilidade = corr / bruto)
}
BIAS <- rbindlist(lapply(c("ativo","cod_fundo","gestora_grupo"), comp_fe))
print(BIAS[, .(fator, n_niveis, var_bruta = signif(var_bruta,3),
               var_corrigida = signif(var_corrigida,3),
               confiabilidade = round(confiabilidade,3))])
cat("  (confiabilidade = quanto da variancia do efeito fixo e sinal, nao ruido de estimacao)\n\n")

SH <- rbindlist(RES)
fwrite(SH,   file.path(DD, "decomp_shapley_erro.csv"))
fwrite(BIAS, file.path(DD, "decomp_var_componentes.csv"))

# --- figura -------------------------------------------------------------------
pl <- SH[dependente %in% c("log(erro^2 + c) [PRIMARIA]", "log(erro^2) condicional a ativo x mes")]
M <- dcast(pl, fator ~ dependente, value.var = "pct_shapley")
pdf(file.path(FIG, "fig_ext_shapley_decomposicao.pdf"), width = 7.5, height = 5)
par(mar = c(4, 9, 3, 2))
barplot(t(as.matrix(M[, -1])), beside = TRUE, horiz = TRUE, las = 1,
        names.arg = M$fator, col = c("#3B6E9E","#B8452E"), border = NA,
        xlab = "% do R2 atribuido (valor de Shapley)",
        main = "De onde vem a variacao do erro")
legend("bottomright", legend = names(M)[-1], fill = c("#3B6E9E","#B8452E"),
       bty = "n", cex = 0.75)
dev.off()
cat("OK - decomp_shapley_erro.csv, decomp_var_componentes.csv, fig_ext_shapley_decomposicao.pdf\n")
