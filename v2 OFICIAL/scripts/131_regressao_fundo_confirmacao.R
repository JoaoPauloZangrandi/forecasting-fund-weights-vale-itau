# =============================================================================
# 131_regressao_fundo_confirmacao.R  (v2 OFICIAL)
#
# CONFIRMACAO. Roda UMA vez, com a especificacao congelada no pre-registro.
# Este e' o resultado do artigo. Nada aqui foi ajustado depois de olhar o
# lado de desenvolvimento -- a lista de regressores vem da Secao 4 do
# pre-registro (commit fbb244f) com as emendas E1 e E2, todas datadas
# antes do script 130 rodar.
#
# B = 9999 (o dev usou 1999).
#
# Roda tambem a amostra S2 (janela movel) como robustez de selecao.
# =============================================================================
suppressPackageStartupMessages({ library(data.table) })

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
set.seed(20260915L)
B_WCB <- 9999L

source(file.path(REPO, "v2 OFICIAL/scripts/_funcoes_extensao.R"))

cat("=============================================================\n")
cat("131 - CONFIRMACAO (roda uma vez)\n")
cat("=============================================================\n\n")

SP <- fread(file.path(DD, "split_fundos_dev_conf.csv"))
SP[, cod_fundo := as.character(cod_fundo)]

roda_amostra <- function(arq, rot) {
  B <- fread(file.path(DD, arq)); B[, cod_fundo := as.character(cod_fundo)]
  D <- merge(B, SP[, .(cod_fundo, grupo)], by = "cod_fundo")
  CONF <- prepara_dummies(D[grupo == "conf"])
  cat(sprintf("\n### %s: %d fundos | %d gestoras ###\n", rot,
              nrow(CONF), uniqueN(CONF$gestora_grupo)))
  r <- roda_bateria(CONF, rotulo_amostra = rot, B = B_WCB, verbose = TRUE)
  r
}

r1 <- roda_amostra("caracteristicas_fundo_treino.csv",    "conf_S1")
r2 <- roda_amostra("caracteristicas_fundo_treino_S2.csv", "conf_S2")

coef   <- rbindlist(list(r1$coef, r2$coef), fill = TRUE)
blocos <- rbindlist(list(r1$blocos, r2$blocos), fill = TRUE)
ajuste <- rbindlist(list(r1$ajuste, r2$ajuste), fill = TRUE)

fwrite(coef,   file.path(DD, "reg_fundo_conf_coef.csv"))
fwrite(blocos, file.path(DD, "reg_fundo_conf_blocos.csv"))
fwrite(ajuste, file.path(DD, "reg_fundo_conf_ajuste.csv"))
grava_ledger(rbindlist(list(r1$ledger, r2$ledger)), DD)

# --- resumo para a tabela principal do artigo --------------------------------
cat("\n=============================================================\n")
cat("TESTES F DE BLOCO -- confirmacao, amostra S1\n")
cat("=============================================================\n")
pr <- blocos[amostra == "conf_S1" & param == "direta" & spec %in% c("A1","A3")]
setorder(pr, dep, spec, bloco)
print(pr[, .(dep, spec, bloco, k, F = round(F,2), p = signif(p,3), p_holm = signif(p_holm,3))])

cat("\n--- Regra de decisao H3 (mandato): tabela 2x2 COMPLETA ---\n")
cat("    {D1, D2} x {com controle de soma_peso, sem controle}\n")
cat("    Sobrevive nas 4 celulas -> mandato. So em {D1, sem} -> aritmetica de sleeve.\n\n")
B1c <- fread(file.path(DD, "caracteristicas_fundo_treino.csv"))
B1c[, cod_fundo := as.character(cod_fundo)]
CONF <- prepara_dummies(merge(B1c, SP[, .(cod_fundo, grupo)], by = "cod_fundo")[grupo == "conf"])
celulas <- list()
for (dep in c("D1","D2")) for (ctrl in c("com","sem")) {
  vs <- c(BLOCO0_DIRETA, BLOCO1, BLOCO2, BLOCO3, BLOCO4,
          if (ctrl == "com") CONTROLES else setdiff(CONTROLES, "soma_peso_medio"))
  cols <- c(dep, vs, "gestora_grupo")
  S <- CONF[complete.cases(CONF[, ..cols])]
  fit <- lm(as.formula(paste(dep, "~", paste(vs, collapse = " + "))), data = S)
  fb <- f_bloco(fit, BLOCO3, S$gestora_grupo)
  celulas[[length(celulas)+1L]] <- data.table(dep = dep, controle_soma_peso = ctrl,
                                              k = fb$k, F = fb$F, p = fb$p, n = nrow(S))
}
tab22 <- rbindlist(celulas)
tab22[, sobrevive := p < 0.05]
print(tab22[, .(dep, controle_soma_peso, k, F = round(F,2), p = signif(p,4), sobrevive)])
veredito <- if (all(tab22$sobrevive)) "MANDATO (sobrevive nas 4 celulas)" else
            if (tab22[dep=="D1" & controle_soma_peso=="sem", sobrevive] &&
                !tab22[dep=="D2" & controle_soma_peso=="com", sobrevive])
              "ARITMETICA DE SLEEVE" else "INCONCLUSIVO"
cat(sprintf("\n>>> VEREDITO H3: %s\n", veredito))
cat(sprintf(">>> A hipotese nula declarada no pre-registro era: mandato NAO acrescenta. %s\n",
            ifelse(all(tab22$sobrevive), "REJEITADA.", "nao rejeitada.")))
fwrite(tab22, file.path(DD, "mandato_tabela_2x2.csv"))

cat("\n=============================================================\n")
cat("AJUSTE\n")
cat("=============================================================\n")
print(ajuste[param == "direta", .(amostra, spec, dep, n, n_clusters, k,
                                  r2 = round(r2,4), r2_aj = round(r2_aj,4))])
cat("\nOK - reg_fundo_conf_*.csv\n")
