# =============================================================================
# 22_holding_horizon.R
#
# Previsao da POSSE de VALE3 (margem extensiva, fundos de Acoes) nos horizontes
# h=1 e h=3 (opacidade de 90 dias). Alvo: tem_{i,t+h} (0/1). Compara:
#   Ingenuo : tem_{i,t}  (repete o estado conhecido)
#   Logit   : logit de tem_{s+h} ~ caracteristicas_s (treino), aplicado a x_t
# OOS, janela expansiva, info ate t. RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
U <- fread(file.path(REPO,"data/processed/painel_extensivo_acoes.csv"))
U[, l_aum:=log(aum)][, l_cot:=log(as.numeric(n_cotistas))]
U <- U[is.finite(l_aum) & is.finite(l_cot)]
U[, monidx := frank(ym, ties.method="dense")]
setorder(U, cod_fundo, monidx)

holding_h <- function(h) {
  tg <- U[, .(cod_fundo, mi=monidx-h, tem_tgt=tem)]
  base <- merge(U, tg, by.x=c("cod_fundo","monidx"), by.y=c("cod_fundo","mi"))  # tem em t+h
  A <- list()
  for (t in (h+6):(max(U$monidx)-h)) {
    trbase <- base[monidx <= t-h]          # pares (s, s+h) ja conhecidos em t
    te <- base[monidx == t]; if(!nrow(te) || nrow(trbase)<50) next
    fit <- glm(tem_tgt ~ l_aum + l_cot + is_fic, data=trbase, family=binomial)
    te[, p_logit := as.integer(predict(fit, te, type="response") > 0.5)]
    A[[length(A)+1L]] <- te[, .(tem_tgt, tem_now=tem,
                                ac_naive=as.integer(tem_tgt==tem),
                                ac_logit=as.integer(tem_tgt==p_logit),
                                muda=as.integer(tem_tgt!=tem))]
  }
  A <- rbindlist(A)
  data.table(h=h, n=nrow(A), taxa_mudanca=round(100*mean(A$muda),1),
             acc_ingenuo=round(100*mean(A$ac_naive),1), acc_logit=round(100*mean(A$ac_logit),1))
}

res <- rbindlist(lapply(c(1,3), holding_h))
cat("==== PREVISAO DA POSSE de VALE3 (fundos de Acoes) por horizonte ====\n")
print(res)
cat("\n(taxa_mudanca = % de fundos que entram OU saem entre t e t+h)\n")
fwrite(res, file.path(REPO,"data/processed/reg22_holding_horizon.csv"))
