# =============================================================================
# 128_construcao_turnover.R  (v2 OFICIAL)
#
# Constroi rotatividade de carteira com DERIVA PASSIVA DESCONTADA.
# Nao existe nada equivalente no repositorio. O script 120 chegou perto,
# mas tem dois defeitos que este script corrige:
#
#   (1) 120 usa G = (1 + retorno_fundo), ignorando o fluxo. Um fundo que
#       captou +10% tem todos os pesos comprimidos ~10%, e o 120 le isso
#       como venda generalizada.
#   (2) 120 faz merge INTERNO entre t-1 e t, entao posicao zerada por
#       completo -- o trade mais ativo que existe -- e' descartada. Isso
#       enviesa turnover para baixo justamente nos gestores mais ativos.
#
# Medida primaria: CHURN DENTRO DA SLEEVE, auto-contido.
#   s_{i,n,t}      = peso_{i,n,t} / soma_m peso_{i,m,t}
#   s_deriva       = s_{t-1}(1+r_n) / soma_m s_{m,t-1}(1+r_m)
#   TURN_{i,t}     = 0.5 * soma_n |s_{n,t} - s_deriva_{n,t}|   em [0,1]
# Como o denominador e' a propria carteira de acoes, fluxo, taxa, retorno
# de cota e tamanho da sleeve CANCELAM. Nao herda o defeito (1).
#
# Medida secundaria, conceitualmente distinta: DSLEEVE, a entrada/saida
# liquida de acoes contra caixa, que TURN por construcao ignora.
#   G_{i,t}        = PL_t / PL_{t-1}, recuperado de mediana(valor_mil/peso)
#   peso_deriva    = peso_{t-1}(1+r_n)/G
#   DSLEEVE_{i,t}  = |soma_n peso_{n,t} - soma_n peso_deriva_{n,t}|
#
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table) })

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
WINSOR_RET <- 0.80
MIN_ATIVOS <- 3L

addm <- function(ym, h) {
  a <- ym %/% 100L; m <- ym %% 100L
  tot <- a * 12L + (m - 1L) + h
  (tot %/% 12L) * 100L + (tot %% 12L) + 1L
}

out <- list()
add <- function(bloco, metrica, valor) {
  out[[length(out)+1L]] <<- data.table(bloco=bloco, metrica=metrica, valor=as.character(valor))
}

cat("=============================================================\n")
cat("128 - Turnover com deriva passiva descontada\n")
cat("=============================================================\n\n")

# --- painel ------------------------------------------------------------------
pp <- fread(file.path(DD, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ym","ativo","peso","valor_mil","gestora_grupo"),
            showProgress = FALSE)
pp[, cod_fundo := as.character(cod_fundo)]
pp[, ticker := trimws(sub(".*- ", "", ativo))]
cat("Painel:", nrow(pp), "linhas |", uniqueN(pp$cod_fundo), "fundos\n")

# --- PL implicito e consistencia ---------------------------------------------
cat("\n--- PL implicito: mediana(valor_mil/peso) dentro de fundo-mes ---\n")
pp[, razao := fifelse(peso > 1e-8, valor_mil / peso, NA_real_)]
pl <- pp[!is.na(razao), .(pl_mil = median(razao, na.rm = TRUE),
                          cv_razao = sd(razao, na.rm=TRUE)/mean(razao, na.rm=TRUE),
                          soma_peso = sum(peso), n_ativos = .N),
         by = .(cod_fundo, ym)]
cat(sprintf("Fundo-mes com PL implicito: %d\n", nrow(pl)))
cat(sprintf("CV da razao valor_mil/peso dentro do fundo-mes -- mediana: %.5f | p90: %.5f\n",
            median(pl$cv_razao, na.rm=TRUE), quantile(pl$cv_razao, .90, na.rm=TRUE)))
cat("  (CV proximo de zero confirma que valor_mil/peso e' constante dentro do fundo-mes,\n")
cat("   ou seja, que o PL implicito esta' bem identificado)\n")
add("PL", "cv_razao_mediana", round(median(pl$cv_razao, na.rm=TRUE), 6))
add("PL", "cv_razao_p90", round(quantile(pl$cv_razao, .90, na.rm=TRUE), 6))

# --- retornos ----------------------------------------------------------------
pr <- fread(file.path(DD, "precos_mensais_final.csv"),
            select = c("ticker","ymk","retorno","fonte"), showProgress = FALSE)
setnames(pr, "ymk", "ym")
pr <- pr[is.finite(retorno)]
pr[, r_w := pmax(pmin(retorno, WINSOR_RET), -WINSOR_RET)]
n_wins <- pr[abs(retorno) > WINSOR_RET, .N]
cat(sprintf("\nRetornos: %d linhas | winsorizados em +-%.0f%%: %d (%.3f%%)\n",
            nrow(pr), 100*WINSOR_RET, n_wins, 100*n_wins/nrow(pr)))
add("retorno", "n_winsorizados", n_wins)

# fracao do peso em ticker sem ajuste por provento (fonte b3_bruto)
fonte_tk <- pr[, .(pct_b3 = mean(fonte == "b3_bruto")), by = ticker]

# --- frame balanceado por UNIAO ----------------------------------------------
cat("\n--- Frame balanceado por UNIAO t-1 / t (corrige o defeito 2 do script 120) ---\n")
A <- pp[, .(cod_fundo, ym, ticker, peso, valor_mil)]
A <- A[, .(peso = sum(peso), valor_mil = sum(valor_mil)), by = .(cod_fundo, ym, ticker)]
A[, sleeve := sum(peso), by = .(cod_fundo, ym)]
A <- A[sleeve > 1e-8]
A[, s := peso / sleeve]

ANT <- copy(A)[, ym := addm(ym, 1L)]
setnames(ANT, c("peso","s","sleeve","valor_mil"),
              c("peso_ant","s_ant","sleeve_ant","valor_mil_ant"))

M <- merge(A, ANT, by = c("cod_fundo","ym","ticker"), all = TRUE)   # UNIAO
M[is.na(peso),     `:=`(peso = 0,     s = 0)]
M[is.na(peso_ant), `:=`(peso_ant = 0, s_ant = 0)]
M[, sleeve     := max(sleeve,     na.rm = TRUE), by = .(cod_fundo, ym)]
M[, sleeve_ant := max(sleeve_ant, na.rm = TRUE), by = .(cod_fundo, ym)]
M <- M[is.finite(sleeve) & is.finite(sleeve_ant)]
cat("Linhas no frame de uniao:", nrow(M), "\n")

n_saida_total <- M[s_ant > 0 & s == 0, .N]
n_entrada     <- M[s_ant == 0 & s > 0, .N]
cat(sprintf("Saidas totais de posicao (descartadas pelo script 120): %d\n", n_saida_total))
cat(sprintf("Entradas de posicao: %d\n", n_entrada))
add("uniao", "n_saidas_totais_recuperadas", n_saida_total)
add("uniao", "n_entradas", n_entrada)

# --- deriva passiva ----------------------------------------------------------
M <- merge(M, pr[, .(ticker, ym, r_w)], by = c("ticker","ym"), all.x = TRUE)
M[is.na(r_w), r_w := 0]      # 12 tickers sem preco = 0,022% do peso (script 125)
M[, num := s_ant * (1 + r_w)]
M[, den := sum(num), by = .(cod_fundo, ym)]
M <- M[den > 1e-12]
M[, s_deriva := num / den]

TM <- M[, .(turn      = 0.5 * sum(abs(s - s_deriva)),
            n_ativos  = sum(s > 0 | s_ant > 0),
            sleeve    = sleeve[1],
            sleeve_ant= sleeve_ant[1],
            pl_der    = sum(peso_ant * (1 + r_w))),
        by = .(cod_fundo, ym)]
TM <- TM[n_ativos >= MIN_ATIVOS]

# DSLEEVE: usa G = PL_t/PL_{t-1}
pl_ant <- copy(pl)[, ym := addm(ym, 1L)][, .(cod_fundo, ym, pl_mil_ant = pl_mil)]
TM <- merge(TM, pl[, .(cod_fundo, ym, pl_mil)], by = c("cod_fundo","ym"), all.x = TRUE)
TM <- merge(TM, pl_ant, by = c("cod_fundo","ym"), all.x = TRUE)
TM[, G := fifelse(is.finite(pl_mil_ant) & pl_mil_ant > 0, pl_mil / pl_mil_ant, NA_real_)]
TM[, dsleeve := abs(sleeve - pl_der / G)]

cat("\n--- Distribuicao de TURN ---\n")
print(round(quantile(TM$turn, c(0,.05,.25,.5,.75,.95,1), na.rm=TRUE), 4))
cat(sprintf("Fora de [0,1]: %d  (tem que ser 0)\n", TM[turn < -1e-9 | turn > 1+1e-9, .N]))
cat(sprintf("Fundo-mes com TURN: %d | com DSLEEVE: %d\n", nrow(TM), TM[is.finite(dsleeve), .N]))
add("TURN", "mediana_fundo_mes", round(median(TM$turn, na.rm=TRUE), 4))
add("TURN", "fora_do_intervalo", TM[turn < -1e-9 | turn > 1+1e-9, .N])

# --- agrega para fundo, no TREINO --------------------------------------------
TR <- TM[ym < 202001L]
TF <- TR[, .(turn_mediano    = median(turn, na.rm=TRUE),
             turn_medio      = mean(turn, na.rm=TRUE),
             turn_sd         = sd(turn, na.rm=TRUE),
             dsleeve_mediano = median(dsleeve, na.rm=TRUE),
             n_meses_turn    = .N), by = cod_fundo]
TF <- TF[n_meses_turn >= 6]
cat(sprintf("\nFundos com turnover no treino (>=6 meses): %d\n", nrow(TF)))
print(round(quantile(TF$turn_mediano, c(0,.1,.25,.5,.75,.9,1), na.rm=TRUE), 4))
add("TURN", "n_fundos_treino", nrow(TF))

# --- PARADA 3: TURN e' distinto do HHI? --------------------------------------
cat("\n--- PARADA 3: TURN e' redundante com a concentracao? ---\n")
hhi <- pp[ym < 202001L, .(hhi = sum(peso^2)), by = .(cod_fundo, ym)][
          , .(hhi_medio = mean(hhi)), by = cod_fundo]
Z <- merge(TF, hhi, by = "cod_fundo")
r_turn_hhi <- cor(Z$turn_mediano, Z$hhi_medio, use = "complete.obs")
r_sp       <- cor(Z$turn_mediano, Z$hhi_medio, method = "spearman", use = "complete.obs")
cat(sprintf("cor(turn_mediano, hhi_medio) = %.4f (Pearson) | %.4f (Spearman), n = %d\n",
            r_turn_hhi, r_sp, nrow(Z)))
cat(sprintf("Regra do pre-registro: redundante se |r| > 0,8  ->  %s\n",
            ifelse(abs(r_turn_hhi) > 0.8, "REDUNDANTE", "DISTINTO, o bloco segue")))
add("PARADA3", "cor_turn_hhi", round(r_turn_hhi, 4))
add("PARADA3", "veredito", ifelse(abs(r_turn_hhi) > 0.8, "redundante", "distinto"))

# fracao do peso em ticker sem ajuste por provento, por fundo
pw <- pp[ym < 202001L]
pw <- merge(pw, fonte_tk, by = "ticker", all.x = TRUE)
pw[is.na(pct_b3), pct_b3 := 0]
b3f <- pw[, .(fonte_b3_pesada = sum(peso * pct_b3) / sum(peso)), by = cod_fundo]
TF <- merge(TF, b3f, by = "cod_fundo", all.x = TRUE)
cat(sprintf("\nfonte_b3_pesada -- mediana: %.3f | p90: %.3f (fracao do peso sem ajuste por provento)\n",
            median(TF$fonte_b3_pesada, na.rm=TRUE), quantile(TF$fonte_b3_pesada, .9, na.rm=TRUE)))

fwrite(TM, file.path(DD, "turnover_fundo_mes.csv"))
fwrite(TF, file.path(DD, "turnover_fundo_treino.csv"))
fwrite(rbindlist(out), file.path(DD, "turnover_diagnostico.csv"))
cat("\nOK - turnover_fundo_mes.csv, turnover_fundo_treino.csv, turnover_diagnostico.csv\n")
