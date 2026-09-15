# =============================================================================
# 129_variaveis_novas_fundo.R  (v2 OFICIAL)
#
# Monta a matriz de caracteristicas por FUNDO, nas duas amostras (S1 treino
# cheio, S2 janela movel) e com as duas dependentes (D1 peso, D2 sleeve).
#
# DISCIPLINA: agrega SEMPRE em duas etapas, fundo-mes -> fundo. O script 95
# faz mean() direto sobre linhas fundo x ativo x mes, o que pondera cada
# fundo pelo numero de ativos que ele carrega. Aqui isso e' corrigido, e o
# delta contra o numero do TCC e' reportado.
#
# PARADA 4: grava correlacoes e VIF ANTES de qualquer regressao de interesse.
#
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(car) })

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
CORTE <- 202001L
PISO_POEIRA <- 0.001

cat("=============================================================\n")
cat("129 - Caracteristicas por fundo (Parada 4)\n")
cat("=============================================================\n\n")

pp <- fread(file.path(DD, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ym","ativo","peso","gestora_grupo",
                       "is_fic","beta_fundo","l_aum","l_cot","flow_aum"),
            showProgress = FALSE)
pp[, cod_fundo := as.character(cod_fundo)]

cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
cel_ok <- cel[peso_mediano >= PISO_POEIRA, .(cod_fundo, ativo)]
pp[, ok_poeira := FALSE]
pp[cel_ok, on = .(cod_fundo, ativo), ok_poeira := TRUE]

# --- nivel fundo-mes ---------------------------------------------------------
FM <- pp[, .(hhi        = sum(peso^2),
             sleeve     = sum(peso),
             n_bruto    = .N,
             n_filt     = sum(ok_poeira),
             l_aum      = l_aum[1],
             l_cot      = l_cot[1],
             beta_fundo = beta_fundo[1],
             flow_aum   = flow_aum[1],
             is_fic     = is_fic[1],
             gestora_grupo = gestora_grupo[1]),
         by = .(cod_fundo, ym)]
cat("Fundo-mes:", nrow(FM), "|", uniqueN(FM$cod_fundo), "fundos\n")
FM <- FM[sleeve > 1e-8 & hhi > 0]
setorder(FM, cod_fundo, ym)

# --- dependentes -------------------------------------------------------------
h1 <- fread(file.path(DD, "etapa3_multiativo_h1.csv"),
            select = c("cod_fundo","ativo","ym","gestora_grupo","erro_oos"),
            showProgress = FALSE)
h1[, cod_fundo := as.character(cod_fundo)]
h1f <- merge(h1, cel_ok, by = c("cod_fundo","ativo"))
rm(h1); invisible(gc())
h1f <- merge(h1f, FM[, .(cod_fundo, ym, sleeve)], by = c("cod_fundo","ym"), all.x = TRUE)
h1f <- h1f[is.finite(sleeve) & sleeve > 1e-6]
h1f[, erro_sleeve := erro_oos / sleeve]

DEP <- h1f[, .(rmse_peso   = sqrt(mean(erro_oos^2)),
               rmse_sleeve = sqrt(mean(erro_sleeve^2)),
               n_obs_teste = .N,
               n_mes_teste = uniqueN(ym)),
           by = .(cod_fundo, gestora_grupo)]
DEP <- DEP[n_obs_teste >= 24 & n_mes_teste >= 6 & rmse_peso > 0 & rmse_sleeve > 0]
DEP[, `:=`(D1 = log(rmse_peso), D2 = log(rmse_sleeve),
           l_n_obs_teste = log(n_obs_teste))]
cat("Fundos com dependente:", nrow(DEP), "\n\n")

# --- construtor de caracteristicas -------------------------------------------
monta <- function(FMsub, rotulo) {
  X <- FMsub[, .(
    beta_fundo   = mean(beta_fundo, na.rm=TRUE),
    l_aum        = mean(l_aum, na.rm=TRUE),
    l_cot        = mean(l_cot, na.rm=TRUE),
    pct_fic      = mean(is_fic, na.rm=TRUE),
    flow_aum     = mean(flow_aum, na.rm=TRUE),
    hhi_medio    = mean(hhi, na.rm=TRUE),
    sd_flow_aum  = sd(flow_aum, na.rm=TRUE),
    sd_log_hhi   = sd(log(hhi), na.rm=TRUE),
    amp_beta     = diff(range(beta_fundo, na.rm=TRUE)),
    sd_d_l_aum   = sd(diff(l_aum)),
    l_n_ativos   = log(mean(n_filt) + 1),
    razao_poeira = mean(n_filt) / mean(n_bruto),
    soma_peso_medio = mean(sleeve, na.rm=TRUE),
    n_meses_treino  = .N
  ), by = cod_fundo]
  X[, amostra := rotulo]
  X[]
}

# S1: treino cheio (>= 12 meses)
FM_tr <- FM[ym < CORTE]
n_mes_tr <- FM_tr[, .N, by = cod_fundo]
S1 <- monta(FM_tr[cod_fundo %in% n_mes_tr[N >= 12, cod_fundo]], "S1")

# S2 (emenda E1): ultimos min(12) meses ANTES de 2020-01, exigindo >= 6
setorder(FM_tr, cod_fundo, -ym)
FM_tr[, rk := seq_len(.N), by = cod_fundo]
S2 <- monta(FM_tr[rk <= 12][cod_fundo %in% n_mes_tr[N >= 6, cod_fundo]], "S2")
cat(sprintf("S1 (treino cheio, >=12m): %d fundos\n", nrow(S1)))
cat(sprintf("S2 (ultimos 12m, >=6m)  : %d fundos\n", nrow(S2)))
cat(sprintf("Ganho de S2 sobre S1    : %d fundos\n\n", nrow(S2) - nrow(S1)))

# --- mandato Anbima ----------------------------------------------------------
an <- fread(file.path(DD, "_cache_classif_anbima.csv"), showProgress = FALSE)
setnames(an, tolower(names(an))); an[, cod_fundo := as.character(cod_fundo)]
cls <- an[ym < CORTE, .N, by = .(cod_fundo, classif_anbima)][order(cod_fundo, -N)]
cls <- cls[, .SD[1], by = cod_fundo][, .(cod_fundo, classe = classif_anbima)]
grupo_de <- function(k) fcase(
  k == "Multimercados Livre", "G1_multimercado",
  k == "Ações Livre",         "G2_base",
  k %in% c("Ações Índice Ativo","Ações Indexados","Fundos de Índices - ETF"), "G3_indexado",
  k %in% c("Ações Valor/Crescimento","Ações Dividendos","Ações Sustentabilidade/Governança"), "G4_estilo",
  k %in% c("Ações Small Caps","Ações Setoriais"), "G5_nicho",
  default = "G0_sem_classe")
cls[, grupo_mandato := grupo_de(classe)]

# --- turnover ----------------------------------------------------------------
TF <- fread(file.path(DD, "turnover_fundo_treino.csv"))
TF[, cod_fundo := as.character(cod_fundo)]

# --- monta as bases finais ---------------------------------------------------
finaliza <- function(X) {
  Y <- merge(DEP, X, by = "cod_fundo")
  Y <- merge(Y, cls[, .(cod_fundo, grupo_mandato)], by = "cod_fundo", all.x = TRUE)
  Y[is.na(grupo_mandato), grupo_mandato := "G0_sem_classe"]
  Y <- merge(Y, TF[, .(cod_fundo, turn_mediano, dsleeve_mediano, fonte_b3_pesada)],
             by = "cod_fundo", all.x = TRUE)
  # forma_hhi: concentracao DADO o numero de posicoes (ortogonaliza breadth x HHI)
  Y <- Y[is.finite(hhi_medio) & hhi_medio > 0 & is.finite(l_n_ativos)]
  Y[, neg_log_hhi := -log(hhi_medio)]
  Y[, forma_hhi := residuals(lm(neg_log_hhi ~ l_n_ativos, data = Y))]
  # E2: ortogonalizacoes fixadas no pre-registro apos a Parada 4
  Yc <- Y[is.finite(sd_flow_aum) & is.finite(flow_aum) &
          is.finite(beta_fundo)  & is.finite(soma_peso_medio)]
  Y[, sd_flow_resid := NA_real_][, beta_resid := NA_real_]
  Y[Yc[, .(cod_fundo)], on = "cod_fundo",
    sd_flow_resid := residuals(lm(sd_flow_aum ~ flow_aum, data = Yc))]
  Y[Yc[, .(cod_fundo)], on = "cod_fundo",
    beta_resid := residuals(lm(beta_fundo ~ soma_peso_medio, data = Yc))]
  Y[]
}
B1 <- finaliza(S1); B2 <- finaliza(S2)
cat(sprintf("Base S1 completa: %d fundos, %d gestoras\n", nrow(B1), uniqueN(B1$gestora_grupo)))
cat(sprintf("Base S2 completa: %d fundos, %d gestoras\n", nrow(B2), uniqueN(B2$gestora_grupo)))
cat(sprintf("Com turnover em S1: %d (%.1f%%)\n\n",
            B1[is.finite(turn_mediano), .N], 100*B1[is.finite(turn_mediano), .N]/nrow(B1)))

cat("--- Distribuicao de mandato em S1 ---\n")
print(B1[, .N, by = grupo_mandato][order(-N)])

# --- delta contra o script 95 ------------------------------------------------
cat("\n--- Delta da agregacao: script 95 (por linha) vs. duas etapas ---\n")
g95 <- pp[ym < CORTE, .(hhi_95 = mean(sum(peso^2))), by = .(cod_fundo, ym, gestora_grupo)][
          , .(hhi_95 = mean(hhi_95)), by = gestora_grupo]
g_novo <- B1[, .(hhi_novo = mean(hhi_medio)), by = gestora_grupo]
cmp <- merge(g95, g_novo, by = "gestora_grupo")
cat(sprintf("cor(hhi por gestora, duas construcoes) = %.4f | dif media = %.5f\n",
            cor(cmp$hhi_95, cmp$hhi_novo), mean(cmp$hhi_novo - cmp$hhi_95)))

# --- PARADA 4: correlacoes e VIF ---------------------------------------------
cat("\n--- PARADA 4: colinearidade, ANTES de qualquer regressao ---\n")
vars <- c("beta_fundo","l_aum","l_cot","pct_fic","flow_aum","hhi_medio",
          "sd_flow_aum","sd_log_hhi","amp_beta","sd_d_l_aum",
          "turn_mediano","dsleeve_mediano","l_n_ativos","forma_hhi","razao_poeira",
          "soma_peso_medio","n_meses_treino","fonte_b3_pesada","l_n_obs_teste")
Bc <- B1[complete.cases(B1[, ..vars])]
cat(sprintf("Fundos com todas as %d variaveis: %d\n", length(vars), nrow(Bc)))
CM <- cor(Bc[, ..vars])
altas <- as.data.table(which(abs(CM) > 0.7 & abs(CM) < 0.999, arr.ind = TRUE))
if (nrow(altas)) {
  altas[, `:=`(v1 = vars[row], v2 = vars[col], r = CM[cbind(row, col)])]
  altas <- altas[row < col][order(-abs(r))]
  cat("\nPares com |r| > 0,7:\n"); print(altas[, .(v1, v2, r = round(r, 3))])
} else cat("\nNenhum par com |r| > 0,7\n")

fit_vif <- lm(as.formula(paste("D1 ~", paste(vars, collapse = " + "))), data = Bc)
v <- vif(fit_vif)
cat("\nVIF (ordenado):\n"); print(round(sort(v, decreasing = TRUE), 2))
cat(sprintf("\nVariaveis com VIF > 10: %s\n",
            ifelse(any(v > 10), paste(names(v)[v > 10], collapse = ", "), "nenhuma")))

# regras do pre-registro
r_disp <- cor(Bc$sd_flow_aum, Bc$sd_d_l_aum)
cat(sprintf("\nRegra dispersoes: cor(sd_flow_aum, sd_d_l_aum) = %.3f -> %s\n", r_disp,
            ifelse(abs(r_disp) > 0.8, "sd_d_l_aum vai para robustez", "as duas podem entrar")))
r_bre <- cor(Bc$l_n_ativos, Bc$hhi_medio)
cat(sprintf("Regra breadth: cor(l_n_ativos, hhi_medio) = %.3f | VIF hhi = %.2f -> %s\n",
            r_bre, v["hhi_medio"],
            ifelse(v["hhi_medio"] > 10, "usar l_n_ativos + forma_hhi", "parametrizacao direta ok")))

fwrite(B1, file.path(DD, "caracteristicas_fundo_treino.csv"))
fwrite(B2, file.path(DD, "caracteristicas_fundo_treino_S2.csv"))
fwrite(data.table(v1 = rep(vars, each=length(vars)), v2 = rep(vars, length(vars)),
                  r = as.vector(CM)), file.path(DD, "colinearidade_diag.csv"))
fwrite(data.table(variavel = names(v), vif = as.numeric(v)),
       file.path(DD, "colinearidade_vif.csv"))
cat("\nOK - caracteristicas_fundo_treino.csv (+S2), colinearidade_diag.csv, colinearidade_vif.csv\n")
