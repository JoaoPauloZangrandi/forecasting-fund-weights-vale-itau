# =============================================================================
# 134_replicacao_h3_e_anual.R  (v2 OFICIAL)
#
# Duas replicacoes independentes da especificacao confirmada, previstas no
# pre-registro (Secao 8, item 3):
#   (a) HORIZONTE: h=3 em vez de h=1. Correlacionado, mas nao identico.
#   (b) TEMPO: sigma_i recomputado so em 2020 e so em 2021. Coeficiente que
#       troca de sinal entre os dois anos nao e' achado.
#
# As caracteristicas NAO mudam (vem do treino). So a dependente muda.
#
# h=6 e h=12 nao existem em nivel de celula -- o script 99 calcula em memoria
# e so grava h1 e h3 (linhas 142-144). Ausencia declarada como limitacao.
# =============================================================================
suppressPackageStartupMessages({ library(data.table) })

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
PISO_POEIRA <- 0.001
set.seed(20260915L)
B_WCB <- 1999L

source(file.path(REPO, "v2 OFICIAL/scripts/_funcoes_extensao.R"))

cat("=============================================================\n")
cat("134 - Replicacoes: h=3 e por ano\n")
cat("=============================================================\n\n")

pp <- fread(file.path(DD, "painel_multiativo_final.csv"),
            select = c("cod_fundo","ym","ativo","peso"), showProgress = FALSE)
pp[, cod_fundo := as.character(cod_fundo)]
cel_ok <- pp[, .(pm = median(peso)), by = .(cod_fundo, ativo)][pm >= PISO_POEIRA, .(cod_fundo, ativo)]
sleeve <- pp[, .(sleeve = sum(peso)), by = .(cod_fundo, ym)]
rm(pp); invisible(gc())

X  <- fread(file.path(DD, "caracteristicas_fundo_treino.csv"))
X[, cod_fundo := as.character(cod_fundo)]
SP <- fread(file.path(DD, "split_fundos_dev_conf.csv")); SP[, cod_fundo := as.character(cod_fundo)]
manter <- setdiff(names(X), c("D1","D2","rmse_peso","rmse_sleeve","n_obs_teste",
                              "n_mes_teste","l_n_obs_teste"))

dep_de <- function(arq, filtro_ym = NULL, rot) {
  H <- fread(file.path(DD, arq), select = c("cod_fundo","ativo","ym","gestora_grupo","erro_oos"),
             showProgress = FALSE)
  H[, cod_fundo := as.character(cod_fundo)]
  H <- merge(H, cel_ok, by = c("cod_fundo","ativo"))
  if (!is.null(filtro_ym)) H <- H[ym %in% filtro_ym]
  H <- merge(H, sleeve, by = c("cod_fundo","ym"))
  H <- H[is.finite(sleeve) & sleeve > 1e-6]
  D <- H[, .(rmse_peso = sqrt(mean(erro_oos^2)),
             rmse_sleeve = sqrt(mean((erro_oos/sleeve)^2)),
             n_obs_teste = .N, n_mes_teste = uniqueN(ym)),
         by = .(cod_fundo, gestora_grupo)]
  D <- D[n_obs_teste >= 24 & n_mes_teste >= 3 & rmse_peso > 0 & rmse_sleeve > 0]
  D[, `:=`(D1 = log(rmse_peso), D2 = log(rmse_sleeve), l_n_obs_teste = log(n_obs_teste))]
  cat(sprintf("%-22s: %d fundos\n", rot, nrow(D)))
  D
}

roda <- function(D, rot) {
  B <- merge(D, X[, ..manter], by = c("cod_fundo","gestora_grupo"))
  B <- merge(B, SP[, .(cod_fundo, grupo)], by = "cod_fundo")
  CONF <- prepara_dummies(B[grupo == "conf"])
  if (nrow(CONF) < 100) { cat("  amostra pequena demais, pulando\n"); return(NULL) }
  cat(sprintf("\n### %s: %d fundos, %d gestoras ###\n", rot, nrow(CONF), uniqueN(CONF$gestora_grupo)))
  roda_bateria(CONF, rotulo_amostra = rot, B = B_WCB, verbose = TRUE)
}

R <- list()
R[["h3"]]    <- roda(dep_de("etapa3_multiativo_h3.csv", NULL, "h=3"), "rep_h3")
anos2020 <- 202001:202012; anos2021 <- 202101:202112
R[["a2020"]] <- roda(dep_de("etapa3_multiativo_h1.csv", anos2020, "h=1, so 2020"), "rep_2020")
R[["a2021"]] <- roda(dep_de("etapa3_multiativo_h1.csv", anos2021, "h=1, so 2021"), "rep_2021")

R <- Filter(Negate(is.null), R)
coef   <- rbindlist(lapply(R, `[[`, "coef"), fill = TRUE)
blocos <- rbindlist(lapply(R, `[[`, "blocos"), fill = TRUE)
ajuste <- rbindlist(lapply(R, `[[`, "ajuste"), fill = TRUE)

fwrite(coef,   file.path(DD, "replicacao_h3_anual_coef.csv"))
fwrite(blocos, file.path(DD, "replicacao_h3_anual.csv"))
grava_ledger(rbindlist(lapply(R, `[[`, "ledger")), DD)

cat("\n=============================================================\n")
cat("TESTES F DE BLOCO NAS REPLICACOES (A1, parametrizacao direta)\n")
cat("=============================================================\n")
pr <- blocos[param == "direta" & spec == "A1"]
print(dcast(pr, bloco + dep ~ amostra, value.var = "p")[order(bloco, dep)])

cat("\n--- Estabilidade de sinal entre 2020 e 2021 (coeficientes A1/D1) ---\n")
cf <- coef[spec == "A1" & param == "direta" & dep == "D1" &
             amostra %in% c("rep_2020","rep_2021"),
           .(amostra, variavel, coef)]
w <- dcast(cf, variavel ~ amostra, value.var = "coef")
if (all(c("rep_2020","rep_2021") %in% names(w))) {
  w[, troca_sinal := sign(rep_2020) != sign(rep_2021)]
  print(w[, .(variavel, r2020 = round(rep_2020,4), r2021 = round(rep_2021,4), troca_sinal)])
  cat(sprintf("\nVariaveis que TROCAM de sinal entre 2020 e 2021: %d de %d\n",
              w[troca_sinal == TRUE, .N], nrow(w)))
  cat("(essas nao podem ser reportadas como achado)\n")
}
cat("\nOK - replicacao_h3_anual.csv, replicacao_h3_anual_coef.csv\n")
