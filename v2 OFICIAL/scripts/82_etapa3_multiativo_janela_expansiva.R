# =============================================================================
# 82_etapa3_multiativo_janela_expansiva.R  (v2 OFICIAL)
#
# Mesma ideia do script 81 (lambda fixo vs. janela expansiva), mas na base
# multiativo completa (script 42: erro_e_multiativo.csv, todas as
# gestoras, todos os ativos, h=1) -- essa e a base que alimenta a maior
# parte da secao "Erros -- dinamica fora da amostra" do TCC_final.tex.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_e_multiativo.csv"))
E[, d := peso_pred - peso]
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

h <- 1L
fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
atual <- E[, .(cod_fundo, gestora_grupo, ativo, ym, d, peso, ym_fut = addm(ym, h))]
M <- merge(atual, fut, by.x = c("cod_fundo","ativo","ym_fut"), by.y = c("cod_fundo","ativo","ym_fut_key"))
M[, dw := peso_fut - peso]
cat("Base h=1 (multiativo):", nrow(M), "obs\n")

CORTE <- 202001L
meses_teste <- sort(unique(M[ym >= CORTE & ym_fut <= 202112L]$ym))
cat("Meses de teste:", length(meses_teste), "(", min(meses_teste), "a", max(meses_teste), ")\n")

rmse <- function(x) sqrt(mean(x^2))
mae  <- function(x) mean(abs(x))

t0 <- Sys.time()
resultado <- list(); lambdas <- list()
for (s in meses_teste) {
  treino_s <- M[ym < s]
  fit_s <- lm(dw ~ 0 + d, data = treino_s)
  lam_s <- unname(coef(fit_s)["d"])
  lambdas[[as.character(s)]] <- data.table(ym = s, n_treino = nrow(treino_s), lambda = lam_s)

  alvo_s <- M[ym == s]
  alvo_s[, dw_prev_expansiva := lam_s * d]
  alvo_s[, erro_expansiva := dw - dw_prev_expansiva]
  resultado[[as.character(s)]] <- alvo_s
  cat("mes", s, "processado (", round(as.numeric(Sys.time()-t0, units="secs"),1), "s )\n")
}
R <- rbindlist(resultado)
LAM <- rbindlist(lambdas)

cat("\n===== Lambda mes a mes (janela expansiva, multiativo h=1) =====\n")
print(LAM)

treino_fixo <- M[ym < CORTE]
fit_fixo <- lm(dw ~ 0 + d, data = treino_fixo)
lam_fixo <- unname(coef(fit_fixo)["d"])
cat(sprintf("\nLambda fixo (treino < 2020-01, script 42) = %.4f\n", lam_fixo))
cat(sprintf("Lambda expansiva: min = %.4f | max = %.4f | ultimo mes = %.4f\n",
            min(LAM$lambda), max(LAM$lambda), LAM[ym == max(ym)]$lambda))

R[, dw_prev_fixo := lam_fixo * d]
R[, erro_fixo := dw - dw_prev_fixo]
R[, dw_prev_naive := 0]
R[, erro_naive := dw - dw_prev_naive]

cat("\n===== Desempenho FORA da amostra (2020-2021, multiativo h=1) =====\n")
cat(sprintf("RMSE lambda fixo       = %.6f\n", rmse(R$erro_fixo)))
cat(sprintf("RMSE janela expansiva  = %.6f\n", rmse(R$erro_expansiva)))
cat(sprintf("RMSE ingenua           = %.6f\n", rmse(R$erro_naive)))
cat(sprintf("\nRazao RMSE fixo/ingenua      = %.4f\n", rmse(R$erro_fixo)/rmse(R$erro_naive)))
cat(sprintf("Razao RMSE expansiva/ingenua = %.4f\n", rmse(R$erro_expansiva)/rmse(R$erro_naive)))

por_mes <- R[, .(rmse_fixo = rmse(erro_fixo), rmse_expansiva = rmse(erro_expansiva),
                  rmse_naive = rmse(erro_naive)), by = ym]
setorder(por_mes, ym)
por_mes[, vence_fixo := rmse_fixo < rmse_naive]
por_mes[, vence_expansiva := rmse_expansiva < rmse_naive]
cat(sprintf("\nFixo vence a ingenua em %d dos %d meses | Expansiva vence em %d dos %d meses\n",
            sum(por_mes$vence_fixo), nrow(por_mes), sum(por_mes$vence_expansiva), nrow(por_mes)))
print(por_mes[, .(ym, rmse_fixo = round(rmse_fixo,5), rmse_expansiva = round(rmse_expansiva,5),
                   rmse_naive = round(rmse_naive,5), vence_fixo, vence_expansiva)])

# --- por gestora: o ranking de dificuldade muda muito entre fixo e expansiva? ---
por_gestora <- R[, .(n=.N, rmse_fixo = rmse(erro_fixo), rmse_expansiva = rmse(erro_expansiva)), by = gestora_grupo]
por_gestora <- por_gestora[n >= 100]
por_gestora[, rank_fixo := rank(-rmse_fixo)]
por_gestora[, rank_expansiva := rank(-rmse_expansiva)]
cat(sprintf("\nCorrelacao de Spearman do ranking de dificuldade por gestora (fixo vs expansiva), %d gestoras: %.4f\n",
            nrow(por_gestora), cor(por_gestora$rank_fixo, por_gestora$rank_expansiva, method="spearman")))

fwrite(R, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_janela_expansiva_resultado.csv"))
fwrite(LAM, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_janela_expansiva_lambdas.csv"))
fwrite(por_mes, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_janela_expansiva_por_mes.csv"))
fwrite(por_gestora, file.path(REPO, "v2 OFICIAL/data/etapa3_multiativo_janela_expansiva_por_gestora.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/etapa3_multiativo_janela_expansiva_*.csv'\n")
