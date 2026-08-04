# =============================================================================
# 67_previsao_ajustada_seguidor.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 5, passo 2: previsao ajustada do seguidor, somando o "eco" do lider:
#
#   dw_ajustado_{seguidor,ativo,t+1} = lambda*d_{seguidor,ativo,t} +
#                                       beta_par * u_{lider,ativo,t}
#
# usa o valor REAL observado de u do lider no mes t (nao uma previsao).
# Regra de desempate (decidida no caminho, avisado ao Joao): se um mesmo
# (seguidor,ativo) tiver mais de um lider candidato, usa SO o de maior
# forca_lideranca -- mais simples, evita ruido de somar varios sinais
# provavelmente correlacionados entre si (lideres do mesmo ativo tendem a
# ser parecidos).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

beta_tab <- fread(file.path(REPO, "v2 OFICIAL/data/pares_lideranca_beta.csv"))
beta_tab[, lider := as.character(lider)]
beta_tab[, seguidor := as.character(seguidor)]
cat("Pares lider->seguidor (com beta):", nrow(beta_tab), "\n")

# 1 lider por (seguidor,ativo): o de maior forca_lideranca
setorder(beta_tab, -forca_lideranca)
lider_escolhido <- beta_tab[, .SD[1], by = .(seguidor, ativo)]
cat("Pares (seguidor,ativo) distintos, apos escolher 1 lider cada:", nrow(lider_escolhido), "\n")
cat("  (de", nrow(beta_tab), "candidatos brutos --",
    round(100*(1-nrow(lider_escolhido)/nrow(beta_tab)),1), "% eram (seguidor,ativo) com mais de 1 lider)\n")

# ---- base de erro (u) por (fundo,ativo,ym), mesma de sempre ----------------
M <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_universo_completo_h1.csv"))
M[, cod_fundo := as.character(cod_fundo)]
lam <- 0.0665
M[, u := dw_corrigido - lam * d]
M <- M[!is.na(u)]

# u do LIDER, no mes t, pra cada (ativo, lider, ym)
u_lider <- M[, .(ativo, cod_fundo, ym, u_lider = u)]

# base do SEGUIDOR (precisa de d e ym do proprio seguidor, pra montar a previsao)
u_seguidor <- M[, .(ativo, cod_fundo, ym, d_seguidor = d, dw_corrigido_seguidor = dw_corrigido)]

comb <- merge(lider_escolhido[, .(seguidor, lider, ativo, beta)], u_seguidor,
              by.x = c("seguidor","ativo"), by.y = c("cod_fundo","ativo"), all.x = FALSE)
comb <- merge(comb, u_lider, by.x = c("lider","ativo","ym"), by.y = c("cod_fundo","ativo","ym"), all.x = FALSE)
cat("\nObs (seguidor,ativo,mes) com lider observado no MESMO mes:", nrow(comb), "\n")

comb[, dw_previsto_normal := lam * d_seguidor]
comb[, eco_lider := beta * u_lider]
comb[, dw_ajustado := dw_previsto_normal + eco_lider]

cat("\nResumo do termo 'eco do lider' (quanto o ajuste muda a previsao normal):\n")
print(summary(comb$eco_lider))
cat("\nResumo da previsao SEM ajuste (so lambda*d):\n"); print(summary(comb$dw_previsto_normal))
cat("Resumo da previsao AJUSTADA (com eco):\n"); print(summary(comb$dw_ajustado))

# erro de cada versao vs o real observado (dw_corrigido do seguidor)
comb[, erro_normal := dw_corrigido_seguidor - dw_previsto_normal]
comb[, erro_ajustado := dw_corrigido_seguidor - dw_ajustado]
rmse <- function(x) sqrt(mean(x^2, na.rm=TRUE))
cat(sprintf("\nRMSE sem ajuste (so lambda*d): %.5f\n", rmse(comb$erro_normal)))
cat(sprintf("RMSE com ajuste (lambda*d + eco): %.5f\n", rmse(comb$erro_ajustado)))
cat(sprintf("Melhora: %.1f%%\n", 100*(rmse(comb$erro_normal)-rmse(comb$erro_ajustado))/rmse(comb$erro_normal)))

fwrite(comb, file.path(REPO, "v2 OFICIAL/data/previsao_ajustada_seguidor.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/previsao_ajustada_seguidor.csv' (", nrow(comb), " obs)\n", sep="")
