# =============================================================================
# 66_beta_pares_lideranca.R  (v2 OFICIAL -- teste_estrategia)  -- v2, corrigido
#
# FASE 5, passo 1: estima o coeficiente beta da relacao lider->seguidor:
#   u_seguidor(t+1-equivalente) = beta * u_lider(t) + residuo
#
# CORRECAO v2 (30/07/2026): a v1 usava o atalho beta = corr * sd_seguidor/
# sd_lider, mas com sd calculado na serie INTEIRA de cada fundo (todos os
# meses que ele tem, nao so os meses que se sobrepoem com aquele parceiro
# especifico) -- isso quebra a garantia matematica do OLS (o RMSE com ajuste
# NUNCA deveria piorar, dentro da amostra, comparado a beta=0; mas piorou
# 44,7% no teste). Corrigido: beta calculado via regressao-atraves-da-origem
# EXATA (beta = sum(x*y)/sum(x^2)) na amostra pareada EXATA de cada par --
# matematicamente identico a rodar lm() por par, so' que vetorizado (soma
# por grupo em vez de ajustar 75 mil modelos um a um).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

M <- fread(file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_universo_completo_h1.csv"))
M[, cod_fundo := as.character(cod_fundo)]
lam <- 0.0665
M[, u := dw_corrigido - lam * d]
M <- M[!is.na(u)]

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv"))
pp[, cod_fundo := as.character(cod_fundo)]
cel <- pp[, .(peso_mediano = median(peso)), by = .(cod_fundo, ativo)]
cel_ok <- cel[peso_mediano >= 0.001, .(cod_fundo, ativo)]
M <- merge(M, cel_ok, by = c("cod_fundo","ativo"))
cat("Base de erro (u), pos filtro de posicao-poeira:", nrow(M), "obs\n")

lid <- fread(file.path(REPO, "v2 OFICIAL/data/pares_lideranca_clara.csv"))
lid[, lider := as.character(lider)]; lid[, seguidor := as.character(seguidor)]
lid[, fundo_a := as.character(fundo_a)]; lid[, fundo_b := as.character(fundo_b)]
cat("Pares de lideranca clara:", nrow(lid), "\n")

# ---- monta, pra CADA par, a amostra pareada exata (mes a mes) --------------
u_lider    <- M[, .(ativo, lider = cod_fundo, ym, u_lider = u)]
u_seguidor <- M[, .(ativo, seguidor = cod_fundo, ym, u_seguidor = u)]

pares_chave <- lid[, .(par_id = .I, lider, seguidor, ativo)]
com_lider    <- merge(pares_chave, u_lider,    by = c("lider","ativo"), allow.cartesian = TRUE)
comb_ym      <- merge(com_lider, u_seguidor, by = c("seguidor","ativo","ym"), allow.cartesian = TRUE)
cat("Observacoes mes-a-mes pareadas (lider,seguidor,ativo,mes):", nrow(comb_ym), "\n")

# beta por regressao-atraves-da-origem exata: beta = sum(x*y)/sum(x^2)
beta_exato <- comb_ym[, .(beta = sum(u_lider*u_seguidor)/sum(u_lider^2), n_obs_par = .N),
                       by = par_id]
lid <- merge(lid[, par_id := .I], beta_exato, by = "par_id", all.x = TRUE)
lid[, par_id := NULL]

cat("\nPares com beta calculavel:", lid[!is.na(beta),.N], "de", nrow(lid), "\n")
cat("Resumo de beta (v2, exato):\n"); print(summary(lid$beta))

extremos <- lid[abs(beta) > 10]
cat("\nPares com |beta|>10 (implausivel):", nrow(extremos), "de", nrow(lid),
    sprintf("(%.2f%%)\n", 100*nrow(extremos)/nrow(lid)))
lid <- lid[abs(beta) <= 10 | is.na(beta)]

fwrite(lid, file.path(REPO, "v2 OFICIAL/data/pares_lideranca_beta.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/pares_lideranca_beta.csv' (", nrow(lid), " pares)\n", sep="")
