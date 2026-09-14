# =============================================================================
# 113_torneio_teste_c_copia_lider.R  (v2 OFICIAL / trilha TORNEIO)
#
# TESTE C -- "quem esta' atras do benchmark copia quem esta' na frente?"
# (segunda pergunta levantada pelo professor). Nenhum trabalho brasileiro
# respondeu isso com carteira; a literatura global tem as duas teorias
# apontando para lados opostos:
#   - torneio (Chen e Pennacchi 2009; Li, Tiwari e Tong 2022): o perdedor se
#     AFASTA do rebanho, porque copiar nao faz ninguem ultrapassar ninguem;
#   - carreira (Chevalier e Ellison 1999; Jiang e Verardo 2018): o perdedor se
#     COLA no rebanho, porque carteira convencional protege o emprego.
#
# DESENHO. Em vez de medir "parecenca" de nivel entre carteiras -- que
# confunde copia com simplesmente carregar as mesmas blue chips -- o teste
# segue a logica de Sias (2004): copiar e' seguir a OPERACAO do outro, nao
# ter a mesma posicao que o outro. Entao a regressao e' um horse race que
# aninha o proprio modelo do TCC:
#
#   dw_{i,n,t} = lambda * d_{i,n,t}                  <- persegue o proprio alvo
#              + gamma_L * dwLider_{n,t-L}           <- segue quem esta' ganhando
#              + gamma_C * dwTodos_{n,t-L}           <- segue a manada em geral
#              + gamma_P * dwPerdedor_{n,t-L}        <- placebo: segue quem perde
#              + interacoes com "estar atras"        <- a pergunta do professor
#
# Se o perdedor copia o lider, a interacao de dwLider com "perdedor" e'
# positiva. Se ele apenas segue a manada, quem carrega o efeito e' dwTodos.
# Se dwPerdedor tambem aparecer, nao e' copia de lider -- e' comovimento
# generico entre carteiras, e o achado morre.
#
# DEFASAGEM DE DIVULGACAO. Pela ICVM 555 o fundo pode omitir a identificacao
# dos ativos da CDA por ate 90 dias. Entao a operacao do lider so pode entrar
# como informacao disponivel com defasagem. O teste roda com L = 1 (divulgacao
# imediata, limite superior do que seria copiavel) e L = 3 (limite legal de
# omissao, cenario conservador).
#
# Todas as medidas de grupo sao LEAVE-ONE-OUT: a operacao do proprio fundo e'
# retirada do agregado do grupo dele, senao o fundo "copiaria a si mesmo".
#
# Entradas: erro_e_multiativo.csv, torneio_desempenho_fundo_mes.csv,
#           precos_mensais_final.csv, retorno_fundo_mensal.csv
# Saidas:   data/torneio_c_horse_race.csv (coeficientes)
#           data/torneio_c_similaridade.csv (descritivo de similaridade)
# RODAR COM CAMINHO ABSOLUTO. Pesado.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(fixest) })
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DD <- file.path(REPO, "v2 OFICIAL/data")
setFixest_notes(FALSE)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

# =============================================================================
# 1. painel de realocacao ativa (mesma construcao do script 111)
# =============================================================================
cat("== montando painel de realocacao ==\n")
E <- fread(file.path(DD, "erro_e_multiativo.csv"),
           select = c("cod_fundo","ativo","ym","peso","peso_pred"))
E[, cod_fundo := as.character(cod_fundo)]
E[, d := peso_pred - peso]

fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
M <- merge(E[, .(cod_fundo, ativo, ym, d, peso, ym_fut = addm(ym, 1L))],
           fut, by.x = c("cod_fundo","ativo","ym_fut"),
           by.y = c("cod_fundo","ativo","ym_fut_key"))
rm(E, fut); invisible(gc())

M[, ticker := trimws(sub(".*- ", "", ativo))]
precos <- fread(file.path(DD, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setnames(precos, c("ymk","retorno"), c("ym_fut","r_ativo"))
rfundo <- fread(file.path(DD, "retorno_fundo_mensal.csv"), select = c("cod_fundo","ymk","retorno_fundo"))
rfundo[, cod_fundo := as.character(cod_fundo)]
setnames(rfundo, "ymk", "ym_fut")
M <- merge(M, precos, by = c("ticker","ym_fut"), all.x = TRUE)
M <- merge(M, rfundo, by = c("cod_fundo","ym_fut"), all.x = TRUE)
M[, dw := peso_fut - peso * (1 + r_ativo) / (1 + retorno_fundo)]   # realocacao ATIVA
M <- M[is.finite(dw) & is.finite(d)]
M[, .(cod_fundo, ativo, ym, ym_fut, peso, d, dw)] -> M
cat("Painel de realocacao ativa:", nrow(M), "obs |", uniqueN(M$cod_fundo), "fundos\n")

# =============================================================================
# 2. quem estava ganhando e quem estava perdendo, mes a mes
# =============================================================================
P <- fread(file.path(DD, "torneio_desempenho_fundo_mes.csv"))
P[, cod_fundo := as.character(cod_fundo)]
P[, decil := cut(rank_pct, breaks = c(0, .1, .9, 1), labels = c("D1","meio","D10"),
                 include.lowest = TRUE)]
STA <- P[, .(cod_fundo, ym, rank_pct, excesso_acum, decil)]

# status do fundo no mes em que a operacao dele acontece (predeterminado:
# posicao acumulada ate o mes anterior ao movimento)
M <- merge(M, STA[, .(cod_fundo, ym, rank_pct, excesso_acum, decil)],
           by = c("cod_fundo","ym"), all.x = TRUE)
M <- M[!is.na(decil)]
cat("Apos exigir status de desempenho:", nrow(M), "obs |", uniqueN(M$cod_fundo), "fundos\n")
cat("Distribuicao de status:\n"); print(M[, .N, by = decil])

# =============================================================================
# 3. operacao agregada de cada grupo, por ativo e mes (leave-one-out)
# =============================================================================
cat("\n== agregando a operacao de cada grupo por ativo-mes ==\n")
AGG <- M[, .(soma_tod = sum(dw), k_tod = .N), by = .(ativo, ym)]
AGG <- merge(AGG, M[decil == "D10", .(soma_lid = sum(dw), k_lid = .N), by = .(ativo, ym)],
             by = c("ativo","ym"), all.x = TRUE)
AGG <- merge(AGG, M[decil == "D1",  .(soma_per = sum(dw), k_per = .N), by = .(ativo, ym)],
             by = c("ativo","ym"), all.x = TRUE)
AGG[is.na(soma_lid), `:=`(soma_lid = 0, k_lid = 0L)]
AGG[is.na(soma_per), `:=`(soma_per = 0, k_per = 0L)]
stopifnot(!anyDuplicated(AGG, by = c("ativo","ym")))
cat("Celulas ativo-mes:", nrow(AGG), "\n")

OWN <- M[, .(cod_fundo, ativo, ym,
             dw_own = dw,
             era_lid = as.integer(decil == "D10"),
             era_per = as.integer(decil == "D1"))]

# =============================================================================
# 4. defasagem de divulgacao: a operacao do grupo entra defasada em L meses
#    (leave-one-out feito NA DATA DE ORIGEM: a operacao que o proprio fundo
#     fez em t-L sai do agregado do grupo dele em t-L, senao o coeficiente
#     capturaria a inercia do proprio fundo -- que e' exatamente a parcela
#     que Sias (2004) separa de "seguir os outros")
# =============================================================================
monta_lag <- function(L) {
  A <- copy(AGG); A[, ym_alvo := addm(ym, L)]
  O <- copy(OWN); O[, ym_alvo := addm(ym, L)]
  K <- M[, .(cod_fundo, ativo, ym)]
  K <- merge(K, A[, .(ativo, ym_alvo, soma_tod, k_tod, soma_lid, k_lid, soma_per, k_per)],
             by.x = c("ativo","ym"), by.y = c("ativo","ym_alvo"))
  K <- merge(K, O[, .(cod_fundo, ativo, ym_alvo, dw_own, era_lid, era_per)],
             by.x = c("cod_fundo","ativo","ym"), by.y = c("cod_fundo","ativo","ym_alvo"),
             all.x = TRUE)
  K[is.na(dw_own), `:=`(dw_own = 0, era_lid = 0L, era_per = 0L)]
  K[, lag_lid := fifelse(k_lid - era_lid > 0, (soma_lid - era_lid*dw_own)/(k_lid - era_lid), NA_real_)]
  K[, lag_per := fifelse(k_per - era_per > 0, (soma_per - era_per*dw_own)/(k_per - era_per), NA_real_)]
  K[, lag_tod := fifelse(k_tod - 1L > 0, (soma_tod - dw_own)/(k_tod - 1L), NA_real_)]
  out <- K[, .(cod_fundo, ativo, ym, lag_lid, lag_per, lag_tod)]
  setnames(out, c("lag_lid","lag_per","lag_tod"),
           paste0(c("lag_lid","lag_per","lag_tod"), L))
  out
}
M <- merge(M, monta_lag(1L), by = c("cod_fundo","ativo","ym"), all.x = TRUE)
M <- merge(M, monta_lag(3L), by = c("cod_fundo","ativo","ym"), all.x = TRUE)
invisible(gc())

M[, atras := as.integer(decil == "D1")]
M[, ativo_ym := paste0(ativo, "_", ym)]

cat("\n== amostra final do horse race ==\n")
cat("L=1:", M[is.finite(lag_lid1), .N], "obs | L=3:", M[is.finite(lag_lid3), .N], "obs\n")

# =============================================================================
# 5. HORSE RACE
# =============================================================================
roda <- function(L) {
  sl <- paste0("lag_lid", L); sp <- paste0("lag_per", L); st <- paste0("lag_tod", L)
  dt <- M[is.finite(get(sl)) & is.finite(get(st))]
  cat("\n\n############ DEFASAGEM DE DIVULGACAO L =", L, "mes(es) ############\n")
  cat("Obs:", nrow(dt), "| fundos:", uniqueN(dt$cod_fundo), "\n")

  cat("\n--- C1: so o lider (segue quem esta' ganhando?) ---\n")
  f1 <- feols(as.formula(sprintf("dw ~ d + %s | cod_fundo + ym", sl)),
              data = dt, cluster = ~ativo_ym)
  print(summary(f1))

  cat("\n--- C2: lider contra manada e contra perdedor (o placebo) ---\n")
  f2 <- feols(as.formula(sprintf("dw ~ d + %s + %s + %s | cod_fundo + ym", sl, st, sp)),
              data = dt, cluster = ~ativo_ym)
  print(summary(f2))

  cat("\n--- C3: A PERGUNTA -- quem esta' atras copia MAIS o lider? ---\n")
  f3 <- feols(as.formula(sprintf("dw ~ d + %s + %s + %s + %s:atras + %s:atras | cod_fundo + ym",
                                 sl, st, sp, sl, st)),
              data = dt, cluster = ~ativo_ym)
  print(summary(f3))

  cat("\n--- C4: versao continua -- copia cresce quando a distancia cresce? ---\n")
  f4 <- feols(as.formula(sprintf("dw ~ d + %s + %s + %s:excesso_acum | cod_fundo + ym",
                                 sl, st, sl)),
              data = dt, cluster = ~ativo_ym)
  print(summary(f4))

  data.table(L = L,
             modelo = c("C1","C2","C3","C4"),
             coef_lider = c(coef(f1)[sl], coef(f2)[sl], coef(f3)[sl], coef(f4)[sl]),
             coef_interacao_atras = c(NA, NA, coef(f3)[paste0(sl, ":atras")], NA),
             coef_interacao_excesso = c(NA, NA, NA, coef(f4)[paste0(sl, ":excesso_acum")]),
             n_obs = c(f1$nobs, f2$nobs, f3$nobs, f4$nobs))
}
RES <- rbindlist(list(roda(1L), roda(3L)))
fwrite(RES, file.path(DD, "torneio_c_horse_race.csv"))

# =============================================================================
# 6. descritivo de similaridade de carteira (complemento, nao o teste central)
# =============================================================================
cat("\n\n=========== C5: SIMILARIDADE DE CARTEIRA (descritivo) ===========\n")
# cosseno entre a carteira do fundo em t e a carteira media dos lideres em t-3
W <- unique(M[, .(cod_fundo, ativo, ym, peso, decil, rank_pct)])
LID <- M[decil == "D10", .(w_lid = mean(peso)), by = .(ativo, ym)]
LID[, ym := addm(ym, 3L)]
S <- merge(W, LID, by = c("ativo","ym"))
SIM <- S[, .(num = sum(peso * w_lid),
             n1 = sqrt(sum(peso^2)), n2 = sqrt(sum(w_lid^2))),
         by = .(cod_fundo, ym, decil, rank_pct)]
SIM[, cosseno := num / (n1 * n2)]
SIM <- SIM[is.finite(cosseno)]
cat("Fundo-mes com similaridade calculavel:", nrow(SIM), "\n\n")
cat("--- cosseno medio com a carteira dos lideres (defasada 3 meses) ---\n")
print(SIM[, .(n = .N, cosseno_medio = round(mean(cosseno), 4),
              cosseno_mediana = round(median(cosseno), 4)), by = decil][order(decil)])
cat("\n--- regressao: similaridade com o lider ~ posicao do proprio fundo ---\n")
print(summary(feols(cosseno ~ rank_pct | cod_fundo + ym, data = SIM, cluster = ~cod_fundo)))
fwrite(SIM, file.path(DD, "torneio_c_similaridade.csv"))

cat("\nOK - 113 concluido\n")
