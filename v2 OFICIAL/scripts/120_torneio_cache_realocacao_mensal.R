# Cacheia a realocacao por fundo-MES (ativa e passiva) para permitir testar
# janelas alternativas sem reler o arquivo de 931 MB toda vez.
suppressPackageStartupMessages(library(data.table))
DD <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau/v2 OFICIAL/data"
SC <- DD
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }

E <- fread(file.path(DD, "erro_e_multiativo.csv"), select = c("cod_fundo","ativo","ym","peso"))
E[, cod_fundo := as.character(cod_fundo)]
fut <- E[, .(cod_fundo, ativo, ym_fut_key = ym, peso_fut = peso)]
M <- merge(E[, .(cod_fundo, ativo, ym, peso, ym_fut = addm(ym, 1L))],
           fut, by.x = c("cod_fundo","ativo","ym_fut"), by.y = c("cod_fundo","ativo","ym_fut_key"))
rm(E, fut); invisible(gc())
M[, ticker := trimws(sub(".*- ", "", ativo))]
precos <- fread(file.path(DD, "precos_mensais_final.csv"), select = c("ticker","ymk","retorno"))
setnames(precos, c("ymk","retorno"), c("ym_fut","r_ativo"))
rfundo <- fread(file.path(DD, "retorno_fundo_mensal.csv"), select = c("cod_fundo","ymk","retorno_fundo"))
rfundo[, cod_fundo := as.character(cod_fundo)]; setnames(rfundo, "ymk", "ym_fut")
M <- merge(M, precos, by = c("ticker","ym_fut"), all.x = TRUE)
M <- merge(M, rfundo, by = c("cod_fundo","ym_fut"), all.x = TRUE)

M[, peso_deriva := peso * (1 + r_ativo) / (1 + retorno_fundo)]   # peso se o gestor nao fizesse nada
M[, dw_ativo   := peso_fut - peso_deriva]                         # decisao
M[, dw_passivo := peso_deriva - peso]                             # deriva de preco
M <- M[is.finite(dw_ativo) & is.finite(dw_passivo)]

Q <- M[, .(ativa = mean(abs(dw_ativo)), passiva = mean(abs(dw_passivo)),
           n = .N, hhi = sum(peso^2)), by = .(cod_fundo, ym)]
Q[, ano := ym %/% 100L][, mes := ym %% 100L]
# mes = mes do peso inicial; a NEGOCIACAO acontece no mes seguinte
Q[, mes_negoc := (mes %% 12L) + 1L]
Q[, ano_negoc := fifelse(mes == 12L, ano + 1L, ano)]
fwrite(Q, file.path(SC, "realoc_fundo_mes.csv"))
cat("gravado:", nrow(Q), "linhas |", uniqueN(Q$cod_fundo), "fundos | ym",
    min(Q$ym), "a", max(Q$ym), "\n")
