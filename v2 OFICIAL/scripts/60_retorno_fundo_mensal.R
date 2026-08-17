# =============================================================================
# 60_retorno_fundo_mensal.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 2, passo 1: retorno MENSAL da cota de cada fundo (universo completo,
# 3.254 fundos) -- necessario como denominador na formula do efeito mecanico:
#
#   w_mecanico_{t+1} = w_t * (1+r_ativo_{t+1}) / (1+r_fundo_{t+1})
#
# Reaproveita a mesma fonte bruta do R/53 (SH_{ano}.csv, coluna COTA), so
# que la' so guardamos retorno DIARIO (pro beta); aqui extraimos o retorno
# MENSAL (ultimo pregao do mes, mesma convencao usada pros precos de ativo
# no R/56).
#
# Cota e' naturalmente neutra a aportes/resgates (quem entra/sai do fundo
# compra/vende pela cota do dia) -- por isso e' a variavel certa pra medir
# retorno "puro" do fundo, sem misturar fluxo de capital.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
DATA_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"
source(file.path(REPO, "v2 OFICIAL/scripts/config_periodo.R"))

alvo <- unique(as.character(fread(file.path(REPO,"v2 OFICIAL/data/universo_completo_gestora.csv"))$cod_fundo))
cat("Fundos-alvo (universo completo):", length(alvo), "\n")

pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }

cotas_all <- list()
for (y in ANO_INICIO:ANO_FIM) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","DATA","COTA"))
  sh[, COD_FUNDO := as.character(COD_FUNDO)]
  sh <- sh[COD_FUNDO %in% alvo]
  sh[, DT := pdate(DATA)][, cota_num := as.numeric(gsub(",", ".", COTA, fixed = TRUE))]
  cotas_all[[length(cotas_all)+1L]] <- sh[, .(cod_fundo = COD_FUNDO, data = DT, cota = cota_num)]
  cat("ano", y, "cotas lidas:", nrow(cotas_all[[length(cotas_all)]]), "\n"); flush.console()
}
cotas <- rbindlist(cotas_all)
cotas <- cotas[!is.na(cota) & cota > 0]
setorder(cotas, cod_fundo, data)
cotas[, ymk := year(data)*100L + month(data)]

mlast <- cotas[cotas[, .I[which.max(data)], by = .(cod_fundo, ymk)]$V1, .(cod_fundo, ymk, data_ref = data, cota)]
setorder(mlast, cod_fundo, ymk)
mlast[, retorno_fundo := cota/shift(cota) - 1, by = cod_fundo]

cat("\nFundo-mes com cota:", nrow(mlast), "| com retorno calculavel:", mlast[!is.na(retorno_fundo), .N], "\n")
cat("Resumo do retorno mensal do fundo (bruto, antes do filtro de sanidade):\n"); print(summary(mlast$retorno_fundo))

# checagem de sanidade, DUAS fontes de artefato diferentes:
# (a) retorno > 200%: cota degenerada por erro de digitacao (confirmado: fundo 371955
#     pula de R$16,60 pra R$22,4 milhoes num mes so, mesmos 4 fundos ja achados no
#     filtro de beta do R/53: 235083, 371890, 371955, 550396).
# (b) retorno < -90%: NAO e' erro de digitacao, e' um evento real nao capturado --
#     desdobramento/grupamento de cotas (o fundo re-basea o valor da cota, tipo um
#     "split" de acoes, sem nenhuma perda economica real). Confirmado no fundo 332186:
#     cota cai de R$2,00 pra R$0,0375 em dez/2018 e CONTINUA crescendo normalmente
#     dali em diante (~2%/mes, igual antes) -- se fosse perda real, nao se recuperaria
#     tao suave. Nos dois casos, o retorno calculado nao reflete economia real, entao
#     e' excluido (nao e' um recorte de "risco/outlier real", e' remover ruido de
#     medicao que contaminaria qualquer regressao que use esse retorno).
extremos <- mlast[(retorno_fundo > 2 | retorno_fundo < -0.9) & !is.na(retorno_fundo)]
cat("Fundo-mes com retorno>200% ou <-90% (evento nao-economico: erro de digitacao ou",
    "desdobramento/grupamento de cotas nao ajustado), excluidos:", nrow(extremos), "\n")
mlast <- mlast[is.na(retorno_fundo) | (retorno_fundo <= 2 & retorno_fundo >= -0.9)]
cat("Resumo do retorno mensal do fundo (apos filtro de sanidade):\n"); print(summary(mlast$retorno_fundo))

fwrite(mlast, file.path(REPO, "v2 OFICIAL/data/retorno_fundo_mensal.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/retorno_fundo_mensal.csv'\n")
