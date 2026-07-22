# =============================================================================
# 25b_panel_multiativo_um_ano.R  (v2 OFICIAL)
#
# Processa UM ANO por vez (passado como argumento de linha de comando), num
# processo R novo a cada chamada -- garante que a memoria e liberada entre
# anos (a versao que fazia tudo num loop, num processo so, estava morrendo
# por memoria, provavelmente porque os 2.820 fundos ja sao quase o universo
# nacional inteiro de fundos de acoes, entao filtrar por CNPJ nao reduzia o
# tamanho do arquivo CONS de forma significativa).
#
# Uso: Rscript 25b_panel_multiativo_um_ano.R 2016
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
y <- as.integer(args[1])
stopifnot(!is.na(y))

DATA_DIR <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
OUT <- file.path(REPO, sprintf("v2 OFICIAL/data/painel_multiativo_ano_%d.csv", y))

pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }

classifica_direta <- function(ativo) {
  ticker <- trimws(sub(".*- ", "", ativo))
  sufixo_chr <- sub(".*?([0-9]+)$", "\\1", ticker)
  tem_sufixo <- sufixo_chr != ticker
  sufixo_num <- rep(NA_integer_, length(ativo))
  sufixo_num[tem_sufixo] <- suppressWarnings(as.integer(sufixo_chr[tem_sufixo]))
  kw_excluir <- "cedid|recebid|[Ss]ubscri|[Cc]ertificado ou recibo de dep|omitid|Outras Ações|IBOVESPA|IBOV11"
  !is.na(sufixo_num) & sufixo_num %in% c(3,4,5,6,7,8,11) & !grepl(kw_excluir, ativo)
}

base <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv"),
              select = c("cod_fundo","gestora_grupo"))
fundos_alvo <- unique(base$cod_fundo)
map_gestora <- unique(base)[!duplicated(cod_fundo)]
rm(base); gc(FALSE)
cat("Ano", y, "| universo fixo de fundos:", length(fundos_alvo), "\n")

cv <- fread(file.path(DATA_DIR, sprintf("cons_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
            select = c("CNPJ","Código","Tipo_Ativo","Data_Competência","Nome_Ativo",
                       "Valor_Ativo_mil","Participação_Ativo"))
cat("Linhas brutas lidas:", nrow(cv), "\n")
stopifnot(all(cv$Tipo_Ativo == "Ações"))
cv[, Tipo_Ativo := NULL]
cv <- unique(cv)
cv[, PESO := as.numeric(Participação_Ativo)]
cv[, Participação_Ativo := NULL]
cv <- cv[!is.na(PESO) & PESO >= 0 & PESO <= 1]
cv <- cv[Código %in% fundos_alvo]
cv <- cv[!grepl("Companhia Fechada", Nome_Ativo, fixed = TRUE)]
cat("Apos filtro fundo-alvo + peso valido:", nrow(cv), "\n")

cv <- cv[classifica_direta(Nome_Ativo)]
cat("Apos filtro posicao direta:", nrow(cv), "|", uniqueN(cv$Nome_Ativo), "ativos\n")

cv[, DATA_COMP := pdate(`Data_Competência`)]
pan <- cv[, .(cod_fundo = Código, cnpj = CNPJ, ativo = Nome_Ativo,
              data = DATA_COMP, ano = year(DATA_COMP), mes = month(DATA_COMP),
              peso = PESO, valor_mil = Valor_Ativo_mil)]
rm(cv); gc(FALSE)
pan <- merge(pan, map_gestora, by = "cod_fundo", all.x = TRUE)

nvale <- pan[ativo == "VALE ON N1 - VALE3", .N]
cat("Linhas finais:", nrow(pan), "| VALE3:", nvale, "\n")

fwrite(pan, OUT)
cat("OK - salvo em '", OUT, "'\n")
