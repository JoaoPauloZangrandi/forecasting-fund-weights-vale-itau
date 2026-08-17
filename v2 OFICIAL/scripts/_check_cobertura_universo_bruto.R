# =============================================================================
# _check_cobertura_universo_bruto.R (temporario, NAO documentado no TCC ainda)
#
# Pergunta do Joao: esquecendo qualquer filtro do projeto (VALE3, gestora,
# Itau), os arquivos BRUTOS (cons_2020.csv, SH_2020.csv, pasta
# Consolidado_MF) ja cobrem sozinhos TODO fundo do Brasil que teve posicao
# em acoes (direta ou via cota) em agosto/2020, ou ja sao um recorte
# parcial na origem?
#
# Verificacao: (1) quantos fundos distintos aparecem na SH em ago/2020
# (universo total de fundos que reportam a CVM naquele mes, qualquer
# estrategia); (2) quantos fundos distintos aparecem no cons_2020.csv com
# Data_Competencia = ago/2020 (fundos com posicao DIRETA em acoes); (3)
# nao ha como provar completude 100% so com dado interno -- precisa
# comparar com numero publico oficial da CVM de fundos registrados/ativos
# na epoca (feito via busca separada, nao neste script).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
SH_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"

pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }

cat("===== (1) SH_2020.csv -- universo total de fundos reportando em ago/2020 =====\n")
sh <- fread(file.path(SH_DIR, "SH_2020.csv"),
            select = c("COD_FUNDO","NOME_FUNDO","GESTORA","CLASSIFICACAO_ANBIMA","DATA"),
            encoding = "UTF-8", showProgress = FALSE)
cat("Linhas totais SH_2020:", nrow(sh), "\n")
sh[, DATA_D := pdate(DATA)]
sh_ago <- sh[format(DATA_D, "%Y-%m") == "2020-08"]
cat("Fundos distintos reportando em algum dia de ago/2020 (SH, qualquer estrategia):",
    uniqueN(sh_ago$COD_FUNDO), "\n\n")

cat("Distribuicao por classificacao Anbima (top 20, ago/2020):\n")
print(sh_ago[, .(fundos = uniqueN(COD_FUNDO)), by = CLASSIFICACAO_ANBIMA][order(-fundos)][1:20])

cat("\n===== (2) cons_2020.csv -- fundos com posicao DIRETA em acoes em ago/2020 =====\n")
cv <- fread(file.path(SH_DIR, "cons_2020.csv"), encoding = "UTF-8", showProgress = FALSE)
cat("Linhas totais cons_2020 (ja filtrado Tipo_Ativo=='Ações'):", nrow(cv), "\n")
cv[, DATA_COMP := pdate(`Data_Competência`)]
cv_ago <- cv[format(DATA_COMP, "%Y-%m") == "2020-08"]
cat("Fundos distintos com QUALQUER linha de acao em ago/2020 (antes do filtro de posicao direta):",
    uniqueN(cv_ago$Código), "\n")

classifica_direta <- function(ativo) {
  ticker <- trimws(sub(".*- ", "", ativo))
  sufixo_chr <- sub(".*?([0-9]+)$", "\\1", ticker)
  tem_sufixo <- sufixo_chr != ticker
  sufixo_num <- rep(NA_integer_, length(ativo))
  sufixo_num[tem_sufixo] <- suppressWarnings(as.integer(sufixo_chr[tem_sufixo]))
  kw_excluir <- "cedid|recebid|[Ss]ubscri|[Cc]ertificado ou recibo de dep|omitid|Outras Ações|IBOVESPA|IBOV11"
  !is.na(sufixo_num) & sufixo_num %in% c(3,4,5,6,7,8,11) & !grepl(kw_excluir, ativo)
}
cv_ago_direta <- cv_ago[classifica_direta(Nome_Ativo)]
cat("Fundos distintos com posicao DIRETA em acoes em ago/2020:", uniqueN(cv_ago_direta$Código), "\n\n")

cat("===== (3) Cruzamento: fundos SH ago/2020 vs fundos com acao direta ago/2020 =====\n")
fundos_sh <- unique(sh_ago$COD_FUNDO)
fundos_com_acao <- unique(cv_ago_direta$Código)
cat("Fundos com acao direta que NAO aparecem na SH ago/2020 (sem match):",
    sum(!fundos_com_acao %in% fundos_sh), "\n")
cat("Cobertura: fundos com acao / fundos totais na SH =",
    sprintf("%.1f%%\n", 100*length(fundos_com_acao)/length(fundos_sh)))

cat("\n===== (4) O proprio Tipo_Ativo do arquivo bruto (pre-filtro do projeto) =====\n")
cat("cons_2020.csv, ja recebido com Tipo_Ativo unico? Confirmando:\n")
print(table(cv[1:min(nrow(cv),2000000)]$Tipo_Ativo))
