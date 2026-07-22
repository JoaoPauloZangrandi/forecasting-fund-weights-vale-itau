# =============================================================================
# 25_panel_multiativo.R  (v2 OFICIAL)  -- v2, memoria-eficiente
#
# ETAPA 3 do plano: painel fundo x ativo x mes de TODAS as posicoes em Acoes
# dos MESMOS 2.820 fundos (41 gestoras) ja usados na Etapa 2.
#
# Reescrito para processar e FILTRAR (so posicao direta) ANO A ANO, gravando
# cada ano ja limpo direto em disco (fwrite append) antes de passar pro
# proximo -- a primeira versao acumulava os 6 anos brutos (16,8 milhoes de
# linhas) inteiros na memoria antes de limpar, e o processo morria (kill,
# provavel estouro de memoria) em pontos diferentes a cada tentativa.
#
# Mesma limpeza de custodia da v1 (R/31): so posicao DIRETA (sufixo de
# ticker 3/4-8/11, sem palavras-chave de emprestimo/subscricao/certificado).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
DATA_DIR <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
OUT <- file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto.csv")

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

base <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv"))
fundos_alvo <- unique(base$cod_fundo)
map_gestora <- unique(base[, .(cod_fundo, gestora_grupo)])
map_gestora <- map_gestora[!duplicated(cod_fundo)]
cat("Universo fixo de fundos:", length(fundos_alvo), "\n")

if (file.exists(OUT)) file.remove(OUT)
total_linhas <- 0L; total_vale <- 0L; primeiro <- TRUE

for (y in 2016:2021) {
  cat("== ano", y, "==\n"); flush.console()
  cv <- fread(file.path(DATA_DIR, sprintf("cons_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE)
  stopifnot(all(cv$Tipo_Ativo == "Ações"))
  cv <- unique(cv)
  cv[, PESO := as.numeric(Participação_Ativo)]
  cv <- cv[!is.na(PESO) & PESO >= 0 & PESO <= 1]
  cv <- cv[Código %in% fundos_alvo]
  cv <- cv[!grepl("Companhia Fechada", Nome_Ativo, fixed = TRUE)]

  # filtra JA AQUI so posicao direta -- corta ~37% das linhas antes de acumular mais nada
  cv <- cv[classifica_direta(Nome_Ativo)]
  cv[, DATA_COMP := pdate(`Data_Competência`)]

  pan <- cv[, .(cod_fundo = Código, cnpj = CNPJ, ativo = Nome_Ativo,
                data = DATA_COMP, ano = year(DATA_COMP), mes = month(DATA_COMP),
                peso = PESO, valor_mil = Valor_Ativo_mil)]
  pan <- merge(pan, map_gestora, by = "cod_fundo", all.x = TRUE)

  nvale_ano <- pan[ativo == "VALE ON N1 - VALE3", .N]
  cat("  ", nrow(pan), "linhas (so posicao direta) |", uniqueN(pan$cod_fundo), "fundos |",
      uniqueN(pan$ativo), "ativos | VALE3:", nvale_ano, "\n"); flush.console()

  fwrite(pan, OUT, append = !primeiro)
  primeiro <- FALSE
  total_linhas <- total_linhas + nrow(pan)
  total_vale <- total_vale + nvale_ano

  rm(cv, pan); gc(FALSE)
}

cat("\nTOTAL linhas gravadas:", total_linhas, "\n")
cat("TOTAL VALE3:", total_vale, "(esperado: 96.349, do painel da Etapa 2)\n")
stopifnot(total_vale == 96349L)
cat("\nOK - salvo em '", OUT, "'\n")
