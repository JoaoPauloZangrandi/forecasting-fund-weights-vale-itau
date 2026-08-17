# =============================================================================
# 51_painel_multiativo_universo_completo.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 0, passo 2: painel fundo x ativo x mes de TODAS as posicoes em Acoes,
# pro universo COMPLETO de 3.254 fundos (R/50) -- mesma limpeza de custodia
# do R/25 (so posicao direta), so SEM o filtro fundos_alvo (VALE3) que o R/25
# aplicava. Objetivo: nao ancorar mais o painel em quem ja teve VALE3, pra
# o "net" de qualquer outro ativo (ex.: Petrobras) nao ficar enviesado pra
# essa sub-amostra (achado do Joao, 30/07/2026).
#
# Mesmo processamento ano-a-ano com fwrite append (R/25), pelo mesmo motivo
# de memoria (16,8 milhoes de linhas brutas nao cabem tudo de uma vez).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
DATA_DIR <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
source(file.path(REPO, "v2 OFICIAL/scripts/config_periodo.R"))
OUT <- file.path(REPO, "v2 OFICIAL/data/painel_multiativo_direto_completo.csv")

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

map_gestora <- fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
map_gestora[, cod_fundo := as.character(cod_fundo)]
cat("Universo completo (mapa gestora):", nrow(map_gestora), "fundos\n")

if (file.exists(OUT)) file.remove(OUT)
total_linhas <- 0L; total_vale <- 0L; primeiro <- TRUE

for (y in ANO_INICIO:ANO_FIM) {
  cat("== ano", y, "==\n"); flush.console()
  # select= reduz uso de memoria (arquivos 2022+ tem 2-2.7GB, RAM da maquina e' limitada
  # -- achado 16/08/2026: rodada sem select= travou silenciosamente por falta de memoria
  # em algum ano >=2020, sem erro visivel no log)
  cv <- fread(file.path(DATA_DIR, sprintf("cons_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("CNPJ","Código","Tipo_Ativo","Data_Competência","Nome_Ativo",
                         "Valor_Ativo_mil","Participação_Ativo"))
  stopifnot(all(cv$Tipo_Ativo == "Ações"))
  cv <- unique(cv)
  cv[, PESO := as.numeric(Participação_Ativo)]
  cv <- cv[!is.na(PESO) & PESO >= 0 & PESO <= 1]
  cv[, Código := as.character(Código)]
  # SEM filtro de fundos_alvo (VALE3) -- essa e' a diferenca-chave vs R/25
  cv <- cv[!grepl("Companhia Fechada", Nome_Ativo, fixed = TRUE)]

  cv <- cv[classifica_direta(Nome_Ativo)]
  cv[, DATA_COMP := pdate(`Data_Competência`)]

  pan <- cv[, .(cod_fundo = Código, cnpj = CNPJ, ativo = Nome_Ativo,
                data = DATA_COMP, ano = year(DATA_COMP), mes = month(DATA_COMP),
                peso = PESO, valor_mil = Valor_Ativo_mil)]
  pan <- merge(pan, map_gestora[, .(cod_fundo, gestora_grupo)], by = "cod_fundo", all.x = TRUE)

  n_sem <- pan[is.na(gestora_grupo), .N]
  nvale_ano <- pan[ativo == "VALE ON N1 - VALE3", .N]
  cat("  ", nrow(pan), "linhas (so posicao direta) |", uniqueN(pan$cod_fundo), "fundos |",
      uniqueN(pan$ativo), "ativos | VALE3:", nvale_ano, "| sem gestora:", n_sem, "\n"); flush.console()

  fwrite(pan, OUT, append = !primeiro)
  primeiro <- FALSE
  total_linhas <- total_linhas + nrow(pan)
  total_vale <- total_vale + nvale_ano

  rm(cv, pan); gc(FALSE)
}

cat("\nTOTAL linhas gravadas:", total_linhas, "\n")
cat("TOTAL VALE3:", total_vale, "(esperado: 96.349 SO' se ANO_FIM==2021; ver PLANO_EXPANSAO_2021_2026.md)\n")
if (ANO_FIM == 2021L) stopifnot(total_vale == 96349L)
cat("\nOK - salvo em '", OUT, "'\n")
