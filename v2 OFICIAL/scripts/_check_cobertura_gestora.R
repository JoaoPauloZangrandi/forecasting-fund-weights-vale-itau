# =============================================================================
# _check_cobertura_gestora.R (temporario, NAO documentado no TCC ainda)
#
# Pergunta do Joao: nossa base tem TODOS os fundos de uma gestora (ex. Kinea)
# que tiveram acoes no periodo, ou so os que tiveram VALE3 especificamente?
# Existe fundo de alguma gestora que fica de fora da nossa base?
#
# Resposta exige verificar 2 coisas separadas:
# (1) o agrupamento de gestora (script 17) so tem 8 regras explicitas
#     (AZ, BNP Paribas, BTG Pactual, Caixa, Credit Suisse, Itau, JGP, XP) --
#     qualquer outra gestora (Kinea inclusive) fica com o nome CRU da SH,
#     sem normalizacao -- pode fragmentar se a grafia variar.
# (2) o universo do painel multiativo (fundos_alvo) e definido por "teve
#     VALE3 direto em algum mes 2016-2021" -- NAO por "e da gestora X" nem
#     por "teve alguma acao". Um fundo Kinea que so teve, digamos, PETR4 e
#     nunca teve VALE3 NAO entra no nosso painel, mesmo sendo fundo de
#     acoes ativo o periodo inteiro.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
SH_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"
GESTORA_ALVO <- "KINEA"

# --- 1) todas as grafias de GESTORA na SH que contem "KINEA" ----------------
cat("===== Grafias de GESTORA contendo '", GESTORA_ALVO, "' na SH (2016-2021) =====\n\n", sep="")
grafias <- list()
for (y in 2016:2021) {
  sh <- fread(file.path(SH_DIR, sprintf("SH_%d.csv", y)),
              select = c("COD_FUNDO","GESTORA"), encoding = "UTF-8", showProgress = FALSE)
  sh <- sh[grepl(GESTORA_ALVO, GESTORA, ignore.case = TRUE)]
  grafias[[as.character(y)]] <- unique(sh[, .(cod_fundo = COD_FUNDO, gestora_raw = GESTORA, ano = y)])
}
grafias_all <- rbindlist(grafias)
cat("Grafias distintas de GESTORA:\n")
print(unique(grafias_all$gestora_raw))
cat("\nFundos distintos com gestora contendo KINEA (qualquer ano, qualquer grafia):",
    uniqueN(grafias_all$cod_fundo), "\n\n")

# --- 2) desses fundos, quais tiveram QUALQUER posicao direta em ACOES ------
#        (nao so VALE3) em algum mes 2016-2021, olhando o cons_YYYY.csv ----
classifica_direta <- function(ativo) {
  ticker <- trimws(sub(".*- ", "", ativo))
  sufixo_chr <- sub(".*?([0-9]+)$", "\\1", ticker)
  tem_sufixo <- sufixo_chr != ticker
  sufixo_num <- rep(NA_integer_, length(ativo))
  sufixo_num[tem_sufixo] <- suppressWarnings(as.integer(sufixo_chr[tem_sufixo]))
  kw_excluir <- "cedid|recebid|[Ss]ubscri|[Cc]ertificado ou recibo de dep|omitid|Outras Ações|IBOVESPA|IBOV11"
  !is.na(sufixo_num) & sufixo_num %in% c(3,4,5,6,7,8,11) & !grepl(kw_excluir, ativo)
}

fundos_kinea <- unique(grafias_all$cod_fundo)
cat("===== Verificando quais fundos Kinea tiveram posicao DIRETA em acoes (qualquer papel) =====\n\n")
tem_acao <- list()
tem_vale3 <- list()
for (y in 2016:2021) {
  cv <- fread(file.path(DATA_DIR <- file.path(SH_DIR), sprintf("cons_%d.csv", y)),
              encoding = "UTF-8", showProgress = FALSE)
  cv <- cv[Código %in% fundos_kinea]
  cv[, PESO := as.numeric(Participação_Ativo)]
  cv <- cv[!is.na(PESO) & PESO > 0 & PESO <= 1]
  cv <- cv[!grepl("Companhia Fechada", Nome_Ativo, fixed = TRUE)]
  cv <- cv[classifica_direta(Nome_Ativo)]
  if (nrow(cv) > 0) {
    tem_acao[[as.character(y)]] <- unique(cv$Código)
    tem_vale3[[as.character(y)]] <- unique(cv[Nome_Ativo == "VALE ON N1 - VALE3"]$Código)
  }
}
fundos_kinea_com_acao <- unique(unlist(tem_acao))
fundos_kinea_com_vale3 <- unique(unlist(tem_vale3))
cat("Fundos Kinea com posicao DIRETA em QUALQUER acao (2016-2021):", length(fundos_kinea_com_acao), "\n")
cat("Fundos Kinea com posicao DIRETA em VALE3 especificamente:", length(fundos_kinea_com_vale3), "\n")
cat("Fundos Kinea com acao mas SEM VALE3 (ficam de fora do nosso painel):",
    length(setdiff(fundos_kinea_com_acao, fundos_kinea_com_vale3)), "\n\n")
if (length(setdiff(fundos_kinea_com_acao, fundos_kinea_com_vale3)) > 0) {
  print(grafias_all[cod_fundo %in% setdiff(fundos_kinea_com_acao, fundos_kinea_com_vale3),
                     .(cod_fundo, gestora_raw)][!duplicated(cod_fundo)])
}

# --- 3) confirmando no nosso painel final quantos fundos aparecem sob "Kinea" ---
cat("\n===== No nosso painel_multiativo_final.csv, gestora_grupo relacionado a Kinea =====\n")
pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"),
            select = c("cod_fundo","gestora_grupo"))
kinea_no_painel <- unique(pp[grepl(GESTORA_ALVO, gestora_grupo, ignore.case = TRUE)])
print(kinea_no_painel[, .(cod_fundo, gestora_grupo)][!duplicated(cod_fundo)])
cat("\nFundos no painel final sob gestora_grupo contendo Kinea:", nrow(kinea_no_painel[!duplicated(cod_fundo)]), "\n")
