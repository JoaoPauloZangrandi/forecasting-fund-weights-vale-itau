# =============================================================================
# 107_reconstroi_SH_cvm.R  (v2 OFICIAL/scripts)
#
# Reconstroi um SH_<ANO>.csv (formato identico ao SH_2016..SH_2021.csv da
# Economatica, ja' usados no pipeline) inteiramente a partir de dado publico
# aberto da CVM, sem depender de acesso a' Economatica. Criado em 15-16/08/2026
# como parte da expansao pro Desafio Quant AI 2026 (dados 2022-2026, que a
# Economatica nao fornece na base "SH"/"cons" que o professor ja tinha
# repassado pra 2016-2021).
#
# METODOLOGIA E LIMITACOES VALIDADAS (ver PLANO_EXPANSAO_2021_2026.md secao
# "Reconstrucao CVM x Economatica" pros numeros completos de validacao):
#
#   COTA / NUMERO_DE_COTISTAS / PATRIMONIO_LIQUIDO / APLICACAO:
#     vem direto do inf_diario_fi (informe diario da CVM). Validado contra
#     SH_2021.csv real: 99-99.9% identico exato. Fonte confiavel.
#
#   GESTORA / CLASSIFICACAO_ANBIMA:
#     PRIORIDADE 1 (100% confiavel onde existe): carry-forward do ultimo valor
#       conhecido nos anos ja existentes (SH_2016..SH_<ANO-1>.csv), casado por
#       CNPJ. Cobre ~80% dos fundos que continuam existindo ano a ano.
#     PRIORIDADE 2 (fallback, ~68-85% preciso): crosswalk empirico
#       Gestor-legal-CVM -> marca-Economatica + heuristica de token no nome
#       do fundo, usado so' pra fundos GENUINAMENTE NOVOS (nunca apareceram
#       em nenhum ano anterior). Ver CROSSWALK_DIR abaixo.
#     TETO CONHECIDO: ~5-6% dos fundos (tipicamente classes de "private
#       banking" com nomenclatura interna tipo "AM3G"/"AGUIA" que nao
#       identificam o gestor real em NENHUMA fonte publica) ficam sem
#       GESTORA/CLASSIFICACAO. Nao afeta o sinal HHI/fragilidade (que nao usa
#       esses campos), so' afeta analises de comparacao de gestoras do TCC.
#
# PRE-REQUISITOS (baixar manualmente antes de rodar):
#   1. inf_diario_fi do ano alvo, 1 CSV por mes, em CVM_RAW_DIR/inf_diario_fi_AAAAMM.csv
#      URL (ano >= 2023): https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/inf_diario_fi_<AAAAMM>.zip
#      URL (ano <= 2022): https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/HIST/inf_diario_fi_<AAAA>.zip (1 zip com 12 CSVs dentro)
#   2. registro_fundo_classe.zip (cadastro CVM 175, snapshot ATUAL -- so' serve
#      de fallback de ultimo recurso pra fundo novo, nunca fonte principal)
#      extrair em CVM_RAW_DIR/registro_fundo_classe/{registro_fundo.csv,registro_classe.csv}
#      URL: https://dados.cvm.gov.br/dados/FI/CAD/DADOS/registro_fundo_classe.zip
#   3. Os 2 CSVs de crosswalk (ja' prontos, gerados em 15/08/2026, ver
#      CROSSWALK_DIR abaixo): crosswalk_gestor_legal_para_marca_FINAL.csv,
#      crosswalk_tokens_nome_fundo_FINAL.csv
# =============================================================================
suppressPackageStartupMessages(library(data.table))

ANO <- as.integer(Sys.getenv("ANO_RECONSTRUCAO", unset = "2022"))

CVM_DATA_DIR  <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
CVM_RAW_DIR   <- Sys.getenv("CVM_RAW_DIR", unset = file.path(CVM_DATA_DIR, "cvm_raw_downloads"))
CROSSWALK_DIR <- Sys.getenv("CROSSWALK_DIR", unset = file.path(CVM_RAW_DIR, "crosswalks"))
OUT_DIR       <- file.path(CVM_DATA_DIR, "reconstruido_cvm")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("[107] Reconstruindo SH_%d.csv a partir de dado CVM bruto\n", ANO))

# ----- 1) inf_diario_fi do ano alvo (1 CSV por mes) -----
# NOTA (16/08/2026): a partir de dez/2023 (reforma CVM 175, fundo -> classe/subclasse)
# a CVM renomeou CNPJ_FUNDO->CNPJ_FUNDO_CLASSE e TP_FUNDO->TP_FUNDO_CLASSE no inf_diario.
# le_normalizado() detecta e renomeia de volta pro nome antigo, arquivo por arquivo (nao
# depois do rbind), senao rbindlist(fill=TRUE) cria as duas colunas com NA cruzado e mistura
# schemas silenciosamente dentro do mesmo ano.
le_normalizado <- function(path) {
  d <- fread(path, encoding="UTF-8")
  if ("CNPJ_FUNDO_CLASSE" %in% names(d)) {
    setnames(d, c("TP_FUNDO_CLASSE","CNPJ_FUNDO_CLASSE"), c("TP_FUNDO","CNPJ_FUNDO"))
  }
  d
}
arqs_mes <- sprintf(file.path(CVM_RAW_DIR, "inf_diario_fi_%d%02d.csv"), ANO, 1:12)
arqs_mes <- arqs_mes[file.exists(arqs_mes)]
if (length(arqs_mes) == 0) stop(sprintf(
  "Nenhum inf_diario_fi_%d*.csv encontrado em %s. Baixe e extraia primeiro (ver cabecalho do script).",
  ANO, CVM_RAW_DIR))
inf <- rbindlist(lapply(arqs_mes, le_normalizado), fill=TRUE)
cat(sprintf("  inf_diario: %d arquivos mensais, %d linhas\n", length(arqs_mes), nrow(inf)))

# CRITICO (achado 16/08/2026): a partir da reforma CVM 175, um mesmo CNPJ_FUNDO_CLASSE
# pode ter VARIAS SUBCLASSES na mesma data (ID_SUBCLASSE), cada uma com seu proprio
# VL_PATRIM_LIQ/CAPTC_DIA/NR_COTST. Sem agregar, SH ficaria com linhas duplicadas por
# fundo/data com valores parciais (o mesmo bug que inflou o peso de propriedade ate' ~20x
# no script 108 antes deste fix). Agrega por soma (campos aditivos) e usa a COTA da
# subclasse de maior PL como representativa (preco de cota nao e' somavel entre subclasses).
if ("ID_SUBCLASSE" %in% names(inf)) {
  n_antes <- nrow(inf)
  setorder(inf, CNPJ_FUNDO, DT_COMPTC, -VL_PATRIM_LIQ)
  inf <- inf[, .(
    TP_FUNDO   = TP_FUNDO[1],
    VL_QUOTA   = VL_QUOTA[1],       # da subclasse de maior PL (nao e' somavel)
    VL_PATRIM_LIQ = sum(VL_PATRIM_LIQ, na.rm=TRUE),
    CAPTC_DIA  = sum(CAPTC_DIA, na.rm=TRUE),
    RESG_DIA   = sum(RESG_DIA, na.rm=TRUE),
    NR_COTST   = sum(NR_COTST, na.rm=TRUE)
  ), by=.(CNPJ_FUNDO, DT_COMPTC)]
  cat(sprintf("  Agregado por subclasse: %d linhas -> %d apos consolidar\n", n_antes, nrow(inf)))
}

# ----- 2) lookup GESTORA/CLASSIFICACAO/NOME por carry-forward (anos ja existentes) -----
anos_existentes <- 2016:(ANO-1)
anos_existentes <- anos_existentes[file.exists(file.path(CVM_DATA_DIR, sprintf("SH_%d.csv", anos_existentes)))]
lookup_hist <- data.table()
for (a in anos_existentes) {
  sh <- fread(file.path(CVM_DATA_DIR, sprintf("SH_%d.csv", a)), encoding="UTF-8",
              select=c("CNPJ","NOME_FUNDO","GESTORA","CLASSIFICACAO_ANBIMA"))
  sh <- unique(sh[GESTORA!="" | CLASSIFICACAO_ANBIMA!=""], by="CNPJ")
  sh[, ano_origem := a]
  lookup_hist <- rbind(lookup_hist, sh, fill=TRUE)
}
if (nrow(lookup_hist) > 0) {
  setorder(lookup_hist, CNPJ, -ano_origem)
  lookup_hist <- unique(lookup_hist, by="CNPJ")
}
cat(sprintf("  Carry-forward: %d CNPJs com GESTORA/CLASSIFICACAO conhecida de anos %s\n",
    nrow(lookup_hist), paste(range(anos_existentes), collapse="-")))

# ----- 3) fallback pra fundos NOVOS (nao cobertos pelo carry-forward) -----
fmt_cnpj <- function(x) {
  x <- sprintf("%014.0f", as.numeric(x))
  sprintf("%s.%s.%s/%s-%s", substr(x,1,2), substr(x,3,5), substr(x,6,8), substr(x,9,12), substr(x,13,14))
}
norm_nome <- function(x) {
  x <- toupper(trimws(x)); x <- iconv(x, from="", to="ASCII//TRANSLIT")
  x <- gsub("[^A-Z0-9 ]", " ", x); trimws(gsub("\\s+"," ",x))
}
registro_dir <- file.path(CVM_RAW_DIR, "registro_fundo_classe")
fallback_ok <- dir.exists(registro_dir) &&
  file.exists(file.path(CROSSWALK_DIR, "crosswalk_gestor_legal_para_marca_FINAL.csv")) &&
  file.exists(file.path(CROSSWALK_DIR, "crosswalk_tokens_nome_fundo_FINAL.csv"))

lookup_fallback <- data.table(CNPJ=character(0), GESTORA_fb=character(0), CLASSIFICACAO_fb=character(0))
if (fallback_ok) {
  fundo  <- fread(file.path(registro_dir, "registro_fundo.csv"), encoding="Latin-1")
  classe <- fread(file.path(registro_dir, "registro_classe.csv"), encoding="Latin-1")
  fundo[, CNPJ_FMT := fmt_cnpj(CNPJ_Fundo)]
  classe_ok <- unique(classe[!is.na(Classificacao_Anbima) & Classificacao_Anbima!="",
                              .(ID_Registro_Fundo, Classificacao_Anbima)], by="ID_Registro_Fundo")
  fundo <- merge(fundo, classe_ok, by="ID_Registro_Fundo", all.x=TRUE)
  fundo[, nome_n := norm_nome(Denominacao_Social)]

  cw_gestor <- fread(file.path(CROSSWALK_DIR, "crosswalk_gestor_legal_para_marca_FINAL.csv"))
  cw_tokens <- fread(file.path(CROSSWALK_DIR, "crosswalk_tokens_nome_fundo_FINAL.csv"))
  achar_por_nome <- function(nm) {
    for (i in seq_len(nrow(cw_tokens))) if (grepl(cw_tokens$padrao_regex[i], nm)) return(cw_tokens$marca[i])
    NA_character_
  }

  fundo <- merge(fundo, cw_gestor, by="Gestor", all.x=TRUE)  # coluna GESTORA vinda do crosswalk
  falta <- is.na(fundo$GESTORA) | fundo$GESTORA==""
  fundo$GESTORA[falta] <- vapply(fundo$nome_n[falta], achar_por_nome, character(1))

  lookup_fallback <- fundo[, .(CNPJ=CNPJ_FMT, GESTORA_fb=GESTORA, CLASSIFICACAO_fb=Classificacao_Anbima)]
  lookup_fallback <- unique(lookup_fallback, by="CNPJ")
  cat(sprintf("  Fallback (fundos novos): %d CNPJs no cadastro CVM 175 disponiveis como reserva\n", nrow(lookup_fallback)))
} else {
  cat("  AVISO: fallback desativado (registro_fundo_classe/ ou crosswalks/ ausente) -- fundos novos ficarao sem GESTORA/CLASSIFICACAO\n")
}

# ----- 4) monta SH final -----
sh_novo <- merge(inf, lookup_hist[, .(CNPJ, NOME_FUNDO, GESTORA, CLASSIFICACAO_ANBIMA)],
                  by.x="CNPJ_FUNDO", by.y="CNPJ", all.x=TRUE)
sh_novo <- merge(sh_novo, lookup_fallback, by.x="CNPJ_FUNDO", by.y="CNPJ", all.x=TRUE)
sh_novo[is.na(GESTORA) | GESTORA=="", GESTORA := GESTORA_fb]
sh_novo[is.na(CLASSIFICACAO_ANBIMA) | CLASSIFICACAO_ANBIMA=="", CLASSIFICACAO_ANBIMA := CLASSIFICACAO_fb]
sh_novo[is.na(NOME_FUNDO), NOME_FUNDO := ""]
sh_novo[is.na(GESTORA), GESTORA := ""]
sh_novo[is.na(CLASSIFICACAO_ANBIMA), CLASSIFICACAO_ANBIMA := ""]

# CRITICO (achado 16/08/2026): fundos que JA' existiam em 2016-2021 tem um
# COD_FUNDO numerico proprio da Economatica (estavel: verificado 0 excecoes
# em 4.426 CNPJs) usado como chave de continuidade em TODO o pipeline
# (scripts 50+ tratam fundos com o mesmo cod_fundo como o MESMO fundo ao
# longo do tempo). Se eu usasse o CNPJ como COD_FUNDO pra 2022+, um fundo
# continuando de 2021 pareceria "novo" no ano seguinte -- quebra a serie
# temporal por fundo. Usa o COD_FUNDO original via crosswalk quando existe;
# so' usa o CNPJ como ID novo pra fundos GENUINAMENTE novos (que nunca
# apareceram em 2016-2021), e isso e' estavel por construcao (mesmo CNPJ
# sempre gera o mesmo ID novo, nao precisa atualizar crosswalk entre anos).
crosswalk_cod <- fread(file.path(CVM_DATA_DIR, "crosswalk_cnpj_cod_fundo_MASTER.csv"))
sh_novo <- merge(sh_novo, crosswalk_cod, by.x="CNPJ_FUNDO", by.y="CNPJ", all.x=TRUE)
sh_novo[, COD_FUNDO_FINAL := fifelse(is.na(COD_FUNDO), CNPJ_FUNDO, as.character(COD_FUNDO))]

# formata no layout do SH_2021.csv (semicolon, DD/MM/YYYY, decimal virgula com R$)
fmt_brl <- function(x) sprintf("R$ %s", formatC(x, format="f", digits=2, big.mark=".", decimal.mark=","))
sh_out <- sh_novo[, .(
  COD_FUNDO = COD_FUNDO_FINAL,
  CNPJ = CNPJ_FUNDO,
  NOME_FUNDO = NOME_FUNDO,
  GESTORA = GESTORA,
  CLASSIFICACAO_ANBIMA = CLASSIFICACAO_ANBIMA,
  DATA = format(as.Date(DT_COMPTC), "%d/%m/%Y"),
  `APLICAÇÃO` = fmt_brl(CAPTC_DIA),
  COTA = VL_QUOTA,
  NUMERO_DE_COTISTAS = NR_COTST,
  `PATRIMONIO_LIQUIDO_(MIL)` = fmt_brl(VL_PATRIM_LIQ/1000)
)]

out_path <- file.path(OUT_DIR, sprintf("SH_%d.csv", ANO))
fwrite(sh_out, out_path, sep=";")
cat(sprintf("\n[107] Salvo: %s (%d linhas)\n", out_path, nrow(sh_out)))
cat(sprintf("  Cobertura GESTORA: %.1f%% | CLASSIFICACAO: %.1f%%\n",
    100*mean(sh_out$GESTORA!=""), 100*mean(sh_out$CLASSIFICACAO_ANBIMA!="")))
cat("  Revisar em reconstruido_cvm/ antes de mover pra CVM_DATA_DIR (nao sobrescreve automaticamente).\n")
