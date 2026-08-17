# =============================================================================
# 108_reconstroi_cons_cvm.R  (v2 OFICIAL/scripts)
#
# Reconstroi um cons_<ANO>.csv (composicao de carteira, formato identico ao
# cons_2016..cons_2021.csv da Economatica) a partir do CDA (Composicao e
# Diversificacao de Aplicacoes) publico da CVM, com LOOK-THROUGH RECURSIVO
# pra resolver fundos-de-fundos (FIC): quando um fundo investe em outro
# fundo em vez de acao direta, a exposicao a' acao e' herdada
# proporcionalmente (peso = valor investido no fundo-alvo / PL do fundo-alvo),
# recursivamente ate' achar holdings diretos ou esgotar profundidade maxima.
# Criado em 15-16/08/2026, ver PLANO_EXPANSAO_2021_2026.md secao
# "Reconstrucao CVM x Economatica" pra validacao completa.
#
# ACHADO CENTRAL (o motivo desse script existir): SEM look-through, so' ~19%
# dos fundos que reportam Ações no cons_2021.csv real batem com o CDA bruto
# da CVM -- os outros ~81% sao FICs (fundo-de-fundos) cuja exposicao a acao
# so' aparece indiretamente via BLC_2 (cotas de outro fundo), nunca no BLC_4
# do proprio fundo. O look-through resolve isso.
#
# VALIDACAO (4 meses/anos testados: dez/2019, dez/2020, dez/2021, jun/2021):
#   entre fundos onde CVM E Economatica reportam alguma posicao em Acoes
#   (~80-85% do universo conhecido), erro mediano no total por fundo:
#   9%-18% (varia por mes/ano, sem padrao limpo tipo "dezembro e' sempre
#   melhor"), 53%-82% dos fundos com erro <20%. TETO CONHECIDO -- nao e'
#   bug, e' resíduo de reconciliacao proprietaria da Economatica que nao
#   esta' no dado bruto publico. Nao afeta a fragilidade do sinal HHI, que
#   agrega centenas de fundos por acao (ruido individual se cancela no
#   agregado). ~15-18% dos fundos por mes ficam sem match nenhum dos dois
#   lados (falso positivo/negativo) -- tipicamente lacuna de relato do CDA
#   naquele mes especifico, nao erro sistematico.
#
# CORRECAO METODOLOGICA aplicada (testada, nao reduz o erro mas e' mais
# correta): soma TP_APLIC "Ações" + "Ações e outros TVM cedidos em
# empréstimo" (posicao ainda e' do fundo mesmo emprestada) e SUBTRAI
# "Obrigações por ações e outros TVM recebidos em empréstimo" (posicao
# tomada emprestada, e' passivo/short).
#
# PRE-REQUISITOS (baixar manualmente antes de rodar):
#   1. cda_fi_BLC_2_<ANO>.csv e cda_fi_BLC_4_<ANO>.csv em CVM_RAW_DIR/cda_<ANO>/
#      URL (ano >= 2023): https://dados.cvm.gov.br/dados/FI/DOC/CDA/DADOS/cda_fi_<AAAAMM>.zip (1 por mes)
#      URL (ano <= 2022): https://dados.cvm.gov.br/dados/FI/DOC/CDA/DADOS/HIST/cda_fi_<AAAA>.zip (1 zip, ano inteiro)
#   2. inf_diario_fi do mesmo ano (mesmo pre-requisito do script 107) -- usado
#      como fonte do PL (patrimonio liquido) de cada fundo-alvo, denominador
#      do peso de propriedade no look-through
# =============================================================================
suppressPackageStartupMessages(library(data.table))

ANO       <- as.integer(Sys.getenv("ANO_RECONSTRUCAO", unset = "2022"))
MAX_DEPTH <- as.integer(Sys.getenv("LOOKTHROUGH_MAX_DEPTH", unset = "20"))
# 20 (nao 6): testado em 15-16/08/2026 -- com profundidade 6 as cadeias eram
# REALMENTE cortadas (fronteira nao zerava no nivel 6: ~27-29 mil pares
# raiz/fundo-intermediario ainda ativos), mas o efeito no erro final e'
# desprezivel (mediana identica em 3 dos 4 meses/anos testados, dez/2019
# 17.5%->17.4%) porque o peso de propriedade nos niveis profundos ja' e'
# muito pequeno. Aumentado mesmo assim por ser mais completo/correto e o
# custo computacional extra ser baixo (cadeias reais terminam sozinhas por
# volta do nivel 14-15).

CVM_DATA_DIR <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
CVM_RAW_DIR  <- Sys.getenv("CVM_RAW_DIR", unset = file.path(CVM_DATA_DIR, "cvm_raw_downloads"))
OUT_DIR      <- file.path(CVM_DATA_DIR, "reconstruido_cvm")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("[108] Reconstruindo cons_%d.csv via look-through recursivo (CDA CVM)\n", ANO))

# NOTA (16/08/2026): a partir de dez/2023 (reforma CVM 175) a CVM renomeou colunas do CDA
# (CNPJ_FUNDO->CNPJ_FUNDO_CLASSE em BLC_4; em BLC_2 alem disso INSERIU uma coluna nova
# ID_SUBCLASSE no meio, mudando a CONTAGEM de colunas). Concatenar arquivos mensais de
# schema diferente direto em bash (cat/tail) desalinha colunas silenciosamente pro BLC_2 --
# por isso aqui SEMPRE le mes a mes (quando disponivel) e normaliza em R antes de juntar,
# nunca confia num CSV anual pre-concatenado fora do R pra BLC_2.
normaliza_blc2 <- function(d) {
  if ("CNPJ_FUNDO_CLASSE" %in% names(d)) {
    setnames(d, c("TP_FUNDO_CLASSE","CNPJ_FUNDO_CLASSE","CNPJ_FUNDO_CLASSE_COTA","NM_FUNDO_CLASSE_SUBCLASSE_COTA"),
                c("TP_FUNDO","CNPJ_FUNDO","CNPJ_FUNDO_COTA","NM_FUNDO_COTA"))
    d[, ID_SUBCLASSE := NULL]
  }
  d
}
normaliza_blc4 <- function(d) {
  if ("CNPJ_FUNDO_CLASSE" %in% names(d)) setnames(d, c("TP_FUNDO_CLASSE","CNPJ_FUNDO_CLASSE"), c("TP_FUNDO","CNPJ_FUNDO"))
  d
}

le_cda_por_mes <- function(bloco, normalizador) {
  dir_mensal <- file.path(CVM_RAW_DIR, "zips_tmp_cda", ANO)
  arqs <- sprintf(file.path(dir_mensal, sprintf("cda_fi_%s_%%d%%02d.csv", bloco)), ANO, 1:12)
  arqs <- arqs[file.exists(arqs)]
  if (length(arqs) > 0) {
    cat(sprintf("  %s: lendo %d arquivos mensais (schema normalizado por mes)\n", bloco, length(arqs)))
    return(rbindlist(lapply(arqs, function(p) normalizador(fread(p, encoding="Latin-1"))), fill=TRUE))
  }
  # fallback: 1 CSV anual ja' extraido (ex: 2022, vindo do HIST/cda_fi_2022.zip, schema unico)
  path_anual <- file.path(CVM_RAW_DIR, sprintf("cda_%d", ANO), sprintf("cda_fi_%s_%d.csv", bloco, ANO))
  if (!file.exists(path_anual)) stop(sprintf(
    "Nem mensal (%s) nem anual (%s) encontrados pra %s/%d.", dir_mensal, path_anual, bloco, ANO))
  cat(sprintf("  %s: lendo arquivo anual unico %s\n", bloco, path_anual))
  normalizador(fread(path_anual, encoding="Latin-1"))
}

blc2_full <- le_cda_por_mes("BLC_2", normaliza_blc2)
blc4_full <- le_cda_por_mes("BLC_4", normaliza_blc4)

le_inf_normalizado <- function(path) {
  d <- fread(path, encoding="UTF-8")
  if ("CNPJ_FUNDO_CLASSE" %in% names(d)) setnames(d, c("TP_FUNDO_CLASSE","CNPJ_FUNDO_CLASSE"), c("TP_FUNDO","CNPJ_FUNDO"))
  d
}
arqs_inf <- sprintf(file.path(CVM_RAW_DIR, "inf_diario_fi_%d%02d.csv"), ANO, 1:12)
arqs_inf <- arqs_inf[file.exists(arqs_inf)]
if (length(arqs_inf) == 0) stop(sprintf("inf_diario_fi de %d nao encontrado em %s.", ANO, CVM_RAW_DIR))
inf_full <- rbindlist(lapply(arqs_inf, le_inf_normalizado), fill=TRUE)

sh_ano_anterior <- max((2016:ANO)[file.exists(file.path(CVM_DATA_DIR, sprintf("SH_%d.csv", 2016:ANO)))])
sh_ref <- fread(file.path(CVM_DATA_DIR, sprintf("SH_%d.csv", sh_ano_anterior)), encoding="UTF-8",
                select=c("CNPJ"))
crosswalk_universo <- unique(sh_ref$CNPJ)
cat(sprintf("  Universo de referencia: %d CNPJs (de SH_%d.csv, o ano conhecido mais recente)\n",
    length(crosswalk_universo), sh_ano_anterior))

lookthrough_1_mes <- function(data_alvo) {
  blc2_d <- blc2_full[DT_COMPTC==data_alvo & !is.na(CNPJ_FUNDO_COTA) & CNPJ_FUNDO_COTA != "",
                       .(CNPJ_FUNDO, CNPJ_FUNDO_COTA, VL_MERC_POS_FINAL)]
  blc4_pos <- blc4_full[DT_COMPTC==data_alvo & TP_APLIC %in%
                         c("Ações","Ações e outros TVM cedidos em empréstimo"),
                         .(CNPJ_FUNDO, CD_ATIVO, DS_ATIVO, VL_MERC_POS_FINAL)]
  blc4_neg <- blc4_full[DT_COMPTC==data_alvo & TP_APLIC=="Obrigações por ações e outros TVM recebidos em empréstimo",
                         .(CNPJ_FUNDO, CD_ATIVO, DS_ATIVO, VL_MERC_POS_FINAL = -VL_MERC_POS_FINAL)]
  blc4_d <- rbind(blc4_pos, blc4_neg)
  if (nrow(blc4_d) == 0) return(NULL)
  blc4_d <- blc4_d[, .(VL_MERC_POS_FINAL=sum(VL_MERC_POS_FINAL)), by=.(CNPJ_FUNDO,CD_ATIVO,DS_ATIVO)]

  # CRITICO (achado 16/08/2026): a partir da reforma CVM 175, um mesmo CNPJ_FUNDO_CLASSE
  # pode ter VARIAS SUBCLASSES na mesma data (ID_SUBCLASSE diferente), cada uma com seu
  # proprio VL_PATRIM_LIQ. unique()/first-row aqui pegaria o PL de UMA SO' subclasse
  # (as vezes a menor) como denominador do peso de propriedade, inflando o peso em ate'
  # ~20x nos casos observados -- por isso SEMPRE soma o PL de todas as subclasses do
  # mesmo CNPJ_FUNDO antes de usar como denominador (156% de erro documentado numa
  # comparacao de continuidade 2025->2026 antes deste fix).
  inf_d <- inf_full[DT_COMPTC==data_alvo, .(VL_PATRIM_LIQ=sum(VL_PATRIM_LIQ, na.rm=TRUE)), by=CNPJ_FUNDO]
  if (nrow(inf_d) == 0) return(NULL)

  raizes <- intersect(crosswalk_universo, inf_d$CNPJ_FUNDO)
  if (length(raizes) == 0) return(NULL)

  fronteira <- data.table(raiz = raizes, atual = raizes, peso = 1.0)
  resultado_list <- list()
  for (nivel in 1:MAX_DEPTH) {
    if (nrow(fronteira) == 0) break
    diretas <- merge(fronteira, blc4_d, by.x="atual", by.y="CNPJ_FUNDO", allow.cartesian=TRUE)
    if (nrow(diretas) > 0) {
      diretas[, valor := VL_MERC_POS_FINAL * peso]
      resultado_list[[length(resultado_list)+1]] <- diretas[, .(raiz, CD_ATIVO, DS_ATIVO, valor)]
    }
    expand <- merge(fronteira, blc2_d, by.x="atual", by.y="CNPJ_FUNDO", allow.cartesian=TRUE)
    if (nrow(expand) == 0) break
    expand <- merge(expand, inf_d, by.x="CNPJ_FUNDO_COTA", by.y="CNPJ_FUNDO")
    expand <- expand[VL_PATRIM_LIQ > 0]
    expand[, peso_novo := peso * VL_MERC_POS_FINAL / VL_PATRIM_LIQ]
    nova_fronteira <- expand[, .(raiz, atual = CNPJ_FUNDO_COTA, peso = peso_novo)]
    nova_fronteira <- nova_fronteira[atual != raiz & !is.na(peso) & peso > 0]
    fronteira <- nova_fronteira
  }
  if (length(resultado_list) == 0) return(NULL)
  agregado <- rbindlist(resultado_list)[, .(valor_mil = sum(valor)/1000), by=.(raiz, CD_ATIVO, DS_ATIVO)]
  agregado[, DT_COMPTC := data_alvo]
  agregado
}

datas_mes <- sort(unique(inf_full$DT_COMPTC))
# so' usa a ULTIMA data de cada mes (fim de mes, mesma convencao do cons_YYYY.csv real)
datas_mes_dt <- as.Date(datas_mes)
datas_fim_mes <- tapply(datas_mes_dt, format(datas_mes_dt, "%Y-%m"), max)
datas_fim_mes <- as.character(as.Date(datas_fim_mes, origin="1970-01-01"))

cat(sprintf("  Rodando look-through pra %d datas de fim-de-mes: %s\n",
    length(datas_fim_mes), paste(datas_fim_mes, collapse=", ")))

todos_meses <- lapply(datas_fim_mes, function(d) {
  cat(sprintf("    %s...\n", d))
  lookthrough_1_mes(d)
})
final <- rbindlist(todos_meses[!sapply(todos_meses, is.null)])
cat(sprintf("\n  Total: %d linhas (raiz,ativo,mes), %d fundos com posicao\n",
    nrow(final), uniqueN(final$raiz)))

# CRITICO (achado 16/08/2026): Participação_Ativo NAO pode ficar vazio -- scripts
# downstream do pipeline oficial (ex.: 51_painel_multiativo_universo_completo.R)
# filtram por `PESO := as.numeric(Participação_Ativo)` e descartam silenciosamente
# toda linha onde isso da' NA. Formula confirmada batendo exato contra cons_2021.csv
# real (2 ativos, mesmo fundo, razao identica): Participacao = Valor_Ativo_mil /
# (PL_do_fundo_na_data_em_mil) -- MESMO denominador (PL do fundo raiz, nao do
# fundo intermediario da cadeia) pra todas as posicoes daquele fundo/data.
pl_raiz <- inf_full[, .(VL_PATRIM_LIQ=sum(VL_PATRIM_LIQ, na.rm=TRUE)), by=.(CNPJ_FUNDO, DT_COMPTC)]
final <- merge(final, pl_raiz, by.x=c("raiz","DT_COMPTC"), by.y=c("CNPJ_FUNDO","DT_COMPTC"), all.x=TRUE)
final[, participacao := fifelse(!is.na(VL_PATRIM_LIQ) & VL_PATRIM_LIQ>0,
                                  valor_mil / (VL_PATRIM_LIQ/1000), NA_real_)]
cat(sprintf("  Participacao_Ativo calculada: %.1f%% das linhas com PL do fundo disponivel\n",
    100*mean(!is.na(final$participacao))))

# ----- formata no layout do cons_2021.csv -----
# Anbima/Nome/COD_FUNDO (classificacao/nome/ID estavel do fundo) vem do SH_<ANO>
# reconstruido pelo script 107 (que PRECISA rodar antes, senao Codigo fica so'
# o CNPJ pra todo mundo -- quebra a continuidade cod_fundo entre anos, mesmo
# bug critico documentado no cabecalho do script 107)
sh_atual <- file.path(OUT_DIR, sprintf("SH_%d.csv", ANO))
info_fundo <- data.table(CNPJ=character(0), Anbima=character(0), Nome=character(0), COD_FUNDO=character(0))
if (file.exists(sh_atual)) {
  sh <- fread(sh_atual, encoding="UTF-8", select=c("CNPJ","NOME_FUNDO","CLASSIFICACAO_ANBIMA","COD_FUNDO"))
  info_fundo <- unique(sh[, .(CNPJ, Anbima=CLASSIFICACAO_ANBIMA, Nome=NOME_FUNDO, COD_FUNDO=as.character(COD_FUNDO))], by="CNPJ")
} else {
  stop(sprintf("SH_%d.csv nao encontrado em %s -- rode o script 107 pra este ano ANTES do 108 (Codigo precisa do COD_FUNDO estavel de la').", ANO, OUT_DIR))
}
final <- merge(final, info_fundo, by.x="raiz", by.y="CNPJ", all.x=TRUE)

# CRITICO (achado 16/08/2026): o cons_YYYY.csv ORIGINAL da Economatica usa PONTO
# decimal (nao virgula) em Valor_Ativo_mil e Participação_Ativo -- confirmado lendo
# o cons_2021.csv real direto ("5.361403671356667", "6.369346644105552E-4"). SO' o
# SH_YYYY.csv usa virgula (com prefixo "R$"). Usar fmt_num_br (virgula) aqui, como
# fiz na 1a versao, corrompe o tipo da coluna silenciosamente pro script 51 (que le
# Valor_Ativo_mil sem conversao, so' funciona se vier numerico de verdade) e zera
# Participação_Ativo inteiro (fread nao autoconverte texto com virgula, as.numeric()
# de "0,05" retorna NA, nao 0.05).
cons_out <- final[, .(
  CNPJ = raiz,
  Anbima = fifelse(is.na(Anbima), "", Anbima),
  Código = fifelse(is.na(COD_FUNDO), raiz, COD_FUNDO),
  Nome = fifelse(is.na(Nome), "", Nome),
  Tipo_Ativo = "Ações",
  Data_Competência = DT_COMPTC,
  Nome_Ativo = paste(DS_ATIVO, "-", CD_ATIVO),
  Valor_Ativo_mil = valor_mil,
  Participação_Ativo = participacao
)]

out_path <- file.path(OUT_DIR, sprintf("cons_%d.csv", ANO))
fwrite(cons_out, out_path, sep=";")
cat(sprintf("\n[108] Salvo: %s (%d linhas)\n", out_path, nrow(cons_out)))
cat("  LIMITACAO CONHECIDA: erro mediano ~9-18%% por fundo (ver cabecalho do script). Revisar\n")
cat("  em reconstruido_cvm/ antes de mover pra CVM_DATA_DIR (nao sobrescreve automaticamente).\n")
