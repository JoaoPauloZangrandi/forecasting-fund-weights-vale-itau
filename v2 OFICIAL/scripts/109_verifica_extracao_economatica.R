# =============================================================================
# 109_verifica_extracao_economatica.R  (v2 OFICIAL/scripts)
#
# Compara uma extracao MANUAL do Economatica contra o arquivo de referencia de
# 2021 que o professor repassou. Serve pra calibrar as configuracoes da
# exportacao (consolidado/look-through ligado, universo de fundos correto,
# periodicidade, colunas) ANTES de extrair 2022-2026.
#
# USO:
#   Rscript 109_verifica_extracao_economatica.R SH   caminho/do/SH_2021.csv
#   Rscript 109_verifica_extracao_economatica.R cons caminho/do/cons_2021.csv
#
# Opcional: comparar contra outro ano de referencia
#   ANO_REFERENCIA=2020 Rscript 109_... SH caminho/do/SH_2020.csv
#
# O script NAO altera nada. So' le e reporta.
#
# Criado em 02/09/2026. Contexto: o Joao tem login de faculdade no Economatica
# e quer extrair 2022-2026 manualmente. Dado real do Economatica e' melhor que
# a reconstrucao CVM dos scripts 107/108, que tem teto de 9-18% de erro
# mediano por fundo. Este script existe pra confirmar que a extracao manual
# reproduz a base do professor antes de confiar nela.
# =============================================================================
suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Uso: Rscript 109_verifica_extracao_economatica.R <SH|cons> <arquivo_candidato.csv>")
}
TIPO      <- args[1]
CANDIDATO <- args[2]
ANO_REF   <- Sys.getenv("ANO_REFERENCIA", unset = "2021")
REF_DIR   <- Sys.getenv("CVM_DATA_DIR",
                        unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")

if (!TIPO %in% c("SH", "cons")) stop("Primeiro argumento deve ser 'SH' ou 'cons'.")
if (!file.exists(CANDIDATO))    stop("Arquivo candidato nao encontrado: ", CANDIDATO)

REFERENCIA <- file.path(REF_DIR, sprintf("%s_%s.csv", TIPO, ANO_REF))
if (!file.exists(REFERENCIA))   stop("Arquivo de referencia nao encontrado: ", REFERENCIA)

linha <- function(c = "-") cat(strrep(c, 78), "\n")
titulo <- function(x) { cat("\n"); linha("="); cat(x, "\n"); linha("=") }
# separador de milhar sem colidir com o decimal (evita aviso do prettyNum)
fmt <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
# veredito: OK / ATENCAO / ERRO
vd <- function(ok, atencao = FALSE) if (ok) "[ OK ]" else if (atencao) "[ATENCAO]" else "[ ERRO ]"

titulo(sprintf("VERIFICACAO DE EXTRACAO ECONOMATICA -- %s, referencia %s", TIPO, ANO_REF))
cat("Candidato  :", CANDIDATO, "\n")
cat("Referencia :", REFERENCIA, "\n")

# ---------------------------------------------------------------- cabecalho
# le so' a 1a linha crua dos dois, pra comparar nome e ordem de coluna
cab_ref  <- readLines(REFERENCIA, n = 1, encoding = "UTF-8", warn = FALSE)
cab_cand <- readLines(CANDIDATO,  n = 1, encoding = "UTF-8", warn = FALSE)
# remove BOM se presente, dos dois lados, pra nao acusar diferenca falsa
tira_bom <- function(x) sub("^\ufeff|^ï»¿", "", x)
bom_ref  <- cab_ref  != tira_bom(cab_ref)
bom_cand <- cab_cand != tira_bom(cab_cand)
cab_ref  <- tira_bom(cab_ref); cab_cand <- tira_bom(cab_cand)

titulo("1. CABECALHO E CODIFICACAO")
cols_ref  <- strsplit(cab_ref,  ";", fixed = TRUE)[[1]]
cols_cand <- strsplit(cab_cand, ";", fixed = TRUE)[[1]]
cat(vd(identical(cols_ref, cols_cand)), "colunas identicas, na mesma ordem\n")
if (!identical(cols_ref, cols_cand)) {
  cat("  esperado:", paste(cols_ref,  collapse = " | "), "\n")
  cat("  recebido:", paste(cols_cand, collapse = " | "), "\n")
  faltando <- setdiff(cols_ref, cols_cand); sobrando <- setdiff(cols_cand, cols_ref)
  if (length(faltando)) cat("  FALTANDO :", paste(faltando, collapse = ", "), "\n")
  if (length(sobrando)) cat("  A MAIS   :", paste(sobrando, collapse = ", "), "\n")
}
cat(vd(bom_cand == bom_ref, atencao = TRUE),
    sprintf("BOM: referencia=%s candidato=%s\n", bom_ref, bom_cand))
# acento correto e' o teste pratico de UTF-8
acento_ok <- !grepl("Ã|Â", cab_cand)
cat(vd(acento_ok, atencao = TRUE),
    "acentuacao do cabecalho", if (acento_ok) "" else "-> parece nao ser UTF-8", "\n")

# ---------------------------------------------------------------- leitura
cat("\nLendo os dois arquivos (pode demorar em cons)...\n")
ref  <- fread(REFERENCIA, sep = ";", encoding = "UTF-8", showProgress = FALSE)
cand <- fread(CANDIDATO,  sep = ";", encoding = "UTF-8", showProgress = FALSE)

titulo("2. TAMANHO")
cat(sprintf("  linhas     referencia %12s | candidato %12s  %s\n",
            fmt(nrow(ref)), fmt(nrow(cand)),
            vd(abs(nrow(cand) - nrow(ref)) / nrow(ref) < 0.02, atencao = TRUE)))

# nomes de coluna variam entre SH e cons; resolve aqui
col_fundo <- if (TIPO == "SH") "COD_FUNDO" else "Código"
col_data  <- if (TIPO == "SH") "DATA"      else "Data_Competência"
col_class <- if (TIPO == "SH") "CLASSIFICACAO_ANBIMA" else "Anbima"

if (!all(c(col_fundo, col_data, col_class) %in% names(cand))) {
  cat("\n[ ERRO ] candidato nao tem as colunas-chave; parando as comparacoes de conteudo.\n")
  quit(status = 1)
}

f_ref  <- unique(ref[[col_fundo]]);  f_cand <- unique(cand[[col_fundo]])
d_ref  <- unique(ref[[col_data]]);   d_cand <- unique(cand[[col_data]])

cat(sprintf("  fundos     referencia %12s | candidato %12s\n",
            fmt(length(f_ref)), fmt(length(f_cand))))
cat(sprintf("  datas      referencia %12s | candidato %12s\n",
            length(d_ref), length(d_cand)))

titulo("3. PERIODICIDADE")
esperado <- if (TIPO == "SH") "diaria (~252 pregoes)" else "mensal (12 datas, ultimo pregao do mes)"
cat("  esperado:", esperado, "\n")
ok_per <- if (TIPO == "SH") length(d_cand) > 200 else length(d_cand) == 12
cat(vd(ok_per), sprintf("candidato tem %d datas distintas\n", length(d_cand)))
if (!ok_per && TIPO == "cons" && length(d_cand) > 12)
  cat("  -> extraiu em frequencia mais fina que mensal. Refazer com periodicidade mensal.\n")
if (!ok_per && TIPO == "SH" && length(d_cand) < 200)
  cat("  -> SH precisa ser DIARIO; sem 252 pregoes o beta de 252 dias nao fecha.\n")

titulo("4. UNIVERSO DE FUNDOS (o erro mais comum)")
inter <- length(intersect(f_ref, f_cand))
cat(sprintf("  na referencia e no candidato : %s\n", fmt(inter)))
cat(sprintf("  so' na referencia (PERDIDOS) : %s\n", fmt(length(setdiff(f_ref, f_cand)))))
cat(sprintf("  so' no candidato (A MAIS)    : %s\n", fmt(length(setdiff(f_cand, f_ref)))))
cob <- inter / length(f_ref)
cat(vd(cob > 0.97, atencao = cob > 0.90), sprintf("cobertura da referencia: %.1f%%\n", 100 * cob))

# classificacao ANBIMA: e' aqui que "filtrei so' fundo de acoes" aparece
titulo("5. CLASSIFICACAO ANBIMA (detecta filtro indevido de fundo)")
cls <- function(dt) {
  u <- unique(dt[, c(col_fundo, col_class), with = FALSE])
  setnames(u, c("f", "cl")); u[, .N, by = cl][order(-N)]
}
cr <- cls(ref); cc <- cls(cand)
cmp <- merge(cr, cc, by = "cl", all = TRUE, suffixes = c("_ref", "_cand"))
cmp[is.na(N_ref), N_ref := 0L][is.na(N_cand), N_cand := 0L]
cmp <- cmp[order(-N_ref)]
cat(sprintf("  %-42s %8s %8s\n", "classificacao", "ref", "cand"))
for (i in seq_len(nrow(cmp)))
  cat(sprintf("  %-42s %8d %8d %s\n", cmp$cl[i], cmp$N_ref[i], cmp$N_cand[i],
              if (cmp$N_ref[i] > 0 && cmp$N_cand[i] == 0) "  <-- SUMIU" else ""))
nao_acoes_ref  <- cmp[!grepl("^Ações", cl), sum(N_ref)]
nao_acoes_cand <- cmp[!grepl("^Ações", cl), sum(N_cand)]
cat(sprintf("\n  fundos que NAO sao 'Ações *': referencia %d | candidato %d\n",
            nao_acoes_ref, nao_acoes_cand))
if (nao_acoes_ref > 0 && nao_acoes_cand < 0.5 * nao_acoes_ref) {
  cat("  [ ERRO ] Voce provavelmente filtrou 'somente fundos de acoes'.\n")
  cat("           A base de referencia inclui Multimercados Livre e ETF.\n")
  cat("           Tire o filtro de CLASSE DE FUNDO; mantenha so' o filtro de ATIVO = Acoes.\n")
} else {
  cat("  [ OK ] proporcao de fundos nao-acoes compativel com a referencia\n")
}

# ---------------------------------------------------------------- conteudo
titulo("6. CONTEUDO")

num_br <- function(x) {  # "R$ 20.729,09" e "3,192127" -> numerico
  x <- gsub("R\\$|\\s", "", as.character(x))
  as.numeric(gsub(",", ".", gsub("\\.", "", x), fixed = FALSE))
}
num_us <- function(x) as.numeric(as.character(x))  # ja' com ponto decimal

if (TIPO == "cons") {
  if (!"Tipo_Ativo" %in% names(cand)) {
    cat("[ ERRO ] falta a coluna Tipo_Ativo\n")
  } else {
    tp <- unique(cand$Tipo_Ativo)
    cat(vd(length(tp) == 1 && tp[1] == "Ações", atencao = TRUE),
        "Tipo_Ativo unico = 'Ações'. Encontrado:", paste(head(tp, 5), collapse = ", "), "\n")
  }
  # composicao do Nome_Ativo. Medido sobre LINHAS, nao sobre nomes distintos:
  # a cauda de companhia fechada e' enorme em nomes distintos (23%) e irrisoria
  # em linhas (0,03%). Referencia 2021: 98,8% acao listada com ticker.
  if ("Nome_Ativo" %in% names(cand)) {
    categoriza <- function(x)
      fifelse(grepl(" - [A-Z]{4}[0-9]{1,2}$", x), "acao listada (ticker)",
      fifelse(grepl("Companhia Fechada", x),      "companhia fechada",
      fifelse(grepl("Direito de Subscri", x),     "direito de subscricao",
      fifelse(grepl("omitidas", x),               "omitidas/confidencial",
                                                  "outros"))))
    br <- data.table(cat = categoriza(cand$Nome_Ativo))[, .N, by = cat][order(-N)]
    br[, pct := 100 * N / sum(N)]
    cat("  composicao do Nome_Ativo (por linha):\n")
    for (i in seq_len(nrow(br)))
      cat(sprintf("    %-26s %6.2f%%\n", br$cat[i], br$pct[i]))
    com_ticker <- br[cat == "acao listada (ticker)", sum(pct)] / 100
    cat(vd(com_ticker > 0.90, atencao = com_ticker > 0.60),
        sprintf("%.1f%% das linhas sao acao listada (referencia 2021: 98,8%%)\n",
                100 * com_ticker))
    if (com_ticker < 0.60)
      cat("  -> proporcao baixa sugere CONSOLIDACAO/LOOK-THROUGH DESLIGADA.\n")
  }
  # participacao: fracao (0 a 1), nao porcentagem. Usa quantil, nao maximo:
  # a referencia 2021 tem 16 linhas em 5,9 milhoes acima de 1 (registro
  # corrompido na fonte, ex. fundo 616291 em out/2021 com 4781), entao o
  # maximo nao discrimina nada. O quantil 99,9% discrimina: fracao ~0,12,
  # porcentagem seria ~12.
  if ("Participação_Ativo" %in% names(cand)) {
    p  <- num_us(cand[["Participação_Ativo"]])
    q999 <- quantile(p, 0.999, na.rm = TRUE)
    cat(sprintf("  Participação_Ativo: mediana %.6f | q99,9 %.4f | acima de 1: %d linhas\n",
                median(p, na.rm = TRUE), q999, sum(p > 1, na.rm = TRUE)))
    cat(vd(q999 < 1, atencao = TRUE),
        "escala de fracao (0 a 1), nao porcentagem\n")
    if (q999 >= 1)
      cat("  -> parece porcentagem. Divida por 100 ou reexporte como fracao.\n")
  }
  # soma da participacao por fundo-mes: o teste mais informativo de todos
  ch <- c(col_fundo, col_data)
  sr <- ref[,  .(s = sum(num_us(get("Participação_Ativo")), na.rm = TRUE)), by = ch]
  sc <- cand[, .(s = sum(num_us(get("Participação_Ativo")), na.rm = TRUE)), by = ch]
  m  <- merge(sr, sc, by = ch, suffixes = c("_ref", "_cand"))
  if (nrow(m) > 0) {
    m[, err := abs(s_cand - s_ref) / pmax(s_ref, 1e-9)]
    cat(sprintf("\n  soma da participacao por fundo-mes, %s pares comparaveis\n",
                fmt(nrow(m))))
    cat(sprintf("    erro mediano %.2f%% | %% de pares com erro <1%%: %.1f%%\n",
                100 * median(m$err, na.rm = TRUE), 100 * mean(m$err < 0.01, na.rm = TRUE)))
    cat(vd(median(m$err, na.rm = TRUE) < 0.01, atencao = median(m$err, na.rm = TRUE) < 0.05),
        "acordo com a referencia\n")
  } else {
    cat("\n  [ATENCAO] nenhum par fundo-mes comparavel; confira formato de data e codigo de fundo\n")
  }
}

if (TIPO == "SH") {
  for (cl in c("COTA", "NUMERO_DE_COTISTAS", "PATRIMONIO_LIQUIDO_(MIL)")) {
    if (!cl %in% names(cand)) { cat("[ ERRO ] falta a coluna", cl, "\n"); next }
    conv <- if (cl == "NUMERO_DE_COTISTAS") num_us else num_br
    ch <- c(col_fundo, col_data)
    a <- ref[,  .(v = conv(get(cl))[1]), by = ch]
    b <- cand[, .(v = conv(get(cl))[1]), by = ch]
    m <- merge(a, b, by = ch, suffixes = c("_ref", "_cand"))
    if (nrow(m) == 0) { cat("  [ATENCAO]", cl, ": nenhum par comparavel\n"); next }
    m[, err := abs(v_cand - v_ref) / pmax(abs(v_ref), 1e-9)]
    ident <- mean(m$err < 1e-6, na.rm = TRUE)
    cat(sprintf("  %-26s %s pares | identicos %.1f%% | erro mediano %.4f%%  %s\n",
                cl, fmt(nrow(m)), 100 * ident,
                100 * median(m$err, na.rm = TRUE),
                vd(ident > 0.98, atencao = ident > 0.90)))
  }
  if ("GESTORA" %in% names(cand)) {
    g_ref <- length(unique(ref$GESTORA)); g_cand <- length(unique(cand$GESTORA))
    vaz <- mean(is.na(cand$GESTORA) | trimws(cand$GESTORA) == "")
    cat(sprintf("\n  GESTORA: %d marcas na referencia, %d no candidato, %.1f%% vazias\n",
                g_ref, g_cand, 100 * vaz))
    cat(vd(vaz < 0.02, atencao = vaz < 0.10),
        "coluna GESTORA preenchida (e' a marca controladora ja' normalizada)\n")
  }
}

titulo("VEREDITO")
cat("Leia de cima para baixo. Qualquer [ ERRO ] invalida a extracao:\n")
cat("  secao 1  cabecalho errado    -> escolha de colunas na exportacao\n")
cat("  secao 3  periodicidade       -> SH e' diario, cons e' mensal\n")
cat("  secao 4  cobertura baixa     -> universo de fundos restrito demais\n")
cat("  secao 5  nao-acoes sumiram   -> filtro indevido de CLASSE DE FUNDO\n")
cat("  secao 6  ticker/soma ruins   -> consolidacao (look-through) desligada\n")
cat("\nSe tudo der [ OK ] em 2021, use exatamente essas configuracoes para 2022-2026.\n")
