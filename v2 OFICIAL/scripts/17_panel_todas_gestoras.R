# =============================================================================
# 17_panel_todas_gestoras.R  (v2 OFICIAL)
#
# ETAPA 2 do plano: painel do peso de VALE3 (posicao direta) em TODOS os
# fundos do universo CVM (nao so Itau), 2016-2021, com a gestora agrupada
# em 41 grupos por marca controladora (confirmado com o Joao em 21/07/2026).
#
# Mesma logica/tratamentos ja validados no R/10 (dedup exata, remocao de
# pesos negativos, merge dia-exato SH x CONS, leitura em blocos do CONS) --
# so tira o filtro ITAU_PATTERNS da SH, mantendo TODOS os fundos.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
DATA_DIR <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
ASSET <- "VALE ON N1 - VALE3"

norm <- function(x){ x<-iconv(as.character(x),"","ASCII//TRANSLIT"); trimws(gsub("[[:space:]]+"," ",toupper(x))) }
pnum <- function(x){ suppressWarnings(as.numeric(gsub(",","",as.character(x),fixed=TRUE))) }
pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }
grp_lines <- function(path){ con<-file(path,"r"); on.exit(close(con)); k<-list()
  repeat{ln<-readLines(con,n=1e6,warn=FALSE); if(length(ln)==0)break
    h<-ln[grepl(ASSET,ln,fixed=TRUE,useBytes=TRUE)]; if(length(h)) k[[length(k)+1L]]<-h}; unlist(k,use.names=FALSE) }
COLS <- c("cnpj","anbima","codigo","nome","tipo_ativo","data_comp","nome_ativo","valor_mil","participacao")

# ---- agrupamento de gestora (41 grupos, confirmado com o Joao) -------------
gestora_grupo <- function(x) {
  ux <- toupper(iconv(as.character(x), "", "ASCII//TRANSLIT"))
  pares <- list(
    c("^AZ ", "AZ Quest"), c("^BNP PARIBAS", "BNP Paribas"), c("^BTG PACTUAL", "BTG Pactual"),
    c("^CAIXA", "Caixa"), c("^CREDIT SUISSE", "Credit Suisse"), c("^ITA", "Itau"),
    c("^JGP", "JGP"), c("^XP", "XP")
  )
  out <- x
  for (p in pares) out[grepl(p[1], ux)] <- p[2]
  out
}

res <- list()
for (y in 2016:2021) {
  cat("== ano", y, "==\n"); flush.console()
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","CNPJ","NOME_FUNDO","GESTORA","DATA"))
  sh[, GESTORA_GRUPO := gestora_grupo(GESTORA)]
  sh[, DATA_D := pdate(DATA)]
  cat("  fundos distintos (SH, todas gestoras):", uniqueN(sh$COD_FUNDO),
      "| gestoras distintas (bruto):", uniqueN(sh$GESTORA),
      "| grupos distintos:", uniqueN(sh$GESTORA_GRUPO), "\n")

  ln <- grp_lines(file.path(DATA_DIR, sprintf("cons_%d.csv", y)))
  cv <- fread(text = ln, sep = ";", header = FALSE, col.names = COLS, showProgress = FALSE)
  cv <- cv[trimws(nome_ativo) == ASSET]; cv <- unique(cv)
  cv[, PESO := pnum(participacao)]; cv[, DATA_COMP := pdate(data_comp)]
  n_neg <- cv[PESO < 0, .N]; n_imp <- cv[PESO > 1, .N]
  if (n_imp > 0) cat("  ATENCAO: removendo", n_imp, "linha(s) com peso > 1 (impossivel, erro de dado)\n")
  cv <- cv[is.na(PESO) | (PESO >= 0 & PESO <= 1)]
  cat("  linhas VALE3 (CONS, todos os fundos, apos dedup+neg+peso>1):", nrow(cv),
      "(", n_neg, "negativos e", n_imp, "impossiveis removidos )\n")

  m <- merge(cv, sh[, .(COD_FUNDO, DATA_D, GESTORA, GESTORA_GRUPO, NOME_FUNDO)],
             by.x = c("codigo","DATA_COMP"), by.y = c("COD_FUNDO","DATA_D"), all.x = TRUE)
  n_sem <- m[is.na(GESTORA_GRUPO), .N]
  cat("  fundo-meses VALE3 apos merge:", nrow(m), "| sem match na SH:", n_sem,
      sprintf("(%.1f%%)\n", 100*n_sem/nrow(m)))

  pan <- m[, .(cod_fundo = codigo, cnpj = cnpj, nome_fundo = NOME_FUNDO,
               gestora_raw = GESTORA, gestora_grupo = GESTORA_GRUPO,
               data = DATA_COMP, ano = year(DATA_COMP), mes = month(DATA_COMP),
               peso_vale3 = PESO, valor_mil = valor_mil)]
  res[[length(res)+1L]] <- pan
}

painel <- rbindlist(res)
cat("\n==== PAINEL FINAL (todas as gestoras), 2016-2021 ====\n")
cat("Obs:", nrow(painel), "| fundos distintos:", uniqueN(painel$cod_fundo),
    "| grupos de gestora distintos:", uniqueN(painel$gestora_grupo), "\n")

dups <- painel[, .N, by = .(cod_fundo, data)][N > 1]
cat("Pares (fundo, mes) duplicados:", nrow(dups), "\n")

cat("\nFundos por grupo de gestora (top 15):\n")
print(painel[, .(fundos = uniqueN(cod_fundo)), by = gestora_grupo][order(-fundos)][1:15])

fwrite(painel, file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv'\n")
