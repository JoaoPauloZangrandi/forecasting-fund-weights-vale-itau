# =============================================================================
# 10_build_panel_allyears.R
# Painel do peso de VALE3 nos fundos Itau, 2016-2021 (generaliza o Passo 2).
# Tratamentos validados em 2016: dedup exata, remocao de negativos, merge
# dia-exato SH x CONS. CONS lido em blocos (readLines + grepl useBytes) para
# baixa memoria e robustez a bytes invalidos.
#
# IMPORTANTE (gotcha): RODAR COM CAMINHO ABSOLUTO. Invocar o Rscript com caminho
# RELATIVO (ex.: "R/10_...R") dispara um segfault do data.table neste build do R
# no Windows; com caminho absoluto roda estavel:
#   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "C:/Users/joaoz/forecasting-fund-weights-vale-itau/R/10_build_panel_allyears.R"
# =============================================================================
suppressPackageStartupMessages(library(data.table))
DATA_DIR <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
ASSET <- "VALE ON N1 - VALE3"; ITAU <- c("ITAU ASSET","ITAU DTVM","ITAU UNIBANCO")
norm <- function(x){ x<-iconv(as.character(x),"","ASCII//TRANSLIT"); trimws(gsub("[[:space:]]+"," ",toupper(x))) }
pnum <- function(x){ suppressWarnings(as.numeric(gsub(",","",as.character(x),fixed=TRUE))) }
pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }
grp <- function(path){ con<-file(path,"r");on.exit(close(con));k<-list()
  repeat{ln<-readLines(con,n=1e6,warn=FALSE);if(length(ln)==0)break
    h<-ln[grepl(ASSET,ln,fixed=TRUE,useBytes=TRUE)];if(length(h))k[[length(k)+1L]]<-h};unlist(k,use.names=FALSE) }
COLS <- c("cnpj","anbima","codigo","nome","tipo_ativo","data_comp","nome_ativo","valor_mil","participacao")

res <- list()
for (y in 2016:2021) {
  cat("== ano", y, "==\n"); flush.console()
  sh <- fread(file.path(DATA_DIR,sprintf("SH_%d.csv",y)), encoding="UTF-8", showProgress=FALSE,
              select=c("COD_FUNDO","CNPJ","NOME_FUNDO","GESTORA","DATA"))
  sh[, GN := norm(GESTORA)]; shi <- sh[grepl(paste(ITAU,collapse="|"),GN)]; shi[, DATA_D := pdate(DATA)]
  itau_funds <- unique(shi$COD_FUNDO)
  ln <- grp(file.path(DATA_DIR,sprintf("cons_%d.csv",y)))
  cv <- fread(text=ln, sep=";", header=FALSE, col.names=COLS, showProgress=FALSE)
  cv <- cv[trimws(nome_ativo)==ASSET]; cv <- unique(cv)
  cv[, PESO := pnum(participacao)]; cv[, DATA_COMP := pdate(data_comp)]
  cv <- cv[is.na(PESO)|PESO>=0]; cvi <- cv[codigo %in% itau_funds]
  m <- merge(cvi, shi[,.(COD_FUNDO,DATA_D,GESTORA,NOME_FUNDO)],
             by.x=c("codigo","DATA_COMP"), by.y=c("COD_FUNDO","DATA_D"), all.x=TRUE)
  cat("  merge ok\n"); flush.console()
  pan <- m[, .(cod_fundo=codigo, cnpj=cnpj, nome_fundo=NOME_FUNDO, gestora=GESTORA,
               data=DATA_COMP, ano=year(DATA_COMP), mes=month(DATA_COMP),
               peso_vale3=PESO, valor_mil=valor_mil)]
  cat("  pan ok\n"); flush.console()
  audit <- data.table(ano=y, obs=nrow(pan), fundos=uniqueN(pan$cod_fundo))
  cat("  audit ok\n"); flush.console()
  res[[length(res)+1L]] <- list(pan=pan, audit=audit)
}
cat("LOOP OK. rbindlist...\n"); flush.console()
painel <- rbindlist(lapply(res, `[[`, "pan"))
cat("painel:", nrow(painel), "obs\n"); flush.console()
print(summary(painel$peso_vale3))
fwrite(painel, file.path(REPO,"data/processed/painel_vale_itau_2016_2021.csv"))
cat("FIM OK\n")
