# =============================================================================
# 20_extensive_margin.R
#
# Margem EXTENSIVA: trata fundos que entram/saem de VALE3 preenchendo ZEROS.
# Universo: fundos de ACOES da Itau, vivos na SH (snapshot do ultimo pregao do
# mes). peso = participacao de VALE3 na CONS, ou 0 se nao tiver (= nao detem).
#   - descreve taxa de posse, entradas e saidas
#   - logit: P(tem VALE3) ~ caracteristicas (o que explica DETER)
#   - previsao OOS da posse: ingenuo (estado anterior) vs logit
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"
norm <- function(x){x<-iconv(as.character(x),"","ASCII//TRANSLIT");trimws(gsub("[[:space:]]+"," ",toupper(x)))}
parse_brl <- function(x){x<-trimws(as.character(x));x<-gsub("R\\$","",x);x<-gsub("[[:space:]]","",x)
  x<-gsub("\\.","",x);x<-gsub(",",".",x);x[x==""]<-NA;suppressWarnings(as.numeric(x))}
pdate <- function(x){x<-trimws(as.character(x));o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o}

# ---- 1) universo: fundo-meses de Acoes Itau (snapshot do ultimo pregao do mes) ----
U <- list()
for (y in 2016:2021) {
  sh <- fread(file.path(DATA_DIR,sprintf("SH_%d.csv",y)), encoding="UTF-8", showProgress=FALSE,
    select=c("COD_FUNDO","CNPJ","NOME_FUNDO","GESTORA","CLASSIFICACAO_ANBIMA","DATA",
             "PATRIMONIO_LIQUIDO_(MIL)","NUMERO_DE_COTISTAS"),
    colClasses=list(character="PATRIMONIO_LIQUIDO_(MIL)"))
  sh[, GN:=norm(GESTORA)]
  sh <- sh[grepl("ITAU ASSET|ITAU DTVM|ITAU UNIBANCO",GN) & grepl("^Ações",CLASSIFICACAO_ANBIMA)]
  sh[, DT:=pdate(DATA)][, ym:=year(DT)*100L+month(DT)]
  last <- sh[sh[, .I[which.max(DT)], by=.(COD_FUNDO,ym)]$V1]   # ultimo pregao do mes
  U[[length(U)+1L]] <- last[, .(cod_fundo=COD_FUNDO, cnpj=CNPJ, nome_fundo=NOME_FUNDO,
    ym, ano=year(DT), mes=month(DT), aum=parse_brl(`PATRIMONIO_LIQUIDO_(MIL)`)*1000,
    n_cotistas=suppressWarnings(as.numeric(NUMERO_DE_COTISTAS)))]
  cat("ano",y,"ok\n"); flush.console()
}
U <- rbindlist(U)
U[, is_fic := as.integer(grepl("\\bFIC\\b|\\bFICFI\\b", norm(nome_fundo)))]

# ---- 2) marca quem TEM VALE3 (peso) e quem nao (0) ----
pan <- fread(file.path(REPO,"data/processed/painel_vale_itau_2016_2021_full.csv"))
w <- pan[, .(cod_fundo, ym=ano*100L+mes, peso=peso_vale3)]
U <- merge(U, w, by=c("cod_fundo","ym"), all.x=TRUE)
U[, tem := as.integer(!is.na(peso))][is.na(peso), peso:=0]
cat("\n== Painel extensivo (fundo-meses de Acoes Itau):", nrow(U), "obs |", uniqueN(U$cod_fundo), "fundos ==\n")
cat("Com VALE3:", U[tem==1,.N], sprintf("(%.0f%%)", 100*mean(U$tem)), "| sem (peso 0):", U[tem==0,.N], "\n")

# ---- 3) descricao: taxa de posse no tempo, entradas/saidas ----
setorder(U, cod_fundo, ym)
U[, tem_prev := shift(tem), by=cod_fundo]
cat("\nTaxa de posse de VALE3 por ano:\n")
print(U[, .(pct_com_vale=round(100*mean(tem),0), fundos=uniqueN(cod_fundo)), by=ano][order(ano)])
cat("\nENTRADAS (tem=1 apos tem=0):", U[tem==1 & tem_prev==0,.N],
    "| SAIDAS (tem=0 apos tem=1):", U[tem==0 & tem_prev==1,.N], "\n")

# ---- 4) logit: o que explica DETER VALE3 ----
U[, l_aum:=log(aum)][, l_cot:=log(n_cotistas)]
U2 <- U[is.finite(l_aum) & is.finite(l_cot)]
lg <- glm(tem ~ l_aum + l_cot + is_fic, data=U2, family=binomial)
cat("\n==== LOGIT: P(tem VALE3) ~ caracteristicas ====\n")
print(round(summary(lg)$coefficients,4))

# ---- 5) previsao OOS da POSSE: ingenuo (estado anterior) vs logit ----
U2[, monidx := frank(ym, ties.method="dense")]
acc <- list()
for (t in 13:max(U2$monidx)) {
  tr <- U2[monidx < t]; te <- U2[monidx == t & !is.na(tem_prev)]
  if (!nrow(te)) next
  fit <- glm(tem ~ l_aum + l_cot + is_fic, data=tr, family=binomial)
  te[, p_logit := as.integer(predict(fit, te, type="response") > 0.5)]
  acc[[length(acc)+1L]] <- te[, .(ym=t, acerto_naive=as.integer(tem==tem_prev),
                                   acerto_logit=as.integer(tem==p_logit),
                                   muda=as.integer(tem!=tem_prev))]
}
A <- rbindlist(acc)
cat("\n==== PREVISAO DA POSSE 1 mes a frente (OOS) ====\n")
cat("Obs:", nrow(A), "| taxa de MUDANCA (entra ou sai):", round(100*mean(A$muda),1), "%\n")
cat(sprintf("Acuracia Ingenuo (repete estado): %.1f%%\n", 100*mean(A$acerto_naive)))
cat(sprintf("Acuracia Logit (caracteristicas): %.1f%%\n", 100*mean(A$acerto_logit)))

fwrite(U, file.path(REPO,"data/processed/painel_extensivo_acoes.csv"))
cat("\nOK - painel extensivo salvo em data/processed/painel_extensivo_acoes.csv\n")
