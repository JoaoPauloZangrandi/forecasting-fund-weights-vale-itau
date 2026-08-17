# =============================================================================
# 50_universo_completo_gestora.R  (v2 OFICIAL -- teste_estrategia)
#
# FASE 0, passo 1: universo COMPLETO de fundos com posicao em acoes 2016-2021
# (nao so quem ja teve VALE3 -- 3.254 fundos, vs. 2.820 do painel atual, 434
# novos). Mapeia cod_fundo -> gestora_grupo pra TODOS eles, direto do SH
# (cadastro), sem depender de o fundo ter tido VALE3 em algum momento.
#
# Mesma funcao gestora_grupo() do R/17 (41 grupos por marca controladora).
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
DATA_DIR <- Sys.getenv("CVM_DATA_DIR", unset = "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF")
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")
source(file.path(REPO, "v2 OFICIAL/scripts/config_periodo.R"))

pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }

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

# ---- passo 1: uniao de todos os fundos com posicao em acoes, ANO_INICIO-ANO_FIM -----
todos <- character(0)
for (y in ANO_INICIO:ANO_FIM) {
  cv <- fread(file.path(DATA_DIR, sprintf("cons_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = "Código")
  todos <- union(todos, unique(as.character(cv$Código)))
  rm(cv); gc(FALSE)
}
cat(sprintf("Universo completo (qualquer posicao em acoes, %d-%d): %d fundos\n", ANO_INICIO, ANO_FIM, length(todos)))

atual <- unique(fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv"))$cod_fundo)
novos <- setdiff(todos, as.character(atual))
cat("Fundos ja no painel atual (ja tiveram VALE3):", length(intersect(todos, as.character(atual))), "\n")
cat("Fundos NOVOS (nunca tiveram VALE3):", length(novos), "\n")
# NOTA (prep 15/08/2026): este stopifnot so' e' valido pro range original
# 2016:2021 (3254/434). Com ANO_FIM > 2021, ele VAI FALHAR -- e' esperado,
# nao e' bug (ver PLANO_EXPANSAO_2021_2026.md, "asserts que vao quebrar").
# Comentar/ajustar o valor esperado quando ANO_FIM mudar.
if (ANO_FIM == 2021L) stopifnot(length(todos) == 3254L, length(novos) == 434L)

# ---- passo 2: mapear cod_fundo -> gestora_grupo/cnpj/nome pra TODOS --------
sh_all <- list()
for (y in ANO_INICIO:ANO_FIM) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","CNPJ","NOME_FUNDO","GESTORA","DATA"))
  sh <- sh[as.character(COD_FUNDO) %in% todos]
  sh[, GESTORA_GRUPO := gestora_grupo(GESTORA)]
  sh[, DATA_D := pdate(DATA)]
  sh_all[[length(sh_all)+1L]] <- sh[, .(cod_fundo = COD_FUNDO, cnpj = CNPJ, nome_fundo = NOME_FUNDO,
                                         gestora_grupo = GESTORA_GRUPO, data = DATA_D)]
  cat("ano", y, "- linhas SH lidas (universo completo):", nrow(sh_all[[length(sh_all)]]), "\n"); flush.console()
  rm(sh); gc(FALSE)
}
SH <- rbindlist(sh_all)
cat("\nTotal linhas SH (universo completo, 2016-2021):", nrow(SH), "\n")

# fundo pode trocar de gestora ao longo do tempo (raro) -- fica com a
# gestora mais FREQUENTE observada pra cada fundo, mesmo criterio implicito
# usado no restante do projeto (uma linha por fundo).
mapa <- SH[, .(freq = .N), by = .(cod_fundo, cnpj, nome_fundo, gestora_grupo)]
setorder(mapa, cod_fundo, -freq)
mapa <- mapa[, .SD[1], by = cod_fundo][, .(cod_fundo, cnpj, nome_fundo, gestora_grupo)]
cat("Fundos mapeados (gestora mais frequente):", nrow(mapa), "de", length(todos), "no universo completo\n")

sem_match <- setdiff(todos, as.character(mapa$cod_fundo))
if (length(sem_match) > 0) cat("ATENCAO:", length(sem_match), "fundos sem match no SH (nao vao entrar no painel)\n")

mapa[, novo := as.integer(as.character(cod_fundo) %in% novos)]
cat("\nDos", nrow(mapa), "mapeados,", sum(mapa$novo), "sao novos (nunca tiveram VALE3)\n")

cat("\nGestoras dos 434 fundos novos (top 10 por numero de fundos):\n")
print(mapa[novo==1, .(fundos = .N), by = gestora_grupo][order(-fundos)][1:10])

fwrite(mapa, file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/universo_completo_gestora.csv'\n")
