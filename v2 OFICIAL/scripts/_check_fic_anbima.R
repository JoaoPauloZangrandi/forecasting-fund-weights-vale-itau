# =============================================================================
# _check_fic_anbima.R (temporario, NAO documentado no TCC ainda)
#
# Verificacao pedida pelo Joao apos eu ter dado uma explicacao ERRADA sobre
# FIC/CONS sem checar (disse que FIC "esconde" exposicao via cota de fundo;
# contraexemplo real do usuario -- fundo 93386 ARX FIC ACOES, posicoes
# diretas e diversificadas em acoes -- derrubou essa hipotese).
#
# Nova hipotese a testar: is_fic tem coeficiente negativo na Etapa 1 (FIC ->
# menos peso em VALE3) porque fundos FIC tendem a estar em categorias Anbima
# mais diversificadas/indexadas, nao porque a exposicao esta "escondida".
#
# Fonte da classificacao Anbima: coluna CLASSIFICACAO_ANBIMA na base SH
# (SH_YYYY.csv), chave COD_FUNDO + DT. Junta com painel_multiativo_final.csv
# (que tem is_fic por cod_fundo+ym) por cod_fundo + ano-mes mais proximo.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
SH_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"),
            select = c("cod_fundo","ym","is_fic"))
fic_por_fundo <- unique(pp[, .(cod_fundo, ym, is_fic)])
cat("Fundo-mes no painel multiativo:", nrow(fic_por_fundo), "\n")

# checa se is_fic e constante por fundo (deveria ser, vem do nome)
chk <- fic_por_fundo[, .(n_distintos = uniqueN(is_fic)), by = cod_fundo]
cat("Fundos com is_fic variando no tempo (nao deveria):", chk[n_distintos > 1, .N], "\n\n")

is_fic_fundo <- unique(fic_por_fundo[, .(cod_fundo, is_fic)])
cat("Fundos distintos:", nrow(is_fic_fundo), "| FIC:", sum(is_fic_fundo$is_fic==1),
    "| FI:", sum(is_fic_fundo$is_fic==0), "\n\n")

sh_list <- list()
for (y in 2017:2021) {
  f <- file.path(SH_DIR, sprintf("SH_%d.csv", y))
  sh <- fread(f, select = c("COD_FUNDO","DATA","CLASSIFICACAO_ANBIMA"),
              encoding = "UTF-8", showProgress = FALSE)
  sh[, ym := as.integer(format(as.Date(DATA, tryFormats=c("%d/%m/%Y","%Y-%m-%d")), "%Y%m"))]
  sh_list[[as.character(y)]] <- unique(sh[, .(cod_fundo = COD_FUNDO, ym, classif_anbima = CLASSIFICACAO_ANBIMA)])
  cat("SH", y, ": OK\n")
}
sh_all <- rbindlist(sh_list)
sh_all <- unique(sh_all, by = c("cod_fundo","ym"))
cat("\nLinhas SH consolidadas (cod_fundo x ym):", nrow(sh_all), "\n\n")

d <- merge(fic_por_fundo, sh_all, by = c("cod_fundo","ym"))
cat("Fundo-mes com match SH (classif_anbima):", nrow(d), "de", nrow(fic_por_fundo),
    sprintf("(%.1f%%)\n\n", 100*nrow(d)/nrow(fic_por_fundo)))

cat("===== Tabela cruzada: classif_anbima x is_fic (contagem fundo-mes) =====\n")
tab <- d[, .N, by = .(classif_anbima, is_fic)]
tab_wide <- dcast(tab, classif_anbima ~ is_fic, value.var = "N", fill = 0)
setnames(tab_wide, c("classif_anbima","FI_n","FIC_n"))
tab_wide[, FI_pct := round(100*FI_n/sum(FI_n), 1)]
tab_wide[, FIC_pct := round(100*FIC_n/sum(FIC_n), 1)]
print(tab_wide[order(-FIC_n)])

cat("\n===== Foco: categorias com 'Índice' ou 'Passivo' no nome =====\n")
d[, indexado := grepl("[IÍ]ndice|[PP]assiv", classif_anbima)]
tab2 <- d[, .(pct_indexado = round(100*mean(indexado),1), n = .N), by = is_fic]
print(tab2)

cat("\n===== Fundo 93386 (ARX FIC ACOES) especificamente =====\n")
print(unique(d[cod_fundo == 93386, .(cod_fundo, is_fic, classif_anbima)]))
