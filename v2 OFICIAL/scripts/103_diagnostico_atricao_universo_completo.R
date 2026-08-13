# =============================================================================
# 103_diagnostico_atricao_universo_completo.R  (v2 OFICIAL)
#
# Por que fundos do universo bruto (3.254, script 50) nao entram na amostra
# final apos exigir as 5 caracteristicas do Informe Diario completas (2.572,
# script 58)? Mesma pergunta ja respondida antes pra base VALE3-ancorada
# (paragrafo de atrito da Secao 3), agora pra base universo completo.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DATA_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"

bruto <- unique(as.character(fread(file.path(REPO, "v2 OFICIAL/data/universo_completo_gestora.csv"))$cod_fundo))
final <- unique(fread(file.path(REPO, "v2 OFICIAL/data/painel_universo_completo_final.csv"))$cod_fundo)
excluidos <- setdiff(bruto, as.character(final))
cat("Universo bruto:", length(bruto), "| Amostra final (5 caract.):", length(final),
    "| Excluidos:", length(excluidos), "\n")

feat <- fread(file.path(REPO, "v2 OFICIAL/data/features_predeterminado_completo.csv"))
feat[, cod_fundo := as.character(cod_fundo)]
feat_ok <- feat[is.finite(aum_prev) & is.finite(cotistas_prev) & is.finite(fluxo_prev) & !is.na(is_fic)]
tem_informe <- unique(feat_ok$cod_fundo)

sem_informe <- setdiff(excluidos, tem_informe)
com_informe_sem_beta <- intersect(excluidos, tem_informe)
cat("\nDos", length(excluidos), "excluidos:\n")
cat(" - nunca tem as 4 caracteristicas do Informe Diario completas ao mesmo tempo:",
    length(sem_informe), sprintf("(%.1f%%)\n", 100*length(sem_informe)/length(excluidos)))
cat(" - tem as 4, mas nunca chegam a ter beta_fundo:", length(com_informe_sem_beta),
    sprintf("(%.1f%%)\n", 100*length(com_informe_sem_beta)/length(excluidos)))

# ---- pra quem tem informe mas nao tem beta: quantos dias de cota valida? ---
pdate <- function(x){ x<-trimws(as.character(x)); o<-as.Date(rep(NA_character_,length(x)))
  for(f in c("%Y-%m-%d","%d/%m/%Y")){m<-is.na(o);if(!any(m))break;o[m]<-as.Date(x[m],format=f)};o }

sh_all <- list()
for (y in 2016:2021) {
  sh <- fread(file.path(DATA_DIR, sprintf("SH_%d.csv", y)), encoding = "UTF-8", showProgress = FALSE,
              select = c("COD_FUNDO","DATA","COTA"))
  sh[, COD_FUNDO := as.character(COD_FUNDO)]
  sh <- sh[COD_FUNDO %in% com_informe_sem_beta]
  sh[, cota_num := as.numeric(gsub(",", ".", COTA, fixed = TRUE))]
  sh_all[[length(sh_all)+1L]] <- sh[!is.na(cota_num) & cota_num > 0, .(cod_fundo = COD_FUNDO)]
  cat("ano", y, "lido\n")
}
cotas <- rbindlist(sh_all)
n_dias <- cotas[, .N, by = cod_fundo]

com_informe_sem_beta_dt <- data.table(cod_fundo = com_informe_sem_beta)
com_informe_sem_beta_dt <- merge(com_informe_sem_beta_dt, n_dias, by = "cod_fundo", all.x = TRUE)
com_informe_sem_beta_dt[is.na(N), N := 0L]

n_jovem <- com_informe_sem_beta_dt[N < 252, .N]
n_outro <- com_informe_sem_beta_dt[N >= 252, .N]
cat(sprintf("\nDos %d sem beta: %d (%.1f%%) tem menos de 252 dias de cota valida (fundo jovem demais); ",
            length(com_informe_sem_beta), n_jovem, 100*n_jovem/length(com_informe_sem_beta)))
cat(sprintf("%d (%.1f%%) acumulam 252+ dias mas mesmo assim nao geram beta (ex.: janela sem par completo com o Ibovespa, ou fundo encerrado logo apos).\n",
            n_outro, 100*n_outro/length(com_informe_sem_beta)))

cat("\nOK\n")
