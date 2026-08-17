# =============================================================================
# _check_multimercado_hipotese.R (temporario, NAO documentado no TCC ainda)
#
# Testa a hipotese: is_fic tem coef negativo no peso porque fundos FIC
# pendem mais para "Multimercados Livre" (achado do teste anterior:
# 54,3% dos fundo-mes FIC sao Multimercados Livre vs 45,7% dos FI), e um
# multimercado que aparece no painel (porque teve VALE3 em algum momento)
# provavelmente destina uma fatia MENOR do patrimonio total a acoes no
# geral -- nao por indexacao, nao por exposicao escondida.
#
# Tambem responde, com verificacao: o universo do painel multiativo e
# EXCLUSIVAMENTE fundos de acoes, ou qualquer fundo que teve acao em algum
# momento? (conferido direto no codigo do script 17/25: fundos_alvo = todo
# fundo CVM, qualquer classificacao, que teve posicao DIRETA em VALE3 em
# QUALQUER mes 2016-2021; para esses fundos, o painel multiativo traz TODAS
# as posicoes diretas em QUALQUER acao, em TODOS os meses -- nao so os
# meses/acoes onde ha VALE3.)
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
SH_DIR <- "C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF"

pp <- fread(file.path(REPO, "v2 OFICIAL/data/painel_multiativo_final.csv"))
cat("Painel multiativo final: ", nrow(pp), "linhas |", uniqueN(pp$cod_fundo), "fundos |",
    uniqueN(pp$ativo), "ativos distintos\n\n")

# --- soma de peso (alocacao total em acoes) por fundo-mes --------------------
soma <- pp[, .(soma_peso = sum(peso), n_ativos = .N, is_fic = is_fic[1]), by = .(cod_fundo, ym)]
cat("Fundo-mes:", nrow(soma), "\n\n")

# --- classificacao anbima (mesma logica do teste anterior, salva em disco) --
sh_cache <- file.path(REPO, "v2 OFICIAL/data/_cache_classif_anbima.csv")
if (!file.exists(sh_cache)) {
  sh_list <- list()
  for (y in 2017:2021) {
    sh <- fread(file.path(SH_DIR, sprintf("SH_%d.csv", y)),
                select = c("COD_FUNDO","DATA","CLASSIFICACAO_ANBIMA"),
                encoding = "UTF-8", showProgress = FALSE)
    sh[, ym := as.integer(format(as.Date(DATA, tryFormats=c("%d/%m/%Y","%Y-%m-%d")), "%Y%m"))]
    sh_list[[as.character(y)]] <- unique(sh[, .(cod_fundo = COD_FUNDO, ym, classif_anbima = CLASSIFICACAO_ANBIMA)])
  }
  sh_all <- unique(rbindlist(sh_list), by = c("cod_fundo","ym"))
  fwrite(sh_all, sh_cache)
} else {
  sh_all <- fread(sh_cache)
}

d <- merge(soma, sh_all, by = c("cod_fundo","ym"))
cat("Fundo-mes com classif_anbima:", nrow(d), "de", nrow(soma),
    sprintf("(%.1f%%)\n\n", 100*nrow(d)/nrow(soma)))

cat("===== HIPOTESE 1: soma_peso (alocacao total em acoes) por classif_anbima =====\n")
resumo <- d[, .(n = .N, soma_peso_media = round(mean(soma_peso),4),
                soma_peso_mediana = round(median(soma_peso),4)), by = classif_anbima]
print(resumo[order(-n)])

cat("\n===== soma_peso: Multimercados Livre vs. categorias 'Acoes' agregadas =====\n")
d[, grupo := ifelse(classif_anbima == "Multimercados Livre", "Multimercados Livre", "Acoes (qualquer subcategoria)")]
resumo2 <- d[, .(n = .N, soma_peso_media = round(mean(soma_peso),4),
                 soma_peso_mediana = round(median(soma_peso),4)), by = grupo]
print(resumo2)
teste_t <- t.test(soma_peso ~ grupo, data = d)
cat(sprintf("\nDiferenca de medias: %.4f | t = %.2f | p = %.2e\n",
            diff(rev(teste_t$estimate)), teste_t$statistic, teste_t$p.value))

cat("\n===== HIPOTESE 2: classif_anbima explica o efeito de is_fic sobre soma_peso? =====\n")
cat("--- Regressao (a): soma_peso ~ is_fic (sem controlar estrategia) ---\n")
fit_a <- lm(soma_peso ~ is_fic, data = d)
print(summary(fit_a)$coefficients)
cat(sprintf("R2 = %.4f\n\n", summary(fit_a)$r.squared))

cat("--- Regressao (b): soma_peso ~ is_fic + classif_anbima (controlando estrategia) ---\n")
fit_b <- lm(soma_peso ~ is_fic + classif_anbima, data = d)
print(summary(fit_b)$coefficients["is_fic",])
cat(sprintf("R2 = %.4f\n\n", summary(fit_b)$r.squared))

cat("===== HIPOTESE 3: is_fic ainda importa DENTRO de cada categoria Anbima? =====\n")
for (cat_i in c("Multimercados Livre", "Ações Livre")) {
  sub <- d[classif_anbima == cat_i]
  fit_sub <- lm(soma_peso ~ is_fic, data = sub)
  cf <- summary(fit_sub)$coefficients
  cat(sprintf("%s (n=%d): coef is_fic = %.4f, t = %.2f, p = %.4f\n",
              cat_i, nrow(sub), cf["is_fic","Estimate"], cf["is_fic","t value"], cf["is_fic","Pr(>|t|)"]))
}

cat("\n===== Confirmando universo do painel: fundo teve VALE3 em algum momento? =====\n")
todas_gestoras <- fread(file.path(REPO, "v2 OFICIAL/data/painel_todas_gestoras_2016_2021.csv"),
                         select = "cod_fundo")
fundos_alvo <- unique(todas_gestoras$cod_fundo)
fundos_painel <- unique(pp$cod_fundo)
cat("Fundos no painel_todas_gestoras (tiveram VALE3 em algum mes):", length(fundos_alvo), "\n")
cat("Fundos no painel multiativo final:", length(fundos_painel), "\n")
cat("Fundos do painel multiativo que NAO estao em fundos_alvo (deveria ser 0):",
    sum(!fundos_painel %in% fundos_alvo), "\n")

cat("\n--- Exemplo: quantos fundo-mes do painel multiativo NAO tem VALE3 naquele mes especifico? ---\n")
tem_vale_no_mes <- pp[ativo == "VALE ON N1 - VALE3", unique(paste(cod_fundo, ym))]
todos_fundo_mes <- unique(pp[, paste(cod_fundo, ym)])
cat("Fundo-mes totais no painel:", length(todos_fundo_mes), "\n")
cat("Fundo-mes COM VALE3 naquele mes:", length(tem_vale_no_mes),
    sprintf("(%.1f%%)\n", 100*length(tem_vale_no_mes)/length(todos_fundo_mes)))
cat("Fundo-mes SEM VALE3 naquele mes (mas o fundo teve VALE3 em outro mes):",
    length(todos_fundo_mes) - length(tem_vale_no_mes),
    sprintf("(%.1f%%)\n", 100*(length(todos_fundo_mes) - length(tem_vale_no_mes))/length(todos_fundo_mes)))
