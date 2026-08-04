# =============================================================================
# 71_teste_fluxo_retorno_restrito.R  (v2 OFICIAL -- teste_estrategia)
#
# Repete o teste do R/70 (fluxo previsto prediz retorno?), agora restrito
# aos ativos onde o modelo ja mostrou margem POSITIVA na Etapa 3 (h=1,
# corrigido) -- ideia: talvez o sinal esteja diluido por ativos onde o
# proprio modelo de ajuste parcial nao funciona bem (margem negativa), e
# restringir aos casos "confiaveis" limpe o teste.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- Sys.getenv("PROJ_DIR", unset = "C:/Users/joaoz/forecasting-fund-weights-vale-itau")

M <- fread(file.path(REPO, "v2 OFICIAL/data/teste_fluxo_prediz_retorno.csv"))
cat("Base original (R/70):", nrow(M), "obs\n")

pa <- fread(file.path(REPO, "v2 OFICIAL/data/etapa3_universo_completo_por_ativo_h1.csv"))
pa_pos <- pa[margem_corr > 0, .(ativo)]
cat("Ativos com margem corrigida positiva (Etapa 3, h=1):", nrow(pa_pos), "de", nrow(pa), "\n")

Mr <- merge(M, pa_pos, by = "ativo")
cat("Obs apos restringir a ativos com margem positiva:", nrow(Mr), "de", nrow(M), "\n")

cat("\n===== (a) Regressao restrita: retorno(t+1) ~ fluxo(t), so ativos com margem>0 =====\n")
fit_eco_r <- lm(retorno_fut ~ fluxo_pct, data = Mr)
print(summary(fit_eco_r)$coefficients)
cat("R2:", round(summary(fit_eco_r)$r.squared, 5), "\n")

fit_normal_r <- lm(retorno_fut ~ fluxo_normal_pct, data = Mr)
cat("\n--- sinal SEM eco, restrito ---\n")
print(summary(fit_normal_r)$coefficients)
cat("R2:", round(summary(fit_normal_r)$r.squared, 5), "\n")

cat("\n===== (b) Quintis, restrito =====\n")
Mr[, quintil := cut(fluxo_pct, quantile(fluxo_pct, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE)]
por_quintil_r <- Mr[, .(n = .N, retorno_medio = mean(retorno_fut), retorno_mediano = median(retorno_fut)), by = quintil]
setorder(por_quintil_r, quintil)
print(por_quintil_r)

t_q5_q1_r <- t.test(Mr[quintil==5]$retorno_fut, Mr[quintil==1]$retorno_fut)
diff_r <- por_quintil_r[quintil==5]$retorno_medio - por_quintil_r[quintil==1]$retorno_medio
cat("\nDiferenca quintil5-quintil1:", round(diff_r,5), "| p-valor:", signif(t_q5_q1_r$p.value,3), "\n")

fwrite(Mr, file.path(REPO, "v2 OFICIAL/data/teste_fluxo_retorno_restrito.csv"))
fwrite(por_quintil_r, file.path(REPO, "v2 OFICIAL/data/quintis_fluxo_retorno_restrito.csv"))
cat("\nOK - salvo\n")
