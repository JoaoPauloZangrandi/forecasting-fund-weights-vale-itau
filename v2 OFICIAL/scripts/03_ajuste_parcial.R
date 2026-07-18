# =============================================================================
# 03_ajuste_parcial.R  (v2 OFICIAL)
#
# ETAPA 3: ajuste parcial, estimado por MQO simples (sem VI, sem efeito-fixo
# de fundo, sem Arellano-Bond -- so a medicao direta).
#
#   w_{i,t+1} = (1-lambda) w_{i,t} + lambda w*_{i,t} + u_{i,t+1}
#   <=>  w_{i,t+1} - w_{i,t} = lambda * d_{i,t} + u_{i,t+1},  d_{i,t} = w*_{i,t} - w_{i,t}
#
# w*_{i,t} = peso_prev da Etapa 2 (g(x_{i,t}' theta_t), o alvo logistico
# ja calculado). d_{i,t} = w* - peso observado no mes t.
#
# Um unico lambda, pooled em todos os fundo-meses, por MQO direto -- sem
# nenhuma correcao para os dois problemas classicos (erro de medida no alvo;
# viés mecanico de w aparecer nos dois lados). E a medicao "ingenua",
# proposital nesta versao.
# RODAR COM CAMINHO ABSOLUTO.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"

E <- fread(file.path(REPO, "v2 OFICIAL/data/erro_cross_section.csv"))
E[, d := peso_prev - peso_vale3]  # distancia ao alvo

# proximo mes calendario (fundo-mes seguinte, nao necessariamente contiguo se faltar mes)
addm <- function(ym, k) { tot <- (ym %/% 100L)*12L + (ym %% 100L - 1L) + k; (tot %/% 12L)*100L + (tot %% 12L) + 1L }
E[, ym_next := addm(ym, 1L)]

nxt <- E[, .(cod_fundo, ym_next_key = ym, peso_next = peso_vale3)]
M <- merge(E, nxt, by.x = c("cod_fundo","ym_next"), by.y = c("cod_fundo","ym_next_key"))
M[, dw := peso_next - peso_vale3]

cat("Fundo-meses com par (t, t+1) disponivel:", nrow(M), "\n")

fit <- lm(dw ~ 0 + d, data = M)  # sem intercepto, forma direta da equacao
lam <- coef(fit)["d"]
se  <- summary(fit)$coefficients["d","Std. Error"]
tval <- summary(fit)$coefficients["d","t value"]
cat(sprintf("\n===== Ajuste parcial (MQO direto, pooled) =====\n"))
cat(sprintf("lambda = %.4f | erro-padrao = %.4f | t = %.2f | R2 = %.4f | n = %d\n",
            lam, se, tval, summary(fit)$r.squared, nobs(fit)))

fit_c <- lm(dw ~ d, data = M)  # com intercepto, para checar se o intercepto e ~0 (esperado por teoria)
cat(sprintf("\nCom intercepto (checagem): intercepto=%.5f (t=%.2f) | lambda=%.4f (t=%.2f)\n",
            coef(fit_c)[1], summary(fit_c)$coefficients[1,3],
            coef(fit_c)[2], summary(fit_c)$coefficients[2,3]))

M[, u := dw - lam*d]  # erro do ajuste parcial

cat("\n===== Erro u do ajuste parcial: estatisticas gerais (", nrow(M), "obs ) =====\n")
cat(sprintf("media(u)     = %.6f\n", mean(M$u)))
cat(sprintf("dp(u)        = %.6f\n", sd(M$u)))
cat(sprintf("min(u)       = %.6f\n", min(M$u)))
cat(sprintf("max(u)       = %.6f\n", max(M$u)))
cat(sprintf("mediana(u)   = %.6f\n", median(M$u)))
cat(sprintf("%% de u > 0   = %.1f%%\n", 100*mean(M$u > 0)))
m <- mean(M$u); s <- sd(M$u)
cat(sprintf("assimetria   = %.3f\n", mean((M$u-m)^3)/s^3))
cat(sprintf("curtose      = %.3f (normal = 3)\n", mean((M$u-m)^4)/s^4))

cat("\n===== Erro u: media e dp POR MES (primeiros 12 meses) =====\n")
por_mes <- M[, .(media_u = mean(u), dp_u = sd(u), n = .N), by = ym]
setorder(por_mes, ym)
print(por_mes[1:12, .(ym, media_u = round(media_u,6), dp_u = round(dp_u,6), n)])
cat(sprintf("\ndp_u ao longo dos meses: media=%.5f dp=%.5f min=%.5f max=%.5f\n",
            mean(por_mes$dp_u), sd(por_mes$dp_u), min(por_mes$dp_u), max(por_mes$dp_u)))

fwrite(M, file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros.csv"))
fwrite(por_mes, file.path(REPO, "v2 OFICIAL/data/ajuste_parcial_erros_por_mes.csv"))
cat("\nOK - salvo em 'v2 OFICIAL/data/ajuste_parcial_erros*.csv'\n")
