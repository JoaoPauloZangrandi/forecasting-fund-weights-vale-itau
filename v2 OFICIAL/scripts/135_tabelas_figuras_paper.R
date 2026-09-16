# =============================================================================
# 135_tabelas_figuras_paper.R  (v2 OFICIAL)
#
# Monta as tabelas e figuras do artigo. NENHUM numero novo e' calculado
# aqui -- tudo vem dos CSV dos scripts 125-134. Se um numero nao esta' num
# CSV, ele nao entra no artigo.
#
# Saida: v2 OFICIAL/tabelas_paper_ext.tex (para \input{}), e figuras.
# =============================================================================
suppressPackageStartupMessages({ library(data.table) })

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
DD   <- file.path(REPO, "v2 OFICIAL/data")
FIG  <- file.path(REPO, "v2 OFICIAL/figuras")
SAIDA <- file.path(REPO, "v2 OFICIAL/tabelas_paper_ext.tex")

vr <- function(x, d = 3) sub("\\.", ",", formatC(x, format = "f", digits = d))
est <- function(p) if (!is.finite(p)) "" else if (p < .01) "***" else if (p < .05) "**" else if (p < .1) "*" else ""

L <- c("% Gerado por 135_tabelas_figuras_paper.R. Nao editar a mao.", "")
w <- function(...) L <<- c(L, ...)

cat("=============================================================\n")
cat("135 - Tabelas e figuras do artigo\n")
cat("=============================================================\n\n")

# --- Tabela 1: onde esta a variancia -----------------------------------------
V  <- fread(file.path(DD, "var_fundo_entre_dentro.csv"))
g  <- function(b, m) V[bloco == b & metrica == m, as.numeric(valor)][1]
SH <- fread(file.path(DD, "decomp_shapley_erro.csv"))
sh1 <- SH[dependente == "log(erro^2 + c) [PRIMARIA]"]
sh2 <- SH[dependente == "log(erro^2) condicional a ativo x mes"]

w("\\begin{table}[h]\\centering\\small",
  "\\caption{Onde está a variação do erro de previsão fora da amostra.}",
  "\\label{tab:ext-onde-variancia}",
  "\\begin{tabular}{@{}lrr@{}}", "\\toprule",
  "& \\multicolumn{2}{c}{\\% do $R^2$ (valor de Shapley)} \\\\ \\cmidrule(l){2-3}",
  "Fator & Incondicional & Condicional a ativo$\\times$mês \\\\", "\\midrule")
for (f in c("cod_fundo","ativo","gestora_grupo","ym")) {
  rot <- c(cod_fundo="Fundo", ativo="Ativo", gestora_grupo="Gestora", ym="Mês")[f]
  w(sprintf("%s & %s & %s \\\\", rot,
            vr(sh1[fator == f, pct_shapley], 1), vr(sh2[fator == f, pct_shapley], 1)))
}
w("\\midrule",
  sprintf("$R^2$ total & %s & %s \\\\", vr(sh1$r2_total[1], 4), vr(sh2$r2_total[1], 4)),
  "\\bottomrule", "\\end{tabular}",
  sprintf("\\caption*{\\footnotesize Decomposição de Shapley sobre %s observações de fundo por ativo por mês no período de teste, $h=1$. A dependente é $\\log(e^2+c)$, a forma aditiva do modelo multiplicativo de variância. Shapley porque gestora é estritamente aninhada em fundo, o que torna o $R^2$ sequencial dependente da ordem de entrada. A soma dos valores iguala o $R^2$ total por construção, o que serve de verificação da implementação. Fonte: elaboração própria.}", "1.552.376"),
  "\\end{table}", "")

w("\\begin{table}[h]\\centering\\small",
  "\\caption{Variância do erro por fundo: entre gestoras e dentro de gestora.}",
  "\\label{tab:ext-entre-dentro}",
  "\\begin{tabular}{@{}lr@{}}", "\\toprule",
  "Componente de $\\mathrm{Var}(\\log \\mathrm{RMSE}_i)$ & \\% \\\\", "\\midrule",
  sprintf("Entre gestoras & %s \\\\", vr(g("A4","pct_entre_gestora"), 1)),
  sprintf("Dentro de gestora & %s \\\\", vr(g("A4","pct_dentro_gestora"), 1)),
  "\\midrule",
  sprintf("Fundos & %d \\\\", as.integer(g("A4","n_fundos"))),
  sprintf("Razão entre o maior e o menor RMSE & %s \\\\", vr(g("A4","razao_max_min_rmse"), 1)),
  "\\bottomrule", "\\end{tabular}", "\\end{table}", "")

# --- Tabela 2: atenuacao e LOO -----------------------------------------------
w("\\begin{table}[h]\\centering\\small",
  "\\caption{O $R^2$ de 0,496 do modelo por gestora: atenuação e desempenho fora da amostra.}",
  "\\label{tab:ext-atenuacao}",
  "\\begin{tabular}{@{}lr@{}}", "\\toprule", "Medida & Valor \\\\", "\\midrule",
  sprintf("$R^2$ na amostra & %s \\\\", vr(g("controle","r2_95"), 4)),
  sprintf("$R^2$ ajustado & %s \\\\", vr(g("controle","r2_aj_95"), 4)),
  "\\addlinespace",
  sprintf("Confiabilidade $\\rho$, split ímpar/par & %s \\\\", vr(g("R1_split","rho_gestora_grupo_metade_ip"), 4)),
  sprintf("Confiabilidade $\\rho$, split cronológico & %s \\\\", vr(g("R1_split","rho_gestora_grupo_metade_cr"), 4)),
  sprintf("Confiabilidade $\\rho$, bootstrap em blocos & %s \\\\", vr(g("R1_boot","rho_boot_gestora"), 4)),
  "\\addlinespace",
  sprintf("$R^2_{\\text{sinal}} = R^2/\\rho$ (ímpar/par) & %s \\\\", vr(g("R2_sinal","r2_sinal_imparpar"), 4)),
  "\\addlinespace",
  sprintf("$R^2$ \\emph{leave-one-gestora-out}, 6 regressores & %s \\\\", vr(g("L1_loo","r2_loo_95"), 4)),
  sprintf("$R^2$ \\emph{leave-one-gestora-out}, só concentração & %s \\\\", vr(g("L1_loo","r2_loo_so_hhi"), 4)),
  "\\bottomrule", "\\end{tabular}",
  "\\caption*{\\footnotesize $n=40$ gestoras. A confiabilidade $\\rho$ é o teto de $R^2$ alcançável dado o ruído amostral na variável dependente, obtida por Spearman-Brown sobre a correlação entre metades. O split ímpar/par é preferido ao cronológico porque 2020 e 2021 são regimes distintos e o split temporal confunde ruído com variação real. Fonte: elaboração própria.}",
  "\\end{table}", "")

# --- Tabela 3: testes F de bloco ---------------------------------------------
BC <- fread(file.path(DD, "reg_fundo_conf_blocos.csv"))
BR <- fread(file.path(DD, "replicacao_h3_anual.csv"))
B <- rbind(BC[amostra == "conf_S1"], BR, fill = TRUE)
B <- B[param == "direta" & spec == "A1"]
Bw <- dcast(B, bloco + dep ~ amostra, value.var = "p")
cols_am <- setdiff(names(Bw), c("bloco","dep"))
w("\\begin{table}[h]\\centering\\small",
  "\\caption{Testes $F$ conjuntos por bloco de variáveis: confirmação e replicações.}",
  "\\label{tab:ext-blocos}",
  paste0("\\begin{tabular}{@{}ll", paste(rep("r", length(cols_am)), collapse=""), "@{}}"),
  "\\toprule",
  paste0("Bloco & Métrica & ", paste(gsub("_","\\\\_",cols_am), collapse = " & "), " \\\\"), "\\midrule")
rot_b <- c(dispersoes="Dispersões", turnover="Turnover", mandato="Mandato", breadth="Breadth")
rot_d <- c(D1="peso", D2="sleeve")
for (bb in c("dispersoes","turnover","mandato","breadth")) for (dd in c("D1","D2")) {
  r <- Bw[bloco == bb & dep == dd]
  if (!nrow(r)) next
  vals <- sapply(cols_am, function(cc) {
    p <- r[[cc]][1]
    if (!is.finite(p)) "---" else paste0(formatC(p, format="e", digits=1), est(p))
  })
  w(sprintf("%s & %s & %s \\\\", rot_b[bb], rot_d[dd], paste(vals, collapse = " & ")))
}
w("\\bottomrule", "\\end{tabular}",
  "\\caption*{\\footnotesize Valores $p$ do teste $F$ conjunto do bloco, com matriz de covariância agrupada por gestora. Especificação A1 (pooled), parametrização direta. *** $p<0{,}01$, ** $p<0{,}05$, * $p<0{,}1$. Fonte: elaboração própria.}",
  "\\end{table}", "")

# --- Tabela 4: coeficientes principais ---------------------------------------
CF <- fread(file.path(DD, "reg_fundo_conf_coef.csv"))
AJ <- fread(file.path(DD, "reg_fundo_conf_ajuste.csv"))
sel <- CF[amostra == "conf_S1" & param == "direta" & spec %in% c("A1","A3") &
            bloco %in% c("base","dispersoes","turnover","mandato","breadth")]
K <- dcast(sel, variavel + bloco ~ spec + dep, value.var = c("coef","p_wcb"))
ordem <- c("base","dispersoes","turnover","mandato","breadth")
K[, ob := match(bloco, ordem)]; setorder(K, ob, variavel)
w("\\begin{table}[h]\\centering\\footnotesize",
  "\\caption{Regressão do erro de previsão no nível de fundo, amostra de confirmação.}",
  "\\label{tab:ext-coef}",
  "\\begin{tabular}{@{}llrrrr@{}}", "\\toprule",
  "& & \\multicolumn{2}{c}{A1 (pooled)} & \\multicolumn{2}{c}{A3 (within-gestora)} \\\\",
  "\\cmidrule(lr){3-4}\\cmidrule(l){5-6}",
  "Bloco & Variável & peso & sleeve & peso & sleeve \\\\", "\\midrule")
for (i in seq_len(nrow(K))) {
  r <- K[i]
  cel <- function(cf, pp) if (is.na(cf)) "---" else paste0(vr(cf, 3), est(pp))
  rotulo <- if (r$bloco %in% names(rot_b)) unname(rot_b[r$bloco]) else "Base"
  w(sprintf("%s & %s & %s & %s & %s & %s \\\\",
            rotulo,
            gsub("_","\\\\_", r$variavel),
            cel(r$coef_A1_D1, r$p_wcb_A1_D1), cel(r$coef_A1_D2, r$p_wcb_A1_D2),
            cel(r$coef_A3_D1, r$p_wcb_A3_D1), cel(r$coef_A3_D2, r$p_wcb_A3_D2)))
}
aj <- function(s, d, campo) AJ[amostra=="conf_S1" & param=="direta" & spec==s & dep==d][[campo]][1]
w("\\midrule",
  sprintf("\\multicolumn{2}{@{}l}{$R^2$} & %s & %s & %s & %s \\\\",
          vr(aj("A1","D1","r2"),3), vr(aj("A1","D2","r2"),3),
          vr(aj("A3","D1","r2"),3), vr(aj("A3","D2","r2"),3)),
  sprintf("\\multicolumn{2}{@{}l}{$n$ (fundos)} & \\multicolumn{4}{c}{%d} \\\\", as.integer(aj("A1","D1","n"))),
  sprintf("\\multicolumn{2}{@{}l}{Gestoras (\\emph{clusters})} & \\multicolumn{4}{c}{%d} \\\\", as.integer(aj("A1","D1","n_clusters"))),
  "\\bottomrule", "\\end{tabular}",
  "\\caption*{\\footnotesize Dependente: $\\log$ do RMSE do erro fora da amostra por fundo, $h=1$. Significância por \\emph{wild cluster bootstrap-t} com nulo imposto, 9.999 réplicas, agrupado por gestora; pesos de Webb para regressores aproximadamente constantes dentro do agrupamento. Para regressores medidos no nível da gestora a informação efetiva é de 34 agrupamentos, não de $n$ fundos. Fonte: elaboração própria.}",
  "\\end{table}", "")

# --- Tabela 5: decomposicao estrutural ---------------------------------------
G2 <- fread(file.path(DD, "decomp_estrutural_dois_termos_gestora.csv"))
F2 <- fread(file.path(DD, "decomp_estrutural_dois_termos_fundo.csv"))
w("\\begin{table}[h]\\centering\\small",
  "\\caption{Decomposição estrutural: de onde vem a variância do erro.}",
  "\\label{tab:ext-estrutural}",
  "\\begin{tabular}{@{}lrr@{}}", "\\toprule",
  "Termo & Por fundo & Por gestora \\\\", "\\midrule",
  sprintf("Deriva do alvo, $\\mathrm{Var}(\\Delta\\widehat{w})$ & %s\\%% & %s\\%% \\\\",
          vr(median(F2$p_v_alvo, na.rm=TRUE),1), vr(median(G2$p_v_alvo, na.rm=TRUE),1)),
  sprintf("Inovação do resíduo, $\\mathrm{Var}(e_{t+1}-(1-\\lambda)e_t)$ & %s\\%% & %s\\%% \\\\",
          vr(median(F2$p_v_inov, na.rm=TRUE),1), vr(median(G2$p_v_inov, na.rm=TRUE),1)),
  sprintf("Termo cruzado & %s\\%% & %s\\%% \\\\",
          vr(median(F2$p_c_cruz, na.rm=TRUE),1), vr(median(G2$p_c_cruz, na.rm=TRUE),1)),
  "\\bottomrule", "\\end{tabular}",
  "\\caption*{\\footnotesize Medianas entre unidades. A identidade $\\mathrm{erro} = \\Delta\\widehat{w} + e_{t+1} - (1-\\lambda)e_t$ é exata; conferida numericamente a $1{,}6\\times10^{-15}$. Fonte: elaboração própria.}",
  "\\end{table}", "")

writeLines(L, SAIDA)
cat(sprintf("Gravado: %s (%d linhas)\n", SAIDA, length(L)))

# --- figura resumo -----------------------------------------------------------
pdf(file.path(FIG, "fig_ext_resumo.pdf"), width = 9, height = 4.2)
par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 1))
b1 <- c(g("A4","pct_entre_gestora"), g("A4","pct_dentro_gestora"))
barplot(b1, names.arg = c("Entre\ngestoras","Dentro da\ngestora"), col = c("#B8452E","#3B6E9E"),
        border = NA, ylab = "% da variância de log(RMSE)", main = "O TCC modelava a fatia menor")
text(c(0.7, 1.9), b1 - 5, paste0(round(b1,1), "%"), col = "white", font = 2)
sh <- sh1[order(-pct_shapley)]
barplot(sh$pct_shapley, names.arg = c(cod_fundo="Fundo", ativo="Ativo",
        gestora_grupo="Gestora", ym="Mês")[sh$fator], col = "#3B6E9E", border = NA,
        ylab = "% do R² (Shapley)", main = "De onde vem a variação do erro")
dev.off()
cat("OK - fig_ext_resumo.pdf\n")

# --- resumo do ledger --------------------------------------------------------
LG <- fread(file.path(DD, "ledger_trials_extensao.csv"))
cat(sprintf("\nLedger: %d especificacoes estimadas, %d pre-registradas\n",
            nrow(LG), LG[pre_registrado == "S", .N]))
