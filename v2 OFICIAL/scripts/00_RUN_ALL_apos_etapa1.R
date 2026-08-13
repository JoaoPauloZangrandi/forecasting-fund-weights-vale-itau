# =============================================================================
# 00_RUN_ALL_apos_etapa1.R
#
# Roda TUDO que precisa rodar de novo depois de qualquer mudanca no inicio da
# Etapa 1 (corte de qualidade de dado, definicao de caracteristica, filtro de
# amostra, etc.) -- na ordem certa de dependencia, sem pular nada.
#
# Existe porque em 12/08/2026 uma mudanca no corte de soma_peso (105%->150%)
# foi feita so na trilha teste_estrategia (Secao 6); a trilha principal
# (Secao 4/5, painel_multiativo_final.csv) e o TCC_finalV2.tex ficaram
# desatualizados por uma sessao inteira sem ninguem perceber, porque nao
# existia uma lista central de "isso tudo depende do inicio da Etapa 1".
# Ver PIPELINE.md (mesma pasta) para o mapa script -> tabela/figura do TCC.
#
# COMO USAR
#   1. Se so mudou o CORTE DE SOMA_PESO (a situacao mais comum): edite o
#      valor de LIM dentro do script 100 (variavel LIM, linha ~21) e rode
#      este script inteiro com RESTAURAR_BACKUP_100 = TRUE (default).
#   2. Se mudou outra coisa da Etapa 1 (ex.: definicao de caracteristica,
#      formula do HHI, filtro de universo): edite o(s) script(s) relevante(s)
#      ANTES de rodar este arquivo, e rode com RESTAURAR_BACKUP_100 = FALSE
#      (nao precisa remexer no corte de soma_peso).
#   3. Depois que este script terminar (sem erro), os dados canonicos estao
#      100% frescos. Falta so a parte que NENHUM script faz sozinho: abrir
#      TCC_finalV2.tex e conferir cada numero contra os logs em
#      v2 OFICIAL/scripts/_log_*_novo.txt (ver PIPELINE.md, coluna "onde no
#      TCC"). Isso e' deliberadamente manual -- nenhum script aqui edita o
#      .tex.
#
# TEMPO ESTIMADO: ~15-25 min (maior parte e' script 99, ~5-8 min, e os
# joins de milhoes de linhas nos scripts 61/62/70/77/84/86/87/88/92/95,
# ~30-90s cada).
#
# RODAR COM CAMINHO ABSOLUTO:
#   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "v2 OFICIAL/scripts/00_RUN_ALL_apos_etapa1.R"
# =============================================================================

REPO <- "C:/Users/joaoz/forecasting-fund-weights-vale-itau"
SCRIPTS <- file.path(REPO, "v2 OFICIAL/scripts")
DATA <- file.path(REPO, "v2 OFICIAL/data")
LOGDIR <- SCRIPTS  # mesma convencao ja usada: _log_NN_novo.txt fica junto dos scripts

RESTAURAR_BACKUP_100 <- TRUE  # ver "COMO USAR" acima

rodar <- function(script, log_suffix = NULL) {
  num <- sub("_.*", "", script)
  if (is.null(log_suffix)) log_suffix <- num
  log_path <- file.path(LOGDIR, sprintf("_log_%s_novo.txt", log_suffix))
  cat(sprintf("\n\n========== RODANDO %s ==========\n", script))
  t0 <- Sys.time()
  status <- system2(
    "C:/Program Files/R/R-4.5.1/bin/Rscript.exe",
    shQuote(file.path(SCRIPTS, script)),
    stdout = log_path, stderr = log_path
  )
  dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
  if (status != 0) {
    stop(sprintf("FALHOU: %s (exit %d, %.1fs) -- ver %s", script, status, dt, log_path))
  }
  cat(sprintf("OK (%.1fs) -- log em %s\n", dt, log_path))
  invisible(readLines(log_path))
}

# =============================================================================
# ETAPA A -- limpeza do painel na origem (soma_peso, script 100)
#
# ARMADILHA (ja caiu nela uma vez): o script 100 tem guarda de backup
# (so copia pro .bak se o .bak ainda nao existir). Se voce mudar o LIM e
# rodar de novo SEM restaurar o painel bruto primeiro, o script aplica o
# novo corte EM CIMA do painel ja cortado pelo LIM antigo -- se o novo corte
# for mais permissivo (LIM maior), isso da um resultado ERRADO (silencioso,
# sem warning) porque as linhas que deveriam voltar a entrar ja foram
# apagadas na rodada anterior. Por isso este passo SEMPRE restaura do backup
# antes de reaplicar o corte, a nao ser que RESTAURAR_BACKUP_100 = FALSE.
# =============================================================================
if (RESTAURAR_BACKUP_100) {
  cat("\n\n========== ETAPA A: restaurando painel bruto do backup antes de recortar ==========\n")
  bak <- file.path(DATA, "painel_multiativo_direto_completo_ANTES_LIMPEZA.csv.bak")
  alvo <- file.path(DATA, "painel_multiativo_direto_completo.csv")
  if (!file.exists(bak)) stop("Backup nao existe: ", bak, " -- rode o script 100 manualmente uma vez pra criar")
  file.copy(bak, alvo, overwrite = TRUE)
  cat("OK - painel bruto restaurado\n")
}
rodar("100_checagem_soma_peso_universo_completo.R")

# =============================================================================
# ETAPA B -- reconstruir o painel final da trilha "universo completo"
#             (Secao 6) e re-sincronizar com a trilha principal (Secao 4/5)
# =============================================================================
rodar("58_merge_final_universo_completo.R")

cat("\n\n========== ETAPA B2: re-sincronizando painel_multiativo_final.csv ==========\n")
{
  bak_pm <- file.path(DATA, sprintf("painel_multiativo_final_ANTES_%s.csv.bak", format(Sys.Date(), "%Y%m%d")))
  if (!file.exists(bak_pm)) file.copy(file.path(DATA, "painel_multiativo_final.csv"), bak_pm)
  file.copy(file.path(DATA, "painel_universo_completo_final.csv"),
            file.path(DATA, "painel_multiativo_final.csv"), overwrite = TRUE)
  cat("OK - painel_multiativo_final.csv sobrescrito com painel_universo_completo_final.csv (backup:",
      basename(bak_pm), ")\n")
}

# =============================================================================
# ETAPA C -- motores de Etapa 1+3 (os dois pesados; geram os arquivos
#             canonicos que quase tudo depois le)
# =============================================================================
rodar("99_pipeline_hhi_lag_etapa1.R")   # Secao 4/5 -- theta/erro_e/etapa3_multiativo_*
rodar("101_cross_section_universo_completo_hhi.R")  # Secao 6 / funil -- theta/peso_pred_universo_completo

# =============================================================================
# ETAPA D -- Etapa 1/2 estatisticas descritivas + Etapa 2/3 da trilha
#             "universo completo"
# =============================================================================
rodar("98_etapa1_etapa2_stats_multiativo.R")   # Secao 5 -- erro_e/erro_u stats
rodar("61_ajuste_parcial_universo_completo.R") # Secao 6 -- Etapa 2 lambda (LER O LOG!)
rodar("62_etapa3_universo_completo.R")         # Secao 6 -- Etapa 3 com correcao mecanica
rodar("82_etapa3_multiativo_janela_expansiva.R") # Secao 5 -- janela expansiva

# =============================================================================
# *** PARADA OBRIGATORIA ***
# Os scripts 63/66/67 tem o lambda da Etapa 2 HARDCODED (nao leem de
# nenhum CSV -- decisao de projeto antiga, nunca corrigida). Depois do
# ETAPA D, abra v2\ OFICIAL/scripts/_log_61_novo.txt, pegue o valor de
# "lambda COM correcao mecanica" e confira se bate com a linha
#     lam <- 0.XXXX
# nos 3 scripts abaixo ANTES de continuar. Se nao bater, edite os 3 (mesmo
# valor nos 3) e so entao rode a Etapa E. Isso e' o passo que mais falhou
# no passado -- nao pule.
# =============================================================================
lam_log <- readLines(file.path(LOGDIR, "_log_61_novo.txt"))
lam_linha <- grep("lambda COM correcao mecanica", lam_log, value = TRUE)
cat("\n\n*** CONFERIR AGORA: ", lam_linha, " ***\n")
cat("*** Compare com 'lam <- 0.XXXX' nos scripts 63/66/67 antes de continuar. ***\n\n")

# =============================================================================
# ETAPA E -- figuras/tabelas da Secao 5 (todas independentes entre si,
#             todas dependem so da Etapa C)
# =============================================================================
for (s in c("93_gera_latex_tabelas_9_10.R",
            "74_figuras_dinamica_erro_tcc.R",
            "75_figura_margem_dinamica.R",
            "77_figura_vies_dinamico_gestora.R",
            "78_figura_beta_vs_erro.R",
            "80_figura_ranking_horizontes.R",
            "84_persistencia_vies_gestora.R",
            "85_correcao_vies_fora_amostra.R",
            "86_decomposicao_e_persistencia_variancia.R",
            "87_explica_variancia_gestora.R",
            "88_concentracao_mes_a_mes.R",
            "92_figura_trajetoria_fundo_v2.R",
            "95_regressao_completa_variancia.R",
            "45_fig_multihorizonte_gestora_rmse_margem.R")) {
  rodar(s)
}

# =============================================================================
# ETAPA F -- cadeia de pares replicantes / sinal do robo (Secao 6,
#             SEQUENCIAL -- cada um le o CSV do anterior)
# =============================================================================
for (s in c("63_pares_replicantes.R",
            "64_significancia_pares_replicantes.R",
            "65_resumo_pares_replicantes.R",
            "66_beta_pares_lideranca.R",
            "67_previsao_ajustada_seguidor.R",
            "68_sinal_robo_reais_agregado.R",
            "69_figuras_teste_estrategia.R")) {
  rodar(s)
}

# =============================================================================
# ETAPA G -- teste decisivo (Secao 6, SEQUENCIAL)
# =============================================================================
for (s in c("70_teste_fluxo_prediz_retorno.R",
            "71_teste_fluxo_retorno_restrito.R",
            "72_figuras_teste_retorno.R")) {
  rodar(s)
}

cat("\n\n=====================================================================\n")
cat("PIPELINE COMPLETO. Todos os dados/figuras canonicos estao frescos.\n")
cat("PROXIMO PASSO (manual, nenhum script faz isso): abrir PIPELINE.md e\n")
cat("conferir, linha por linha, cada numero do TCC_finalV2.tex contra os\n")
cat("logs _log_*_novo.txt gerados agora. So depois disso, recompilar e\n")
cat("commitar.\n")
cat("=====================================================================\n")
