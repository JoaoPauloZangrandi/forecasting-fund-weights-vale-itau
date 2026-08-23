# =============================================================================
# config_periodo.R  (v2 OFICIAL/scripts)
#
# Config central do range de anos da ingestao de dado bruto (CONS/SH da CVM,
# COTAHIST da B3). Criado em 15/08/2026 como preparacao pra expansao de
# amostra 2016-2021 -> 2016-2026 (ver v2 OFICIAL/PLANO_EXPANSAO_2021_2026.md).
#
# ANTES da expansao: so' este arquivo precisa ser editado (ANO_FIM), em vez
# de editar ~10 scripts um por um sob pressao de prazo. Scripts que dependem
# do range de ano devem fazer source(file.path(dirname(...), "config_periodo.R"))
# e usar ANO_INICIO:ANO_FIM em vez de 2016:2021 hardcoded.
#
# NAO editar isto ainda -- so' quando os dados de 2022-2026 estiverem
# realmente disponiveis em CVM_DATA_DIR e data/raw/. Ate la, ANO_FIM=2021L
# mantem o comportamento identico ao pipeline atual (nenhuma mudanca de
# resultado so' por este arquivo existir).
# =============================================================================
ANO_INICIO <- 2016L
ANO_FIM    <- 2021L   # Revertido 23/08/2026: sem verdade-base (Economatica) pra validar
                       # 2022-2026 reconstruido de dado publico da CVM -- teto de erro de
                       # 9-18%/fundo so' foi confirmado pra 2019-2021 (ver PLANO_EXPANSAO_2021_2026.md).
                       # TCC volta a usar so' o range com dado original da Economatica.

cat(sprintf("[config_periodo.R] Range de anos ativo: %d:%d\n", ANO_INICIO, ANO_FIM))
