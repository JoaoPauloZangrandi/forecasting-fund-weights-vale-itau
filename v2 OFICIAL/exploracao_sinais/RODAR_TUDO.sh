#!/bin/bash
# =============================================================================
# RODAR_TUDO.sh  (v2 OFICIAL/exploracao_sinais)
#
# Script mestre criado em 15/08/2026, preparacao pra expansao de dados
# 2016-2021 -> 2016-2026 (ver v2 OFICIAL/PLANO_EXPANSAO_2021_2026.md).
#
# Roda TODOS os scripts numerados de exploracao_sinais/scripts/ (01 a 87 no
# momento da criacao, mais o que for adicionado depois), NA ORDEM, um por
# um, logando cada saida separadamente. Pedido explicito do usuario
# (15/08/2026): "quero rerodar a bateria toda de TUDO que testamos,
# absolutamente tudo" -- este script existe pra isso ser 1 comando em vez
# de rodar 80+ scripts manualmente sob pressao de prazo.
#
# NAO para no primeiro erro -- continua pros proximos scripts e reporta um
# resumo no final (script com erro isolado nao deve travar o resto da
# bateria). Logs individuais ficam em RERUN_2016_2026/_log_NN.txt.
#
# COMO USAR:
#   1. Confirmar que v2 OFICIAL/data/precos_mensais_final.csv e
#      v2 OFICIAL/data/painel_multiativo_final.csv ja foram atualizados
#      pra cobrir ate 2026 (rodar o pipeline oficial primeiro, ver
#      PLANO_EXPANSAO_2021_2026.md, Etapas 0-1).
#   2. bash RODAR_TUDO.sh
#   3. Conferir RESUMO_RERUN.txt ao final pra ver o que falhou (se algo
#      falhar, o log individual do script mostra o erro exato).
# =============================================================================
set -u
cd "$(dirname "$0")"

RSCRIPT="/c/Program Files/R/R-4.5.1/bin/Rscript.exe"
OUTDIR="RERUN_2016_2026"
mkdir -p "$OUTDIR"

RESUMO="$OUTDIR/RESUMO_RERUN.txt"
echo "Rerun completo iniciado em $(date)" > "$RESUMO"
echo "" >> "$RESUMO"
printf "%-55s %-10s %s\n" "script" "status" "duracao(s)" >> "$RESUMO"

# lista todos os scripts numerados, em ordem numerica (nao alfabetica --
# 2 viria antes de 10 alfabeticamente, o que seria errado)
mapfile -t SCRIPTS < <(ls scripts/ | grep -E '^[0-9]+_.*\.R$' | sort -t_ -k1,1n)

echo "Total de scripts encontrados: ${#SCRIPTS[@]}"
echo ""

N_OK=0
N_ERRO=0
INICIO_TOTAL=$(date +%s)

for f in "${SCRIPTS[@]}"; do
  nome="${f%.R}"
  logfile="$OUTDIR/_log_${nome}.txt"
  echo "=== Rodando $f ==="
  t0=$(date +%s)
  "$RSCRIPT" "scripts/$f" > "$logfile" 2>&1
  rc=$?
  t1=$(date +%s)
  dur=$((t1-t0))
  if [ $rc -eq 0 ]; then
    echo "  OK (${dur}s)"
    printf "%-55s %-10s %s\n" "$f" "OK" "$dur" >> "$RESUMO"
    N_OK=$((N_OK+1))
  else
    echo "  ERRO (rc=$rc, ${dur}s) -- ver $logfile"
    printf "%-55s %-10s %s\n" "$f" "ERRO(rc=$rc)" "$dur" >> "$RESUMO"
    N_ERRO=$((N_ERRO+1))
  fi
done

FIM_TOTAL=$(date +%s)
DUR_TOTAL=$((FIM_TOTAL-INICIO_TOTAL))

echo "" >> "$RESUMO"
echo "Total: ${#SCRIPTS[@]} scripts | OK: $N_OK | ERRO: $N_ERRO | Tempo total: ${DUR_TOTAL}s (~$((DUR_TOTAL/60))min)" >> "$RESUMO"

echo ""
echo "===================================================="
echo "RERUN COMPLETO. Total: ${#SCRIPTS[@]} | OK: $N_OK | ERRO: $N_ERRO"
echo "Tempo total: ${DUR_TOTAL}s (~$((DUR_TOTAL/60)) min)"
echo "Resumo salvo em: $RESUMO"
echo "===================================================="
if [ $N_ERRO -gt 0 ]; then
  echo ""
  echo "Scripts com erro (ver log individual pra detalhe):"
  grep "ERRO" "$RESUMO"
fi
