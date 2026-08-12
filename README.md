# Peso de VALE3 nos fundos de investimento brasileiros

TCC (FGV-EESP): modelo de fator dinâmico para o peso de ações em carteiras de fundos brasileiros
--- cross-section logística (Etapa 1) + ajuste parcial (Etapa 2) + esquema fora da amostra
(Etapa 3), generalizado do caso inicial (VALE3, fundos do Itaú) para o universo CVM inteiro
(todas as gestoras, todas as ações).

## Documento

- **`TCC_finalV2.tex`** --- versão atual do TCC. Base multiativo única do início ao fim (2.296
  fundos, 40 grupos de gestora, 501 ativos), Etapa 1 com 6 características (inclui
  $\text{HHI}_{\text{resto}}$, concentração do restante da carteira).
- **`TCC_final.tex`** --- versão anterior (5 características, troca de base entre seções).
  Mantida para referência/comparação.

Compilar com `latexmk -pdf TCC_finalV2.tex` (MiKTeX).

## Estrutura do repositório

```text
R/, R_full.R          Pipeline v1: só VALE3, só fundos do Itaú (ponto de partida do projeto)
v2 OFICIAL/scripts/    Pipeline v2: generalizado (todas as gestoras, todas as ações),
                       numerado sequencialmente (01-99+); é o que alimenta TCC_finalV2.tex
v2 OFICIAL/data/       Saídas dos scripts (CSVs) -- fora do Git (v2 OFICIAL/data/* no
                       .gitignore, alguns arquivos passam de 1 GB); rodar os scripts p/ gerar
v2 OFICIAL/figuras/    Figuras (.pdf) usadas nos documentos -- versionadas
v2 OFICIAL/v2 OFICIAL.tex, teste_estrategia.tex
                       Documentos de trabalho/exploração, não o TCC oficial
docs/                  log_decisoes.md e anotacoes_orientador_e_ideias.md (histórico de
                       decisões metodológicas e forense de dados); explanation/results.tex
                       são rascunhos antigos
```

Scripts com prefixo `_` (ex.: `_check_*.R`, `_piloto_*.R`) são exploratórios/temporários, não
fazem parte da cadeia oficial e não ficam versionados.

## Pipeline principal (multiativo, o que alimenta o TCC atual)

Ordem de dependência resumida --- ver comentário de cabeçalho de cada script para detalhes:

1. `17`--`29`: construção do painel (todas as gestoras, todos os ativos) →
   `painel_multiativo_final.csv`
2. `90_pipeline_limpa_soma_peso.R`: exclui fundo-mês com soma de peso $>105\%$ (erro de
   reporte da CDA)
3. `99_pipeline_hhi_lag_etapa1.R`: Etapa 1 (6 características, célula ativo-mês) + Etapa 3
   (h=1,3,6,12) --- **é este script que produz os arquivos "oficiais" hoje**
   (`erro_e_multiativo.csv`, `etapa3_multiativo_*.csv`, `theta_multiativo.csv`); os scripts
   `30`/`37`/`42`/`44` (versão de 5 características, anterior ao HHI) escrevem nos mesmos
   nomes de arquivo e **não devem ser rerodados** sem querer
4. `45`--`95`: figuras e tabelas do TCC, lidos diretamente dos arquivos padrão acima

Rodar com `Rscript.exe` e caminho absoluto (os scripts não assumem working directory).

## Bases brutas

Não ficam no Git. Caminho padrão esperado:

```text
C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF
```

Se estiverem em outra pasta, defina a variável de ambiente `CVM_DATA_DIR`. Fonte: CDA (Composição
e Diversificação de Aplicações) e Informe Diário, ambos da CVM.
