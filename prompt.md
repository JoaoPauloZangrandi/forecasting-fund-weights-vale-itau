# PROMPT / HANDOFF — Projeto "Forecasting de pesos de VALE3 nos fundos Itaú"

> **Para que serve este arquivo:** é o ponto único de retomada do projeto. Se você
> é um agente de IA (Claude Code, Codex, etc.) continuando este trabalho, **leia
> este arquivo inteiro primeiro**. Ele descreve TUDO: objetivo, metodologia do
> orientador, bases, decisões, estado atual, onde paramos e como trabalhar. O
> detalhe forense de qualidade de dados está em `docs/log_decisoes.md` (leia
> também). Mantenha **os dois** arquivos atualizados a cada passo.
>
> **Repo:** https://github.com/JoaoPauloZangrandi/forecasting-fund-weights-vale-itau
> **Local:** `C:\Users\joaoz\forecasting-fund-weights-vale-itau`
> **Última atualização deste arquivo:** após o Passo 4 (features de fundo).

---

## 1. Como trabalhar (REGRAS — não negociáveis)

1. **Ir um passo por vez.** Explicar o que vai fazer ANTES de fazer e **esperar o
   "ok"** do orientando. Nunca empilhar vários passos/scripts de uma vez.
2. **Questionar cada decisão metodológica** e não fazer suposições precipitadas
   ("não passar a carroça na frente dos bois"). O orientando quer ENTENDER cada
   coisa antes de rodar.
3. **Parsing é crítico.** Sempre conferir o formato bruto, certificar 0 NAs após
   parse, e validar contra outra fonte quando possível. Bases CVM têm formatos
   traiçoeiros e MISTURAM formatos entre colunas (ver seção 4).
4. **Na dúvida metodológica, gerar 2+ versões/variantes** em vez de escolher uma
   só (o orientando prefere comparar números).
5. **Registrar absolutamente tudo** em `docs/log_decisoes.md` (decisões, achados,
   raciocínio, forense) e manter este `prompt.md` atualizado.
6. **Linguagem: R** (R 4.5.1). Rodar:
   `& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/<script>.R`
7. **Entregar no GitHub em DOIS formatos:** scripts segmentados (`R/01`, `R/02`…)
   **e** `R_full.R` (todo o código num arquivo só, sem segmentação). Manter os dois
   sincronizados a cada passo.
8. **Commitar e dar push a cada passo** (mensagens descritivas em pt). Salvar o
   contexto sempre, para sobreviver a troca de sessão/ferramenta.

---

## 2. O projeto

**Objetivo:** prever o **peso de VALE3** dentro dos fundos da gestora **Itaú**, e com
isso entender **quais características dos fundos** explicam/preveem a posição.

**Orientador:** Prof. **Maurício Ferraresi Jr.** (FGV EESP). É um TCC.
**Aluno/orientando:** João Paulo Zangrandi (FGV EESP, economia/finanças).

**Por que peso primeiro:** é mais simples e relativamente *bounded* em [0,1].
Escopo inicial: **VALE3 posição direta**, **gestora Itaú**, **ano 2016** (piloto).
Depois generaliza para 2017–2021 e, possivelmente, outras ações/gestoras.

### Anotações da reunião com o orientador (cruas, organizadas)
- Pegar VALE; ~1000 fundos no mercado têm VALE; ideia de **filtro de Kalman**.
- **Regressão em dois passos.** Passo 1: uma **cross-section** (já temos muita
  informação) — regredir o peso de VALE3 nos fundos contra características; o
  **beta da OLS vira um fator latente** que reduz um problema de alta dimensão a
  uma dimensão menor (resume "como as posições são explicadas pelas características
  dos fundos"). Passo 2: tornar isso **dinâmico no tempo** → propor um formalismo
  ao longo do tempo: θ de janeiro = θ de fevereiro + choque (tipo **random walk**);
  quem ajuda a prever é o **θ**. É um **modelo de fator dinâmico**.
- A cross-section pode ser estimada por **ML** (aprender quais características
  preveem o peso de amanhã).
- Escolher ~**5 características de fundo** e **1–2 da ação**.
- **Características de fundo:** AUM; aplicação − resgate (fluxo líquido); FIC ou FI;
  nº de cotistas; (retirar fundos exclusivos em algum momento).
- **Características da ação:** preço da VALE; beta da VALE — medidos no **último dia
  do mês anterior** ao da análise (testar também início do mês e outras datas).
- Previsão multi-horizonte: usar dados até t para prever t+1, t+2, t+3…; ou usar 2
  meses para prever os próximos; até cobrir todos os meses. Comparar estimado vs
  real → **erro**.
- Produto: uma **matriz de erros** (nº de fundos do Itaú × nº de características).
  Testar propriedades dos erros (média idealmente 0, estacionariedade, variância
  baixa).
- "A graça": ter posições consolidadas + modelo preditivo para o peso, e entender
  **quais características os fundos olham**.

---

## 3. Plano de execução (passo a passo)

| Passo | O que é | Status |
|------|---------|--------|
| 1 | Carga/sanidade das bases CONS e SH | ✅ feito |
| 2 | Painel fundo×mês do **peso** de VALE3 (Itaú, 2016) | ✅ feito |
| 3 | Característica de **fluxo** (captação/resgate/líquido) | ✅ feito |
| 4 | Características de fundo: **AUM, nº cotistas, FIC/FI** (+ANBIMA) | ✅ feito |
| 5 | Características da ação: **preço e beta da VALE** (fonte externa) | ⬜ próximo |
| — | Remover fundos exclusivos (1–2 cotistas) | ⬜ futuro |
| — | Generalizar para **2017–2021** | ⬜ futuro |
| — | **Regressão cross-section** (peso ~ características → beta = fator latente) | ⬜ futuro |
| — | **Modelo de fator dinâmico** (θ random walk / Kalman) + previsão + matriz de erros | ⬜ futuro |

---

## 4. Bases de dados

**Diretório dos dados brutos (fora do git):**
`C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF` (override por env
`CVM_DATA_DIR`). Contém só CONS e SH (2016–2021). Informe Diário é baixado para
`data/raw/` (gitignored; `R_full.R` baixa sozinho se faltar).

### 4.1 CONS (`cons_AAAA.csv`) — composição consolidada, MENSAL
Colunas: `CNPJ | Anbima | Código | Nome | Tipo_Ativo | Data_Competência |
Nome_Ativo | Valor_Ativo_mil | Participação_Ativo`.
- Decimal **ponto** (americano); valores às vezes em **notação científica**
  (`7.18E-4`). `Data_Competência` em `YYYY-MM-DD` (último dia útil do mês).
- `Participação_Ativo` é o **peso** e é uma **FRAÇÃO** (0–~0,151; máx 15,1%), não %.
- Ativo-alvo: **exatamente** `Nome_Ativo == "VALE ON N1 - VALE3"` (posição direta).
- Quirk: às vezes **linhas EXATAMENTE duplicadas** (ex. fundo 187623) → dedup.

### 4.2 SH (`SH_AAAA.csv`) — série histórica, DIÁRIA
Colunas: `COD_FUNDO | CNPJ | NOME_FUNDO | GESTORA | CLASSIFICACAO_ANBIMA | DATA |
APLICAÇÃO | COTA | NUMERO_DE_COTISTAS | PATRIMONIO_LIQUIDO_(MIL)`.
- **MISTURA formatos:** `DATA` em `DD/MM/YYYY`; `APLICAÇÃO` e `PATRIMONIO_LIQUIDO`
  em `"R$ x.xxx,xx"` (BR com cifrão); `COTA` em formato americano puro.
- `APLICAÇÃO` = **captação bruta diária** (sempre ≥0); **NÃO tem resgate**.
- `PATRIMONIO_LIQUIDO_(MIL)` em **MIL** de R$ (multiplicar por 1000 p/ R$ cheios).
- `CLASSIFICACAO_ANBIMA` é **estratégia** (Ações Livre, Multimercados…), **não**
  distingue FIC/FI.

### 4.3 Informe Diário de Fundos (`inf_diario_fi`) — CVM Dados Abertos, DIÁRIO
Fonte do **fluxo líquido verdadeiro** (tem captação E resgate).
- URL: `https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/` (mensais 2021+) e
  `.../HIST/inf_diario_fi_AAAA.zip` (anual, anos antigos; **2016** = HIST).
- **Muito mais limpo** que a SH: sep `;`, decimal `.`, sem `R$`; `fread` dá 0 NAs.
- Colunas: `CNPJ_FUNDO;DT_COMPTC;VL_TOTAL;VL_QUOTA;VL_PATRIM_LIQ;CAPTC_DIA;RESG_DIA;NR_COTST`.
- PL (`VL_PATRIM_LIQ`) em **reais cheios** (a SH é em mil → fator 1000).
- Join ao painel por **CNPJ + data**.

### 4.4 Preço/beta da VALE — FONTE EXTERNA (passo 5, ainda não feito)
Não está em CONS/SH. Orientando **autorizou** puxar de **fonte externa extremamente
confiável**. Medir no **último dia do mês anterior** (testar variantes de data).
Decidir a fonte com o orientando (ex.: B3, Yahoo Finance via R, etc.).

### 4.5 Chave de join principal
`CONS.Código == SH.COD_FUNDO` (validado). CNPJ idêntico (mascarado
`xx.xxx.xxx/xxxx-xx`) entre CONS, SH e Informe Diário → permite join por CNPJ.

---

## 5. Decisões travadas

- **Alvo:** peso de VALE3 (fração), posição direta, `"VALE ON N1 - VALE3"`.
- **Gestora:** SH.GESTORA ∈ {Itaú Asset Management, Itaú DTVM, Itaú Unibanco}
  (match por forma normalizada: sem acento, maiúsculo).
- **Fundos exclusivos:** **NÃO** remover por ora (manter simples). Anotado p/ depois.
- **Merge SH×CONS (passo 2) e snapshot de features (passo 4):** **dia-exato**
  (`SH.DATA == CONS.Data_Competência`), 0% de perda.
- **Fluxo (passo 3):** fonte = **Informe Diário** (`CAPTC_DIA − RESG_DIA`),
  agregado por **soma no mês-calendário** da competência; validado com a SH.
- **AUM:** PL na competência, convertido p/ **R$ cheios** (×1000).
- **FIC/FI:** pelo **NOME** por **token** `\bFIC\b`/`\bFICFI\b` (ANBIMA não serve).
- **Período v1:** só **2016**.

---

## 6. Estado atual — onde estamos

**Passos 1 a 4 FEITOS, commitados e no GitHub.** O painel final atual é
`data/processed/painel_vale_itau_2016_features.csv` (regenerável; `data/processed/`
é gitignored). É um painel **fundo × mês** (2016), 4.062 obs, 388 fundos, 12 meses.

### Colunas do painel final
| Coluna | Significado |
|--------|-------------|
| `cod_fundo`, `cnpj`, `nome_fundo`, `gestora` | identificação do fundo |
| `is_fic` | 1=FIC, 0=FI (pelo nome) |
| `classif_anbima` | estratégia ANBIMA (extra) |
| `data`, `ano`, `mes` | competência (último dia útil do mês) |
| **`peso_vale3`** | **ALVO** — peso (fração) de VALE3 na carteira |
| `valor_mil` | valor da posição VALE3 (CONS, em mil) |
| `aum` | patrimônio líquido na competência (R$ cheios) |
| `n_cotistas` | nº de cotistas na competência |
| `captacao`, `resgate`, `fluxo_liq` | fluxo mensal (Informe Diário, R$ cheios) |
| `n_dias` | nº de dias úteis somados no fluxo do mês |
| `captacao_sh` | captação somada da SH (p/ comparação) |
| `div_captacao` | flag 0/1: SH e Informe Diário discordam na captação |

### Lado direito da equação (características)
- ✅ **Fundo:** AUM, nº cotistas, FIC/FI (passo 4) + fluxo líquido (passo 3).
- ⬜ **Ação:** preço e beta da VALE (passo 5, externo) — ÚNICO que falta.

### Achados de qualidade de dados já tratados (resumo; detalhe no log)
- Dedup de linhas exatas na CONS; remoção de pesos negativos (resíduos ~−1e-6).
- `SH.APLICAÇÃO == CAPTC_DIA` do Informe Diário em 99,66% (ao centavo) → parsing
  certificado. ~0,3% divergem por diferença de série CVM (não bug): padrão A =
  spike de 1 dia; padrão B = razão constante (FIC 318231: SH=0,7631×Informe em
  123/251 dias). Decisão: usar Informe Diário, manter flag `div_captacao`, **sem**
  teste de robustez na regressão (só registrar). Ver Apêndice A do log.
- Agregação de fluxo verificada por recomputo independente (janela de data): bate
  exato; 0 desalinhamento de mês.
- FIC por token validado (substring==token, 0 falso positivo em 2016).
- ⚠️ Mediana de cotistas = 2 → muitos fundos exclusivos (remoção futura).

---

## 7. ONDE PARAMOS / próximo passo

**Próximo = Passo 5: preço e beta da VALE.** Pendências a decidir com o orientando
ANTES de codar:
1. **Fonte** do preço/beta (externa, confiável) — qual? (B3 oficial, Yahoo via R…).
2. **Data de medição:** "último dia do mês anterior" como default; testar variantes
   (início do mês, último fechamento, etc.) → gerar versões.
3. **Beta:** janela de estimação (ex.: 12/24/36 meses de retornos) e **índice de
   mercado** (Ibovespa? IBX?). Definir.
4. **Parsing/alinhamento:** certificar como sempre (0 NAs, datas casadas com a
   competência do painel).

Depois do passo 5: considerar remover exclusivos; generalizar 2017–2021 (reaplicar
TODOS os testes de parsing/validação — ver regras do log, esp. regra 6); e então a
**regressão cross-section** (passo 1 da metodologia do orientador).

---

## 8. Estrutura do repo e como rodar

```
R/01_load_bases.R            # passo 1: carga/sanidade
R/02_build_panel_vale_itau.R # passo 2: painel do peso de VALE3
R/03_add_fund_flows.R        # passo 3: fluxo (Informe Diário) + flag div_captacao
R/04_add_fund_features.R     # passo 4: aum, cotistas, FIC/FI, ANBIMA
R_full.R                     # TODO o código num arquivo só (espelha R/01..04)
docs/log_decisoes.md         # log vivo: decisões, achados, forense (Apêndice A)
prompt.md                    # este arquivo (handoff)
data/raw/                    # bruto (gitignored); Informe Diário baixado aqui
data/processed/              # saídas (gitignored, regeneráveis)
```

**Rodar tudo (recomendado, robusto a diretório):**
`& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R_full.R`
Os scripts fazem `setwd(PROJ_DIR)` sozinhos (default no caminho do repo; override
por env `PROJ_DIR`). Se rodar manualmente no RStudio fora do projeto, faça antes
`setwd("C:/Users/joaoz/forecasting-fund-weights-vale-itau")`.

**Ferramentas:** R 4.5.1 em `C:\Program Files\R\R-4.5.1\bin\Rscript.exe`
(fora do PATH). Pacote principal: `data.table`.
