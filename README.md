# Forecasting de pesos de ações em fundos

Novo projeto para construir, passo a passo, um painel de pesos de ações dentro de fundos.

Nesta primeira versão, o repositório faz apenas uma coisa: carregar as bases CONS e SH em R e validar que os
arquivos estão acessíveis.

## Bases

As bases brutas não ficam no Git. O caminho padrão esperado é:

```text
C:/Users/joaoz/Downloads/Consolidado_MF/Consolidado_MF
```

Se as bases estiverem em outra pasta, defina a variável de ambiente `CVM_DATA_DIR`.

## Primeiro script

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/01_load_bases.R
```

Por padrão, o script lê uma amostra de 1.000 linhas de cada arquivo anual CONS e SH, apenas para checar
caminhos e colunas. Ele não filtra Itaú, não filtra VALE e não calcula pesos.

Para testar mais linhas:

```powershell
$env:LOAD_ROWS = "10000"
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/01_load_bases.R
```

Para carregar arquivos completos, usar depois, com cuidado:

```powershell
$env:FULL_LOAD = "TRUE"
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/01_load_bases.R
```
