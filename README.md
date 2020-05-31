# kdd-2020-enem-database



## Scripts

### Importação da base de dados

1. Para importar a base de dados execute o arquivo `r-workspace/importa-filtra-basededados.R`
2. O arquivo `r-workspace/output/spCandidatos2012.csv` contendo resultado de alunos para o estado de São Paulo será gerado.

### Análise descritiva

1. Para cada atributo da base existe um gráfico de distribuição em `analise-descritiva.R`
2. Caso queira gerar os gráficos no diretório `analise-descritiva` utilize o script `graficos-analise-descritiva.R`

### Análise de Grupos

1. Executar o script `r-workspace/analise-agrupamento.R`

### Regras de Associação

1. Executar o script `r-workspace/preprocessamento-regras-associacao.R`
2. Converter base para transacional `python-workspace/prepare-database-association-rule.py`
3. Executar `r-workspace/analise-regras-de-associacao.R`
