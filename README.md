# Template

Template genérico multipropósito. Pode acomodar diversos projetos simples ou medianos.

Sugestão de uso:

## Pasta "Arquivos"

Concentra arquivos pertinentes ao projeto, mas que não são as fontes de dados (.xlsx, .parquet, etc) — tampouco são os códigos/scripts.

Exemplo: Dicionários, enunciados, arquivos auxiliares, etc.

## Pasta dados

Aqui serão inseridos os dados do projeto. Pode ser subdividida em pastas de dados crus, dados trabalhados e dados prontos, se necessário.

## Pasta rdocs

Aqui constarão os códigos e scripts utilizados na análise. Pode ser subdividida para modularização do projeto. Contém pasta source, com arquivo dirigido para gráficos mais simples e comuns do ggplot, além de arquivo source com funções pertinentes para tarefas rotineiras de DA/DS em R.

## Pasta resultados

Esta pasta salva resultados diretos dos scripts, como por exemplo, gráficos salvos para posterior exportação. Pode ser utilizada também para guardar arquivos do tipo .RDS ou equivalentes.