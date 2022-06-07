# Análise Exploratória | Risco de Crédito

## Descrição

Esta é a primeira etapa de um projeto para prever o risco de crédito utilizando uma base disponível no repositório da [UCI](https://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29). <br>
E no que consiste essa primeira etapa?
- [Preparação da base](https://github.com/avmachado/AED_CreditRisk/blob/main/CreditRisk_preparacao.R): Envolve basicamente a transformação das variáveis utilizando Label Encoding para facilitar a análise exploratória e demais atividades envolvendo Machine Learning.<br>
O Label Enconding nada mais é do que codificar as suas variáveis com mais de um label com números de 0 a n_labels - 1. Ou seja: se uma variáveis contém 5 labels, os  valores do encoding vão de 0 a 4. <br><br>
<b>OBS.:</b> Para atividades de Machine Learning, o uso do Label Encoding pode não ser tão recomendado para variáveis com mais de dois Labels, pois pode ser confundido pelo modelo como uma variável categórica ordinal, mesmo que a variável não seja. Para esses casos é interessante utilizar o One-Hot Encoding. Porém, por motivos de avaliar a diferença entre uma codifcação e outra, as próximas etapa serão feitas utilizando as duas formas de codificação. Mais informações podem ser encontradas [aqui](https://vitalflux.com/when-use-labelencoder-python-example/).

- [Análise Exploratória de Dados - AED](https://github.com/avmachado/AED_CreditRisk/blob/main/CreditRisk_AED.R): Esse passo é de extrema importância para conhecer melhor como os dados estão organizados, se eles fazem sentido, quais são as características da base (tipos de variáveis, relacionamento entre variáveis, distribuição dos dados, etc). Os insights, análises e demais considerações podem ser conferidos no R script, assim como os [plots](https://github.com/avmachado/AED_CreditRisk/tree/main/plots) gerados utilizando tanto o pacote de base do R quanto o pacote ggplot2.
