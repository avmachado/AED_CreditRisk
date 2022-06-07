# Mini Projeto - Análise de Risco de Crédito

# Análise Exploratória

setwd()

# Carregamento de pacotes
library(e1071)
library(gmodels)
library(dplyr)
library(ggplot2)

############################# ANÁLISE EXPLORATÓRIA #############################

# Lendo os dados
dados <- read.csv('german_dataset_tratado.csv')

View(dados)

# Configurando o tipo dos dados
# Definindo colunas como fator
dados[, c(1, 3:4, 6:10, 12, 14:21)] <- lapply(dados[, c(1, 3:4, 6:10, 12, 14:21)], as.factor)
str(dados)

# Definindo colunas como numeric (basicamente todas as colunas, exceto as defininas anteriormente)
dados[, -c(1, 3:4, 6:10, 12, 14:21)] <- lapply(dados[, -c(1, 3:4, 6:10, 12, 14:21)], as.numeric)
str(dados)

# Visualizando as variáveis numéricas

# Variável duration_month
summary(dados$duration_month)
# Observa-se que a média é maior que a mediana, logo a distribuição está um pouco
# alongada para a direita (right skewed) -> Distribuição assimétrica à direita.
# Isso pode ser comprovado calculando o skewness e plotando o histograma da coluna.
skewness(dados$duration_month)
# skewness positivo, logo distribuição assimétrica à direita
hist(dados$duration_month, main = 'Histograma de Duração em Meses', col = 'darkseagreen',
     xlab = 'Duração (meses)', ylab = 'Frequência')

?hist

# Variável credit_amount
summary(dados$credit_amount)
# Observa-se que a média é maior que a mediana, logo a distribuição está um pouco
# alongada para a direita (right skewed) -> Distribuição assimétrica à direita.
# Isso pode ser comprovado calculando o skewness e plotando o histograma da coluna.
skewness(dados$credit_amount)
# skewness positivo, logo distribuição assimétrica à direita
hist(dados$credit_amount, main = 'Histograma de Quantidade de Crédito', col = 'darkslategray3',
     xlab = 'Quantidade de Crédito', ylab = 'Frequência')

# Representação da variável através do boxplot
# Criando espaço para plot dos dois gráficos juntos
par(mfrow = c(2, 1))

# Inserindo o histograma
hist(dados$credit_amount, main = 'Histograma de Quantidade de Crédito', col = 'darkslategray3',
     xlab = 'Quantidade de Crédito', ylab = 'Frequência')

# Inserindo o boxplot
?boxplot
b1 <- boxplot(dados$credit_amount, horizontal = TRUE, col = 'darkslategray3', varwidth = TRUE,
        main = 'Boxplot - Quantidade de Crédito')
b1$out
# Vemos aqui que existem vários outliers na quantidade de crédito dos indivíduos.
# É interessante lembrar que para a construção de modelos preditivos, é importante
# tratar esses valores outliers para não comprometer a qualidade do modelo.
# Podemos inclusive verificar o perfil das pessoas que possuem altos valores de
# quantidade de crédito, em relação por exemplo, ao risco de crédito
outliers <- dados %>%
  filter(credit_amount %in% b1$out)

# Visualizando
outliers %>%
  group_by(credit_risk) %>%
  ggplot(aes(x = credit_risk, y = credit_amount)) +
  geom_bar(stat = 'identity', position = 'dodge', fill = '#528b8b') +
  ggtitle("Risco de Crédito dos Outliers") +
  scale_x_discrete(name ="Risco de Crédito") +
  scale_y_continuous(name ="Quantidade de Crédito")
# Notamos que esses valores em outlier não parece diferir quanto ao risco de crédito,
# já que a distribuição é bem parecida.

# Podemos ainda verificar como é o relacionamento entre essas duas variáveis:
par(mfrow = c(1, 1))
dados %>%
  group_by(credit_risk) %>%
  ggplot(aes(x = credit_risk, y = credit_amount)) +
  geom_boxplot(fill = '#528b8b') +
  ggtitle("Risco de Crédito e Quantidade de Crédito") +
  scale_x_discrete(name ="Risco de Crédito") +
  scale_y_continuous(name ="Quantidade de Crédito")

# Complementando esse boxplot, é possível fazer um teste de hipóteses para saber
# se há uma diferença significativa entre grupos (0  e 1 - bom e ruim risco de crédito)
# em relação à quantidade de crédito disponível.
credit.test <- t.test(formula = credit_amount ~ credit_risk,
                       data = dados)
credit.test
# O valor-p encontrado foi de 2.478e-05, dado que a hipótese nula é que não há
# diferença significativa entre os grupos. Logo, podemos considerar que temos
# evidências estatísticas para aceitar a hipótese nula. Ou seja, parece mesmo
# não haver diferença significativa entre os riscos de crédito (bom e ruim) em
# relação à disponibilidade de crédito do indivíduo.

# Variável age
summary(dados$age)
# Observa-se que a média é maior que a mediana, logo a distribuição está um pouco
# alongada para a direita (right skewed) -> Distribuição assimétrica à direita.
# Isso pode ser comprovado calculando o skewness e plotando o histograma da coluna.
skewness(dados$age)
# skewness positivo, logo distribuição assimétrica à direita
hist(dados$age, main = 'Distribuição da Idade', col = 'lightsalmon2',
     xlab = 'Idade', ylab = 'Frequência')

#### Variáveis categóricas

# Podemos construir gráficos para avaliar como estão distribuídas as categorias
dados %>% 
  group_by(credit_risk, personal_status_and_sex) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = credit_risk, y = n, fill = personal_status_and_sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_discrete(name ="Risco de Crédito")

# Nesse caso vemos que a maioria das pessoas que tem bom risco de crédito são
# homens solteiros. E também são a maioria entre o grupo de risco de crédito ruim.
# Aqui devemos abrir um parênteses e destacar que das 1000 observações, 700 são de
# risco de crédito ruim, e 300 de bom risco de crédito.
table(dados$credit_risk)

# Podemos avaliar esse comportamento para diversas outras variáveis
# Avaliando purpose
dados %>% 
  group_by(credit_risk, purpose) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = credit_risk, y = n, fill = purpose)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_discrete(name ="Risco de Crédito")

# No grupo que tem baixo risco de crédito, os principais motivos para crédito no 
# banco são para compra de rádio/televisão e carro novo. No grupo com alto risco
# de crédito, os principais motivos são a compra de carro novo, seguido de rádio/
# televisão.

# Avaliando present_employment_since
dados %>% 
  group_by(credit_risk, present_employment_since) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = credit_risk, y = n, fill = present_employment_since)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_discrete(name ="Risco de Crédito")

# Nesse aspecto, o padrão da distribuição é parecido. Em ambos, o grupo que mais
# domina é o de pessoas que estão entre 1 e 4 anos no emprego atual.

# Job
dados %>% 
  group_by(credit_risk, job) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = credit_risk, y = n, fill = job)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_discrete(name ="Risco de Crédito")

# Vemos que a maior ocorrência aqui é de pessoas especializadas ou em empregos formais.

# housing
dados %>% 
  group_by(credit_risk, housing) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = credit_risk, y = n, fill = housing)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_discrete(name ="Risco de Crédito")

# Em ambas as categorias, a maior ocorrência é de pessoas que possuem casa própria.

# saving_account
dados %>% 
  group_by(credit_risk, saving_account) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = credit_risk, y = n, fill = saving_account)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_x_discrete(name ="Risco de Crédito")

# Observamos que a maior ocorrência é de pessoas que possuem entre 0 e 100 DM na
# sua conta bancária.

# Podemos fazer o teste do Qui-quadrado para verificar se há possibilidade de duas
# variáveis estarem relacionadas.
# A princípio é interessante verificar se o status pessoal e o sexo possuem algum
# tipo de relação com o risco de crédito.
# Farei essa análise para todas as variáveis que analisei anteriormente nos
# gráficos de barras.

# O teste do qui-quadrado é um teste de hipótese não paramétrico, que busca
# avaliar a associação entre duas variáveis qualitativas nominais.

CrossTable(dados$credit_risk, dados$personal_status_and_sex, chisq = TRUE)
?CrossTable
?chisq.test
# chisq.test(dados$credit_risk, dados$personal_status_and_sex)
# chisq.test produz o mesmo resultado que o CrossTable com chisq = TRUE

# O teste do Qui-Quadrado examina se as linhas e colunas da tabela de contingência
# são significantemente associadas. Aqui podemos elaborar a tabela de contingência
# com o CrossTable ou passar diretamente as variáveis da tabela na função chisq.test.
# Ver aqui o embasamento para essas afirmações: 
# https://www.pluralsight.com/guides/testing-for-relationships-between-categorical-variables-using-the-chi-square-test

# Elaborando as hipóteses
# Hipótese Nula (H0): As variáveis são independentes.
# Hipótese Alternativa (H1): As variáveis não são independentes, ou seja, existe
# dependência entre elas.

# No caso das variáveis personal_status_and_sex e credit_risk, encontramos um 
# valor-p de 0,022238.
# O p-valor (ou valor-p ou p-value) nos informa o quanto é possível obter os dados
# que temos, partindo do pressuposto que a hipótese nula é verdadeira. 
# Quanto menor o valor-p, mais somos levados a rejeitar a hipótese nula. Por quê?
# O valor-p é uma proporção. Se o valor-p é 0.05, isso significa que 5% do tempo
# veríamos um teste estatístico tão extremo quanto o que foi encontrado se a 
# hipótese nula fosse verdadeira. Ou seja, somente em 5% do tempo observaríamos
# esses dados caso a hipótese nula fosse realmente verdadeira.
# Fonte interessante: 
# https://www.scribbr.com/statistics/p-value/#:~:text=The%20p%2Dvalue%20only%20tells,your%20alternative%20hypothesis%20is%20true.

# Trazendo esses conceitos para o nosso caso, observamos que em 2,22% do tempo,
# caso as nossas variáveis fossem independentes, teríamos os dados dispostos da
# maneira em que eles se encontram. Portanto, é uma probabilidade pequena o
# suficiente para dizer que a hipótese nula é verdadeira. Ou seja: Possuímos
# evidências estatísticas para rejeitar a hipótese nula e aceitar a hipótese
# alternativa de que o estado civil e o sexo de uma pessoa possui dependência
# com o quanto ela é um potencial risco de crédito.

# É importante ressaltar aqui que o valor-p de 0,022238 não é tão menor que o
# nível de significância utilizado de 0.05. Portanto, mais adiante neste trabalho,
# podemos avaliar se realmente essa variável pode impactar futuramente no modelo
# de Machine Learning ou não.

# Replicando o teste para as outras variáveis:
CrossTable(dados$credit_risk, dados$purpose, chisq = TRUE)

# Nesse caso especificamente temos algumas ocorrência menores que 5 na tabela de
# contingência, visto que algumas ocorrências passam de 200. Segundo a literatura,
# pode ser mais interessante nesse caso utilizar o Fisher's Exact Test.
fisher.test(table(dados$credit_risk, dados$purpose), simulate.p.value = TRUE)
# p-valor menor que 0,05, logo há evidências suficientes para não rejeitar a hipótese
# nula, de que as variáveis são depentes.

# Continuando para as demais variáveis
CrossTable(dados$credit_risk, dados$present_employment_since, chisq = TRUE)
# Mesma conclusão da variável anterior.

CrossTable(dados$credit_risk, dados$job, chisq = TRUE)
# Aqui, temos um p-valor bastante distante de 0,05. Logo, não rejeitamos a hipótese nula
# de que as variáveis são independentes. Daqui poderíamos dizer que provavelmente
# o status do emprego do indivíduo (o grau de senioridade) não parece ter muita
# influência no quanto ele possui risco de crédito.

CrossTable(dados$credit_risk, dados$housing, chisq = TRUE)
# Mesma conclusão da variável purpose.

CrossTable(dados$credit_risk, dados$saving_account, chisq = TRUE)
# Nota-se que o valor-p nesse caso é muito menor em relação aos anteriores, quase 
# 0. Portanto, aqui temos evidências para rejeitar a hipótese nula, com base no teste estatístico,
# sendo esta variável possivelmente uma que afete o comportamento do risco de crédito.
# Ou seja, o indivíduo pode ser mais arriscado que outro, dependendo do quanto ele
# possui na sua conta do banco. E isso parece fazer sentido, correto?

# Encerro aqui a análise exploratória desse dataset, com alguns insights interessantes:

# 1 - Parece não haver uma diferença significativa entre os indivíduos com bom e
# ruim riscos de crédito quando analisamos o quanto de crédito os mesmos têm disponíveis.

# 2 - Pode ser que o status civil + o sexo do indivíduo tenha uma relação pequena
# com o risco de crédito, porém como é uma variável que combina essas duas informações,
# pode ser que ela não tenha tanta relevância assim. A sugestão aqui seria a de 
# construir um modelo com e sem essa variável para avaliação da performance.

# 3 - Dentre as variáveis analisadas, a que parece ter mais influência no risco de
# crédito do indivíduo é a "saving_account", ou seja, o quanto o indivíduo possui
# na sua conta do banco parece refletir no sentido de ser um bom ou um mau pagador
# em caso de cessão de crédito.

# De próximos passos, é interessante avaliar de uma maneira global quais são as
# variáveis mais relevantes e que gerariam um modelo com boa eficiência na previsão
# do risco de crédito. A partir daí, selecionar aquelas que impactam mais no
# modelo preditivo.
