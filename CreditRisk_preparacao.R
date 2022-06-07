# Mini Projeto 2 - Análise de Risco de Crédito

# O objetivo deste projeto é avaliar o risco de concessão de crédito a 
# clientes de instituições financeiras

# Como o diretório já foi setado ao criar o projeto no RStudio, não há necessidade
# de fazê-lo novamente. Porém, se surgir a necessidade eventualmente no futuro:
# setwd("C:/Users/vieir/Documents/DSA/FCD/RMicrosoftAzure/Cap18")

# FASES DO PROJETO
# 0 - Limpeza e organização dos dados
# 1 - Análise Exploratória
# 2 - Feature Selection (Seleção de variáveis)
# 3 - Preparação do Dataset
# 4 - Criação do Modelo
# 5 - Avaliação do Modelo
# 6 - Otimização do Modelo

## Instalação e Carregamento de Pacotes
library(readr)
library(tidyr)
library(dplyr)


########################## ANÁLISE EXPLORATÓRIA ################################

# Primeiro faremos a importação do dataset
dados <- read.csv('original_german_dataset.csv', header = FALSE)
?read_csv

# Verificando as colunas
str(dados)
summary(dados)

# Descrevendo as colunas segundo o site oficial: 
# https://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29

######################## INFORMAÇÕES DOS ATRIBUTOS #############################

#### existing_checking_account: (qualitative)
#Status of existing checking account
#A11 (0): ... < 0 DM
#A12 (1): 0 <= ... < 200 DM
#A13 (2): ... >= 200 DM / salary assignments for at least 1 year
#A14 (3): no checking account

#### duration_month: (numerical)
# Duration in month

#### credit_history: (qualitative)
# Credit history
# A30 (0): no credits taken/ all credits paid back duly
# A31 (1): all credits at this bank paid back duly
# A32 (2): existing credits paid back duly till now
# A33 (3): delay in paying off in the past
# A34 (4): critical account/ other credits existing (not at this bank)

#### purpose: (qualitative)
# Purpose
# A40 (0): car (new)
# A41 (1): car (used)
# A42 (2): furniture/equipment
# A43 (3): radio/television
# A44 (4): domestic appliances
# A45 (5): repairs
# A46 (6): education
# A47 (7): (vacation - does not exist?)
# A48 (8): retraining
# A49 (9): business
# A410 (10): others

#### credit_amount: (numerical)
# Credit amount

#### saving_account: (qualitative)
# Savings account/bonds
# A61 (0): ... < 100 DM
# A62 (1): 100 <= ... < 500 DM
# A63 (2): 500 <= ... < 1000 DM
# A64 (3): .. >= 1000 DM
# A65 (4): unknown/ no savings account

#### present_employment_since: (qualitative)
# Present employment since
# A71 (0): unemployed
# A72 (1): ... < 1 year
# A73 (2): 1 <= ... < 4 years
# A74 (3): 4 <= ... < 7 years
# A75 (4): .. >= 7 years

#### disposable_income_rate: (numerical)
# Installment rate in percentage of disposable income

#### personal_status_and_sex: (qualitative)
# Personal status and sex
# A91 (0): male : divorced/separated
# A92 (1): female : divorced/separated/married
# A93 (2): male : single
# A94 (3): male : married/widowed
# A95 (4): female : single

#### debtors_guarantor: (qualitative)
# Other debtors / guarantors
# A101 (0): none
# A102 (1): co-applicant
# A103 (2): guarantor

#### present_residence_since: (numerical)
# Present residence since

#### property: (qualitative)
# Property
# A121 (0): real estate
# A122 (1): if not A121 : building society savings agreement/ life insurance
# A123 (2): if not A121/A122 : car or other, not in attribute 6
# A124 (3): unknown / no property

#### age: (numerical)
# Age in years

#### installment_plans: (qualitative)
# Other installment plans
# A141 (0): bank
# A142 (1): stores
# A143 (2): none

#### housing: (qualitative)
# Housing
# A151 (0): rent
# A152 (1): own
# A153 (2): for free

#### credits_this_bank: (numerical)
# Number of existing credits at this bank

#### job: (qualitative)
# Job
# A171 (0): unemployed/ unskilled - non-resident
# A172 (1): unskilled - resident
# A173 (2): skilled employee / official
# A174 (3): management/ self-employed/highly qualified employee/ officer

#### people_provide_manutenance: (numerical)
# Number of people being liable to provide maintenance for

#### telephone: (qualitative)
# Telephone
# A191 (0): none
# A192 (1): yes, registered under the customers name

#### foreign_worker: (qualitative)
# foreign worker
# A201 (0): yes
# A202 (1): no

#### credit_risk: (qualitative)
# 1 (0): good credit risk
# 2 (1): bad credit risk

# Visualizando o dataframe
View(dados)

# Separando as colunas
?separate
dados2 <- dados %>% separate(V1, c('existing_checking_account', 'duration_month',
                         'credit_history', 'purpose', 'credit_amount',
                         'saving_account', 'present_employment_since',
                         'disposable_income_rate', 'personal_status_and_sex',
                         'debtors_guarantor', 'present_residence_since',
                         'property', 'age', 'installment_plans', 'housing',
                         'credits_this_bank', 'job', 'people_provide_manutenance',
                         'telephone', 'foreign_worker', 'credit_risk'))
View(dados2)
str(dados2)

# Com os atributos acima definidos e as colunas devidamente nomeadas, podemos
# prosseguir com a limpeza dos dados. 

# Observe que nos atributos em que temos as variáveis categóricas, 
# irei substituir os valores de string pelo número em parênteses descrito
dados2_0 <- dados2

# coluna existing_checking_account
dados2_0$existing_checking_account[dados2_0$existing_checking_account == 'A11'] <- 0
dados2_0$existing_checking_account[dados2_0$existing_checking_account == 'A12'] <- 1
dados2_0$existing_checking_account[dados2_0$existing_checking_account == 'A13'] <- 2
dados2_0$existing_checking_account[dados2_0$existing_checking_account == 'A14'] <- 3

# coluna credit_history
dados2_0$credit_history[dados2_0$credit_history == 'A30'] <- 0
dados2_0$credit_history[dados2_0$credit_history == 'A31'] <- 1
dados2_0$credit_history[dados2_0$credit_history == 'A32'] <- 2
dados2_0$credit_history[dados2_0$credit_history == 'A33'] <- 3
dados2_0$credit_history[dados2_0$credit_history == 'A34'] <- 4

# coluna purpose
dados2_0$purpose[dados2_0$purpose == 'A40'] <- 0
dados2_0$purpose[dados2_0$purpose == 'A41'] <- 1
dados2_0$purpose[dados2_0$purpose == 'A42'] <- 2
dados2_0$purpose[dados2_0$purpose == 'A43'] <- 3
dados2_0$purpose[dados2_0$purpose == 'A44'] <- 4
dados2_0$purpose[dados2_0$purpose == 'A45'] <- 5
dados2_0$purpose[dados2_0$purpose == 'A46'] <- 6
dados2_0$purpose[dados2_0$purpose == 'A47'] <- 7
dados2_0$purpose[dados2_0$purpose == 'A48'] <- 8
dados2_0$purpose[dados2_0$purpose == 'A49'] <- 9
dados2_0$purpose[dados2_0$purpose == 'A410'] <- 10

# coluna saving_account
dados2_0$saving_account[dados2_0$saving_account == 'A61'] <- 0
dados2_0$saving_account[dados2_0$saving_account == 'A62'] <- 1
dados2_0$saving_account[dados2_0$saving_account == 'A63'] <- 2
dados2_0$saving_account[dados2_0$saving_account == 'A64'] <- 3
dados2_0$saving_account[dados2_0$saving_account == 'A65'] <- 4

# coluna present_employment_since
dados2_0$present_employment_since[dados2_0$present_employment_since == 'A71'] <- 0
dados2_0$present_employment_since[dados2_0$present_employment_since == 'A72'] <- 1
dados2_0$present_employment_since[dados2_0$present_employment_since == 'A73'] <- 2
dados2_0$present_employment_since[dados2_0$present_employment_since == 'A74'] <- 3
dados2_0$present_employment_since[dados2_0$present_employment_since == 'A75'] <- 4

# coluna personal_status_and_sex
dados2_0$personal_status_and_sex[dados2_0$personal_status_and_sex == 'A91'] <- 0
dados2_0$personal_status_and_sex[dados2_0$personal_status_and_sex == 'A92'] <- 1
dados2_0$personal_status_and_sex[dados2_0$personal_status_and_sex == 'A93'] <- 2
dados2_0$personal_status_and_sex[dados2_0$personal_status_and_sex == 'A94'] <- 3
dados2_0$personal_status_and_sex[dados2_0$personal_status_and_sex == 'A95'] <- 4

# coluna debtors_guarantor
dados2_0$debtors_guarantor[dados2_0$debtors_guarantor == 'A101'] <- 0
dados2_0$debtors_guarantor[dados2_0$debtors_guarantor == 'A102'] <- 1
dados2_0$debtors_guarantor[dados2_0$debtors_guarantor == 'A103'] <- 2

# coluna property
dados2_0$property[dados2_0$property == 'A121'] <- 0
dados2_0$property[dados2_0$property == 'A122'] <- 1
dados2_0$property[dados2_0$property == 'A123'] <- 2
dados2_0$property[dados2_0$property == 'A124'] <- 3

# coluna installment_plans
dados2_0$installment_plans[dados2_0$installment_plans == 'A141'] <- 0
dados2_0$installment_plans[dados2_0$installment_plans == 'A142'] <- 1
dados2_0$installment_plans[dados2_0$installment_plans == 'A143'] <- 2

# coluna housing
dados2_0$housing[dados2_0$housing == 'A151'] <- 0
dados2_0$housing[dados2_0$housing == 'A152'] <- 1
dados2_0$housing[dados2_0$housing == 'A153'] <- 2

# coluna job
dados2_0$job[dados2_0$job == 'A171'] <- 0
dados2_0$job[dados2_0$job == 'A172'] <- 1
dados2_0$job[dados2_0$job == 'A173'] <- 2
dados2_0$job[dados2_0$job == 'A174'] <- 3

# coluna telephone
dados2_0$telephone[dados2_0$telephone == 'A191'] <- 0
dados2_0$telephone[dados2_0$telephone == 'A192'] <- 1

# coluna foreign_worker
dados2_0$foreign_worker[dados2_0$foreign_worker == 'A201'] <- 0
dados2_0$foreign_worker[dados2_0$foreign_worker == 'A202'] <- 1

# coluna credit_risk
dados2_0$credit_risk[dados2_0$credit_risk == '1'] <- 0
dados2_0$credit_risk[dados2_0$credit_risk == '2'] <- 1

# Visualizando a tabela
View(dados2_0)
str(dados2_0)

# Precisamos definir agora qual o tipo das colunas, uma vez que todas estão como chr

# Definindo colunas como fator
dados2_0[, c(1, 3:4, 6:10, 12, 14:21)] <- lapply(dados2_0[, c(1, 3:4, 6:10, 12, 14:21)], as.factor)
str(dados2_0)

# Definindo colunas como numeric (basicamente todas as colunas, exceto as defininas anteriormente)
dados2_0[, -c(1, 3:4, 6:10, 12, 14:21)] <- lapply(dados2_0[, -c(1, 3:4, 6:10, 12, 14:21)], as.numeric)
str(dados2_0)

# Com os dados devidamente preparados, irei salvar aqui o dataset e criar um novo
# RScript para seguir com as próximas etapas
write_csv(dados2_0,'german_dataset_tratado.csv')

