library(readr)
library(psych)
library(lavaan)
library(stringr)
library(tidyverse)
library(readxl)

df <- read_xlsx("Base de dados/Construção de Instrumentos 2022.2 (respostas).xlsx")
df <- df %>% rename(TCLE = "Tendo em vista os itens acima apresentados, eu, de forma livre e esclarecida, manifesto meu consentimento para participar da pesquisa.")
df$TCLE[df$TCLE == 'Concordo'] <- '1'
df$TCLE[df$TCLE == 'Não concordo'] <- '0'
df <- df %>% filter(TCLE==1) # Eliminação de não respondentes

# saparando intrumetnos de impulsividade
df_impulsividade <- df[, 10:51]

efaParallel <- fa.parallel(df_impulsividade, fm= "wls", fa= "fa", cor = "poly")

efaParallel

# 4 fatores com eingenvalues acima de 1

efa <- fa(df_impulsividade, nfactors = 4 ,fm= "wls", rotate = "oblimin", cor = "poly")
summary(efa)
fa.sort(efa$loadings)
