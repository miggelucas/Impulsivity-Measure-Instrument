library(readr)
library(psych)
library(lavaan)
library(stringr)
library(tidyverse)
library(readxl)

df <- read_xlsx("Base de dados/Construção de Instrumentos 2022.2 (respostas).xlsx")
# removendo quem negou o TCLE
df <- na.omit(df)

# saparando intrumetnos de impulsividade
df_impulsividade <- df[, 10:51]

KMO(df_impulsividade)

efaParallel <- fa.parallel(df_impulsividade, fm= "wls", fa= "fa", cor = "poly")
efaParallel

# 4 fatores com eingenvalues acima de 1

efa <- fa(df_impulsividade, nfactors = 4 ,fm= "wls", rotate = "oblimin", cor = "poly")
summary(efa)

fa.sort(efa$loadings)
colnames(df_impulsividade)
# itens problemáticos
# Persisto até o fim, mesmo que o objetivo demande tempo.
# Atividades que demandam tempo me deixam incomodado(a).
# Consigo me atentar aos detalhes quando realizo uma tarefa.
# Consigo me manter realizando uma tarefa por um longo período de tempo. 
# Perco prazos com frequência.
# Penso muito sobre o meu futuro. 
# Antes de tomar uma decisão, analiso a situação cuidadosamente.
# Quando vou viajar, arrumo minha mala de última hora.
# Geralmente me coloco em situações perigosas.        
# Tomo decisões rapidamente, sem parar para pensar muito.

itens_exclusao = c(5, 6, 7, 9, 16, 17, 19, 20, 24, 37)
df_impulsividade_select <- df_impulsividade
df_impulsividade_select <- df_impulsividade_select[, -itens_exclusao]


KMO(df_impulsividade_select)
efaParallel_select <- fa.parallel(df_impulsividade_select, fm= "wls", fa= "fa", cor = "poly")
efaParallel_select


efa_select <- fa(df_impulsividade_select, nfactors = 4 ,fm= "wls", rotate = "oblimin", cor = "poly")
summary(efa_select)
fa.sort(efa_select$loadings)

## Fatores
# Falta de Atencao
# Impulsividade disfuncional
# Planejamento
# Impulsividade Funcional / Busca de sensações




### Precisao ###



cargas_fatoriais <- fa.sort(efa_select$loadings)
itens_atencao <- names(cargas_fatoriais[,1][abs(cargas_fatoriais[,1]) > 0.3])
itens_impDis <- names(cargas_fatoriais[,2][abs(cargas_fatoriais[,2]) > 0.3])
itens_planej <- names(cargas_fatoriais[,3][abs(cargas_fatoriais[,3]) > 0.3])
itens_impFun <- names(cargas_fatoriais[,4][abs(cargas_fatoriais[,4]) > 0.3])


df_atencao <- df_impulsividade[, itens_atencao]
df_impDis <- df_impulsividade[, itens_impDis]
df_planej <- df_impulsividade[, itens_planej]
df_impFun <- df_impulsividade[, itens_impFun]

omega(df_atencao, nfactors = 1, poly = TRUE)
# alpha = 0.89
# omega = 0.89
omega(df_impDis, nfactors = 1, poly = TRUE)
# alpha = 0.81
# omega = 0.81
omega(df_planej, nfactors = 1, poly = TRUE)
# alpha = 0.79
# omega = 0.79
omega(df_impFun, nfactors = 1, poly = TRUE)
# alpha = 0.75
# omega = 0.76





### Segunda Ordem (?) ###


# escores_fatoriais <- predict(efa, df_impulsividade)
# 
# efaParallel2 <- fa.parallel(escores_fatoriais, fm= "minres", fa= "fa")
# 
# efa2 <- fa(df_impulsividade, nfactors = 1 ,fm= "minres", rotate = "oblimin")
# summary(efa2)
# fa.sort(efa2$loadings)

