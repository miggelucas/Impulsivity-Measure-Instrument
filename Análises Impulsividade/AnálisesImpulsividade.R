library(readr)
library(psych)
library(lavaan)
library(stringr)
library(tidyverse)
library(readxl)


# importando dados
df <- read_csv("Construção de Instrumentos 2022.2 (respostas) - Respostas ao formulário 1-3.csv")
df <- read_xlsx("Construção de Instrumentos 2022.2 (respostas).xlsx")

# Renomear
df <- df %>% rename(TCLE = "Tendo em vista os itens acima apresentados, eu, de forma livre e esclarecida, manifesto meu consentimento para participar da pesquisa.")

# TCLE
df$TCLE[df$TCLE == 'Concordo'] <- '1'
df$TCLE[df$TCLE == 'Não concordo'] <- '0'

# Eliminação de negativos
df <- df %>% filter(TCLE==1) # Eliminação de não respondentes

# saparando intrumetnos de impulsividade
df_impulsividade <- df[, 10:51]

# Definir o número de colunas a serem renomeadas
num_cols <- 42

# Gerar os novos nomes das colunas usando um loop for
new_names <- character(num_cols)
for (i in 1:num_cols) {
  new_names[i] <- paste0("impul", i)
}

colnames(df_impulsividade) <- new_names
# verificando adequabilidade para análise fatorial exploratória

kmo <- KMO(df_impulsividade)
# kmo = 0.8933
bartlett <- bartlett.test(df_impulsividade)
# pvalor ~ 0

# amostra possui bons indicadores para prosseguir na análise

# Extração de fatores

fa_parallel <- fa.parallel(df_impulsividade, fa = "fa", cor = "poly", fm = "wls")
fa_parallel$fa.values

# análises paralelas indicam diferentes números
# algorítimo = 6
# scree plot = 5 ~ 6
# eingevalue = 4
# modelo teórico = 5

efa <- fa(df_impulsividade, nfactors = 6, cor = "poly", rotate = "oblimin", fm = "wls")
efa$e.values
efa$loadings

cargas_fatorias <- fa.sort(efa$loadings)
cargas_fatorias

# Itens que seriam removidos em uma análise visual
# Item 20
# Item 35
# Item 33

cargas_fatorias[,1][abs(cargas_fatorias[,1]) > 0.3]
# item 5 removido
cargas_fatorias[,2][abs(cargas_fatorias[,2]) > 0.3]
# todos os itens sem apresentar carga em outro fator
cargas_fatorias[,3][abs(cargas_fatorias[,3]) > 0.3]
# itens 5, 9 e 18 removidos
cargas_fatorias[,4][abs(cargas_fatorias[,4]) > 0.3]
# item 9 removido
cargas_fatorias[,5][abs(cargas_fatorias[,5]) > 0.3]
# iten 18 removido
cargas_fatorias[,6][abs(cargas_fatorias[,6]) > 0.3]
# nenhum item com carga fatorial em outro fator

modelo_cfa <- 'f1 =~ impul12 + impul8 + impul11 + impul10 + impul13 + impul14 + impul7 + impul6 + impul16 
               f4 =~ impul30 + impul29 + impul4 + impul28 + impul27 + impul41 + impul3 + impul24 + impul33 
               f3 =~ impul19 + impul2 +  impul25 + impul26 + impul21 + impul17
               f2 =~ impul31 + impul34 + impul32 + impul40  
               f6 =~ impul18 +  impul23 + impul42 + impul22 + impul15 + impul1 
               f5 =~ impul36 + impul38 + impul39 + impul37'


cfa <- cfa(df_impulsividade, model = modelo_cfa)
summary(cfa, fit.measures = TRUE, standardized = FALSE)


str <- "impul12 impul8 impul11 impul10 impul13 impul14 impul7 impul6 impul16"

# extraindo apenas os números da string
numeros <- as.integer(str_extract_all(str, "(?<=impul)\\d+"))

# imprimindo a lista de números inteiros
print(numeros)


# 

# AFE_2 -------------------------------------------------------------------

# Itens reduzidos
df_impulsividade_2 <- df_impulsividade %>% select(1,2,3,4,8,9,10,11,12,13,
                                                  14,15,18,19,22,23,24,26,
                                                  27,28,29,30,31,32,33,34,
                                                  36,38,39,41,42)

# Paralell analysis

poly_im <- df_impulsividade_2 %>% polychoric(.)   # matriz de correlações policóricas para análise paralela.
rho_im <- poly_im$rho # guardando apenas o Rho de Spearman da matriz de correlações policóricas
scree_im<-scree(poly_im)

pa_R <- fa.parallel(rho_im, n.obs=828, fa="fa") # análise paralela sobre a matriz de correlações policóricas
pa_R$fa.values #mostra os eigenvalues com os dados experimentais
pa_R$fa.sim #mostra os eigenvalues com os dados simulados


# Teste do modelo com 4 fatores
im_efa <- fa(df_impulsividade_2, nfactors = '4', cor='poly', 
            fm='wls', rotate = 'geominQ')

im_efa$e.values
im_efa$loadings

# Teste do modelo com 3 fatores
im_efa_2 <- fa(df_impulsividade_2, nfactors = '3', cor='poly', 
             fm='wls', rotate = 'geominQ')

im_efa_2$e.values
im_efa_2$loadings %>% view()

# Teste do modelo com 6 fatores
im_efa_3 <- fa(df_impulsividade_2, nfactors = '6', cor='poly', 
             fm='wls', rotate = 'geominQ')

im_efa_3$e.values
im_efa_3$loadings %>% view()

# Teste do modelo com 7 fatores
im_efa_4 <- fa(df_impulsividade_2, nfactors = '7', cor='poly', 
             fm='wls', rotate = 'geominQ')

im_efa_4$e.values
im_efa_4$loadings

names(df_impulsividade_2)

# Fator 1
# Item 7 é negativo
df_impulsividade_2 %>% select(3,5,7:11) %>% psych::omega()
df_impulsividade_2 %>% select(3,5,7:11) %>% psych::alpha(check.keys = TRUE)

# Fator 2
# Item 18 é negativo
df_impulsividade_2 %>% select(4,17,18,20:22,24,25,27:30) %>% psych::omega()
df_impulsividade_2 %>% select(4,17,18,20:22,24,25,27:30) %>% psych::alpha(check.keys = TRUE)

# Fator 3
df_impulsividade_2 %>% select(1:2,6,12:16,23,26) %>% psych::omega()
df_impulsividade_2 %>% select(1:2,6,12:16,23,26) %>% psych::alpha(check.keys = TRUE)

# Inverter itens negativos
df_impulsividade_2$H_7 <- ifelse(df_impulsividade_2$`Costumo realizar minhas atividades sem perder o foco.` == 1, 5,
                                 ifelse(df_impulsividade_2$`Costumo realizar minhas atividades sem perder o foco.` == 2, 4,
                                        ifelse(df_impulsividade_2$`Costumo realizar minhas atividades sem perder o foco.` == 4, 2,
                                               ifelse(df_impulsividade_2$`Costumo realizar minhas atividades sem perder o foco.` == 5, 1, df_impulsividade_2$`Costumo realizar minhas atividades sem perder o foco.`))))

df_impulsividade_2$H_18 <- ifelse(df_impulsividade_2$`Em conversas com amigos, penso antes de falar.` == 1, 5,
                                 ifelse(df_impulsividade_2$`Em conversas com amigos, penso antes de falar.` == 2, 4,
                                        ifelse(df_impulsividade_2$`Em conversas com amigos, penso antes de falar.` == 4, 2,
                                               ifelse(df_impulsividade_2$`Em conversas com amigos, penso antes de falar.` == 5, 1, df_impulsividade_2$`Em conversas com amigos, penso antes de falar.`))))

# Cálculo dos fatores de impulsividade
# Fator 1
df_impulsividade_2$F_IM1 <- df_impulsividade_2 %>% select(3,5,8:11,32) %>% rowMeans()

# Fator 2
df_impulsividade_2$F_IM2 <- df_impulsividade_2 %>% select(4,17,20:22,24,25,27:30,33) %>% rowMeans()

# Fator 3
df_impulsividade_2$F_IM3 <- df_impulsividade_2 %>% select(1:2,6,12:16,23,26) %>% rowMeans()

# Copiando o banco df para df_geral, com a finalidade de salvar os escores dos fatores de impulsividade e CE
df_geral <- df

df_geral$REO <- REO
df_geral$RESBP <- RESBP
df_geral$EE <- EE
df_geral$PE <- PE
df_geral$RESAP <- RESAP
df_geral$CEG <- CEG

df_geral <- bind_cols(df_geral, select(df_impulsividade_2, "F_IM1", "F_IM2", "F_IM3"))
