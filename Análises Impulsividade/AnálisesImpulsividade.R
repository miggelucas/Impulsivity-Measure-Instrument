library(readr)
library(psych)
library(lavaan)
library(stringr)
library(tidyverse)
library(readxl)

# Testando Emanuel

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

# Paralell analysis

poly_im <- df_impulsividade %>% polychoric(.)   # matriz de correlações policóricas para análise paralela.
rho_im <- poly_im$rho # guardando apenas o Rho de Spearman da matriz de correlações policóricas
scree_im<-scree(poly_im)

pa_R <- fa.parallel(rho_im, n.obs=830, fa="fa") # análise paralela sobre a matriz de correlações policóricas
pa_R$fa.values #mostra os eigenvalues com os dados experimentais
pa_R$fa.sim #mostra os eigenvalues com os dados simulados

# OBS.: a análise paralela indicou 6 fatores, o scree-plot 3 ou 4 fatores,
# e pelo critério de Kaiser-Guttman (eigenvalue > 1), formariam-se quatro fatores.
# Assim, optou-se pela extração de 4 fatores.

# Fatores
names(R_2)
r_efa <- fa(R, nfactors = '4', cor='poly', 
            fm='wls', rotate = 'geominQ')
# nessa analise, o item 7 apresentou cargas inferiores a 0,3 em todos os fatores.

R <- R[ ,c(1:6,8:24)]

r_efa <- fa(R, nfactors = '4', cor='poly', 
            fm='wls', rotate = 'geominQ')

r_efa$e.values
# resultados em tabelas
names (r_efa)
r_efa$loadings

#install.packages("DT")  # pacote para fazer tabelas.
#library(DT)
#view(R)

#datatable(r_itens$loadings[1:23, 1:3], rownames=TRUE, editable = TRUE) 

library(knitr)  #pacote para tabelas
kable(r_efa$loadings[1:23, 1:4],digits = 2)

#R-Fidedignidade
#names(riasec_2)
#riasec_2 %>% select(24:29,36,41:43,45) %>% alpha()
