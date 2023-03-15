library(readr)
library(psych)
library(lavaan)
library(stringr)


## LUCAS ESTEVE AQYU

## Emanuel está aqui

# importando dados
df <- read_csv("Construção de Instrumentos 2022.2 (respostas) - Respostas ao formulário 1-3.csv")

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