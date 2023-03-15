# Scrip para calcular competências emocionais 

library(readr)
library(dplyr)
library(readxl)
library(psych)
library(lavaan)

HSE_AP <- read_excel("HSE_AP.xlsx")

df <- read_csv("database.csv")

# duas pessoas não aceitarm participar da pesquisa
df <- na.omit(df)


# os itens de competências se iniciam ao vetor 52:85
colnames(df)
df_competencias_emocionais <- df[,52:85]


# itens de competência para esse df de referência 12:45
colnames(HSE_AP)
df_referencia <- HSE_AP[,12:45]


# df_geral <- df_geral %>% rename(C3 = 'Por mais que tente, não consigo controlar a expressão do que estou sentindo.')
# df_geral <- df_geral %>% rename(C16 = 'Frustrações deixam-me desanimado/a por bastante tempo.')
# df_geral <- df_geral %>% rename(C28 = 'Tenho vergonha de expressar os meus sentimentos.')

# os itens 3, 16 e 28 precisam ser invertidos

# df_geral$CF1 <- df_geral %>% select(12,16,19,26,34,37,40,42) %>% rowMeans()
# df_geral$CF2 <- df_geral %>% select(15,22,25,137,32,35,38) %>% rowMeans()
# df_geral$CF3 <- df_geral %>% select(17,19,138,43) %>% rowMeans()
# df_geral$CF4 <- df_geral %>% select(13,18,21,23,30,33,44) %>% rowMeans()
# df_geral$CF5 <- df_geral %>% select(136,20,24,28,31,36,45) %>% rowMeans()
# df_geral$C_GERAL <- df_geral %>% select(139:143) %>% rowMeans()

# o item 28 (interido em 138) aparece tanto no CF5 quanto CF3?

# itens invertidos NÃO CORRIGEM index
cf1_index <- c(12,16,19,26,34,37,40,42) - 11
cf2_index <- c(15,22,25,16 + 11,32,35,38) - 11
cf3_index <- c(17,19,28 + 11,43) - 11
cf4_index <- c(13,18,21,23,30,33,44) - 11
cf5_index <- c(3 + 11, 20,24,31,36,45) - 11

cf_geral_index <- c(cf1_index, cf2_index, cf3_index, cf4_index, cf5_index)

# invertendo itens
itens_invertidos_index <- c(3, 16, 28)

for (index in itens_invertidos_index) {
  df_competencias_emocionais[, index] <- 6 - df_competencias_emocionais[, index]
}

df_cf1 <- df_competencias_emocionais %>% select(cf1_index)
df_cf2 <- df_competencias_emocionais %>% select(cf2_index)
df_cf3 <- df_competencias_emocionais %>% select(cf3_index)
df_cf4 <- df_competencias_emocionais %>% select(cf4_index)
df_cf5 <- df_competencias_emocionais %>% select(cf5_index)


df_competencias_emocionais$cf1 <- rowMeans(df_cf1)
df_competencias_emocionais$cf2 <- rowMeans(df_cf2)
df_competencias_emocionais$cf3 <- rowMeans(df_cf3)
df_competencias_emocionais$cf4 <- rowMeans(df_cf4)
df_competencias_emocionais$cf5 <- rowMeans(df_cf5)

df_competencias_emocionais$cf_geral <- df_competencias_emocionais %>% select(cf_geral_index) %>% rowMeans()
df_competencias_emocionais$cf_geral


# pedindo precisão 

alpha(df_cf1)
# alpha 0.88

alpha(df_cf2)
# alpha 0.84

alpha(df_cf3)
# alpha 0.63

alpha(df_cf4)
# alpha 0.77

alpha(df_cf5)
# 0.5

alpha(df_competencias_emocionais %>% select(cf_geral_index))
# 0.88



# Análise epxlortaria

df_efa <- df[,52:85]

num_columns <- ncol(df_efa)

new_names <- character()
for (i in 1:num_columns) {
  new_names[i] <- paste0("ce", i)
}
colnames(df_efa) <- new_names

efa <- fa(df_efa, nfactors = 5, rotate = "oblimin", fm = "wls")
fa.sort(efa$loadings)

cf3_analise_index <- c(6, 18, 28, 32)
cf5_análise_index <- c(3, 13, 20, 25, 30) 

tens_invertidos_index <- c(3, 16, 28)

for (index in itens_invertidos_index) {
  df_efa[, index] <- 6 - df_efa[, index]
}

df_cf3_analise <- df_efa %>% select(cf3_analise_index)
df_cf5_analise <- df_efa %>% select(cf5_análise_index)

alpha(df_cf3_analise)
# alpha 0.73
alpha(df_cf5_analise)
# alpha 0.61


cf1_index
# 1  5  8 15 23 26 29 31
cf2_index
# 4 11 14 16 21 24 27
cf3_index
# 6  18 28 32
cf4_index
# 2  7 10 12 19 22 33
cf5_análise
# 3 13 20 25 30





#####
#df_escores <- df_competencias_emocionais %>% select("cf1", "cf2", "cf3", "cf4", "cf5", "cf_geral")

#df_escores
#write.csv(df_escores, "Escores Competências Emocionais", row.names=FALSE)

