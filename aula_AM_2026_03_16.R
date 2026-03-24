# Importando_dados --------------------------------------------------------
dados <- delitos_sp[, 2:5]
View(dados)

# Ponto_Central -----------------------------------------------------------
#Soma Total
soma <- colSums(dados)
soma

#Média
media <- colMeans(dados)
media

#Mediana - acha através do Summary
summary(dados)


# Variancia_Covariancia ---------------------------------------------------
#variância

variancia_HD <- var(dados$Homicídio.doloso)
variancia_HD

for (nomes_col in names(dados)){
  result[nomes_col] <- var(dados[nomes_col])
  
}
print(result)


#covariância

matriz_cov <- cov(dados)
View(matriz_cov)


# Correlação --------------------------------------------------------------
# correlação a partir de [0.6 -0.6] já pode ser considerada uma correlação relevante
# menos que isso, é correlação baixa

cor(dados$Homicídio.doloso, dados$Furto)

matriz_cor <- cor(dados)
View(matriz_cor)

pairs(dados) #gera graficos de disperção

#Juntando gráfico com o valor da correlação

if (!require("psych")) install.packages("psych")

library(psych)

pairs.panels(dados)










