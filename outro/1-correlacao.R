#CORRELACAO 

# A - Bando de dados TREES
dados <- trees
View(dados)
head(dados) 
#Renomeando as colunas dos dados TREES
colnames(dados) <- c("Circunferencia", "Altura", "Volume") 
View(dados)

# 1-Normalização dos dados --------------------------------------------------

colMeans(dados)  #calcular a média de coluna por coluna
dados_norm <- scale(dados)
head(dados_norm)
colMeans(dados_norm) #Média é 0
sd(dados_norm[,1]) #Desvio-padrão é 1

# 2-Calcula a matriz de correlação ------------------------------------------
matcor <- cor(dados_norm)
print(matcor, digits =2)

#Analisando a correlacao por grafico
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)
corrplot(matcor, method = "circle")
#Visualizando por numero
corrplot(matcor, method = "number")


# 3-Teste de Bartlett -----------------------------------------------------
if(!require(psych)) install.packages("psych") 
library(psych)
bartlett_result <- cortest.bartlett(dados_norm)
print(bartlett_result)


# 4- Teste KMO ------------------------------------------------------------
kmo_result <- KMO(dados_norm)
print(kmo_result)  #Medida de adequação da amostra (MSA)

