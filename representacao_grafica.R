
# Histograma --------------------------------------------------------------

######EXEMPLO 1 #####
#A - Criando um conjunto de dados aleatorios 
dados <-sample(10:200, 200, replace = TRUE)
#Escolhi de 10 até 200, 200 numeros. Replace = TRUE significa que pode ter numeros repetidos

hist(dados) #gera um histograma

#Incrementando o gráfico
#Colocando cor (https://r-charts.com/colors/)
hist(dados,
     col = "lightblue",  #cor
     main = "Histograma dos dados") #titulo

#Acrescentando curva de distribuição no histograma
hist(dados,
     col = "lightblue",  #cor
     main = "Histograma dos dados", #titulo
     freq = FALSE,  #Histograma tenha area total de um
     breaks = 50,) 
curve(dnorm(x, mean = mean(dados), sd = sd(dados)), add = TRUE)

######EXEMPLO 2 #####
#B - Utilizando o banco de dados ja existente chamado IRIS
head(iris) #visualiza as 6 primeiras linhas dos dados IRIS

hist(iris$Sepal.Length, 
     col = "#008B00",   #Cor
     freq = F,
     breaks = 30)
curve(dnorm(x, mean = mean(iris$Sepal.Length), sd = sd(iris$Sepal.Length)), add = TRUE)

#Teste de Normalidade
shapiro.test(iris$Sepal.Length) 
#p-value < 0.05, logo distribuição NAO é normal


######EXEMPLO 3 #####
#C - Criando um conjunto de dados com media 10 e sd=1
dnormal <- rnorm(1000, mean = 10, sd = 1) #rnorm gera dados de uma distribuicao normal
hist(dnormal, 
     col = "khaki",   #Cor
     freq = F,
     breaks = 40)
curve(dnorm(x, mean = mean(dnormal), sd = sd(dnormal)), add = TRUE)

#Teste de Normalidade
shapiro.test(dnormal) 
#p-value > 0.05, logo distribuição NORMAL


# Diagrama de Ramo-E-Folhas -----------------------------------------------

head(trees)  #Carregando um banco de dados ja existente chamado TREES

#Renomeando as colunas dos dados TREES
colnames(trees) <- c("Circunferencia", "Altura", "Volume") 
trees

stem(trees$Circunferencia)  #Diagrama de ramo-e-folhas

stem(trees$Volume)



# Diagrama de Dispersão ---------------------------------------------------

#Criando um grafico entre Circunferencia e Volume
plot(trees$Circunferencia ~ trees$Volume, data = trees)

#Criando um grafico entre Circunferencia e Altura
plot(trees$Circunferencia ~ trees$Altura, data = trees)

#Colocando uma linha de tendencia no grafico
abline(lm(trees$Circunferencia ~ trees$Altura, data = trees), col = "red")

#Criando um grafico com todas as variaveis
pairs(trees)

#Construindo um grafico mais completo com pacote psych
if(!require(psych)) install.packages("psych") 
library(psych)
variav <- c("Circunferencia", "Altura", "Volume") 
pairs.panels(trees[ , variav])


# Box-Plot ----------------------------------------------------------------
####EXEMPLO 1####
#Bos-plot dos dados trees
boxplot(trees)

boxplot(trees,
        col = "lightpink")
boxplot(trees$Volume)$out  #identifica o valor do outlier

####EXEMPLO 2####
#Criando um conjunto de dados
dados <- data.frame(Pessoa = seq(1:50),
                    Idade = sample(5:30, 50, replace = TRUE),
                    Altura = sample(145:190, 50, replace = TRUE))

View(dados)

boxplot(dados)

#Conjunto de dados com outliers
dados$Idade[10] <- 100
boxplot(dados)
boxplot(dados$Idade)$out #identifica o valor do outlier


# Faces de Chernoff -------------------------------------------------------
####EXEMPLO 1####
#Utilizando os dados Idade e Altura gerados no exemplo anterior
##Intalar pacote se necessario
if(!require(aplpack)) install.packages("aplpack") 
library(aplpack)

#Gerar o gráfico dos 6 primeiros dados
faces(dados[1:6, 2:3])
print(dados[1:6, 2:3])

####EXEMPLO 2####
#Utilizando os dados de mtcars ja existentes
faces(mtcars[1:6, 1:5])
print(mtcars[1:6, 1:5])

head(mtcars)



