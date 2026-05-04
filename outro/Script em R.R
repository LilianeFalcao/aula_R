#..........................................................................
# Exemplo 1 ---------------------------------------------------------------
#..........................................................................
#Considere o problema de número de reclamações em diferentes sistemas de atendimento.

#Esses dados sao numero de reclamacoes em diferentes sistemas de atendimento
y <- c(2370, 1687, 2592, 2283, 2910, 3020, 
       1282, 1527, 871, 1025, 825, 920,
       562, 321, 636, 317, 485, 842, 71, 82, 
       173, 127, 132, 150, 129, 227, 193, 62, 96, 44)
View(y)

## 1. Verificando a normalidade ----
#H0: Tem uma distribuicao normal
shapiro.test(y)

#Fazendo o histograma
hist(y)

##2. Normalizando os dados ----
### 2.1 Utilizando a Raiz Quadrada ----
##Só faz sentido em números positivos
y2 <- sqrt(y)
shapiro.test(y2)  #Nao normalizou

### 2.2 Utilizando o Logaritmo ----
##Só faz sentido em números positivos

y3 <- log(y)
shapiro.test(y3)   #Normalizou
hist(y3)

print(y3) 

#quando vc usa dados transformados, quando for apresentar os dados tem que aplicar o reverso. Voltando os
#dados na sua forma padrão
## Além disso, se perguntar quando e qual transformação é necessário para seu conjunto de dados

#..........................................................................
# Exemplo 2 ---------------------------------------------------------------
#..............Transformacao de Box-Cox....................................

#Importar o conjunto de dados do arquivo exemplo1_dados.xlsx
dados <- exemplo1_dados
View(dados)

## pop - 0 : Não Faliu   pop - 1: Faliu

## 1. Verificando a normalidade ----
#H0: A distribuicao é normal
shapiro.test(dados$x1)
shapiro.test(dados$x2)  #Nao e normal
shapiro.test(dados$x3)  #Nao e normal
shapiro.test(dados$x4)

#Precisamos normalizar x2 e x3

##2.1 Normalizando os dados de x3 ----
### 2.1.1 Utilizando a Raiz Quadrada ----
x3 <- sqrt(dados$x3)
shapiro.test(x3)  #normalizou

##2.2 Normalizando os dados de x2 ----

#Como ha valores negativos, precisamos deslocar os valores
##Funciona tbm se pegar o melhor valor da lista, somar um valor X que deixe positivo e aplicar em todos

constante <- abs(min(dados$x2)) + 1
x2 <- dados$x2 + constante

### 2.2.1 Utilizando a Raiz Quadrada ----
x2.raiz <- sqrt(x2)
shapiro.test(x2.raiz) #Nao Normalizou

### 2.2.2 Utilizando o Logaritmo ----
x2.log <- log(x2)
shapiro.test(x2.log)   #Nao Normalizou

#Quando o Log e o Quadrado n funciona aplica o Box-Cox (ou outras)

### 2.2.3 Utilizando a transformacao Box-Cox 
if(!require(forecast)) install.packages("forecast") 
library(forecast)

#O valor tambem precisa ser positivo, entao vou fazer uma translacao com os valore somando 1
lambda <- BoxCox.lambda(x2, method=c("loglik"), lower=-1, upper = 10) ## Valor de Lambda
lambda 
x2.t <- BoxCox(x2, lambda) 
shapiro.test(x2.t) #Normalizou




