dados <- data.frame(
  Marketing = c(15,18,20,22,25,30,28,35,40,45,50,55,60,65,70),
  Vendedores = c(5,6,7,6,8,9,7,10,11,12,13,14,15,16,17),
  Tamanho = c(120,150,160,155,180,200,190,220,250,270,300,320,350,370,400),
  Faturamento = c(200,220,250,240,270,300,290,330,360,390,420,450,480,510,550))


# AJustar modelo de regressão linear multipla -----------------------------
# Instalando pacotes necessários ------------------------------------------
if(!require(psych)) install.packages("psych") 
library(psych) 

if(!require(car)) install.packages("car") 
library(car)

if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)

#Variavel dependente Faturamento - Independentes Marketing, Vendedores e Tamanho
#ou teste de Bartllet - quanto a matriz de correlação é muito grande.

matrizCorr <- cor(dados)
matrizCorr

pairs.panels(dados)

corrplot(matrizCorr, method = "circle")

#Alternativa para contrapor a correlação muito alta, Aplica agrupamento com análise 
#Fatorial

##1.2 - Teste de Normalidade para as variaveis independentes (Teste de Shapiro-Wilk) ----
#H0: Os dados sao normais p-value > 0.05 
shapiro.test(dados$Marketing)   
shapiro.test(dados$Vendedores) 
shapiro.test(dados$Tamanho) 

#Só nas independentes.

#__________________________________________________________________________
# 2 - Estimacao da equacao de regressao -----------------------------------
##2.1 - Estimando um modelo de regressao

modeloStep <- lm(dados, formula = dados$Faturamento ~ dados$Marketing + dados$Vendedores + dados$Tamanho) 
modeloStep$coefficients

summary(modeloStep)

#Analise dos residuos
plot(modeloStep, which = c(1:3), pch = 20)

vif(modeloStep)

#Agora escolhe um ajusta o modelo para melhorar os dados - Stepwise, retirar a variável com base no VIF ou no Teste-T

#AlgoritmodeloStep
#Algoritmo Stepwise

s <- step(modeloStep)
s$coefficients

summary(modeloStep) #informar o Residual standard error
s2 <- step(modeloStep, scale = 4.826^2)
s2$coefficients

#Função =>  Faturamento = 3.068 * Marketing + 0.63060 * Tamanho

modelo2 <- lm(dados, formula = dados$Faturamento ~ dados$Marketing + dados$Vendedores ) 
modelo2$coefficients

s <- step(modelo2)
s$coefficients

summary(modelo2) #informar o Residual standard error
s2 <- step(modelo2, scale = 5.288^2)
s2$coefficients

##2.2 - Avaliacao da precisao de previsao ----
###2.2.1 - Modelo s ----
#Teste F, teste T e R^2
summary(s)

#Teste F -> p-value: < 2.2e-16
#teste T ( serve para avaliar a significância individual de cada coeficiente. )
#     -> Ambos abaixo de 0.05 então consideramos que tem impacto
# R^2 -> 0.9982  então tem 99% de confiabilidade

#Analise dos residuos
plot(s, which = c(1:3), pch = 20)

#Teste para Independencia dos residuos (Durbin-Watson)
#H0: Os residuos sao intependentes

durbinWatsonTest(s) 
# Os resíduos do seu modelo não apresentam autocorrelação. 
#0.438 são independentes

#Teste de Normalidade para os residuos (Teste de Shapiro-Wilk)
#H0: Os residuos possuem distribuicao normal

shapiro.test(s$residuals)  

#p-value = 0.9961

#Teste de ausencia de Multicolinearidade (VIF > 10 existe)
vif(s)

#Existe multicolinearidade 

#Melhor Versão
#Função =>  Faturamento = 3.06878 * Marketing + 0.63060 * Tamanho
Marketing <- 50000

Tamanho <- 300

Faturamento <- 3.06878 * Marketing + 0.63060 * Tamanho

Faturamento  #153628.2

# Exercicio 2 -------------------------------------------------------------
#Analise Discriminante
#Bom Desempenho  e Ruim Desempenho

dados <- data.frame(
  Grupo = factor(c(
    rep("Bom",12),
    rep("Ruim",12)
  )),
  Matematica = c(
    80,85,88,90,87,92,89,91,93,86,84,88,
    60,62,58,65,63,61,59,57,66,64,55,68),
  Estatistica = c(
    78,82,85,87,84,89,86,88,90,83,81,85,
    58,60,56,63,61,59,57,55,64,62,53,66),
  Programacao = c(
    82,86,89,91,88,93,90,92,94,87,85,89,
    62,64,60,67,65,63,61,59,68,66,57,70))

View(dados)
# 2 - Analisando as suposições da Analise Discriminante -------------------
## 2.1 - Normalidade das variaveis independentes ----
#H0: Distribuicao Normal p-value > 0.05

shapiro.test(dados$Matematica)
shapiro.test(dados$Estatistica)
shapiro.test(dados$Programacao)

# Testando a normalidade apenas para o grupo "Bom"
shapiro.test(dados$Matematica[dados$Grupo == "Bom"])

# Testando a normalidade apenas para o grupo "Ruim"
shapiro.test(dados$Matematica[dados$Grupo == "Ruim"])


#Nenhum Normal

if(!require(forecast)) install.packages("forecast") 
library(forecast)

# 1. Garante que todos os valores sejam estritamente positivos (> 0)
constante <- abs(min(dados$Matematica)) + 1
Matematica_positiva <- dados$Matematica + constante

# 2. Encontra o lambda ideal baseado no dado transladado original
library(forecast)
lambda <- BoxCox.lambda(Matematica_positiva, method = "loglik", lower = -2, upper = 2)

# 3. Aplica a transformação e testa a normalidade
Matematica.t <- BoxCox(Matematica_positiva, lambda)
shapiro.test(Matematica.t)

#Segunda tentativa Matematica

if(!require(LambertW)) install.packages("LambertW")
library(LambertW)

# O Gaussianize detecta e remove a assimetria automaticamente
Matematica_lambert <- Gaussianize(dados$Matematica, type = "hh")

# Testa a normalidade
shapiro.test(Matematica_lambert)

# -------------------------------------------------------------------------

shapiro.test(dados$Estatistica)
#Pode aplicar arrumar a variável não normal com a elevação ao quadrado se caso ela não seja negativa como no caso de x3
Estatistican <- sqrt(dados$Estatistica)
shapiro.test(Estatistican)

# 1. Garante que todos os valores sejam estritamente positivos (> 0)
constante <- abs(min(dados$Estatistica)) + 1
Estatistica_positiva <- dados$Estatistica + constante

# 2. Encontra o lambda ideal baseado no dado transladado original
library(forecast)
lambda <- BoxCox.lambda(Estatistica_positiva, method = "loglik", lower = -2, upper = 2)

# 3. Aplica a transformação e testa a normalidade
Estatistica.t <- BoxCox(Estatistica_positiva, lambda)
shapiro.test(Estatistica.t)

# O Gaussianize detecta e remove a assimetria automaticamente
Estatistica_lambert <- Gaussianize(dados$Estatistica, type = "hh")

# Testa a normalidade
shapiro.test(Estatistica_lambert)


# -------------------------------------------------------------------------

shapiro.test(dados$Programacao)

#Pode aplicar arrumar a variável não normal com a elevação ao quadrado se caso ela não seja negativa como no caso de x3
Programacaon <- sqrt(dados$Programacao)
shapiro.test(Programacaon)


dados2 <- dados
dados2$Matematica <- Matematica_lambert
dados2$Estatistica <- Estatistica_lambert
dados2$Programacao <- Programacaon
View(dados2)

## 2.2 - Liniaridade das variaveis
if(!require(psych)) install.packages("psych") 
library(psych) 
pairs.panels(dados2[ , 2:4])

# todos tem correlação forte
cor(dados2[, 2:4])

# 3 - Estimacao do modelo discriminante -----------------------------------
## 3.1 - Estimando a funcao discriminante linear - Estimacao simultanea ----
if(!require(MASS)) install.packages("MASS") 
library(MASS)
#d <- lda(pop ~ ., data = dados2) - pega tudo então precisa excluir os id primeiro
d <- lda(Grupo ~ Matematica + Estatistica, data = dados2)
print(d)

#Z = -1.3887 * Matematica + 1.1589
#e com base no Z de corte, vc acha a faixa para ser bom ou ruim


## 3.2 - Calculo de escores Z discriminantes ----
predicoes <- predict(object = d, newdata = dados2)
head(predicoes$class)

## 3.3 - Calculo do escore de corte otimo ----
#Numero de observacoes em cada grupo
n0 <- length(dados2$Grupo[dados2$Grupo == "Bom"]) 
n1 <- length(dados2$Grupo[dados2$Grupo == "Ruim"])

#Centroides dos grupos
z0 <- d$means[1, ] %*% d$scaling
z1 <- d$means[2, ] %*% d$scaling
z <- (n0*z1 + n1*z0)/(n0+n1)
z  #escore de corte otimo

#Z de corte -19.99   
# z > Fun - Aluno ruim  z < Fun - Aluno Bom

#Visualizando as classes em cada previsao
comparacao <- cbind(dados2$pop, as.character(predicoes$class))
colnames(comparacao) <- c("observado", "previsto")
View(comparacao)

## 3.4 - Avaliacao do modelo ----

#Matriz de confusao
if(!require(caret)) install.packages("caret") 
library(caret)

# Criar uma tabela de confusão para comparar as previsões com as verdadeiras classes
predito <- predicoes$class
observado <- as.factor(dados2$Grupo)
c <- confusionMatrix(predito, observado)
print(c) #Analisar Accuracy e Sensitivity

# 4 - Estimando o modelo sem x4 -------------------------------------------
## 4.1 - Estimando o modelo e calculando os escores discriminantes ----
d2 <- lda(pop ~ x1 + x2 + x3, data = dados2)
print(d2)
predicoes2 <- predict(object = d2, newdata = dados2)

## 4.2 - Avaliando o modelo ----

#Matriz de confusao
predito2 <- predicoes2$class
observado2 <- as.factor(dados2$pop)
c2 <- confusionMatrix(predito2, observado2)
print(c2)


# 5 - Fazendo previsões com o modelo completo ----------------------------
#Utilizando a funcao discriminante modelo completo pop = x1 + x2 + x3 + x4
coef <- d$scaling
z  #escore de corte otimo

x <- c(0.51, 0.11, 3.27, 0.35)

z_calculado <- sum(coef * x)
print(z_calculado)

# Comparação
resultado <- ifelse(z_calculado < z, 0, 1)

# Exibir o resultado
print(resultado)












