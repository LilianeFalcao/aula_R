#Queremos classificar uma empresa em "nao vai falir" e "vai falir"
#x1: fluxo de caixa / total de debitos; 
#x2: receita liquida da empresa / ativos totais; 
#x3: ativos correntes / debitos correntes; 
#x4: ativos correntes / receita liquida das vendas. 
#pop: empresas que faliram - pop = 0;
#     e as que nao faliram por pop = 1.

# 1- Carregando dados do arquivo (exemplo1_dados.xlsx) -----------------------------------------------------

dados <- exemplo1

# 2 - Analisando as suposições da Analise Discriminante -------------------
## 2.1 - Normalidade das variaveis independentes ----
#H0: Distribuicao Normal p-value > 0.05
shapiro.test(dados$x1)
shapiro.test(dados$x2) #não é normal

constante <- abs(min(dados$x2)) + 1
x2n <- dados$x2 + constante
x2n <- sqrt(dados$x2 +1)
shapiro.test(x2n)

if(!require(forecast)) install.packages("forecast") 
library(forecast)

#O valor tambem precisa ser positivo, entao vou fazer uma translacao com os valore somando 1
lambda <- BoxCox.lambda(x2n, method=c("loglik"), lower=-1, upper = 15) ## Valor de Lambda 
#// no Upper ajusta se vai bater no teto ou vai ter um espaço para ser calculada de fato
lambda 
x2.t <- BoxCox(x2n, lambda) 
shapiro.test(x2.t) #Normalizou

shapiro.test(dados$x3)
#Pode aplicar arrumar a variável não normal com a elevação ao quadrado se caso ela não seja negativa como no caso de x3
x3n <- sqrt(dados$x3)
shapiro.test(x3n)

shapiro.test(dados$x4)

dados2 <- dados
dados2$x2 <- x2.t
dados2$x3 <- x3n
View(dados2)

## 2.2 - Liniaridade das variaveis
if(!require(psych)) install.packages("psych") 
library(psych) 
pairs.panels(dados2[ , 2:5])

# X4 não há correlação com nenhuma das variáveis
cor(dados2[, 2:5])

# 3 - Estimacao do modelo discriminante -----------------------------------
## 3.1 - Estimando a funcao discriminante linear - Estimacao simultanea ----
if(!require(MASS)) install.packages("MASS") 
library(MASS)
#d <- lda(pop ~ ., data = dados2) - pega tudo então precisa excluir os id primeiro
d <- lda(pop ~ x1 + x2 + x3 + x4, data = dados2)
print(d)

## 3.2 - Calculo de escores Z discriminantes ----
predicoes <- predict(object = d, newdata = dados2)
head(predicoes$class)

## 3.3 - Calculo do escore de corte otimo ----
#Numero de observacoes em cada grupo
n0 <- length(dados2$pop[dados2$pop == 0]) 
n1 <- length(dados2$pop[dados2$pop == 1])
#Centroides dos grupos
z0 <- d$means[1, ] %*% d$scaling
z1 <- d$means[2, ] %*% d$scaling
z <- (n0*z1 + n1*z0)/(n0+n1)
z  #escore de corte otimo

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
observado <- as.factor(dados2$pop)
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

