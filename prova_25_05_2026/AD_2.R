# 1- Carregando dados do arquivo (HBAT.sav) -----------------------------------------------------
dados <- HBAT[ , 7:19]
x4 <- HBAT$x4 #Regiao dos EUA (0 - localizado nos EUA, 1 - fora dos EUA)
dados <- cbind.data.frame(x4, dados)
View(dados)

# 2 - Analisando as suposições da Analise Discriminante -------------------
## 2.1 - Normalidade das variaveis independentes ----
#H0: Distribuicao Normal  p-value > 0,05
shapiro.test(dados$x6) #n normal
shapiro.test(dados$x7) #n normal
shapiro.test(dados$x8) #normal
shapiro.test(dados$x9) #normal
shapiro.test(dados$x10) #normal
shapiro.test(dados$x11) #normal
shapiro.test(dados$x12)  #n normal
shapiro.test(dados$x13) #n normal
shapiro.test(dados$x14) #normal
shapiro.test(dados$x15) #normal
shapiro.test(dados$x16) #n normal
shapiro.test(dados$x17) #n normal
shapiro.test(dados$x18) #normal

## 2.2 - Liniaridade das variaveis
if(!require(psych)) install.packages("psych") 
library(psych) 
pairs.panels(dados[ , 2:14])

# aplicar teste de bartlett quando o gráfico não da 

bartlett.test(dados[ , 2:14]) #quando o p-value é pequeno

# 3 - Estimacao do modelo discriminante -----------------------------------
## 3.1 - Estimando a funcao discriminante linear - Estimacao simultanea ----
#Sem x6, x7, x12, x13, x16 e x17, pois nao tem distribuicao normal
if(!require(MASS)) install.packages("MASS") 
library(MASS)
d <- lda(x4 ~ x8 + x9 + x10 +
           x11 + x14 + x15 + x18, data = dados)

print(d)

## 3.2 - Calculo de escores Z discriminantes ----
predicoes <- predict(object = d, newdata = dados)
head(predicoes$class)

## 3.3 - Calculo do escore de corte otimo ----
#Numero de observacoes em cada grupo
n0 <- length(dados$x4[dados$x4 == 0]) 
n1 <- length(dados$x4[dados$x4 == 1])
#Centroides dos grupos
z0 <- d$means[1, ] %*% d$scaling
z1 <- d$means[2, ] %*% d$scaling
z <- (n0*z1 + n1*z0)/(n0+n1)
z  #escore de corte otimo


## 3.4 - Avaliacao do modelo ----
#Matriz de confusao
if(!require(caret)) install.packages("caret") 
library(caret)
predito <- predicoes$class
observado <- as.factor(dados$x4)
c <- confusionMatrix(predito, observado)
print(c)


# 4 - Calculando o modelo ideal utilizando stepwise -----------------------
if(!require(klaR)) install.packages("klaR") 
library(klaR)
#Aplicando o stepwise
greedy.wilks(x4 ~ ., data = dados, niveau = 0.05)

#Obtendo a funcao discriminante
d2 <- lda(x4 ~ x7 + x11 + x12 + x13 + x17, data = dados)
print(d2)

#Calculando os escores discriminantes pela funcao obtida d3
predito2 <- predict(object = d2, newdata = dados)

#Avaliando o modelo
predito2 <- predito2$class
observado2 <- as.factor(dados$x4)
c2 <- confusionMatrix(predito2, observado2)
print(c2)

# 5 - Fazendo previsões com o modelo completo ----------------------------
#Utilizando a funcao discriminante modelo completo x4 = x7 + x11 + x12 + x13 + x17
coef <- d2$scaling
z  #escore de corte otimo

x <- c(x7=3.6, x11=5.9, x12=5.8, x13=9.3, x17=6.1)

z_calculado <- predict(d2, newdata = data.frame(x7=3.6, x11=5.9, x12=5.8, x13=9.3, x17=6.1))
print(z_calculado)

# Comparação
resultado <- ifelse(z_calculado$x < z, 0, 1)

# Exibir o resultado
print(resultado)
