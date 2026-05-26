#REGRESSAO MULTIPLA
# Instalando pacotes necessários ------------------------------------------
if(!require(psych)) install.packages("psych") 
library(psych) 

if(!require(car)) install.packages("car") 
library(car)

if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)

# Carregando dados do arquivo hbat.sav ------------------------
View(HBAT)
dados <- HBAT[ , 7:20]
View(dados)

# 1 - Verificando as Suposições: Linearidade e Normalidade ----------------

##1.1 - Linearidade: Matriz de correlacao ----
matcor <- cor(dados)
View(matcor)

pairs.panels(dados)

corrplot(matcor, method = "circle")
corrplot(matcor, method = "number")

##1.2 - Teste de Normalidade para as variaveis independentes (Teste de Shapiro-Wilk) ----
#H0: Os dados sao normais
shapiro.test(dados$x6)   
shapiro.test(dados$x7) 
shapiro.test(dados$x8) 
shapiro.test(dados$x9)
shapiro.test(dados$x10)   
shapiro.test(dados$x11) 
shapiro.test(dados$x12) 
shapiro.test(dados$x13) 
shapiro.test(dados$x14)   
shapiro.test(dados$x15) 
shapiro.test(dados$x16) 
shapiro.test(dados$x17) 
shapiro.test(dados$x18) 


#__________________________________________________________________________
# 2 - Estimacao da equacao de regressao -----------------------------------
##2.1 - Estimando um modelo de regressao s
modeloStep <- lm(dados, formula = dados$x19 ~ dados$x6 + dados$x7 + dados$x8 +
                   dados$x9 + dados$x10 + dados$x11 + dados$x12 + dados$x13 +
                   dados$x14 + dados$x15 + dados$x16 + dados$x17 + dados$x18) 
modeloStep$coefficients

#Algoritmo Stepwise
s <- step(modeloStep)
s$coefficients

summary(modeloStep) #informar o Residual standard error
s2 <- step(modeloStep, scale = 0.5663^2)
s2$coefficients

##2.2 - Avaliacao da precisao de previsao ----
###2.2.1 - Modelo s ----
#Teste F, teste T e R^2
summary(s)

#Analise dos residuos
plot(s, which = c(1:3), pch = 20)

#Teste para Independencia dos residuos (Durbin-Watson)
#H0: Os residuos sao intependentes
durbinWatsonTest(s) 

#Teste de Normalidade para os residuos (Teste de Shapiro-Wilk)
#H0: Os residuos possuem distribuicao normal
shapiro.test(s$residuals)  

#Teste de ausencia de Multicolinearidade (VIF > 10 existe)
vif(s)


#__________________________________________________________________________
# 3. Estimando um novo modelo sem a vari?vel x16 -----------------------------------
##3.1 - Estimando um modelo de regressao s2
s2 <- lm(dados, formula = dados$x19 ~ dados$x6 + dados$x7 + 
                   dados$x9 + dados$x11 + dados$x12) 
s2$coefficients

##3.2 - Avaliacao da precisao de previsao ----
###3.2.1 - Modelo s ----
#Teste F, teste T e R^2
summary(s2)

#Analise dos residuos
plot(s2, which = c(1:3), pch = 20)

#Teste para Independencia dos residuos (Durbin-Watson)
durbinWatsonTest(s2) 

#Teste de Normalidade para os residuos (Teste de Shapiro-Wilk)
shapiro.test(s2$residuals)  

#Teste de ausencia de Multicolinearidade (VIF > 10 existe)
vif(s2)


# Comparação entre os modelos - Critério de Akaike ------------------------
#Quanto menor, melhor
AIC(s, s2)


