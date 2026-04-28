#REGRESSAO SIMPLES
#Importar os dados ARVORE da pasta - Import Fron Text (read)
dados <- arvore

#1 - Calculo do preditor de referencia (media)
mediaAlt <- mean(dados$altura_m)
mediaAlt

mediaDia <- mean(dados$diametro_cm)
mediaDia

#2 - Matriz de correlacao  Sempre acima de 0.30
matcor <- cor(dados[ , 2:3])
View(matcor)

#3 - Estimacao da equacao de regressao simples
modelo <- lm(dados, formula = dados$altura_m ~ dados$diametro_cm) # ~ equivale a igualdade
modelo$coefficients
print(modelo$coefficients, digits = 3)

# Quando tiver os coeficientes, monta a equação  VariavelIndependente = coef1 + corf2 *  VariavelDependente

#4 - Avaliacao da precisao de previsao
#4.1 - Analise do t-value, R^2e F-statistic
# T- tem que ser próximo de Zero
#o R Quadrado e o R ajustado tem que ser 
# p-value é oq aprova a hipotese ou não, se for menor que 0.05 rejeita

summary(modelo)

#4.2 - Analise dos residuos
# equação Altura = 11.08 + 0.06 * diametro
#Analise Grafica
plot(modelo, which = c(1:3), pch = 20)

#sequencia dos graficos
# 1. Média, os pontos tem q ser próximo ou igual 0
# 2. Normalidade, os pontos devem ser próximos da linha
# 3. Variabilidade/ Variancia, testa se a variabilidade é constante.

#Teste de Normalidade para os residuos (Teste de Shapiro-Wilk)
#p-value < 0.05 significa residuos NÃO são normais
shapiro.test(modelo$residuals) 
#Independ?ncia dos residuos (teste Durbin-watson)
if (!require("car")) install.packages("car")

durbinWatsonTest(modelo) #Valor entre 1.5 e 2 - p-value > 0.05
# H0: os residuos são independentes

#Conclusão
#Equação: Altura = 11.8007 + 0.0612*diametro
#R² = 0.5695 e R²_Ajustado =0.5653
#Test-T: (H0: Coeficiente é 0) - ambos deram valores menores que zero, logo, coeficientes diferentes de zero.
#Teste-F: (H0: Reta constante) - a reta não é constante
# Avaliação dos gráficos
##### 1º. Média, os pontos tem q ser próximo ou igual 0 - OK
##### 2º. Normalidade, os pontos devem ser próximos da linha - Visualmente parecia Normal
##### 3º. Variabilidade (Variância), testa se a variabilidade é constante -

#Analise da normalidade pelo Shapiro (H0: Os dados são normais)
### p-value = 0.004, ou seja, < 0.05. Portanto não são normais

#Analise da normalidade pelo DurbinWatsonTest - como o p = 0.002, dados não 

# Modelo deve ser melhorado

#Sem o Intercepto
modelo2 <- lm(dados, formula = dados$altura_m ~ -1 + dados$diametro_cm)
summary(modelo2)





