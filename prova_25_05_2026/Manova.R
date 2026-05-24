#  Exemplo 3: Método de Ensino (tres grupos)

# 1 - Carregando os dados  ------------------------------------------------
#Carregando os dados (tratamento.csv)
dados <-  tratamento
colnames(dados) <- c("Ensino", "Nota_Matematica", "Nota_Portugues")
View(dados)

#2 - Verificando a Normalidade das variaveis ----
#Teste de Normalidade (Shapiro-Wilk)
#H0: Possiu uma distribuição normal
shapiro.test(dados$Nota_Matematica) #normal
shapiro.test(dados$Nota_Portugues) #normal

#3 - Testar se os grupos sao iguais para todas as variaveis ----
#MANOVA
## Construcao do modelo:
modelo <- manova(cbind(Nota_Matematica, Nota_Portugues) ~ Ensino, data=dados)
#H0: média são iguais
summary(modelo, test="Roy")   #Teste Roy
summary(modelo, test="Wilks")  #Teste lambda de Wilks
summary(modelo)  #Teste Pillai
#Conclusao: as medias sao diferentes

#4 - ANOVA para identificar qual variável contribui mais ----
# Se a MANOVA for significativa, realizamos ANOVAs separadas para cada variável dependente
# para identificar qual(is) variável(is) contribuiu(íram) para o efeito multivariado.
summary.aov(modelo)

#Todos foram significativos, ou seja, houve impacto na Nota_Matematica e Nota_Portugues

#5 - Ver quais pares de grupos (A vs B, A vs C, B vs C) são significativamente diferentes ----
aov_Matematica <- aov(Nota_Matematica ~ Ensino, data=dados)
summary(aov_Matematica)

aov_Portugues <- aov(Nota_Portugues ~ Ensino, data=dados)
summary(aov_Portugues)


#Calculando a média para cada variavel dividida nos grupos A, B e C
aggregate(cbind(Nota_Matematica, Nota_Portugues) ~ Ensino,
          data = dados,
          FUN = mean)


