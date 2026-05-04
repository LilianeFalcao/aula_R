
# Analisando Dados Perdidos -----------------------------------------------
## O maximo de dados perdidos só pode ser 10%
## Os valores perdidos pode ser substituido pela Média ou Mediana da amostra
# tem que olhar o grau de aleatoriedade para que não fique tendencioso 

#1 - Criando a Base de dados
df <- data.frame (Pessoa = seq(1:300),
                  Idade = NA,
                  Altura = NA, 
                  Emprego = NA)
df$Idade <- sample(20:60, 300, replace = TRUE)
df$Peso <- sample(45:80, 300, replace = TRUE)
df$Altura <- sample(145:195, 300, replace = TRUE)
df$Emprego <- sample(0:1, 300, replace = TRUE)

## Emprego - 0 : Desempregado Emprego - 1 : Empregado

View(df)

df$Idade[1:4] <- NA
aleatorio <- sample(1:300, 70, replace = TRUE)
df$Peso[aleatorio] <- NA

df$Altura[108:250] <- NA
View(df)

#2 - Tratando perda de dados
is.na(df)     #Verifica se tem algum dado perdido
any(is.na(df))  #Mostra se tem algum dado perdido 

#3 - Percentual de dados faltantes
colSums(is.na(df))  #Numero de dados estao faltando em cada variavel
nrow(df)  #Numero de linhas (todal de dados)

NAS <- round(colSums(is.na(df))*100/nrow(df), 2)  
NAS
NAS[NAS > 10] #Filtro: mostrando as variaveis com valores maiores que 10


#4 - Excluindo variavel
df$Altura <- NULL

#5 - Verificando o percentual de dados faltantes
NAS <- round(colSums(is.na(df))*100/nrow(df), 2)  
NAS
colSums(is.na(df))
View(df)

#6 - Excluindo a linha inteira (coluna Idade)
#Podemos excluir a linha onde tem os dados perdidos
df_1 <- df[!is.na(df$Idade),]
View(df_1)


#7 - Verificando o percentual de dados faltantes
NAS <- round(colSums(is.na(df_1))*100/nrow(df_1), 2)  
NAS

#8 - Teste de aleatoriedade 
grupo1 <- df_1[!is.na(df_1$Peso),] #Variavel peso com dados
grupo2 <- df_1[is.na(df_1$Peso),]  #Variavel peso sem dados

#(p-value > 0.05 significa que as medias sao iguais)
#Testando a aleatoriedade da IDADE 
## as médias de quem respondeu a variável tem que ser igual a das pessoas que não responderam
#teste T testa as médias   H0: As médias são iguais
t.test(grupo1$Idade, grupo2$Idade)
#Atencao: variavel EMPREGO e dicotomica


#9 - Substituindo os valores perdidos pela media da variavel
summary(df_1$Peso)
mean(df_1$Peso, na.rm = TRUE)  #Calcula a media dos valores da variavel
#marcar que tem dados perdidos

df_1$Peso[is.na(df_1$Peso)] <- mean(df_1$Peso, na.rm = TRUE)  #Adicionando a media no valor perdido do peso
View(df_1)


#10 - Percentual de dados perdidos
NAS <- round(colSums(is.na(df_1))*100/nrow(df_1), 2)  
NAS
any(is.na(df_1))  #Mostra se tem algum dado perdido 

#Na hora de gerar o relatório deve ser descrito todos os pontos importantes no caso desse por exemplo:
# A Variável Altura foi excluída por ter 50% de dados perdidos,
# A variável Peso tinha um valor de 20% porém optamos por ficar com ela, ele foi substituido pelo valor da média, pois tinha aleátoriedade
#

# Analisando outliers -----------------------------------------------------

#1 - Analisando outliers - Observaoes atipicas
df_2 <- df_1
df_2$Pessoa <- NULL  #Excluiu a variavel ID
df_2$Emprego <- NULL  #Exclui a variavel EMPREGO
boxplot(df_2)  #Verificando qual variavel possui outliers

boxplot.stats(df_2$Idade)$out  #Verificando o valor do outliers
boxplot.stats(df_2$Peso)$out

#Criando um outlier
df_2$Idade[3] <- 100
boxplot(df_2)
boxplot.stats(df_2$Idade)$out  

df_2[df_2$Idade == 100, ] ##mostra a linha onde teve um OUTLIER

