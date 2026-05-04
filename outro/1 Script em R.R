#Perda de dados HBAT_MISSING Livro Hair
#1 - Carregar os dados HBAT_MISSING do DATASET (importar: SAS)

#2 - Renomear os dados para df
df <- HBAT

#3 - Verificando as variáveis com dados perdidos
any(is.na(df))
NAS <- round(colSums(is.na(df))*100/nrow(df), 2)  
NAS
colSums(is.na(df))
NAS[NAS > 10]

#4 - Excluir a variavel V1
df_1 <- df  #Criar um novo conjuntos de dados
df_1$V1 <- NULL  #Excluiu a variavel V1
View(df_1)
NAS <- round(colSums(is.na(df_1))*100/nrow(df_1), 2)  
NAS
colSums(is.na(df_1))
NAS[NAS > 10]

#5 - Eliminando as linhas correspondentes de V10 at? V14 com perda de dados
df_2 <- df_1[!is.na(df_1$V10),]
df_2 <- df_2[!is.na(df_2$V11),]
df_2 <- df_2[!is.na(df_2$V12),]
df_2 <- df_2[!is.na(df_2$V13),]
df_2 <- df_2[!is.na(df_2$V14),]
View(df_2)

NAS <- round(colSums(is.na(df_2))*100/nrow(df_2), 2)  
NAS
colSums(is.na(df_2))
NAS[NAS > 10]

#6 - Analisar qual variavel eliminar 
View(df_2)
#Eliminando dados perdidos de V9, reduz 4 dados ao conjunto e impacta em reducao de 2 para V3 e 1 de V6
#Eliminando dados perdidos de V6
df_2 <- df_2[!is.na(df_2$V9),]
NAS <- round(colSums(is.na(df_2))*100/nrow(df_2), 2)  
NAS
colSums(is.na(df_2))
NAS[NAS > 10]

#6 - Testando a aleatoriedade 
grupo1 <- df_2[!is.na(df_2$V3),] #Variavel V3 com dados
grupo2 <- df_2[is.na(df_2$V3),]  #Variavel V3 sem dados
#(p-value > 0.05 significa que as medias sao iguais)
t.test(grupo1$V2, grupo2$V2) #são iguais
t.test(grupo1$V4, grupo2$V4) #são iguais
t.test(grupo1$V5, grupo2$V5) #são iguais
t.test(grupo1$V6, grupo2$V6) #são iguais
t.test(grupo1$V7, grupo2$V7) #são iguais 
t.test(grupo1$V8, grupo2$V8) #são iguais
t.test(grupo1$V9, grupo2$V9) #são iguais


#7 - Substituindo os valores perdidos pela media da variavel
df_2$V3[is.na(df_2$V3)] <- mean(df_2$V3, na.rm = TRUE)  
View(df_2)

#Verificando numero dados perdidos
NAS <- round(colSums(is.na(df_2))*100/nrow(df_2), 2)  
NAS
colSums(is.na(df_2))
NAS[NAS > 10]

#8 - Testando a aleatoriedade 
grupo1 <- df_2[!is.na(df_2$V2),] #Variavel V2 com dados
grupo2 <- df_2[is.na(df_2$V2),]  #Variavel V2 sem dados

t.test(grupo1$V3, grupo2$V3) #são iguais
t.test(grupo1$V4, grupo2$V4) #são iguais
t.test(grupo1$V5, grupo2$V5) #Não são iguais
t.test(grupo1$V6, grupo2$V6) #Não são iguais
t.test(grupo1$V7, grupo2$V7) #são iguais
t.test(grupo1$V8, grupo2$V8) #são iguais
t.test(grupo1$V9, grupo2$V9) #são iguais

#Analisar qual variavel eliminar 
View(df_2)
#Eliminando as observacoes em V8
df_3 <- df_2[!is.na(df_2$V8),]
NAS <- round(colSums(is.na(df_3))*100/nrow(df_3), 2)  
NAS
colSums(is.na(df_3))
NAS[NAS > 10]

#9 - Testando a aleatoriedade 
grupo1 <- df_3[!is.na(df_3$V2),] #Variavel V2 com dados
grupo2 <- df_3[is.na(df_3$V2),]  #Variavel V2 sem dados

t.test(grupo1$V3, grupo2$V3)
t.test(grupo1$V4, grupo2$V4)
t.test(grupo1$V5, grupo2$V5)
t.test(grupo1$V6, grupo2$V6)
t.test(grupo1$V7, grupo2$V7)
t.test(grupo1$V8, grupo2$V8)
t.test(grupo1$V9, grupo2$V9)

df_3$V2[is.na(df_3$V2)] <- mean(df_3$V2, na.rm = TRUE)  

#8 - Percentual de dados perdidos
NAS <- round(colSums(is.na(df_3))*100/nrow(df_3), 2)  
NAS
colSums(is.na(df_3))


#9 - Substituindo os demais dados perdidos pelas medias
df_3$V5[is.na(df_3$V4)] <- mean(df_3$V4, na.rm = TRUE) 
df_3$V6[is.na(df_3$V5)] <- mean(df_3$V5, na.rm = TRUE) 
df_3$V7[is.na(df_3$V7)] <- mean(df_3$V7, na.rm = TRUE) 

any(is.na(df_3))  #Mostra se tem algum dado perdido 


#9 - Analisando outliers - Observa??es at?picas
df_4 <- df_2
df_4$ID <- NULL  #Excluiu a variavel ID
boxplot(df_4)  #Verificando qual variavel possui outliers

boxplot(df_4$V7)  #Analisando o outliers da variavel V7
boxplot.stats(df_4$V7)$out  #Verificando o valor do outliers

df_4[df_4$V7 == 1.7, ] 







