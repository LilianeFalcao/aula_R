dados_clientes <- clientes[,2:5]
View(dados_clientes)

# 1 - Padronização dos dados ----------------------------------------------
media <-  colMeans(dados_clientes)
print(media)

#1.1 Padronizando os dados
dados.p <- scale(dados_clientes)
media.p <- colMeans(dados.p)
print(media.p)

# 2 - Determinando a medida de similaridade -------------------------------
## 2.1 Calculando a distancia Euclidiana ----

d.eucl <- dist(dados.p, method = "euclidean")
print(d.eucl)

#Vizualizando a distancia euclidiana para as QUATRO primeiras empresas:
round(as.matrix(d.eucl)[1:4, 1:4], 2)  #arrendondando para duas casas decimais

#3.2 Metodo da ligacao media
metod.ligMed <- hclust(d.eucl, method = "average")
cor(d.eucl, cophenetic(metod.ligMed))

#Pela ligacao media o valor da correlacao foi maior, portanto sera o utilizado

if(!require(factoextra)) install.packages("factoextra") 
library("factoextra")
fviz_dend(metod.ligMed)

#Reduzindo o tamanho da letra
fviz_dend(metod.ligMed, cex = 0.5)

# Pode-se utilizar alguns indicadores para auxiliar a escolha do numero de agrupamentos. 

if(!require(NbClust)) install.packages("NbClust") 
library("NbClust")

nb <- NbClust(dados.p, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "average", index = "all")

#Portanto, utilizaremos um agrupamento em 2 grupos

#5. Interpretacao e validacao dos agrupamentos. ------------------------

##5.1 Obtendo os agrupamentos ----
grupo <- cutree(metod.ligMed,k=3)
#Numero de membros em cada agrupamento
table(grupo)

##5.2 Obtendo o nome dos membros no agrupamento 1 ----
rownames(dados.p)[grupo == 1]
rownames(dados.p)[grupo == 2]
if(!require(factoextra)) install.packages("factoextra") 
library("factoextra")
fviz_dend(metod.ligMed)

#Reduzindo o tamanho da letra
fviz_dend(metod.ligMed, cex = 0.5)

# Pode-se utilizar alguns indicadores para auxiliar a escolha do numero de agrupamentos. 

if(!require(NbClust)) install.packages("NbClust") 
library("NbClust")

nb <- NbClust(dados.p, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "average", index = "all")

#Portanto, utilizaremos um agrupamento em 2 grupos

#5. Interpretacao e validacao dos agrupamentos. ------------------------

##5.1 Obtendo os agrupamentos ----
grupo <- cutree(metod.ligMed,k=3)
#Numero de membros em cada agrupamento
table(grupo)

##5.2 Obtendo o nome dos membros no agrupamento 1 ----
rownames(dados.p)[grupo == 1]
rownames(dados.p)[grupo == 2]
rownames(dados.p)[grupo == 3]

#5.3 Visualizando o resultado do agrupamento no dendrograma ----
fviz_dend(metod.ligMed, k = 2, # Dividido em 2 grupos
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#8B2B36","#FC4E07"),
          color_labels_by_k = TRUE, # Colocando cada grupo de uma cor
          rect = TRUE # Adicionando um retangulo nos grupos
)



#5.4 Visualizando o resultado do agrupamento no dendrograma ----
nb <- NbClust(dados.p, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "average", index = "all")


fviz_dend(metod.ligMed, k = 5, # Dividido em 5 grupos
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00BB0C", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # Colocando cada grupo de uma cor
          rect = TRUE # Adicionando um retangulo nos grupos
)
#####
#Pergunta: Identificar se a urbanização influencia mais 
#um tipo de crime do que outro na formação dos grupos.
#####


#5.3 Visualizando o resultado do agrupamento no dendrograma ----
fviz_dend(metod.ligMed, k = 2, # Dividido em 2 grupos
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#8B2B36"),
          color_labels_by_k = TRUE, # Colocando cada grupo de uma cor
          rect = TRUE # Adicionando um retangulo nos grupos
)



#5.4 Visualizando o resultado do agrupamento no dendrograma ----
nb <- NbClust(dados.p, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "average", index = "all")


fviz_dend(metod.ligMed, k = 5, # Dividido em 5 grupos
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00BB0C", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # Colocando cada grupo de uma cor
          rect = TRUE # Adicionando um retangulo nos grupos
)
#####
#Pergunta: Identificar se a urbanização influencia mais 
#um tipo de crime do que outro na formação dos grupos.
#####


#Mudar a largura do dendograma
#como calcular a média entre os 3 grupos



