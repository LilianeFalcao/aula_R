dados <- USArrests
View(dados)

colnames(dados) <- c("Homicídio", "Assalto", "População Urbana","Estupro")
View(dados)
#Homicídio: numero de homicídios a cada 100.000 hab.
#Assalto: número de assaltos a cada 100.000 hab.
#População urbana: Percentual da população urbana
#Estupro: numero de estupro a cada 100.000 hab.


# 1 - Padronização dos dados ----------------------------------------------
media <-  colMeans(dados)
print(media)

#1.1 Padronizando os dados
dados.p <- scale(dados)
media.p <- colMeans(dados.p)
print(media.p)

# 2 - Determinando a medida de similaridade -------------------------------
## 2.1 Calculando a distancia Euclidiana ----

d.eucl <- dist(dados.p, method = "euclidean")
print(d.eucl)

#Vizualizando a distancia euclidiana para as QUATRO primeiras empresas:
round(as.matrix(d.eucl)[1:4, 1:4], 2)  #arrendondando para duas casas decimais

# 3. Selecao do algoritmo de agrupamento -------------------------------
?hclust

#3.1 Metodo hierarquico de Ward  ----
metod.ward2 <- hclust(d = d.eucl, method = "ward.D2")
# Calculando a matriz cofenetica
cor(d.eucl, cophenetic(metod.ward2))

#3.1 Metodo hierarquico de Ward  ----
metod.ward <- hclust(d = d.eucl, method = "ward.D")
# Calculando a matriz cofenetica
cor(d.eucl, cophenetic(metod.ward))

#3.2 Metodo da ligacao media
metod.ligMed <- hclust(d.eucl, method = "average")
cor(d.eucl, cophenetic(metod.ligMed))

#3.3 Metodo da ligacao Complete
metod.complete <- hclust(d.eucl, method = "complete")
cor(d.eucl, cophenetic(metod.complete))

#3.4 Metodo da ligacao single
metod.single <- hclust(d.eucl, method = "single")
cor(d.eucl, cophenetic(metod.single))

#3.5 Metodo da ligacao mcquitty
metod.mcquitty <- hclust(d.eucl, method = "mcquitty")
cor(d.eucl, cophenetic(metod.mcquitty))

#3.6 Metodo da ligacao median
metod.median <- hclust(d.eucl, method = "median")
cor(d.eucl, cophenetic(metod.median))

#3.7 Metodo da ligacao centroid
metod.centroid <- hclust(d.eucl, method = "centroid")
cor(d.eucl, cophenetic(metod.centroid))

#Pela ligacao media o valor da correlacao foi maior, portanto sera o utilizado


# 4. Definicao do numero de agrupamentos -------------------------------
#4.1 Obtendo o dendrograma ----

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
grupo <- cutree(metod.ligMed,k=2)
#Numero de membros em cada agrupamento
table(grupo)

##5.2 Obtendo o nome dos membros no agrupamento 1 ----
rownames(dados.p)[grupo == 1]
rownames(dados.p)[grupo == 2]

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

