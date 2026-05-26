
# Importar os dados do arquivo empresas.xlsx. ----
dados <- empresas

#Definindo a primeira coluna do data frame como nome
dados <-data.frame(dados[,2:4], row.names = dados$...1)
View(dados)

# 1 - Padronização dos dados ----------------------------------------------
# idealmente seria sempre, mass quando as médias tem uma discrepancia muito grande entre os números padroniza
media <-  colMeans(dados)
print(media)

#1.1 Padronizando os dados, médias são 0 e desvios 1
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
metod.ward <- hclust(d = d.eucl, method = "ward.D2")

# Calculando a matriz cofenetica
## Compara as distancias efetivamente observadas entre os objetos e
## as distancias previstas a partir do processo de agrupamento.
#Quanto maior for a correlacao, melhor.
# Correlacao entre a distancia da matriz cofenetica e a distancia original
cor(d.eucl, cophenetic(metod.ward))

#3.2 Metodo da ligacao media
metod.ligMed <- hclust(d.eucl, method = "average")
# Correlacao entre a distancia cofenetica e  a distancia original
cor(d.eucl, cophenetic(metod.ligMed))

#Pela ligacao media o valor da correlacao foi maior, portanto sera o utilizado

# 4. Definicao do numero de agrupamentos -------------------------------
#4.1 Obtendo o dendrograma ----

if(!require(factoextra)) install.packages("factoextra") 
library("factoextra")

fviz_dend(metod.ligMed)

#Reduzindo o tamanho da letra
fviz_dend(metod.ligMed, cex = 0.7)

# Pode-se utilizar alguns indicadores para auxiliar a escolha do numero de agrupamentos. 

if(!require(NbClust)) install.packages("NbClust") 
library("NbClust")

nb <- NbClust(dados.p, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "average", index = "all")

#Portanto, utilizaremos um agrupamento em 4 grupos
#5. Interpretacao e validacao dos agrupamentos. ------------------------

##5.1 Obtendo os agrupamentos ----
grupo <- cutree(metod.ligMed,k=4)
#Numero de membros em cada agrupamento
table(grupo)

##5.2 Obtendo o nome dos membros no agrupamento 1 ----
rownames(dados.p)[grupo == 1]
rownames(dados.p)[grupo == 2]
rownames(dados.p)[grupo == 3]
rownames(dados.p)[grupo == 4]

#5.3 Visualizando o resultado do agrupamento no dendrograma ----
fviz_dend(metod.ligMed, k = 4, # Dividido em 4 grupos
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00BB0C", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # Colocando cada grupo de uma cor
          rect = TRUE # Adicionando um retangulo nos grupos
)


