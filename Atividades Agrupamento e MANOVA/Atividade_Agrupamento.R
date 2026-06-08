#Nome: Liliane de Oliveira Falcão

# Importações -------------------------------------------------------------

if(!require(factoextra)) install.packages("factoextra") 
library("factoextra")

if(!require(NbClust)) install.packages("NbClust") 
library("NbClust")

# Atividade 1 -------------------------------------------------------------
#Utilize técnicas de análise agrupamento para identificar possíveis grupos de clientes.

clientes <- data.frame(
  Cliente = paste0("C",1:12),
  Alimentacao = c(1200,1300,1250,4000,4200,4100,2200,2300,2400,800,850,780),
  Lazer = c(300,350,320,1500,1600,1580,700,750,720,200,180,210),
  Transporte = c(450,470,430,900,950,920,600,620,610,300,290,310))

View(clientes)
#A - Padronize as variáveis numéricas.

media <-  colMeans(clientes[,2:4])
print(media)

# As médias tem escalas muito diferentes, principalmente olhando a variável "Alimentação" em relação
# as demais variáveis

#Padronizando

dados.p <- scale(clientes[,2:4])
media.p <- colMeans(dados.p)
print(media.p)

#B - Calcule a matriz de distâncias euclidianas.

d.eucl <- dist(dados.p, method = "euclidean")
print(d.eucl)

#Vizualizando a distancia euclidiana:
round(as.matrix(d.eucl)[1:4, 1:4], 2)  

#com a visualização, notamos que os pontos 2 e 3, são mais próximos do 1

#C - Ajuste um modelo de cluster utilizando: Método de Ward.D2
#3.1 Metodo hierarquico de Ward  ----
metod.ward <- hclust(d = d.eucl, method = "ward.D2")
cor(d.eucl, cophenetic(metod.ward))

#O Valor obtido foi 0.923, e quanto mais perto de 1 o agrupamento é excelente. 
#A árvore reflete muito bem a realidade dos seus dados originais.

#D - Construa o dendrograma.
fviz_dend(metod.ward)

#ou pode usar assim:  
plot(metod.ward)
# Dendograma mais simplificado

#E - Indique quantos grupos parecem mais adequados.

#A melhor opção é com 4 grupos, porém com  10 grupos também é uma opção aceitável
nb <- NbClust(dados.p, distance = "euclidean", min.nc = 2,
              max.nc = 10, method ="ward.D2", index = "all")

#F - Interprete os grupos encontrados.

grupo <- cutree(metod.ward,k=4)
grupo

rownames(dados.p)[grupo == 1]
rownames(dados.p)[grupo == 2]
rownames(dados.p)[grupo == 3]
rownames(dados.p)[grupo == 4]

#5.3 Visualizando o resultado do agrupamento no dendrograma ----
fviz_dend(metod.ward, k = 4, # Dividido em 4 grupos
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00BB0C", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # Colocando cada grupo de uma cor
          rect = TRUE # Adicionando um retangulo nos grupos
)

# Atividade 2 -------------------------------------------------------------

municipios <- data.frame(
  Municipio = paste0("M",1:15),
  PIB = c(18,20,19,55,58,60,35,36,34,12,11,13,70,72,68),
  Escolarizacao = c(72,74,73,95,96,94,85,84,83,65,66,64,98,99,97),
  Desemprego = c(12,11,13,4,5,4,7,8,7,15,16,14,3,2,3),
  IDH = c(0.68,0.69,0.67,0.90,0.91,0.92,0.80,0.79,0.81,0.60,0.59,0.61,0.95,0.96,0.94))


View(municipios)
#A - Padronize as variáveis numéricas.

media <-  colMeans(municipios[,2:5])
print(media)

# As médias tem escalas muito diferentes, principalmente olhando a variável "Escolarizacao" em relação
# à IDH e Desemprego.

#Padronizando

dados.p <- scale(municipios[,2:5])
media.p <- colMeans(dados.p)
print(media.p)

#B - Calcule a matriz de distâncias euclidianas.

d.eucl <- dist(dados.p, method = "euclidean")
print(d.eucl)

#Vizualizando a distancia euclidiana:
round(as.matrix(d.eucl)[1:4, 1:4], 2)  

#Notamos que os pontos 2 e 3 são bem próximos do 1, assim como o 2 e 3 são relativamente próximos

#c) Ajuste modelos de cluster utilizando: Método de Ward.D2, Single, Complete.

?hclust

#A correlação entre a distância Euclidiana e a matriz Conphenetic, a que a relação for maior é o melhor método para serem utilizados

#3.1 Metodo hierarquico de Ward  ----
metod.ward <- hclust(d = d.eucl, method = "ward.D2")
cor(d.eucl, cophenetic(metod.ward))

#3.1 Metodo hierarquico de Single  ----
metod.single <- hclust(d = d.eucl, method = "single")
cor(d.eucl, cophenetic(metod.single))

#3.1 Metodo hierarquico de complete  ----
metod.complete <- hclust(d = d.eucl, method = "complete")
cor(d.eucl, cophenetic(metod.complete))


#D - Ao ajustar o Método, deve ser escolhido o que a correlação for maior, comparando os métodos Ward D2, Single
# e Complete, notamos que o Complete e o Ward D2 possuem maior correlação, porém a do Complete é a maior de 
# Todas, sendo, 0.8473, então seguirei com ela.

#E - Construa o dendrograma.
fviz_dend(metod.complete)

#F- Indique quantos grupos parecem mais adequados.
nb <- NbClust(dados.p, distance = "euclidean", min.nc = 2,
              max.nc = 10, method ="complete", index = "all")

#A melhor opção é com 3 grupos, porém com 5 grupos também foi aceitável

#Ajudando na interpretacao dos resultados

grupo <- cutree(metod.complete,k=3)
grupo

rownames(dados.p)[grupo == 1]
rownames(dados.p)[grupo == 2]
rownames(dados.p)[grupo == 3]

#5.3 Visualizando o resultado do agrupamento no dendrograma ----
fviz_dend(metod.complete, k = 3, # Dividido em 4 grupos
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00BB0C", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # Colocando cada grupo de uma cor
          rect = TRUE # Adicionando um retangulo nos grupos
)

#Ajudando na interpretacao dos resultados

municipios$Grupo <- grupo
dados <- municipios[,2:6]
aggregate(dados, by = list(Grupo = dados$Grupo), mean)

# Com isso é gerada as médias em relação aos grupos adquiridos, para que seja feitas as análise dos dados
# Interpretando os dados

# Atividade 3 -------------------------------------------------------------

estudantes <- data.frame(
  Aluno = paste0("A",1:18),
  Estudo = c(5,6,5,20,22,21,12,11,13,3,4,2,18,19,17,8,9,7),
  Frequencia = c(70,72,71,98,97,99,85,84,86,60,62,58,95,94,96,78,80,79),
  Media = c(60,62,61,95,96,94,80,79,81,50,52,48,90,91,89,70,72,71),
  Faltas = c(15,14,16,1,2,1,6,7,5,20,21,22,3,2,4,10,9,11))

View(estudantes)
#A - Padronize as variáveis numéricas.

media <-  colMeans(estudantes[,2:5])
print(media)

# As médias das variáveis "Frequência" e "Média", são bem discrepantes comparadas as variáveis "Estudo" e "Faltas"
#Padronizando

dados.p <- scale(estudantes[,2:5])
media.p <- colMeans(dados.p)
print(media.p)

#B - Calcule a matriz de distâncias euclidianas.

d.eucl <- dist(dados.p, method = "euclidean")
print(d.eucl)

#Vizualizando a distancia euclidiana:
round(as.matrix(d.eucl)[1:4, 1:4], 2)  

#com a visualização, notamos que o ponto 3, é bem próximos do 1

#C - Ajuste um modelo de cluster utilizando: Método de Ward.D2, Complete e Average
#3.1 Metodo hierarquico de Ward  ----
metod.ward <- hclust(d = d.eucl, method = "ward.D2")
cor(d.eucl, cophenetic(metod.ward))

#3.1 Metodo hierarquico de complete  ----
metod.complete <- hclust(d = d.eucl, method = "complete")
cor(d.eucl, cophenetic(metod.complete))

#3.1 Metodo hierarquico de Avarage  ----
metod.average <- hclust(d = d.eucl, method = "average")
cor(d.eucl, cophenetic(metod.average))

#D - Notamos que as correlações foram muito próximas, porém a Avarage ainda é um pouco maior tendo valor de 0.7692

#E - Construa o dendrograma.
fviz_dend(metod.average)

#F- Indique quantos grupos parecem mais adequados.
nb <- NbClust(dados.p, distance = "euclidean", min.nc = 2,
              max.nc = 10, method ="average", index = "all")

#A melhor opção é com 3 grupos, porém com 6 grupos também foi aceitável

#Ajudando na interpretacao dos resultados

grupo <- cutree(metod.average,k=3)
grupo

rownames(dados.p)[grupo == 1]
rownames(dados.p)[grupo == 2]
rownames(dados.p)[grupo == 3]

#5.3 Visualizando o resultado do agrupamento no dendrograma ----
fviz_dend(metod.average, k = 3, # Dividido em 4 grupos
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00BB0C", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # Colocando cada grupo de uma cor
          rect = TRUE # Adicionando um retangulo nos grupos
)


# Respostas ---------------------------------------------------------------

# Quais perfis acadêmicos foram identificados? 

#Grupo Azul (Estudantes 1, 2, 3, 10, 11, 12): Perfil de alto desempenho e regularidade. Estão fortemente agrupados em uma altura baixa do dendrograma, o que indica que possuem características 
#homogêneas e consistentes de engajamento elevado.

#Grupo Verde (Estudantes 4, 5, 6, 13, 14, 15): Perfil intermediário. Apresentam oscilações moderadas nas métricas acadêmicas, mas mantêm um padrão que os afasta dos extremos.

#Grupo Amarelo (Estudantes 7, 8, 9, 16, 17, 18): Perfil de baixo rendimento ou desengajamento. A separação inicial desse grande braço (ligado pelo topo preto ao grupo verde)
#sugere que as variáveis de comportamento deste grupo são opostas às do grupo azul.


# Há grupos com risco de evasão? 

#Sim, o grupo amarelo tem maior risco de evasão, pois a linha preta superior mostra que o grupo amarelo se funde aos demais 
#em um nível de dissimilaridade muito alto (perto da altura ~ 2.5). Isso significa que o comportamento deles destoa 
#completamente do padrão dos alunos azuis.


#Existe relação entre frequência e desempenho?

#O dendrograma confirma uma forte relação de dependência positiva entre frequência e desempenho.
#Agrupamento por Similaridade: O algoritmo agrupa elementos que se comportam de forma parecida. 
#O fato de haver uma divisão clara e simétrica em três blocos bem definidos indica que os alunos com alta frequência 
#compartilham as melhores notas (Grupo Azul), enquanto os alunos ausentes compartilham reprovações e notas baixas(Grupo Amarelo).

#Conclusão: A frequência atua como um fator preditor direto do sucesso ou do fracasso acadêmico na base de dados analisada.

















