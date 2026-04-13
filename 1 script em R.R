
# EXEMPLO 1 #

#Carregando os dados (arquivo dados_pca-exemplo.csv)

# 1-Normalização dos dados --------------------------------------------------
dados <- dados_pca.exemplo
head(dados) #Observe que a primeira coluna é só identificação da empresa
colMeans(dados[, 2:ncol(dados)])  #calcular a média de coluna por coluna
#padronizando os dados - scale já retorna os dados com a média 0 e o desvio - 1
dados_norm <- scale(dados [, 2:ncol(dados)])
head(dados_norm)
colMeans(dados_norm) #Média é 0
sd(dados_norm[, 2]) #Desvio-padrão é 1


# 2-Matriz de correlação --------------------------------------------------
mat_cor <- cor(dados_norm)
print(mat_cor, digits = 2)

if(!require(corrplot)) install.packages("corrplot") # Outra forma de calcular correlação
library(corrplot)
corrplot(mat_cor, method = "circle")
corrplot(mat_cor, method = "number")

# 3-Autovalores -----------------------------------------------------------
#eigen - função de calculo de autovalores
ev <- eigen(mat_cor)
print(ev$values, digits = 2)

ev$values/sum(ev$values)


# 5-Calculando PCA ----------------------------------------------------------

resultado <- princomp(dados_norm)

summary(resultado)

#Visualização dos PCA
if(!require(factoextra)) install.packages("factoextra") 
library(factoextra)
fviz_eig(resultado, addlabels = TRUE) 

#Critério de Kaiser
#De acordo com o numéro de auto valores maiores que um pe a quantidade de quantitativo que deve ser 
# Escolhido

# 5-Definindo o numero de componentes principais --------------------------
plot(resultado)
abline(h = 1, col = "red", lwd = 2) #insere a linha horizontal)

biplot(resultado, scale=0)
fviz_pca_var(resultado, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
resultado$scores



