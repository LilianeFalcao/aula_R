
# Atv_01 ------------------------------------------------------------------


# Atv_02 ------------------------------------------------------------------

dados_iris <- iris[, 1:4]
dados_iris

colMeans(dados_iris)  

dados_iris_norm <- scale(dados_iris)
colMeans(dados_iris_norm) 
sd(dados_iris_norm) 

# 2-Matriz de correlação --------------------------------------------------
mat_cor <- cor(dados_iris_norm)
print(mat_cor, digits = 2)

ev <- eigen(mat_cor)
print(ev$values, digits = 2)

ev$values/sum(ev$values)

# 5-Calculando PCA ----------------------------------------------------------

resultado <- princomp(dados_iris_norm)
summary(resultado)

# Atv_3 -------------------------------------------------------------------

dados_cadeia <- USArrests

colMeans(dados_cadeia)  

dados_cadeia_norm <- scale(dados_cadeia)
colMeans(dados_cadeia_norm) 
sd(dados_cadeia_norm) 

matriz_corr <- cor(dados_cadeia_norm)
print(matriz_corr, digits = 2)

ev <- eigen(matriz_corr)
print(ev$values, digits = 2)

ev$values/sum(ev$values)

resultado <- princomp(dados_cadeia_norm)
summary(resultado)


