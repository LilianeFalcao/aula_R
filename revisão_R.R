
# Revisao -----------------------------------------------------------------

dados <- data.frame(Metodo = c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C"),
                    Nota = c(7.5, 8.0, NA, 6.5, 7.0, 8.5, 9.0, 8.7, NA, 9.2, 6.0, 6.5, 7.0, 6.8, NA),
                    Horas = c(10, 12, 11, NA, 9, 14, 15, 13, 14, NA, 8, 9, 10, 9, 8), 
                    Frequencia = c(80, 85, 78, 82, NA, 90, 92, 88, 91, 93, 75, 78, NA, 77, 76))

View(dados)

# 1. a- tratamento -----------------------------------------------------------

any(is.na(dados))
NAS <- round(colSums(is.na(dados))*100/nrow(dados), 2)  
NAS
colSums(is.na(dados))
NAS[NAS > 10]

# 1. b- substituição ------------------------------------------------------

# Nota --------------------------------------------------------------------

grupo1 <- dados[!is.na(dados$Nota),] 
grupo2 <- dados[is.na(dados$Nota),]  

t.test(grupo1$Nota, grupo2$Nota)  #não foi suficiente 
t.test(grupo1$Horas, grupo2$Horas)  #são iguais
t.test(grupo1$Frequencia, grupo2$Frequencia)  #são iguais

dados$Nota[is.na(dados$Nota)] <- mean(dados$Nota, na.rm = TRUE)  

View(dados)

#Horas

grupo1 <- dados[!is.na(dados$Horas),] 
grupo2 <- dados[is.na(dados$Horas),]  

t.test(grupo1$Nota, grupo2$Nota)  #são iguais
t.test(grupo1$Horas, grupo2$Horas)  #não foi suficiente 
t.test(grupo1$Frequencia, grupo2$Frequencia)  #são iguais

dados$Horas[is.na(dados$Horas)] <- mean(dados$Horas, na.rm = TRUE)  

# Frequencia --------------------------------------------------------------

grupo1 <- dados[!is.na(dados$Frequencia),] 
grupo2 <- dados[is.na(dados$Frequencia),]  

t.test(grupo1$Nota, grupo2$Nota)  #são iguais
t.test(grupo1$Horas, grupo2$Horas)  #são iguais 
t.test(grupo1$Frequencia, grupo2$Frequencia)  #não foi suficiente 

dados$Frequencia[is.na(dados$Frequencia)] <- mean(dados$Frequencia, na.rm = TRUE)  

# 2. Estatística descritiva -----------------------------------------------

# 2. a- Médias ------------------------------------------------------------
medias <- colMeans(dados[, c("Nota", "Horas", "Frequencia")])
medias

# 2. b- Variancia e Covariancia -------------------------------------------

#Variancia
var_nota <- var(dados$Nota)
var_horas <- var(dados$Horas)

#Covariancia
matriz_cov <- cov(dados[, 2:3])
View(matriz_cov)

# Matriz de Correlação ----------------------------------------------------

cor_notas_horas <- cor(dados[, 2:3])
cor_notas_horas

cor_notas_freq <- cor(dados[, c("Horas", "Frequencia")])
cor_notas_freq

#  Interprete os resultados (força e direção da relação) ------------------

#b) Interpretação
#Nota e Horas: Geralmente apresentam correlação positiva e forte. Isso significa que
#quanto mais horas de estudo, maior tende a ser a nota.
#Nota e Frequência: Também costuma ser positiva. Se o coeficiente estiver próximo de 1,
#a relação é forte; próximo de 0, é fraca.

#4. Visualização ------------------------------------------------------------
# 4. a-  boxplot da Nota por Método. --------------------------------------

boxplot(Nota ~ Metodo, data = dados, 
        main = "Desempenho por Método de Ensino",
        xlab = "Método", ylab = "Nota", col = "lightblue")

# 4. b- Outliers. --------------------------------------

# Observando o gráfico, pontos fora das "hastes" (whiskers) são candidatos a outliers.


# 5. a- Normalidade: Teste Geral ------------------------------------------
# Se p-valor > 0.05, os dados seguem distribuição normal.

shapiro.test(dados$Nota)
 
#Distribuição normal


# 5. b- Por Grupo ---------------------------------------------------------

tapply(dados$Nota, dados$Metodo, shapiro.test)

# 6. ANOVA e Teste de Tukey --------------------------------------------------
# 6. a- Verificando diferença entre médias (ANOVA) ------------------------

modelo_anova <- aov(Nota ~ Metodo, data = dados)
summary(modelo_anova)

#A ANOVA testa a hipótese nula de que todas as médias são iguais.
#Se o p-value for menor que 0.05, rejeitamos a hipótese nula: pelo menos um método é
#diferente dos outros.


# 6. b- Teste de Tukey ----------------------------------------------------

#O Tukey faz a comparação "par a par" (A-B, A-C, B-C).
#Como interpretar: No resultado, olhe para a coluna p adj. Se for menor que 0.05, aquele
#par específico (ex: B-A) possui diferença estatisticamente significativa.

tukey_result <- TukeyHSD(modelo_anova)
print(tukey_result)
plot(tukey_result)


