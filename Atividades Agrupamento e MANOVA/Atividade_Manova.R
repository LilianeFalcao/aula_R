# Nome: Liliane de Oliveira Falcão ----------------------------------------

if(!require(Hotelling)) install.packages("Hotelling") 
library(Hotelling)

if(!require(car)) install.packages("car")
library(car)

# Atividade 1 -------------------------------------------------------------

dados <- data.frame(
  Ensino = factor(c(rep("Tradicional",10), rep("Hibrida",10), rep("Projetos",10))),
  Matematica = c(
    68,70,72,69,71,73,74,70,72,71,
    75,77,78,80,79,81,82,76,78,79,
    82,84,85,87,86,88,89,83,85,86),
  Estatistica = c(
    66,68,71,65,69,72,70,67,70,69,
    73,76,75,79,77,80,81,74,77,78,
    79,83,82,86,84,87,88,81,84,85),
  Programacao = c(
    71,74,73,72,75,77,76,73,75,74,
    78,81,80,83,82,84,85,79,81,82,
    86,88,87,91,89,92,93,87,89,90))

View(dados)

# a) Obtenha as médias para cada grupo.  ----------------------------------

medias_grupo <- aggregate(. ~ Ensino, data = dados, mean)
print(medias_grupo)

# b) Verifique os pressupostos da MANOVA: ---------------------------------
# normalidade multivariada; 

# Matemática por grupo
tapply(dados$Matematica, dados$Ensino, shapiro.test)#é normal
# Estatística por grupo
tapply(dados$Estatistica, dados$Ensino, shapiro.test)#é normal
# Programação por grupo
tapply(dados$Programacao, dados$Ensino, shapiro.test)#é normal

#Para o Teste de Levene, a hipótese nula - H0 assume que as variâncias dos grupos são iguais.
# Olhando para a coluna Pr(>F) (os p-valores)

leveneTest(Matematica ~ Ensino, data = dados)
leveneTest(Estatistica ~ Ensino, data = dados)
leveneTest(Programacao ~ Ensino, data = dados)

#Como todos os valores ficaram muito acima de (0,05), significa que a dispersão das notas dos alunos 
#é estatisticamente equivalente entre os três métodos de ensino (Tradicional, Híbrida e Projetos).

# c) Ajuste o modelo MANOVA.  ---------------------------------------------
modelo <- manova(cbind(Matematica, Estatistica, Programacao) ~ Ensino, data=dados)

# d) Interprete os testes: ------------------------------------------------

#H0:As médias de todas as disciplinas combinadas são iguais entre os três métodos de ensino (Tradicional, Híbrida e Projetos).

summary(modelo)  #Teste Pillai
#grau de liberdade é o número das variáveis - 1 

summary(modelo, test="Wilks")  #Teste lambda de Wilks

#Os resultados dos 2 testes, de Lambda de Wilks e de Pillais, são bem menores que 0.05,
#sendo Pillai ->  52 2.068e-07 *** e Lambda Wilks ->50 4.438e-13 ***

#Nesse caso a hipotese nula deve ser rejeitada, pois existe uma diferençaa estatisticamente significativa no desempenho dos alunos 
#(considerando Matemática, Estatística e Programação juntas) dependendo do método de ensino utilizado (Tradicional, Híbrida ou Projetos). 
#O tipo de ensino realmente impacta as notas.


# e) Caso a MANOVA seja significativa, realize as ANOVAs univariadas --------

summary.aov(modelo)

# 2. Teste de Tukey (Post-Hoc) para detalhar onde estão as diferenças
TukeyHSD(anova_matematica)
TukeyHSD(anova_estatistica)
TukeyHSD(anova_programacao)

# 3. Médias gerais para o desempenho global

aggregate(. ~ Ensino, data = dados, FUN = mean)

# f) Em quais disciplinas os grupos diferem?  -----------------------------

#Ao olhar a coluna Pr(>F) no resultado de cada summary(anova_...) gerado, você notará que os grupos diferem em todas as disciplinas 
#(Matemática, Estatística e Programação). Todos os p-valores univariados são muito menores que (0,05).
#Analisando o detalhamento das comparações em pares feitas pelo teste de Tukey (TukeyHSD), 
#as conclusões são:
#Matemática: Todas as metodologias são significativamente diferentes entre si ((p < 0,05)).
#Estatística: Todas as metodologias são significativamente diferentes entre si ((p < 0,05)).
#Programação: Todas as metodologias são significativamente diferentes entre si ((p < 0,05)).

# g) Qual metodologia apresentou melhor desempenho global?  ---------------

#A metodologia que apresentou o melhor desempenho global foi a de Projetos.
#Comparando as médias gerais calculadas anteriormente, notamos que a de projetos se mantem com médias entorno de 85 e 89.2 
#Enquanto as demais se mantem entre as casas de 68.7 e no maxímo 81.5


# Atividade 2 -------------------------------------------------------------

dados <- data.frame(
  Treinamento = factor(c(
    rep("Musculacao",10),
    rep("Funcional",10),
    rep("Cross",10)
  )),
  Forca = c(
    80,82,84,83,81,85,86,82,84,83,
    79,81,80,82,83,81,80,82,81,79,
    92,94,93,95,91,96,94,93,95,92),
  Resistencia = c(
    75,77,74,76,75,78,76,74,77,75,
    76,75,77,74,76,75,78,76,74,77,
    75,76,74,77,75,76,75,77,74,76),
  Flexibilidade = c(
    70,72,71,69,73,70,71,72,69,71,
    71,70,72,71,69,72,70,71,72,70,
    70,71,69,72,70,71,70,72,69,71))

# a) Obtenha as médias para cada grupo.  ----------------------------------

medias_grupo <- aggregate(. ~ Treinamento, data = dados, FUN = mean)
print(medias_grupo)

# b) Verifique os pressupostos da MANOVA: ---------------------------------
# normalidade multivariada; 

# Força por grupo
tapply(dados$Forca, dados$Treinamento, shapiro.test)#é normal

# Resistencia por grupo
tapply(dados$Resistencia, dados$Treinamento, shapiro.test)#é normal

# Flexibilidade por grupo
tapply(dados$Flexibilidade, dados$Treinamento, shapiro.test)#é normal

#Para o Teste de Levene, a hipótese nula - H0 assume que as variâncias dos grupos são iguais.
# Olhando para a coluna Pr(>F) (os p-valores)

leveneTest(Forca ~ Treinamento, data = dados)
leveneTest(Resistencia ~ Treinamento, data = dados)
leveneTest(Flexibilidade ~ Treinamento, data = dados)

#Como todos os valores ficaram acima de (0,05), significa que a dispersão dos treinamento
#é estatisticamente equivalente entre os três métodos de exercício (Força, Resistencia e Flexibilidade).

# c) Ajuste o modelo MANOVA.  ---------------------------------------------
modelo <- manova(cbind(Forca, Resistencia, Flexibilidade) ~ Treinamento, data=dados)

# d) Interprete os testes: ------------------------------------------------

#H0:As médias dos treinamentos combinados são iguais entre os três métodos de exercício (Força, Resistencia e Flexibilidade).
summary(modelo)  #Teste Pillai
summary(modelo, test="Wilks")  #Teste lambda de Wilks

#Os resultados dos 2 testes, de Lambda de Wilks e de Pillais, são bem menores que 0.05,
#sendo Pillai ->   52 7.15e-06 *** e Lambda Wilks -> 50 4.497e-13 ***

#Nesse caso a hipotese nula deve ser rejeitada, pois existe uma diferençaa estatisticamente significativa no desempenho  
#(considerando Força, Resistencia e Flexibilidade juntas) dependendo do método de exercício utilizado. 

# e) Caso a MANOVA seja significativa, realize as ANOVAs univariadas --------

summary.aov(modelo)

#A variável que destoa do conjunto é a variável Força, onde obteve o resultado, 2.2e-16 ***, que é consideravelmente
#abaixo de 0.05

# 3. Médias gerais para o desempenho global
aggregate(dados[, 2:4], by = list(Metodologia = dados$Treinamento), FUN = mean)

# f) Em quais disciplinas os grupos diferem?  -----------------------------

#Ao olhar a coluna Pr(>F) no resultado de cada summary(anova_...) gerado, você notará que os grupos diferem um entre os treinos, O p-value da variável Força é bem menor que (0,05)
#enquanto as demais, deram valores próximos, ou seja,as conclusões são:
#Força: A metodologia é significativamente diferente entre si ((p < 0,05)).
#Resistencia: A metodologia é parecida entre si ((p > 0,05)).
#Flexibilidade: A metodologia é parecida entre si ((p > 0,05)).

# g) Qual metodologia apresentou melhor desempenho global?  ---------------









