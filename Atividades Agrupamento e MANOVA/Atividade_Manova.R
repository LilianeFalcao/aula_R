# Nome: Liliane de Oliveira Falcão ----------------------------------------

if(!require(Hotelling)) install.packages("Hotelling") 
library(Hotelling)

if(!require(car)) install.packages("car")
library(car)

if(!require(MVN)) install.packages("MVN")
library(MVN)

# Teste t: compara médias de dois grupos para uma variável resposta.
# ANOVA: compara médias de três ou mais grupos para uma variável resposta.
# MANOVA: compara médias de três ou mais grupos considerando simultaneamente
# duas ou mais variáveis resposta.

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

#Isso testa apenas normalidade univariada.
# Matemática por grupo
tapply(dados$Matematica, dados$Ensino, shapiro.test)#é normal
# Estatística por grupo
tapply(dados$Estatistica, dados$Ensino, shapiro.test)#é normal
# Programação por grupo
tapply(dados$Programacao, dados$Ensino, shapiro.test)#é normal


#H0: Os dados seguem distribuição normal multivariada.
#H1: Os dados não seguem distribuição normal multivariada.

#se p > 0,05 não rejeitamos H₀ e assumimos normalidade multivariada.
# Grupo Tradicional
mvn(
  data = subset(dados, Ensino == "Tradicional")[,c("Matematica","Estatistica","Programacao")],
  mvnTest = "mardia"
)

# Grupo Híbrida
mvn(
  data = subset(dados, Ensino == "Hibrida")[,c("Matematica","Estatistica","Programacao")],
  mvnTest = "mardia"
)

# Grupo Projetos
mvn(
  data = subset(dados, Ensino == "Projetos")[,c("Matematica","Estatistica","Programacao")],
  mvnTest = "mardia"
)

#A normalidade multivariada foi avaliada por meio do teste de Mardia para cada grupo analisado. As hipóteses testadas foram: 
#H₀: os dados seguem distribuição normal multivariada; H₁: os dados não seguem distribuição normal multivariada. Como os valores de p foram superiores 
#a 0,05 para os grupos avaliados, não houve evidências para rejeitar H₀, indicando que o pressuposto de normalidade multivariada da MANOVA foi atendido.

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
#sendo Pillai ->  Pr(>F) = 2.068e-07 < 0.05 *** e Lambda Wilks ->50 4.438e-13 ***

#Nesse caso a hipotese nula deve ser rejeitada, pois existe uma diferençaa estatisticamente significativa no desempenho dos alunos 
#(considerando Matemática, Estatística e Programação juntas) dependendo do método de ensino utilizado (Tradicional, Híbrida ou Projetos). 
#O tipo de ensino realmente impacta as notas.


# e) Caso a MANOVA seja significativa, realize as ANOVAs univariadas --------

summary.aov(modelo)

# Aqui ele dá a relação entre as médias de forma estátistica, ou seja, se o P-Value próximo ou igual à Zero, significa que as médias
# São estatisticamente diferentes e se o P-Value é > ou igual a 0.05, as médias são estatisticamente iguais.

# 2. Teste de Tukey (Post-Hoc) para detalhar onde estão as diferenças
anova_matematica <- aov(Matematica ~ Ensino, data=dados)
anova_estatistica <- aov(Estatistica ~ Ensino, data=dados)
anova_programacao <- aov(Programacao ~ Ensino, data=dados)

TukeyHSD(anova_matematica)
TukeyHSD(anova_estatistica)
TukeyHSD(anova_programacao)

#2. Interpretação pelo Valor p (P-valor) ou Intervalo de ConfiançaSe você estiver gerando a análise em softwares como R, Python ou 
#Minitab:Se o valor p < 0,05: A diferença entre as médias daquele par específico é estatisticamente significativa.Se o valor p > 0,05: 
#Não há diferença estatística significativa entre os dois grupos.Intervalo de Confiança: Se o intervalo gerado para a diferença entre duas 
#médias cruzar o valor 0 (ex: -2 até +5), significa que eles são estatisticamente iguais.

# 3. Médias gerais para o desempenho global

aggregate(. ~ Ensino, data = dados, mean)

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

medias_grupo <- aggregate(. ~ Treinamento, data = dados, mean)
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
#A variância das medidas é homogênea entre os grupos de treinamento
#(Musculação, Funcional e Cross).

# c) Ajuste o modelo MANOVA.  ---------------------------------------------
modelo <- manova(cbind(Forca, Resistencia, Flexibilidade) ~ Treinamento, data=dados)

# d) Interprete os testes: ------------------------------------------------

# H0: Os grupos de treinamento (Musculação, Funcional e Cross)
# possuem o mesmo vetor de médias para Força, Resistência e Flexibilidade.

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
#A metodologia Cross apresentou o melhor desempenho global. Embora os três grupos tenham médias semelhantes em Resistência e Flexibilidade,
#o grupo Cross obteve uma média de Força substancialmente superior às demais metodologias. Como a análise MANOVA indicou diferença significativa entre os
#grupos e a ANOVA univariada mostrou que essa diferença ocorre principalmente na variável Força, conclui-se que o treinamento Cross foi o que apresentou o 
#melhor resultado geral entre os métodos avaliados.








