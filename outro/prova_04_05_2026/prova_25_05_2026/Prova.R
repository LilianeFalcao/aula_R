################################################################################
#           GUIA DE CONSULTA RÁPIDA: PROVA DE ANÁLISE MULTIVARIADA             #
#   Tópicos: Regressão Múltipla, Análise Discriminante (LDA) e MANOVA          #
################################################################################

# ==============================================================================
# 1) REGRESSÃO LINEAR MÚLTIPLA
# ==============================================================================
# DICA DE OURO: O professor quer ver se você sabe validar os pressupostos 
# ANTES de confiar cegamente nos coeficientes e nos p-valores.
# DICA DE OURO: Sempre olhe primeiro o teste F global (última linha do summary).
# Se p > 0.05, o modelo não funciona e você nem precisa interpretar o resto!

# --- Passo 1.1: Criar/Carregar os Dados e Pacotes ---
if(!require(car)) install.packages("car")
if(!require(lmtest)) install.packages("lmtest")
library(car)
library(lmtest)

dados_reg <- data.frame(
  Marketing   = c(15,18,20,22,25,30,28,35,40,45,50,55,60,65,70), [cite: 17]
  Vendedores  = c(5,6,7,6,8,9,7,10,11,12,13,14,15,16,17), [cite: 18]
  Tamanho     = c(120,150,160,155,180,200,190,220,250,270,300,320,350,370,400), [cite: 19]
  Faturamento = c(200,220,250,240,270,300,290,330,360,390,420,450,480,510,550) [cite: 20]
)

# --- Passo 1.2: Ajustar o Modelo Completo ---
mod_completo <- lm(Faturamento ~ Marketing + Vendedores + Tamanho, data = dados_reg) [cite: 22, 23]
summary(mod_completo) 

# DICA DE ANÁLISE (summary):
# - Coeficientes (Estimate): "Para cada aumento de 1 unidade em X, Y varia [valor], mantendo as outras constantes"[cite: 25].
# - R2 Ajustado: Use sempre o "Adjusted R-squared" para modelos múltiplos (penaliza variáveis inúteis)[cite: 26].

# --- Passo 1.3: Análise Gráfica dos Pressupostos ---
par(mfrow = c(1, 2))
plot(mod_completo, which = 1) # Resíduos vs Valores Ajustados (Homocedasticidade/Linearidade) [cite: 34]
plot(mod_completo, which = 2) # Normal Q-Q (Normalidade dos resíduos visualmente) [cite: 35]
par(mfrow = c(1, 1))

# --- Passo 1.4: Testes Formais dos Pressupostos ---
# A) Normalidade dos Resíduos (Shapiro-Wilk) -> H0: Resíduos são normais (Queremos p > 0.05) [cite: 30]
shapiro.test(residuals(mod_completo))

# B) Independência (Durbin-Watson) -> H0: Não há autocorrelação (DW deve ser próximo de 2) [cite: 31]
durbinWatsonTest(mod_completo)

# C) Multicolinearidade (VIF) 
# DICA DE OURO: Se VIF > 5 (ou 10), há multicolinearidade grave. As variáveis explicativas 
# estão "andando juntas" e os p-valores individuais perdem a precisão[cite: 32].
vif(mod_completo)

# --- Passo 1.5: Ajuste de Novo Modelo e Comparação (Se necessário) ---
# Removendo a variável problemática com alto VIF ou não significativa [cite: 37]
mod_reduzido <- lm(Faturamento ~ Marketing, data = dados_reg)
summary(mod_reduzido)

# Comparação formal de modelos (Se p > 0.05, o modelo mais simples é melhor!) [cite: 38]
anova(mod_reduzido, mod_completo)

# --- Passo 1.6: Predição ---
novo_cenario <- data.frame(Marketing = 50, Vendedores = 12, Tamanho = 300) [cite: 42, 43, 44]
predict(mod_reduzido, newdata = novo_cenario) [cite: 39]


# ==============================================================================
# 2) ANÁLISE DISCRIMINANTE LINEAR (LDA)
# ==============================================================================
# DICA DE OURO: A LDA serve para CLASSIFICAR um indivíduo em um grupo baseado em variáveis contínuas[cite: 45, 53].
# DICA DE OURO: A variável que mais discrimina/contribui é aquela com o MAIOR coeficiente 
# em valor absoluto (ignore o sinal de + ou - na tabela de coeficientes lineares)[cite: 60].

if(!require(MASS)) install.packages("MASS")
library(MASS)

dados_lda <- data.frame(
  Grupo = factor(c(rep("Bom", 12), rep("Ruim", 12))), [cite: 56]
  Matematica = c(80,85,88,90,87,92,89,91,93,86,84,88, 60,62,58,65,63,61,59,57,66,64,55,68), [cite: 56]
  Estatistica = c(78,82,85,87,84,89,86,88,90,83,81,85, 58,60,56,63,61,59,57,55,64,62,53,66), [cite: 56]
  Programacao = c(82,86,89,91,88,93,90,92,94,87,85,89, 62,64,60,67,65,63,61,59,68,66,57,70) [cite: 56]
)

# --- Passo 2.1: Ajustar o Modelo LDA ---
mod_lda <- lda(Grupo ~ Matematica + Estatistica + Programacao, data = dados_lda) [cite: 57]
print(mod_lda)

# --- Passo 2.2: Matriz de Confusão e Acurácia ---
pred_lda <- predict(mod_lda, dados_lda) [cite: 61]
matriz_confusao <- table(Real = dados_lda$Grupo, Predito = pred_lda$class)
print(matriz_confusao)

# Acurácia (Diagonal principal dividida pelo total) [cite: 62]
acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)
cat("Acurácia do Modelo:", acuracia * 100, "%\n")

# --- Passo 2.3: Classificar Novo Caso ---
novo_aluno <- data.frame(Matematica = 70, Estatistica = 68, Programacao = 72) [cite: 64, 65, 66]
predict(mod_lda, newdata = novo_aluno) [cite: 63]


# ==============================================================================
# 3) MANOVA (ANÁLISE DE VARIÂNCIA MULTIVARIADA)
# ==============================================================================
# DICA DE OURO: Usamos MANOVA quando queremos avaliar o impacto de variáveis categóricas 
# em MÚLTIPLAS variáveis dependentes AO MESMO TEMPO[cite: 77].

# DICA DE OURO (O maior erro de prova): A MANOVA só diz "há diferença global". Você é OBRIGADO 
# a rodar as ANOVAs univariadas (summary.aov) e o teste de Tukey DEPOIS para detalhar quem é melhor[cite: 116, 118, 119]!

dados_manova <- data.frame(
  Ensino = factor(c(rep("Presencial", 10), rep("Hibrido",10), rep("EAD", 10))), [cite: 81, 82, 83, 84]
  Matematica = c(82,85,88,79,90,84,87,91,86,83, 75,78,80,77,79,81,76,74,82,78, 68,70,72,66,71,69,73,67,65,74), [cite: 85, 86, 87]
  Estatistica = c(80,83,85,78,88,82,84,89,85,81, 72,75,77,74,76,78,73,71,79,75, 64,66,68,63,67,65,69,62,61,70), [cite: 88, 89, 90, 91]
  Programacao = c(85,88,90,84,92,87,89,93,91,86, 78,80,82,79,81,83,77,76,84,80, 70,72,74,69,73,71,75,68,67,76) [cite: 92, 93]
)

# --- Passo 3.1: Análise Descritiva e Correlação ---
aggregate(cbind(Matematica, Estatistica, Programacao) ~ Ensino, data = dados_manova, mean) [cite: 94, 98]
cor(dados_manova[, c("Matematica", "Estatistica", "Programacao")]) # Devem ser correlacionadas! [cite: 99]

# --- Passo 3.2: Pressuposto de Normalidade ---
mod_linear_m <- lm(cbind(Matematica, Estatistica, Programacao) ~ Ensino, data = dados_manova)
residuos <- residuals(mod_linear_m)
apply(residuos, 2, shapiro.test) # Testa normalidade por coluna [cite: 100]

# --- Passo 3.3: Executando a MANOVA ---
Y <- cbind(dados_manova$Matematica, dados_manova$Estatistica, dados_manova$Programacao) [cite: 103, 104, 107, 109]
fit_manova <- manova(Y ~ dados_manova$Ensino) [cite: 102, 111]

summary(fit_manova, test = "Wilks") [cite: 114]
summary(fit_manova, test = "Pillai") [cite: 115]
#Se p < 0.05: Existe diferença multivariada significativa[cite: 112]. 
# Wilks' Lambda perto de 0 = Forte separação. Pillai é o mais robusto a violações.

# --- Passo 3.4: Desdobramento (SÓ FAÇA SE A MANOVA DEU SIGNIFICATIVA!) ---
summary.aov(fit_manova) # 1. ANOVAs Univariadas (Vê quais matérias isoladas diferem) [cite: 118]

# 2. Teste de Tukey (Descobre quais grupos específicos são diferentes entre si) [cite: 119]
TukeyHSD(aov(Matematica ~ Ensino, data = dados_manova))
TukeyHSD(aov(Estatistica ~ Ensino, data = dados_manova))
TukeyHSD(aov(Programacao ~ Ensino, data = dados_manova))
# DICA: Olhe o 'p adj'. Se p < 0.05, as modalidades têm desempenho significativamente diferente


# Guia 2 ------------------------------------------------------------------

################################################################################
#              SCRIPT MESTRE: PREPARAÇÃO PARA A PROVA PRÁTICA                  #
#         Tópicos: Regressão Avançada, Discriminante Avançada e MANOVA         #
################################################################################

# ==============================================================================
# TÓPICO 1: REGRESSÃO LINEAR MÚLTIPLA E SELEÇÃO DE MODELOS (STEPWISE)
# ==============================================================================

# --- 1.1 Carga de Dados de Exemplo ---
dados_reg <- data.frame(
  Marketing = c(15,18,20,22,25,30,28,35,40,45,50,55,60,65,70),
  Vendedores = c(5,6,7,6,8,9,7,10,11,12,13,14,15,16,17),
  Tamanho = c(120,150,160,155,180,200,190,220,250,270,300,320,350,370,400),
  Faturamento = c(200,220,250,240,270,300,290,330,360,390,420,450,480,510,550)
)

# --- 1.2 Diagnóstico Inicial (Antes do Ajuste) ---
# Matriz de correlação e visualizações para identificar multicolinearidade prévia
matrizCorr <- cor(dados_reg)
if(!require(corrplot)) install.packages("corrplot"); library(corrplot)
corrplot(matrizCorr, method = "circle")

if(!require(psych)) install.packages("psych"); library(psych)
pairs.panels(dados_reg) # Gráfico com correlações numéricas e histogramas

# --- 1.3 Ajuste do Modelo Completo e Diagnóstico de Resíduos ---
modeloStep <- lm(Faturamento ~ Marketing + Vendedores + Tamanho, data = dados_reg)
summary(modeloStep) # Avalie R^2, Teste F global, e p-valores individuais

# Análise de diagnóstico padrão via gráficos
plot(modeloStep, which = c(1:3), pch = 20)

# Verificação de Multicolinearidade via VIF (Fator de Inflação da Variância)
if(!require(car)) install.packages("car"); library(car)
vif(modeloStep) # Se VIF > 10, existe multicolinearidade severa impossibilitando o modelo.

# --- 1.4 Seleção Algorítmica de Variáveis (Stepwise) ---
# Executando o critério AIC automático para remoção de variáveis
s <- step(modeloStep)
summary(s) # Novo sumário contendo apenas as variáveis selecionadas

# Inserindo escala com base no erro padrão residual (caso solicitado pelo professor)
# Exemplo se o erro residual do summary anterior foi 4.826:
s2 <- step(modeloStep, scale = 4.826^2)

# --- 1.5 Validação do Modelo Final Selecionado ---
# Teste de Independência dos resíduos (Durbin-Watson) -> H0: Ausência de autocorrelação
durbinWatsonTest(s) # Queremos p-value > 0.05 para validar a independência

# Teste de Normalidade dos resíduos (Shapiro-Wilk) -> H0: Resíduos são normais
shapiro.test(s$residuals) # Queremos p-value > 0.05 para validar a normalidade

# --- 1.6 Predição Manual Baseada na Equação ---
# Fórmula resultante: Faturamento = intercepto + (b1 * Var1) + (b2 * Var2)
# Multiplique os coeficientes obtidos nos dados reais respeitando as unidades métricas.


# ==============================================================================
# TÓPICO 2: ANÁLISE DISCRIMINANTE LINEAR (LDA) COM TRATAMENTO DE DADOS
# ==============================================================================

# --- 2.1 Estrutura de Dados ---
dados_lda <- data.frame(
  Grupo = factor(c(rep("Bom",12), rep("Ruim",12))),
  Matematica = c(80,85,88,90,87,92,89,91,93,86,84,88, 60,62,58,65,63,61,59,57,66,64,55,68),
  Estatistica = c(78,82,85,87,84,89,86,88,90,83,81,85, 58,60,56,63,61,59,57,55,64,62,53,66),
  Programacao = c(82,86,89,91,88,93,90,92,94,87,85,89, 62,64,60,67,65,63,61,59,68,66,57,70)
)

# --- 2.2 Teste Estrito de Suposição: Normalidade Individual ---
shapiro.test(dados_lda$Matematica) # Se p-value < 0.05, quebra o pressuposto.

# Se o professor exigir testar a normalidade POR GRUPO (Abordagem correta):
shapiro.test(dados_lda$Matematica[dados_lda$Grupo == "Bom"])

# --- 2.3 Caixa de Ferramentas: Correção de Não-Normalidade (Aulas Práticas) ---
# Estratégia A: Raiz Quadrada (Para dados estritamente positivos e distribuições assimétricas)
Estatistican <- sqrt(dados_lda$Estatistica)

# Estratégia B: Transformação de Box-Cox (Requer valores positivos)
if(!require(forecast)) install.packages("forecast"); library(forecast)
constante <- abs(min(dados_lda$Matematica)) + 1 # Translação para evitar valores zero/negativos
Matematica_positiva <- dados_lda$Matematica + constante
lambda <- BoxCox.lambda(Matematica_positiva, method = "loglik")
Matematica_boxcox <- BoxCox(Matematica_positiva, lambda)

# Estratégia C: Gaussianização de LambertW (Remove assimetrias severas automaticamente)
if(!require(LambertW)) install.packages("LambertW"); library(LambertW)
Matematica_lambert <- Gaussianize(dados_lda$Matematica, type = "hh")

# --- 2.4 Ajuste do Modelo Discriminante Simultâneo ---
if(!require(MASS)) install.packages("MASS"); library(MASS)
# NOTA: Empregue apenas as variáveis que passaram na normalidade ou suas versões transformadas!
d_lda <- lda(Grupo ~ Matematica + Estatistica, data = dados_lda)
print(d_lda)

# --- 2.5 Cálculo Teórico do Escore de Corte Ótimo (Z) ---
n0 <- length(dados_lda$Grupo[dados_lda$Grupo == "Bom"]) 
n1 <- length(dados_lda$Grupo[dados_lda$Grupo == "Ruim"])

# Multiplicação matricial das médias de cada grupo pelos coeficientes de escalonamento (scaling)
z0 <- d_lda$means[1, ] %*% d_lda$scaling
z1 <- d_lda$means[2, ] %*% d_lda$scaling
z_corte_otimo <- (n0 * z1 + n1 * z0) / (n0 + n1)
print(z_corte_otimo) 

# --- 2.6 Classificação, Matriz de Confusão e Avaliação do Modelo ---
predicoes_lda <- predict(object = d_lda, newdata = dados_lda)

if(!require(caret)) install.packages("caret"); library(caret)
observado <- as.factor(dados_lda$Grupo)
matriz_c <- confusionMatrix(predicoes_lda$class, observado)
print(matriz_c) # Extraia daqui as métricas fundamentais: "Accuracy" e "Sensitivity"

# --- 2.7 Seleção via Stepwise Multivariado (Greedy Wilks) ---
if(!require(klaR)) install.packages("klaR"); library(klaR)
# Seleciona automaticamente variáveis que maximizam a separação dos grupos
greedy.wilks(Grupo ~ Matematica + Estatistica + Programacao, data = dados_lda, niveau = 0.05)

# --- 2.8 Predição para Novos Casos Desconhecidos ---
# Cenário de teste: classificar caso usando a função nativa predict()
novos_valores <- data.frame(Matematica = 70, Estatistica = 68, Programacao = 72)
predict(d_lda, newdata = novos_valores)


# ==============================================================================
# TÓPICO 3: ANÁLISE DE VARIÂNCIA MULTIVARIADA (MANOVA)
# ==============================================================================

# --- 3.1 Carga e Preparação de Dados ---
dados_manova <- data.frame(
  Ensino = factor(c(rep("Presencial", 10), rep("Hibrido", 10), rep("EAD", 10))),
  Nota_Matematica = c(82,85,88,79,90,84,87,91,86,83, 75,78,80,77,79,81,76,74,82,78, 68,70,72,66,71,69,73,67,65,74),
  Nota_Portugues = c(80,83,85,78,88,82,84,89,85,81, 72,75,77,74,76,78,73,71,79,75, 64,66,68,63,67,65,69,62,61,70)
)

# --- 3.2 Verificação de Normalidade Univariada ---
shapiro.test(dados_manova$Nota_Matematica)
shapiro.test(dados_manova$Nota_Portugues)

# --- 3.3 Construção e Testes da MANOVA ---
# H0: Os vetores de médias dos grupos são estritamente iguais.
modelo_manova <- manova(cbind(Nota_Matematica, Nota_Portugues) ~ Ensino, data = dados_manova)

# Critérios de saída comuns em avaliações:
summary(modelo_manova, test = "Wilks")  # Teste mais cobrado (Lambda de Wilks)
summary(modelo_manova, test = "Pillai") # Mais robusto contra desvios de premissas
summary(modelo_manova, test = "Roy")    # Critério da maior raiz raiz raiz

# Se p-value < 0.05 -> Rejeita H0. As médias multivariadas diferem entre as modalidades.

# --- 3.4 Desdobramento Univariado (Apenas se a MANOVA for Significativa!) ---
# Identifica quais variáveis contínuas sofreram impactos de forma isolada
summary.aov(modelo_manova)

# --- 3.5 Identificação de Pares e Direção de Efeitos ---
# Descobrir quais grupos específicos se diferenciam entre si (Exemplo: Presencial vs EAD)
aov_mat <- aov(Nota_Matematica ~ Ensino, data = dados_manova)
TukeyHSD(aov_mat) # Analise o campo 'p adj' buscando valores menores que 0.05

# Cálculo de estatísticas descritivas (Médias) para responder "qual grupo obteve melhor performance"
aggregate(cbind(Nota_Matematica, Nota_Portugues) ~ Ensino, data = dados_manova, FUN = mean)


# Dica --------------------------------------------------------------------

# ==============================================================================
# ESTRATÉGIA DE PROVA: ROTEIRO DE TOMADA DE DECISÃO
# ==============================================================================
# 
# 1. Na Regressão: Quando usar Stepwise ou VIF?
#    - O Problema: Variáveis independentes altamente correlacionadas inflam a 
#      variância dos coeficientes (o R não consegue isolar o efeito de cada uma).
#    - O Diagnóstico: O comando vif(modelo) acusa valores maiores que 10.
#    - A Solução na Prova: Execute o comando step(modelo). Ele usará o algoritmo 
#      Stepwise backward/forward com base no critério estatístico AIC para 
#      remover automaticamente as variáveis redundantes até encontrar o modelo ótimo.
# 
# 2. Na Análise Discriminante (LDA): O que fazer com a Não-Normalidade?
#    Se o teste de Shapiro-Wilk apontar que uma variável preditora não é normal 
#    (p < 0.05), você possui três caminhos aceitos em avaliações práticas:
#    - Remoção Direta: Excluir a variável e rodar a função lda() apenas com as 
#      preditoras normais (Abordagem simultânea restrita).
#    - Transformação Matemática: Aplicar sqrt(X) para dados assimétricos à direita 
#      ou utilizar a função estruturada Gaussianize() do pacote LambertW antes 
#      de estimar o modelo.
#    - Abordagem Stepwise: Utilizar o critério greedy.wilks(). O teste Lambda de 
#      Wilks selecionará as variáveis mais robustas para a separação dos grupos, 
#      minimizando os impactos iniciais de distribuições não-ideais.
# 
# 3. Na MANOVA: Interpretando Wilks vs Pillai
#    - Wilks' Lambda: É a métrica padrão. Ele mede a proporção da variância total 
#      que não é explicada pelo efeito do grupo. Portanto, quanto mais próximo de 
#      0 for o valor de Wilks, melhor e mais forte é a separação entre os grupos 
#      na sua prova.
#    - Pillai's Trace: Se a questão de prova demonstrar que as amostras possuem 
#      tamanhos muito desequilibrados ou que os testes de normalidade falharam 
#      levemente, cite textualmente: "Utilizou-se o critério de Pillai por ser 
#      estatisticamente mais robusto a desvios de premissas multivariadas".
# ==============================================================================


