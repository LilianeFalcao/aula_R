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



