treinamento <- c(2,3,4,5,6,7,8,9,10,11)
vendas <- c(18,20,23,25,27,30,32,35,36,40)

#A
dados <- data.frame(treinamento, vendas)

# Regressao Linear Simples ------------------------------------------------
# variável resposta : Vendas   Explicativa: Horas_Treinamento

#1 - Calculo do preditor de referencia (media)
mediaTrei<- mean(dados$treinamento)
mediaTrei #6.5

mediaVend<- mean(dados$vendas)
mediaVend #28.6

matrizCor <- cor(dados)
View(matrizCor)

#3 - Estimacao da equacao de regressao simples
#B
modelo <- lm(dados, formula = dados$vendas ~ dados$treinamento) 
modelo$coefficients
print(modelo$coefficients, digits = 3)

#Equação da reta estimada 
#C
# Vendas = 13.08 + 2.39  * treinamento

#D
#Para cada 1 hora adicional de treinamento, 
#espera-se um aumento médio de 2,4 unidades nas vendas.

#E
summary(modelo)

# R² = 0.9958 e R²_ajustado = 0.9953 
# Ambos tem valores próximos de 1

# F
# x = treinamento, y = vendas
plot(dados$treinamento, dados$vendas, 
     main = "Relação: Treinamento vs Vendas",
     xlab = "Horas de Treinamento", 
     ylab = "Vendas", 
     pch = 19,      # Tipo de ponto (círculo preenchido)
     col = "black") # Cor dos pontos

# 2. Adiciona a reta de regressão ajustada
# O comando abline desenha a reta baseada nos coeficientes do modelo
abline(modelo, col = "blue", lwd = 2)

#G 
#modelo de regressão é altamente confiável R^2 = 99,6% e
#estatisticamente significante p < 0,001, 
#demonstrando que cada hora adicional de treinamento gera um impacto real e
#previsível de aproximadamente 2,39 unidades nas vendas."


# Regressão Multipla ------------------------------------------------------
# Instalando pacotes necessários ------------------------------------------
if(!require(psych)) install.packages("psych") 
library(psych) 
if(!require(car)) install.packages("car") 
library(car)

# a) Digite o banco de dados no R
treinamento <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
experiencia <- c(1, 2, 2, 3, 4, 5, 5, 6, 7, 8)
faltas <- c(5, 4, 4, 3, 3, 2, 2, 1, 1, 0)
produtividade <- c(52, 55, 58, 62, 66, 70, 73, 77, 81, 85)

dados <- data.frame(treinamento, experiencia, faltas, produtividade)


#Produtividade como resposta 
#Treinamento, Experiencia e Faltas como variáveis explicativas. 

matcor <- cor(dados)
View(matcor)

#B
#H0: Os dados sao normais o p-valor > 0,05
shapiro.test(dados$treinamento)    #Válido
shapiro.test(dados$experiencia) #Válido
shapiro.test(dados$faltas) #Válido
shapiro.test(dados$produtividade) # Válido

# b) Ajuste o modelo de regressão linear múltipla
modelo <- lm(produtividade ~ treinamento + experiencia + faltas, data = dados)

# Exibe os coeficientes, testes de significância e ajuste geral (Letras c, d, e)
summary(modelo)

#c) A equação de regressão estimada é:
#Produtividade = 2.93 * Treinamento + 1.33 * Experiencia + 0.46 * Faltas.

#d)
# Verifique quais variáveis são significativas ao nível de 5%
#Avaliamos os $p$-valores individuais associados a cada coeficiente (teste t):
#reinamento: $p$-valor < 0.001 (Significativa).Experiência: $p$-valor = 0.013 
#(Significativa).Faltas: $p$-valor = 0.318 (Não significativa ao nível de 5%).

#e)
# R² = 0.9995  e R²_ajustado = 0.9993
#Como no Teste-F, deu um valor  muito inferior a 0.05, concluímos que 
#o modelo como um todo é altamente significativo

# f) Verifique a presença de multicolinearidade (VIF)
vif(modelo)

# g) Avalie os resíduos
# Análise gráfica
par(mfrow = c(2, 2))
plot(modelo)

# Testes formais de resíduos
shapiro.test(modelo$residuals) # Teste de Normalidade
bptest(modelo)                 # Teste de Homocedasticidade
dwtest(modelo)                 # Teste de Independência

# h) Previsão da produtividade
novo_funcionario <- data.frame(treinamento = 12, experiencia = 6, faltas = 1)
previsao <- predict(modelo, newdata = novo_funcionario)
print(previsao)


# Questão 3 ---------------------------------------------------------------

# a) Entrada de Dados
# Criamos vetores para as variáveis independentes (X) e a dependente (Y)
funcionarios <- c(12, 15, 18, 20, 22, 25, 27, 30, 32, 35)
publicidade  <- c(8, 10, 12, 13, 15, 17, 18, 20, 22, 25)
area         <- c(350, 420, 500, 550, 620, 700, 760, 820, 900, 980)
faturamento  <- c(210, 240, 275, 290, 320, 355, 370, 405, 430, 470)

# Estruturação em DataFrame: essencial para manipulação estatística no R
dados <- data.frame(funcionarios, publicidade, area, faturamento)

# b) Ajuste do Modelo
# O faturamento é função (~) da combinação linear das variáveis explicativas.
# O método utilizado internamente é o de Mínimos Quadrados Ordinários (MQO).
modelo <- lm(faturamento ~ funcionarios + publicidade + area, data = dados)

# c, d, e, f) Diagnóstico Geral do Modelo
# O comando summary() fornece a base para a análise teórica:
resumo <- summary(modelo)
print(resumo)

# --- ANÁLISE TEÓRICA DOS RESULTADOS ---

# c) EQUAÇÃO ESTIMADA:
# Y = Intercepto + (beta1 * X1) + (beta2 * X2) + (beta3 * X3)
# Os coeficientes ficam na coluna 'Estimate' do summary.

# d) INTERPRETAÇÃO DOS COEFICIENTES (Ceteris Paribus):
# Cada coeficiente representa a variação média em Y para o aumento de uma unidade em X,
# mantendo-se todas as outras variáveis constantes no modelo.

# e) SIGNIFICÂNCIA ESTATÍSTICA (Teste t individual):
# Olhamos a coluna Pr(>|t|). Se p-valor < 0.05, a variável contribui significativamente 
# para o modelo. Caso contrário, não podemos afirmar que o coeficiente é diferente de zero.
# Nota teórica: Neste dataset, a alta correlação entre 'area' e 'funcionarios' 
# gera multicolinearidade, o que costuma inflar os p-valores individuais.

# f) QUALIDADE DO AJUSTE:
# R-squared (R²): Proporção da variância de Y explicada por X. (Quanto mais perto de 1, melhor).
# Adj. R-squared: Penaliza a inclusão de variáveis irrelevantes. Útil em modelos múltiplos.
# Teste F (p-value global): Verifica se o modelo como um todo é melhor do que 
# simplesmente usar a média do faturamento para prever os valores.

# g) ANÁLISE DE RESÍDUOS (Pressupostos do Modelo Linear):
# Para que o modelo seja válido, os resíduos devem:
# 1. Ter média zero e variância constante (Homocedasticidade).
# 2. Seguir uma distribuição normal.
# 3. Ser independentes (não apresentar padrões temporais ou espaciais).
par(mfrow = c(2, 2)) # Divide a tela para 4 gráficos de diagnóstico
plot(modelo)

# h) PREVISÃO
# Aplicamos os coeficientes calculados aos novos valores de entrada.
nova_loja <- data.frame(funcionarios = 28, publicidade = 19, area = 800)
previsao  <- predict(modelo, newdata = nova_loja)

cat("\n--- RESULTADO DA PREVISÃO ---\n")
cat("Faturamento Estimado:", previsao, "\n")

