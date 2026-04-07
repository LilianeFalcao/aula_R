set.seed(42) # Para resultados consistentes
df_treino_dados <- data.frame(
  Metodologia = c(rep("Scrum", 6), rep("Kanban", 6), rep("Waterfall", 6)),
  LeadTime = c(10, 12, NA, 11, 9, 13, 7, 8, 7, NA, 9, 8, 20, 22, 25, NA, 18, 21),
  Bugs = c(2, 3, 2, 4, NA, 3, 1, 1, NA, 2, 1, 2, 5, 6, 4, 7, NA, 5),
  Experiencia_Anos = c(2, 3, 3, 4, 2, 3, 5, 4, 6, 5, 4, 5, 10, 8, 9, 7, 8, 9)
)

# Questão 1: Tratamento de Dados (Data Cleaning) --------------------------

any(is.na(df_treino_dados))
NAS <- round(colSums(is.na(df_treino_dados))*100/nrow(df_treino_dados), 2)  
NAS
colSums(is.na(df_treino_dados))
NAS[NAS > 10]

View(df_treino_dados)

--------------------------------------------------------------------
  
grupo1 <- df_treino_dados[!is.na(df_treino_dados$LeadTime),] 
grupo2 <- df_treino_dados[is.na(df_treino_dados$LeadTime),]  

t.test(grupo1$LeadTime, grupo2$LeadTime)
t.test(grupo1$Bugs, grupo2$Bugs)
t.test(grupo1$Experiencia_Anos, grupo2$Experiencia_Anos)

df_treino_dados$LeadTime[is.na(df_treino_dados$LeadTime)] <- mean(df_treino_dados$LeadTime, na.rm = TRUE)  

View(df_treino_dados)

grupo1 <- df_treino_dados[!is.na(df_treino_dados$Bugs),]
grupo2 <- df_treino_dados[is.na(df_treino_dados$Bugs),]

t.test(grupo1$LeadTime, grupo2$LeadTime)
t.test(grupo1$Bugs, grupo2$Bugs)
t.test(grupo1$Experiencia_Anos, grupo2$Experiencia_Anos)

df_treino_dados$Bugs[is.na(df_treino_dados$Bugs)] <- mean(df_treino_dados$Bugs, na.rm = TRUE)  

# Questão 2: Exploração e Correlação --------------------------------------
#a) Calcule o desvio padrão da variável LeadTime.

var_LeadTime <- var(df_treino_dados$LeadTime)
var_LeadTime

#b) Calcule a correlação entre LeadTime e Experiencia_Anos.

correlacao_LeadTime_ExpAnos <- cor(df_treino_dados[, c("LeadTime","Experiencia_Anos")])
correlacao_LeadTime_ExpAnos

#c) Interprete: se a correlação for negativa, o que isso significa no contexto de experiência
#do desenvolvedor e tempo de entrega?



# Questão 3: Visualização (EDA) -------------------------------------------


# Guia Prático: Interpretação de Análises Estatísticas no R

Este guia mapeia o fluxo clássico de uma análise de dados paramétrica, detalhando o que analisar em cada etapa e os cuidados necessários com as "pegadinhas" estatísticas.

---

## 1. Tratamento de Dados (Valores Ausentes / NAs)

**Objetivo do Script:** Calcular a proporção de NAs, testar se a ausência tem relação com outras variáveis (Teste T) e substituir os NAs pela média.

* **Como interpretar:**
    * Se o Teste T entre os grupos (com NA vs. sem NA) **não** apontar diferença significativa (`p-value > 0.05`), sugere-se que o dado está faltando de forma aleatória (MCAR - *Missing Completely At Random*).
* > **⚠️ Dica de Ouro:** Fique atento à imputação pela média. Embora prática, ela **reduz a variância** e pode inflar artificialmente a força das correlações. Se questionado sobre desvantagens, essa é a resposta central.

---

## 2. Estatística Descritiva e Correlação

**Objetivo do Script:** Calcular médias, variância, covariância e a matriz de correlação (Pearson).

* **Como interpretar a Correlação:**
    * **Sinal (+ ou -):** Indica a direção. 
        * *Positiva:* Variáveis sobem juntas (ex: horas de estudo e nota). 
        * *Negativa:* Uma sobe, a outra desce.
    * **Valor (0 a 1):** Indica a força.
        * **0.0 a 0.3:** Fraca
        * **0.3 a 0.7:** Moderada
        * **0.7 a 1.0:** Forte
* > **⚠️ Dica de Ouro:** Lembre-se do mantra "correlação não implica causalidade". O teste não prova que estudar mais *causou* a nota maior, apenas que os dois fenômenos andam juntos.

---

## 3. Visualização (Boxplot)



**Objetivo do Script:** Gerar um boxplot da Nota separada por Método para análise visual da distribuição.

* **Como interpretar visualmente:**
    * **Linha central da caixa (Mediana):** Se a caixa de um método estiver bem acima dos outros, já é um indicativo visual de superioridade.
    * **Tamanho da caixa (Amplitude Interquartil):** Indica a dispersão dos dados "centrais". Caixas muito longas indicam alta variabilidade.
    * **Pontos avulsos (Outliers):** Pontos fora das hastes indicam observações atípicas (ex: alunos com notas muito acima ou abaixo do padrão daquele método).

---

## 4. Pressupostos: Teste de Normalidade (Shapiro-Wilk)



[Image of standard normal distribution curve]


**Objetivo do Script:** Validar se a distribuição dos dados segue uma curva normal, requisito fundamental para a ANOVA.

* **Como interpretar:**
    * **Hipótese Nula (H0):** "Os dados são normais".
    * **`p-value > 0.05`:** Não rejeitamos H0. Conclusão: Os dados **são normais** (Cenário ideal para seguir com testes paramétricos).
    * **`p-value < 0.05`:** Os dados **não são normais**.
* > **⚠️ Dica de Ouro:** Este é um dos poucos testes em que se "torce" para um p-valor alto. Se falhar na normalidade, o correto seria adotar um teste não-paramétrico (como Kruskal-Wallis).

---

## 5. Inferência: ANOVA e Teste de Tukey

**Objetivo do Script:** Comparar se as médias de múltiplos grupos (Métodos A, B e C) são iguais (ANOVA) e, se não forem, identificar onde estão as diferenças (Tukey).

### 5.1. ANOVA
* **Hipótese Nula (H0):** Média de A = Média de B = Média de C.
* **`p-value < 0.05`:** Rejeita-se H0. Conclusão: **Pelo menos uma** das médias é diferente. (A ANOVA não diz qual é).

### 5.2. Teste de Tukey (O "Fofoqueiro")
* Realiza comparações par a par (A-B, A-C, B-C).
* Olhe para a coluna **`p adj`** (p-valor ajustado).
* Se **`p adj < 0.05`** em um par (ex: B-A), existe diferença estatisticamente significativa entre eles. Verifique a coluna `diff` ou as médias descritivas para saber quem foi superior.

---

## 📌 Resumo Rápido de P-Value

| Tipo de Teste | Significado de `p < 0.05` | Significado de `p > 0.05` |
| :--- | :--- | :--- |
| **Associação/Diferença (T-Test, ANOVA, Tukey)** | Existe diferença/efeito real. ("Encontrei algo!") | Não há evidências de diferença. ("São estatisticamente iguais") |
| **Pressupostos (Shapiro-Wilk)** | Fuga da normalidade. ("Temos um problema") | Distribuição normal. ("Tudo certo para a ANOVA") |




