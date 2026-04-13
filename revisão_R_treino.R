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
