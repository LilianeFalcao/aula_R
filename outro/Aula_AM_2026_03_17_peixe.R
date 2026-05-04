df_peixes <- peixe
View(df_peixes)

shapiro.test(df_peixes$desempenho)

hist(df_peixes$desempenho)


# Anova -------------------------------------------------------------------
#h0: As médias são iguais?
#A importância é o valor do Pr(>F) se for baixo as médias são diferentes

Anova_peixe <- aov(desempenho ~ tecnica, data = df_peixes)
summary(Anova_peixe)

#Como as médias são iguais, onde há diferença?
# Usamos o Teste Tukey - quando elas são diferentes


if (!require("multcompView")) install.packages("multcompView")
library(multcompView)

TukeyHSD(Anova_peixe)


# Coração -----------------------------------------------------------------

data_coracao <- coracao
View(data_coracao)

anova_pressao <- aov(Pressao ~ Grupo, data = data_coracao)
summary(anova_pressao)

TukeyHSD(anova_pressao)






