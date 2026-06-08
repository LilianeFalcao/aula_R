# Guia de Revisão: Análise Multivariada e Tratamento de Dados

Este guia compila os principais conceitos, fórmulas e passos práticos para a aplicação de **Correlação**, **Regressão Linear**, **Análise Fatorial** e **Tratamento de Dados**, com base nos scripts e materiais de aula.

---

## 1. Tratamento de Dados e Normalidade
Antes de qualquer análise multivariada, é necessário garantir que os dados atendem aos pressupostos estatísticos.

### Teste de Normalidade (Shapiro-Wilk)
- **H0 (Hipótese Nula):** Os dados seguem uma distribuição normal.
- **Interpretação:** - $p > 0,05$: Os dados são normais (Aceita H0).
  - $p < 0,05$: Os dados NÃO são normais (Rejeita H0).

### Técnicas de Transformação
Se os dados não forem normais, utilize as seguintes transformações:
1.  **Raiz Quadrada (`sqrt(y)`):** Indicada para assimetria leve.
2.  **Logaritmo (`log(y)`):** Indicada para assimetria forte.
3.  **Translação:** Caso existam valores negativos ou zero, some uma constante antes de transformar:
    `constante <- abs(min(dados)) + 1`
4.  **Box-Cox:** Caso as transformações simples falhem, utiliza-se o parâmetro $\lambda$ ideal.

---

## 2. Correlação e Adequação da Amostra
A correlação mede a força da relação entre variáveis.

### Verificação Prévia (Análise Fatorial)
* **Teste de Bartlett:** Verifica se a matriz de correlação é uma matriz identidade. 
    - *Requisito:* $p < 0,05$.
* **Teste KMO (MSA):** Mede a adequação da amostra.
    - *Excelente:* $> 0,80$.
    - *Inadequado:* $< 0,50$.

---

## 3. Regressão Linear Simples
Utilizada para prever uma variável dependente ($Y$) a partir de uma independente ($X$).

**Equação:** $\hat{Y} =  lpha +  eta X$

### Avaliação do Modelo
1.  **Coeficiente de Determinação ($R^2$):** Indica a proporção da variância de $Y$ explicada por $X$. (Quanto mais próximo de 1, melhor).
2.  **Teste T (Coeficientes):** Verifica se $ lpha$ e $ eta$ são significativamente diferentes de zero ($p < 0,05$).
3.  **Análise de Resíduos:**
    - **Média dos Resíduos:** Deve ser próxima de zero.
    - **Independência (Durbin-Watson):** $p > 0,05$ indica que os resíduos não estão correlacionados.
    - **Homocedasticidade:** A variância dos erros deve ser constante (sem padrões de "funil" nos gráficos).

---

## 4. Análise Fatorial (AF)
Técnica de interdependência que agrupa variáveis originais em **fatores** latentes.

### Passos Principais
1.  **Extração (PCA):** Geralmente baseada em Componentes Principais.
2.  **Critério de Kaiser:** Mantém-se apenas fatores com **Autovalores (Eigenvalues) > 1**.
3.  **Rotação Varimax:** Ajusta as cargas fatoriais para facilitar a interpretação (cada variável "pertence" claramente a um fator).
4.  **Cargas Fatoriais:** Representam a correlação entre a variável e o fator. No script, utiliza-se frequentemente um corte de **0,50** ou **0,75** para definir a importância da variável no fator.

---

## Resumo de Comandos R Essenciais

| Função | Objetivo |
| :--- | :--- |
| `scale(dados)` | Normaliza os dados (Média 0, DP 1). |
| `shapiro.test(x)` | Testa a normalidade da variável. |
| `cor(dados)` | Gera a matriz de correlação. |
| `lm(Y ~ X)` | Cria o modelo de Regressão Linear. |
| `principal(..., rotate="varimax")` | Executa a Análise Fatorial Rotacionada. |
| `durbinWatsonTest(modelo)` | Testa a independência dos resíduos. |



