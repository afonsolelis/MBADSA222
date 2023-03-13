## Exemplo 1:
#analysis of variance:
aov(modelo_tempodist) 
anova(modelo_tempodist)

#cálculo da estatística F:
(1638.8514/1)/(361.1486/8)

#Distribuição F
f <- rf(100000, df1 = 10, df2 = 70)
hist(f, breaks = 100)
abline(v = qf(0.05, df1 = 10, df2 = 70, lower.tail = F), col="red",lwd=4)

summary(modelo_tempodist)

#Cálculo do p-value do F do nosso modelo:
pf(36.30309, df1 = 1, df2 = 8, lower.tail = F)

#Cálculo do F crítico do nosso modelo:
qf(0.05, df1 = 1, df2 = 8, lower.tail = F)

#Cálculo do p-value crítico do nosso modelo:
pf(5.317655, df1 = 1, df2 = 8, lower.tail = F)

#Teste t de Student:
t <- rt(100000, df=8)
hist(t, breaks = 100)
abline(v = qt(0.025, df=8, lower.tail = F), col="red", lwd=4)
abline(v = qt(0.025, df=8, lower.tail = T), col="red", lwd=4)

#Cálculo do p-value do t do nosso modelo:
pt(6.025, df=8, lower.tail = F)*2

#######
## Exemplo 3:
aggregate(corrupcao$cpi ~ corrupcao$regiao, FUN = mean)

#Mudança da categoria de referência:
corrupcao_dummies2 <- dummy_columns(.data = corrupcao,
                                   select_columns = "regiao",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = F)

#Visualizando a base de dados dummizada
corrupcao_dummies2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#Referência para 'América do Sul'
corrupcao_dummies2 <- corrupcao_dummies2[,-3]

modelo_corrupcao_dummies2 <- lm(cpi ~ . - pais, corrupcao_dummies2)

#Parâmetros do modelo_corrupcao_dummies2
summary(modelo_corrupcao_dummies2)
