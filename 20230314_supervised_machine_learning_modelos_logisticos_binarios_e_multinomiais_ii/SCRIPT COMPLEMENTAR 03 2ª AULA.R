# Exemplo 4:

# Modelo OLS ERRADO!
modelo_errado <- lm(formula = atrasado ~ dist + sem,
                    data = AtrasadoMultinomial)
summary(modelo_errado)

AtrasadoMultinomial$atrasado <- as.numeric(AtrasadoMultinomial$atrasado)

# Vamos recarregar o dataset original!

glimpse(AtrasadoMultinomial)

# Primeira observação:
predict(modelo_atrasado, 
        data.frame(dist = 20.5, sem = 15), 
        type = "probs")

predict(modelo_atrasado, 
        data.frame(dist = 20.5, sem = 15), 
        type = "class")
