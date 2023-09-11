####
# Poisson:
pois <- rpois(1000, lambda = 2)
hist(pois, breaks = 10)

# Negative Binomial Type 2:
nbinom <- rnbinom(1000, size = 0.5, mu = 9) # size = delta; mu = theta
hist(nbinom, breaks = 10)

# Verificação da significância estatística do parâmetro de forma (theta) da
#distribuição binomial negativa
modelo_bneg$theta / modelo_bneg$SE.theta
# Se este cálculo for maior que 1.96, verificamos a significância estatística
#de theta ao nível de confiança de 95%
# Ou seja, se isso ocorrer, ficamos com modelo BNEG!

#Cálculo manual do fit do modelo binomial negativo:
#Predict modelo_bneg antes da mudança da lei:
exp(1.9469 + 0.0400 * 23 - 4.2746 * 0 + 0.4527 * 0.5)

#Predict modelo_bneg depois da mudança da lei:
exp(1.9469 + 0.0400 * 23 - 4.2746 * 1 + 0.4527 * 0.5)

#Cálculo manual do fit do modelo ZIP:
(1 - (1/(1 + exp(-(-1.6117 -0.9524*0.5)))))*
  (exp(2.488857 + 0.020020*23 -4.287651*0 + 0.093714*0.5))


####
# Modelo OLS:
modelo_lm <- lm(formula = violations ~ staff + post + corruption,
                data = corruption)
summary(modelo_lm)
library(nortest)
sf.test(modelo_lm$residuals)
logLik(modelo_lm)
lrtest(modelo_zinb, modelo_lm)

# Box-Cox
library(car)
lambda_BC <- powerTransform(corruption$violations)
summary(corruption$violations)

corruption$violations1 <- corruption$violations + 0.001
summary(corruption$violations1)
lambda_BC <- powerTransform(corruption$violations1)
lambda_BC

#Inserir o lambda de Box-Cox na base de dados
corruption$bc_violations <- (((corruption$violations1 ^
                                 lambda_BC$lambda) - 1) / lambda_BC$lambda)

# Modelo Box-Cox
modelo_bc <- lm(formula = bc_violations ~ staff + post + corruption,
                data = corruption)
summary(modelo_bc)
sf.test(modelo_bc$residuals)
logLik(modelo_bc)
lrtest(modelo_zinb, modelo_bc)

# Gráfico de LogLiks
my_plot3 <-
  data.frame(Poisson = logLik(modelo_poisson),
             ZIP = logLik(modelo_zip),
             Bneg = logLik(modelo_bneg),
             ZINB = logLik(modelo_zinb),
             OLS = logLik(modelo_lm),
             OLS_BC = logLik(modelo_bc)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = format(value, digts = 3)),
            color = "black",
            size = 3.5,
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "#453781FF",
                                           "orange", "#FDE725FF",
                                           "grey20", "grey40")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot3



