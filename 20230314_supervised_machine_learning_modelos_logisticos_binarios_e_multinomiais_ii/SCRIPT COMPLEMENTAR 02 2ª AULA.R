# Exemplo 3:

extract_eq(step_fidelidade_dummies, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

# Likelihood ratio test (lrtest):
logLik(modelo_fidelidade_dummies)
logLik(step_fidelidade_dummies)

chi2 <- 2*(-773.5675 -(-773.6044))
chi2

# Supondo uma única preditora: variável 'idade'
modelo_preliminar <- glm(formula = fidelidade ~ idade,
                         data = dados_fidelidade,
                         family = "binomial")
summary(modelo_preliminar)

ROC_preliminar <- roc(response = dados_fidelidade$fidelidade,
                      predictor = modelo_preliminar$fitted.values)
ROC_preliminar

logLik(modelo_preliminar)
logLik(step_fidelidade_dummies)

lrtest(modelo_preliminar, step_fidelidade_dummies)

plot(ROC_preliminar, col = "orange", lty = 2, main = "Comparação entre ROCs")
plot(ROC, col = "darkorchid", add = T)

# Teste de DeLong - Elisabeth DeLong - Biometrics (1988)
roc.test(ROC_preliminar,ROC)




