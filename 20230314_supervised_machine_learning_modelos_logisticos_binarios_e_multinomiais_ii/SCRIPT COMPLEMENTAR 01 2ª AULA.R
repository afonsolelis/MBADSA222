# Exemplo 2:

celsius <- function(far){
  celsius = 5 * ((far - 32) / 9)
  print(celsius)
}

celsius(53)
celsius(81)
celsius(77)
celsius(70)
celsius(34)

# Curva ROC
ROC <- roc(response = challenger$falha,
           predictor = step_challenger$fitted.values)
ROC$auc

ggplotly(
  ggroc(ROC, color = "blue", size = 2) +
  theme_bw()
  )


