# Complemento Exemplo 04:

bebes$zcomprimento <- scale(bebes$comprimento)
summary(bebes$zcomprimento)
sd(bebes$zcomprimento)

sf.test(bebes$comprimento)
sf.test(bebes$zcomprimento)

########
sf.test2 <- function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 500000)) 
    stop("sample size must be between 5 and 500000")
  y <- qnorm(ppoints(n, a = 3/8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- pnorm(z, lower.tail = FALSE)
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia MBA DSA USP", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

sf.test2(modelo_linear$residuals)

#####
#Fazendo predições com os modelos OLS linear e Box-Cox
#qual é o comprimento esperado de um bebê com 19 semanas de vida?
#Modelo OLS Linear:
predict(object = modelo_linear,
        data.frame(idade = 19),
        interval = "confidence", level = 0.95)

#Modelo Não Linear (Box-Cox):
predict(object = modelo_bc,
        data.frame(idade = 19),
        interval = "confidence", level = 0.95)
#Não podemos nos esquecer de fazer o cálculo para a obtenção do fitted
#value de Y (variável 'comprimento')
(((22992.53 * 2.659051) + 1)) ^ (1 / 2.659051)

################################################
# Complemento Exemplo 05:

modelo_auxiliar1 <- lm(formula = retorno ~ endividamento,
                      data = empresas)
summary(modelo_auxiliar1)

modelo_auxiliar2 <- lm(formula = retorno ~ . -empresa -endividamento,
                       data = empresas)
summary(modelo_auxiliar2)

modelo_auxiliar3 <- lm(formula = retorno ~ . -empresa -endividamento
                       -disclosure,
                       data = empresas)
summary(modelo_auxiliar3)

modelo_auxiliar4 <- lm(formula = retorno ~ disclosure,
                       data = empresas)
summary(modelo_auxiliar4)

#####
empresas$zativos <- scale(empresas$ativos)
empresas$zliquidez <- scale(empresas$liquidez)

modelo_auxiliar5 <- lm(formula = retorno ~ zativos + zliquidez,
                       data = empresas)
summary(modelo_auxiliar5)
confint(modelo_auxiliar5, level = 0.95)

empresas$zativos <- NULL
empresas$zliquidez <- NULL
