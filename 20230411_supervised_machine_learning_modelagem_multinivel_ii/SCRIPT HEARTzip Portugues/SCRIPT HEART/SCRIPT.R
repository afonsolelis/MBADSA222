# GRÁFICO DO AMOR
# Summit Consulting LLC

#------------------------------------------------------------------------------#

# requer ImageMagick (http://www.imagemagick.org/)
# Pacotes utilizados
#install.packages("animation", repos = "http://rforge.net", type = "source")
pacotes <- c("dplyr","ggplot2","pryr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

require(animation)
require(dplyr)
require(ggplot2)
require(pryr)

#------------------------------------------------------------------------------#

#Equação da curva do coração
coracao <- quote((x^2 + y^2 - 1)^3 - x^2 * y^3)

#Equação da curva do coração para dado x
coracao_at_x <- function(x) {
  function(y) eval(substitute_q(coracao, list(x = x)), list(y = y))
}

#Desenhando a curva do coração para cada x e encontrando as raízes da
#expressão resultante em y
coracao_x <- seq(-1.136, 1.136, 0.001)
coracao_y_lower <- sapply(coracao_x, function(x) uniroot(coracao_at_x(x),
                                                         c(-2, 0.6))$root)
coracao_y_upper <- sapply(coracao_x, function(x) uniroot(coracao_at_x(x),
                                                         c(0.6, 2))$root)

#Fazendo a junção do dataframe
coracao_df <- data.frame(x = rep(coracao_x, 2), 
                       y = c(coracao_y_lower, coracao_y_upper))

#Esboço
with(coracao_df, plot(x, y))


#Gerando um dataframe com uma linha por x, para que possamos preencher o coração
coracao_df_minmax <- data.frame(x = coracao_x,  
                              y_min = coracao_y_lower, 
                              y_max = coracao_y_upper)

set.seed(20150214)

#Preenchendo o coração com desvios aleatórios em cada x e rejeitando
#aqueles que estão fora da curva do coração
coracao_full <- apply(coracao_df_minmax, 
                    1, 
                    function(w) {
                      x <- w["x"]
                      y_min = w["y_min"]
                      y_max = w["y_max"]
                      y <- rnorm(2, mean = 0.33)
                      y <- y[between(y, y_min, y_max)]
                      x <- x[any(is.finite(y))]
                      data.frame(x, y, row.names = NULL)
                    })

#ALternando de lista para dataframe
coracao_full <- bind_rows(coracao_full)

#Adicionando números aleatórios para cor e tamanho
coracao_full <- coracao_full %>%
  mutate(z1 = runif(n()),
         z2 = pmin(abs(rnorm(n())), 3), 
         order = runif(n())) %>%
  arrange(order)

#Plotando o coração
p <- ggplot(coracao_full,
            aes(x, y, color = z1, size = z2)) + 
  geom_point(pch = -1 * as.hexmode(9829)) + 
  scale_color_gradient(limits = c(0, 1), low = "#FF2B95", high = "#810181") + 
  scale_size(limits = c(0, 5), range = c(0.1, 19)) + 
  theme_classic()
p


#Gerando a figura amor.png
png("amor.png", 700, 500)
p
dev.off()

#Gerando a figura amor.gif (com animação)
saveGIF({
  fill_steps <- 50  # frames de preenchimento do coração
  float_steps <- 25  # frames flutuantes do coração
  
  for (i in seq(fill_steps + float_steps)) {
    
    #Encontrando o número de corações para preencher nesta etapa
    num_coracao <- min(i, fill_steps) * nrow(coracao_full) / fill_steps
    
    #Fazendo o coração flutuar (deslocando cada ponto para cima), dado que
    #o coração está preenchido
    if (i > fill_steps) {
      j <- i - fill_steps
      j_scale <- uniroot(function(x) (x * float_steps)^2 - 2.5, c(0, 1))$root
      y_change <- (j_scale * j)^2
      coracao_full <- mutate(coracao_full, y = y + y_change)
    }
    
    #Plotando o coração
    p <- ggplot(coracao_full[seq(num_coracao), ], 
                aes(x, y, color = z1, size = z2)) + 
      geom_point(pch = -1 * as.hexmode(9829)) + 
      scale_color_gradient(limits = c(0, 1), low = "#FF2B95", high = "#810181") + 
      scale_size(limits = c(0, 3), range = c(0.1, 20)) + 
      theme_bw() + 
      coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(-1.25, 1.5))
    print(p)
  }
}, 
movie.name = "amor.gif", 
interval = 0.1, 
nmax = 30, 
ani.width = 600, 
ani.height = 400)

