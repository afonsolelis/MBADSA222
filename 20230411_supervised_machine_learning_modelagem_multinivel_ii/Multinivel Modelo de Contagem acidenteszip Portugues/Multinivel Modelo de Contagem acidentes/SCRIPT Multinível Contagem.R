##############################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS           #
##############################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","reshape2",
             "lmtest","splines","jtools","questionr","MASS","pscl","overdisp",
             "glmmTMB","lme4")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


##############################################################################
#             EXEMPLO EXTRA - CARREGAMENTO DA BASE DE DADOS                  #
##############################################################################
load("acidentes.RData")

##############################################################################
#                   OBSERVAÇÃO DA BASE DE DADOS acidentes                    #
##############################################################################
#Visualizando a base de dados
acidentes %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Estatísticas descritivas univariadas e tabela de frequências
summary(acidentes)

#Tabela de frequências da variável dependente (função freq para gerar tabelas de
#frequência do pacote questionr)
freq(acidentes$acidentes) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Histograma da variável dependente
acidentes %>%
  ggplot() +
  geom_bar(aes(x = factor(acidentes)), stat="count", fill = "deepskyblue") +
  geom_text(aes(x = factor(acidentes),label = ..count..), 
            stat="count", vjust = -0.8, size = 3.5, color = "black") +
  labs(x = "Acidentes",
       y = "Frequência") +
  theme_classic()


#Diagnóstico preliminar para observação de eventual igualdade entre a média e
#a variância da variável dependente 'acidentes'
acidentes %>%
  summarise(Média = mean(acidentes),
            Variância = var(acidentes)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                font_size = 30)


##############################################################################
#                        ESTIMAÇÃO DO MODELO POISSON                         #
##############################################################################
#Modelo ZI Poisson
poisson.glm <- glm(acidentes ~ alcool,
                   data = acidentes,
                   family = "poisson")

#Observando os parâmetros do modelo
summary(poisson.glm)

#Extração do valor do LL
logLik(poisson.glm)

#LR Test
lrtest(poisson.glm)

#Adicionando os fitted values de lambda de poisson.glm à base de dados
acidentes$lambda <- poisson.glm$fitted.values


##############################################################################
#                              ESTIMAÇÃO DO ZI BNEG                          #
##############################################################################
# Modelo ZI Binomial Negativo
nbin.glm <- glm.nb(acidentes ~ alcool,
                   data = acidentes)

#Observando os parâmetros do modelo
summary(nbin.glm)

#Extração do valor do LL
logLik(nbin.glm)

#LR Test
lrtest(poisson.glm, nbin.glm)

#Adicionando os fitted values de zi.nbin.glm à base de dados
acidentes$u <- nbin.glm$fitted.values

#Decréscimo nos LL's dos modelos
data.frame(Poisson = logLik(poisson.glm),
           BNEG = logLik(nbin.glm)) %>%
  rename(`Poisson` = 1,
         `BNEG` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("bisque4","coral4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Overdispersion Test:
overdisp(acidentes,
         dependent.position = 4,
         predictor.position = 5)

#Multilevel Poisson:
poisson.glmm <- glmmTMB(formula = acidentes ~ alcool +
                          (1 | estado) + (1 | municipio) + (1 | distrito), 
                        family = poisson, 
                        data = acidentes)

summary(poisson.glmm)
logLik(poisson.glmm)

#Multilevel Binomial Negativo:
nbin.glmm <- glmer.nb(formula = acidentes ~ alcool +
                        (1 | estado) + (1 | municipio) + (1 | distrito),
                      data = acidentes)

summary(nbin.glmm)
logLik(nbin.glmm)

#Decréscimo nos LL's dos modelos
data.frame(Poisson = logLik(poisson.glm),
           BNEG = logLik(nbin.glm),
           Poisson_Multilevel = logLik(poisson.glmm),
           BNEG_Multilevel = logLik(nbin.glmm)) %>%
  rename(`Poisson` = 1,
         `BNEG` = 2,
         `Poisson Multilevel` = 3,
         `BNEG Multilevel` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.2, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("bisque4","coral4","darkorchid","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Comparação entre os parãmetros dos modelos (atente-se para as diferenças nas
#magnitudes dos parâmetros!)
export_summs(poisson.glm, nbin.glm, poisson.glmm, nbin.glmm,
             model.names = c("POISSON", "BNEG",
                             "POISSON MULTILEVEL", "BNEG MULTILEVEL"))

####################################### FIM ####################################
