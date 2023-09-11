# Use a imagem oficial do RStudio
FROM rocker/rstudio:4.0.5

# Defina um ambiente não interativo para a instalação de pacotes
ENV DEBIAN_FRONTEND noninteractive

# Defina a senha para o usuário "rstudio"
RUN echo "rstudio:123456" | chpasswd

# Instale as bibliotecas do sistema necessárias para os pacotes R
RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libudunits2-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libmagick++-dev

# Instale os pacotes R solicitados
RUN R -e "install.packages(c('tidyverse', 'writexl', 'ggplot2', 'dplyr', 'reshape2', 'ggrepel', 'rgl', 'car', 'sf', 'esquisse', 'readxl', 'plotly', 'ggrepel', 'knitr', 'kableExtra', 'misc3d', 'plot3D', 'cluster', 'factoextra', 'ade4', 'kableExtra', 'factoextra', 'cluster', 'janeaustenr', 'lexiconPT', 'wordcloud', 'stringr', 'tidyr', 'tm', 'e1071', 'gmodels', 'SnowballC', 'caret', 'titanic', 'rpart', 'plotROC', 'Rmisc', 'scales', 'gtools'), dependencies=TRUE)"

# Abra a porta 8787 para acessar o RStudio
EXPOSE 8787

# Execute o RStudio Server
CMD ["/init"]
