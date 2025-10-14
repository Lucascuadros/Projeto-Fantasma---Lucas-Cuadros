source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #
library(readxl)
library(dplyr)
library(ggplot2)

#Lendo a página do Excel
infos_clientes <- read_excel("relatorio_old_town_road.xlsx", 
                                      sheet = "infos_clientes")

#Fazendo as tranformações necessárias
infos_clientes$Weight_lbs <- round(infos_clientes$Weight_lbs * 0.45359237,2)
infos_clientes<- infos_clientes %>%
  rename(Peso_Kg = Weight_lbs)
infos_clientes$Height_dm <- round(infos_clientes$Height_dm * 10,2)
infos_clientes <- infos_clientes %>%
  rename(Altura_cm = Height_dm)

#Fazendo um gráfiico de dispersão
Grafico_dispersao <- ggplot(infos_clientes) +
  aes(x = Peso_Kg, y = Altura_cm) +
  geom_point(colour = "#A11D21", size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", linewidth = 1) +
  labs(
    x = "Peso (Kg)",
    y = "Altura (cm)"
  ) +
  theme_estat()
