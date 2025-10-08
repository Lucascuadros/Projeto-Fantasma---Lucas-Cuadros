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
#Lendo a planilha no excel
library(readxl)
relatorio_vendas <- read_excel("relatorio_old_town_road.xlsx")
infos_vendas <- read_excel("relatorio_old_town_road.xlsx", 
                                      sheet = "infos_vendas")
infos_produtos <- read_excel("relatorio_old_town_road.xlsx", 
                                      sheet = "infos_produtos")
infos_vendas1 <- rename(infos_vendas, SaleID = Sal3ID)
infos_produtos1 <- rename(infos_produtos, ItemID = Ite3ID)
planilha_1 <- inner_join(infos_produtos1, infos_vendas1)
planilha_final <- inner_join(planilha_1, relatorio_vendas)
planilha_final$Valor_compra <- planilha_final$UnityPrice * planilha_final$Quantity
planilha_final$Valor_compra <- planilha_final$Valor_compra * 5.31
planilha_final <- planilha_final %>%
  mutate(Ano = year(ymd(Date)))
analise_1 <- planilha_final %>% 
  filter(Ano >= 1880 & Ano <= 1889) %>%
  group_by(Ano) %>%
  summarise(Total = sum(Valor_compra, na.rm = TRUE)) %>%
  mutate(Total_dividido = Total / 18)
ggplot(analise_1, aes(x = factor(Ano), y = Total)) +
  geom_bar(stat = "identity") +
  labs(title = "Receita total média",
       x = "Ano",
       y = "Receita Média") +
  theme_minimal()