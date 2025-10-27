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
#Lendo as páginas ainda não lidas
clientes <- read_excel("relatorio_old_town_road.xlsx", 
                                      sheet = "infos_clientes")
relatorio_vendas <- read_excel("relatorio_old_town_road.xlsx", 
                                      sheet = "relatorio_vendas")
infos_lojas1 <- read_excel("relatorio_old_town_road.xlsx", 
              sheet = "infos_lojas")

#Fazendo uma planilha para calcular
infos_lojas2 <- rename(infos_lojas1, StoreID = Stor3ID)
plan1 <- rename(clientes, ClientID = Cli3ntID)
plan2 <- inner_join(plan1, relatorio_vendas)
análise_3 <- inner_join(plan2, infos_lojas2)
análise_3$SaleID = NULL
análise_3$Date = NULL
análise_3$Quantity = NULL
dados_ambar <- análise_3 %>%
  filter(CityID == "2")
dados_ambar_unicos <- dados_ambar %>%
  distinct(ClientID, StoreID, .keep_all = TRUE)

#Gerando os gráficos
Boxplot1 <- ggplot(dados_ambar_unicos) +
  aes(x = factor(NameStore, levels = sort(unique(NameStore))), y = Age) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary( fun = "mean", geom = "point", shape = 23, size = 3, fill = "#FFFFFF") +
  labs(x = "Loja", y = "Idade") +
  theme_estat()

#Gerando tabela
dados_ambar_unicos %>%
  group_by(StoreID) %>%
  summarise(
    media = mean(Age),
    mediana = median(Age),
    desvio = sd(Age),
    minimo = min(Age),
    maximo = max(Age)
  )


