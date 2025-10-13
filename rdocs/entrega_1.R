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
infos_cidades <- read_excel("relatorio_old_town_road.xlsx", 
                                      sheet = "infos_cidades")
infos_lojas <- read_excel("relatorio_old_town_road.xlsx", 
                                      sheet = "infos_lojas")

#Renomeando as Colunas em Comum, para que possa juntá-las
infos_vendas1 <- rename(infos_vendas, SaleID = Sal3ID)
infos_produtos1 <- rename(infos_produtos, ItemID = Ite3ID)
infos_cidades1 <- rename(infos_cidades, CityID = C1tyID)
infos_lojas1 <- rename(infos_lojas, StoreID = Stor3ID)

#Juntando os Data Frames
planilha_1 <- inner_join(infos_produtos1, infos_vendas1)
planilha_final <- inner_join(planilha_1, relatorio_vendas)
planilha_2 <- inner_join(infos_cidades1, infos_lojas1)
ultima_planilha <- inner_join(planilha_2, planilha_final)

#Multiplicando o Preço por Unidade pela Quantidade. Gerando o valor da compra
planilha_final <- planilha_final %>%
  mutate(Valor_compra = Quantity * UnityPrice)

#Convertendo os Valores das Moedas
planilha_final <- planilha_final %>%
  mutate(Valor_compra = Valor_compra * 5.31)

#Transformando as Datas em Anos
planilha_final <- planilha_final %>%
  mutate(Ano = year(ymd(Date)))

#Filtrando pelos Anos e Descobrindo a Média
analise_1 <- planilha_final %>% 
  filter(Ano >= 1880 & Ano <= 1889) %>%
  group_by(Ano) %>%
  summarise(Total = sum(Valor_compra)) %>%
  mutate(Total_dividido = Total / 18)

#Fazendo o Gráfico 1
grafico_1<- ggplot(analise_1) +
  aes(x=factor(Ano), y=Total_dividido, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21", size=2) +
  labs(x="Ano", y="Receita Média") +
  theme_estat()  

#Fazendo a tabela de Vendas por loja
"tabela_vendas <- ultima_planilha %>%
  group_by(NameCity) %>%
  summarise(
    Quantidade_Vendas = n(),
    Valor_Total = sum(Valor_compra),
    Valor_Medio = mean(Valor_compra)) %>%
  mutate(Valor_Medio = round(Valor_Medio))"
 
#Calculando o numero de lojas por cidade
"tabela_lojas <- ultima_planilha %>%
  group_by(CityID, NameCity) %>%
  summarise(
    Numero_Lojas = n_distinct(StoreID)
  ) %>%
  arrange(CityID)"

#Fazendo a tabela de valor total vendido por cidade
"tabela_vendas_cidade <- ultima_planilha %>%
  group_by(CityID, NameCity) %>%
  summarise(
    Valor_Total = sum(Valor_compra)) %>%
  arrange(CityID)
tabela_vendas_cidade <- inner_join(tabela_vendas_cidade, tabela_lojas)"

#Fazendo a tabela de valor médio vendido por loja em cada cidade
"tabela_media_cidade <- tabela_vendas_cidade %>%
  mutate(Valor_Medio_cidade = Valor_Total / Numero_Lojas)"





