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

#Renomeando as Colunas em Comum, para que possa juntá-las
infos_vendas1 <- rename(infos_vendas, SaleID = Sal3ID)
infos_produtos1 <- rename(infos_produtos, ItemID = Ite3ID)

#Juntando os Data Frames
planilha_1 <- inner_join(infos_produtos1, infos_vendas1)
planilha_final <- inner_join(planilha_1, relatorio_vendas)

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
ggplot(analise_1, aes(x = factor(Ano), y = Total_dividido, group = 1)) +
#  geom_bar(stat = "identity", fill = "#CC9900") +
  geom_line(aes(y = Total_dividido), group = 1, color = "#A11D21", size = 1.5) +
  geom_point(aes(y = Total_dividido), color = "#003366") +
  labs(title = "Valor Médio Vendido por Loja entre os Anos 1880 e 1889",
       x = "Ano",
       y = "Valor Médio Vendido por Loja") +
  theme_minimal()

#Fazendo o Gráfico 2
ggplot(analise_1, aes(x = factor(Ano), y = Total_dividido, group = 1)) +
  geom_bar(stat = "identity", fill = "#A11D21") +
  labs(title = "Valor Médio Vendido por Loja entre os Anos 1880 e 1889",
       x = "Ano",
       y = "Valor Médio Vendido por Loja") +
  theme_minimal()

#Análise - 1:
# A partir da primeira análise, nota-se que há um crescimento no valor médio vendidos pelas lojas por ano.
# Entretanto, percebe-se uma leve queda no gráfico no ano 1888.
# Além disso, vê-se um crescimento significativo no ano de 1889.
# 1888-1889 > 1884-1885 > 1880-1881 > 1882-1883 > 1886-1887 > 1883-1884 > 1885-1886 > 1881-1882 > 1887-1888
# Portanto, coclui-se que, apesar da queda em 1888, o valor médio vendido pelas lojas por ano vêm crescendo conforme os anos.
