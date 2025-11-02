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
  geom_point(colour = "#A11D21", alpha=0.4, size = 3) +
  labs(x = "Peso (Kg)", y = "Altura (cm)") +
  theme_estat()

#Fazendo um quadro
quadro_peso_altura <- infos_clientes %>%
  summarise(
    `Peso - Média (kg)` = mean(Peso_Kg, na.rm = TRUE),
    `Peso - Mediana (kg)` = median(Peso_Kg, na.rm = TRUE),
    `Peso - Desvio Padrão (kg)` = sd(Peso_Kg, na.rm = TRUE),
    `Peso - Mínimo (kg)` = min(Peso_Kg, na.rm = TRUE),
    `Peso - Máximo (kg)` = max(Peso_Kg, na.rm = TRUE),
    `Altura - Média (cm)` = mean(Altura_cm, na.rm = TRUE),
    `Altura - Mediana (cm)` = median(Altura_cm, na.rm = TRUE),
    `Altura - Desvio Padrão (cm)` = sd(Altura_cm, na.rm = TRUE),
    `Altura - Mínimo (cm)` = min(Altura_cm, na.rm = TRUE),
    `Altura - Máximo (cm)` = max(Altura_cm, na.rm = TRUE),
    '')
quadro_peso_altura

covariancia <- cov(infos_clientes$Peso_Kg, infos_clientes$Altura_cm)
var_peso <- 142.25
var_altura <- 97.38
covariancia/((var_peso*var_altura)**(1/2))

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
    maximo = max(Age),
    variancia = var(Age)
  )
# Ultima análise
God <- inner_join(planilha_final, ultima_planilha)

God_1889 <- God %>%
  filter(Ano == 1889)

God_1889$Date = NULL

God_1889$ClientID = NULL

God_1889$CityID = NULL

God_1889$NameCity = NULL

receita_por_loja <- God_1889 %>%
  group_by(StoreID) %>%
  summarise(Receita_Loja = sum(Valor_compra)) %>%
  arrange(desc(Receita_Loja))

top3_lojas <- receita_por_loja %>%
  slice_head(n = 3)

GOD <- God_1889 %>%
  filter(StoreID %in% top3_lojas$StoreID)

vendas_por_produto_loja <- GOD %>%
  group_by(StoreID, ItemID) %>%
  summarise(Quantidade_Total = sum(Quantity)) %>%
  arrange(StoreID, desc(Quantidade_Total))

GOD <- vendas_por_produto_loja %>%
  group_by(StoreID) %>%
  slice_max(order_by = Quantidade_Total, n = 3) %>%
  ungroup()

GOD <- inner_join(GOD, infos_produtos1)

GOD$UnityPrice = NULL

top3_lojas <- inner_join(top3_lojas, infos_lojas2)

top3_lojas$CityID = NULL

Top_Lojas <- ggplot(top3_lojas) +
  aes(x = NameStore, y = Receita_Loja) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Loja", y = "Receita") +
  theme_estat()

top_tend <- GOD %>%
  filter(StoreID == 5)

top_ferraria1 <- GOD %>%
  filter(StoreID == 17)

top_ouro <- GOD %>%
  filter(StoreID == 7)

Top_Ferraria <- ggplot(top_ferraria1) +
  aes(x = NameProduct, y = Quantidade_Total) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Produto", y = "Quantidade") +
  theme_estat()

Top_Ouro <- ggplot(top_ouro) +
  aes(x = NameProduct, y = Quantidade_Total) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Produto", y = "Quantidade") +
  theme_estat()

Top_Tend <- ggplot(top_tend) +
  aes(x = NameProduct, y = Quantidade_Total) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Produto", y = "Quantidade") +
  theme_estat()
