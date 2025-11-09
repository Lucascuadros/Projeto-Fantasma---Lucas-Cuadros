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

receita_1889 <- God_1889 %>%
  summarise(receita_1889 = sum(Valor_compra))

receita_top <- top3_lojas %>%
  summarise(receita_top = sum(Receita_Loja))

percentual_1889 <- ((receita_top/receita_1889)*100)

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
  
ouro <- GOD %>%
  filter(StoreID == 7)

tend <- GOD %>%
  filter(StoreID == 5)

ferraria <- GOD %>%
  filter(StoreID == 17)

Fim <- data.frame(
  Loja = c(rep("Ouro Fino", 3), rep("TendTudo", 3), rep("Ferraria Apache", 4)),
  Produto = c("Botas de Couro", "Whisky", "Chapéu de Couro", "Espingarda", "Whisky", "Colt.45", "Chapéu de Couro", "Espingarda", "Machado", "Whisky"),
  Quantidade = c(52, 49, 45, 53, 49, 43, 52, 42, 41, 41))

kobe <- ggplot(Fim, aes(x = Loja, y = Quantidade, fill = Produto)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Quantidade),
            position = position_dodge(width = 9),
            vjust = -0.5, hjust = 0.5, size = 3.5) +
  labs(x = "Loja", y = "Produtos vendidos", fill = "Produto") +
  theme_estat()




