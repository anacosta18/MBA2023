##----------------------------- INSTALAÇÃO DE PACOTES--------------------

pacotes <- c("readxl", "plyr","nc","dplyr", "ggplot2","plotly","tidyverse","reshape2","knitr","kableExtra",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","gganimate",
             "ggridges","viridis","hrbrthemes","PerformanceAnalytics", "cowplot","epiDisplay","plotly", 
             "tidyverse", 
             "ggrepel", 
             "knitr", 
             "kableExtra",
             "reshape2",
             "misc3d",
             "plot3D", 
             "cluster", 
             "factoextra",
             "arsenal",
             "rworldmap",
             "viridis",
             "RColorBrewer",
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "Hmisc" # matriz de correlações com p-valor
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


## ------------------------------ CARREGANDO O DB------------------------------
mdndc <- read.csv(file = "20230723_MasterDE7.csv", header = TRUE, sep = ";", dec = ",")

##transformando variaveis métricas do NDC em dummies

mdndc_scaled <-as.data.frame(scale(mdndc[,6:17]))
row.names(mdndc_scaled) <-mdndc$country

# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(mdndc[,6:17]), type="pearson")

correl <- rho$r # Matriz de correlações
sig_correl <- round(rho$P, 4) # Matriz com p-valor dos coeficientes

# Visualização das distribuições das variáveis, scatters, valores das correlações
chart.Correlation(mdndc[,6:17], histogram = TRUE, pch = "+")


# Método de Elbow para identificação do número ótimo de clusters
dev.off()
fviz_nbclust(mdndc_scaled, kmeans, method = "wss", k.max = 20)
names(mdndc)


##SELECIONADO 9 CLUSTERS

mdndc_kmeans<- kmeans(mdndc_scaled,
                      centers = 9)

# Adicionando a variável
mdndc$cluster_K <- as.factor(mdndc_kmeans$cluster)
mdndc_scaled$cluster_K <- as.factor(mdndc_kmeans$cluster)



## visão da distribuição dos clusters
cluster_K <- tab1(mdndc$cluster_K,
                  xlab = "Cluster", 
                  ylab = "Número de paises", 
                  main = "Distribuição de países pelos Clusters")

##todas as variaveis influenciaram para a clusterização

mapdata <- joinCountryData2Map(mdndc,
                               nameJoinColumn = "iso3",
                               joinCode = "ISO3")
map_cluster_k <- mapCountryData(mapdata,
                                nameColumnToPlot = "cluster_K",
                                catMethod="categorical",
                                colourPalette = viridis::turbo(9))


write.table(mdndc, "20230812_MasterDE8.csv", sep = ";", dec = ",")



## ------------------------- Visões dos clusters -----------------

panel_2<- mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= pop_pop_million, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "População (em milhões)")

panel_3 <- mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= pop_density, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Densidade Populacional (pessoas/km²)")


panel_4<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= pop_sex_ratio, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Relação Entre sexos (homens/100 mulheres)")


panel_5<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= export_dolar, colour = cluster_K))+
  geom_boxplot()+
  theme_bw()+
  labs(x ="Cluster" , y = "Exportação (bilhões de dólares)")

panel_6<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= import_dolar, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Importação (bilhões de dólares)")


panel_7<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= gpd_dolar, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Produto Interno Bruto (bilhões de Dólares)")



panel_8<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= gpd_perc, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Crescimento PIB (%)")




panel_9<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= FDI_WDI/1000000000, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Investimento Externo (bilhões de dólares)")



panel_10<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= co2_eq_WDI/1000, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Emissão CO2 (Mton Co2eq)")


panel_11<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= co2percapita, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Emissão de CO2 per capita (tonCO2eq/pessoa)")


panel_12<-mdndc %>% group_by(cluster_K) %>%
  ggplot(aes(x=cluster_K, y= co2pergpd, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y =  "Emissão de CO2 per PIB (kgCO2eq/dólar)")


ggarrange(panel_2, panel_3, panel_4, panel_5, panel_6, panel_7, ncol =3, nrow=2, common.legend = T, legend = "bottom")
ggarrange(panel_8, panel_9, panel_10, panel_11, panel_12,ncol =3, nrow=2, common.legend = T, legend = "bottom")


## foco em emissões de CO2 sem cluster 7

mdndc %>%
  ggplot(aes(x=cluster_K, y= co2_eq_WDI/1000, colour = cluster_K))+
  geom_boxplot()+
  theme_bw() +
  labs(x ="Cluster" , y = "Emissão CO2 (Mton Co2eq)")


