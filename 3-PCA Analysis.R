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

mdndc <- read.csv(file = "20230812_MasterDE8.csv", header = TRUE, sep = ";", dec = ",")


mdndc_scaled <-as.data.frame(scale(mdndc[,6:17]))
row.names(mdndc_scaled) <-mdndc$country


# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(mdndc[,6:17]), type="pearson")

correl <- rho$r # Matriz de correlações
sig_correl <- round(rho$P, 4) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  mdndc[,6:17] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = ) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())

# Visualização das distribuições das variáveis, scatters, valores das correlações
chart.Correlation(mdndc[,6:17], histogram = TRUE, pch = "+")

##---------------- Elaboração a Análise Fatorial Por Componentes Principais----------- 

# Teste de esfericidade de Bartlett
cortest.bartlett(mdndc_scaled)
KMO(mdndc[,6:17])

# Elaboração da análise fatorial por componentes principais

md_fatorial <- principal(mdndc_scaled,
                      nfactors = length(mdndc_scaled),
                      rotate = "none",
                      scores = TRUE)
md_fatorial

# Eigenvalues (autovalores)
md_eigenvalues <- round(md_fatorial$values, 5)
md_eigenvalues
round(sum(md_eigenvalues), 2) ## check - 12 fatores, ok!

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(md_fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos scores fatoriais
scores_fatoriais <- as.data.frame(md_fatorial$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos fatores propriamente ditos
fatores <- as.data.frame(md_fatorial$scores)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(md_fatorial$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(md_fatorial$communality)) %>%
  rename(comunalidades = 1)


##-------------- Elaboração da Análise Fatorial por Componentes Principais-------------
##---------------- Fatores extraídos a partir de autovalores maiores que 1 ###

# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(md_eigenvalues > 1)
print(k)

# Elaboração da análise fatorial por componentes principais sem rotação
# Com quantidade 'k' de fatores com eigenvalues maiores que 1
md_fatorial2 <- principal(mdndc_scaled,
                       nfactors = k,
                       rotate = "none",
                       scores = TRUE)
md_fatorial2


# Identificação da variância compartilhada em cada fator
variancia_compartilhada2 <- as.data.frame(md_fatorial2$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada2) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

round(variancia_compartilhada2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos scores fatoriais
scores_fatoriais2 <- as.data.frame(md_fatorial2$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos fatores propriamente ditos
fatores2 <- as.data.frame(md_fatorial2$scores)

# Cálculo das cargas fatoriais
cargas_fatoriais2 <- as.data.frame(unclass(md_fatorial2$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo das comunalidades com apenas os 'k' ('k' = 3) primeiros fatores
comunalidades2 <- as.data.frame(unclass(md_fatorial2$communality)) %>%
  rename(comunalidades = 1)
round(comunalidades2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

#incluindo as cargas fatoriais na tabela

mdndc$PC1 <- fatores2$PC1
mdndc$PC2 <- fatores2$PC2
mdndc$PC3 <- fatores2$PC3
mdndc$PC4 <- fatores2$PC4


# Loading plot com as cargas dos dois primeiros fatores
cargas_fatoriais2[, 1:2] %>% 
  data.frame() %>%
  rownames_to_column("variáveis") %>%
  ggplot(aes(x = PC1, y = PC2, label = variáveis)) +
  geom_point(color = "darkorchid",size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "orange") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "orange") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  labs(x = "PC1 (41.7%)", y = "PC2 (15.0%)")+
  theme_bw()


##Observação das cargas fatoriais de cada componente
cargas_fatoriais2$variavel <- row.names(cargas_fatoriais2)
cargas2_rotate <-pivot_longer(cargas_fatoriais2,cols = PC1:PC4, names_to = "componentes", values_to = "carga_fatorial")

ggplot(cargas2_rotate, aes(x=variavel, y= carga_fatorial, fill=variavel))+
  geom_bar(stat = "identity")+
  facet_grid(~componentes)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_viridis_d(option = "viridis",
              labels = c('Emissão CO2 total',
             'Emissão CO2 per capita',
             'Emissão CO2 per PIB',
             'Exportações',
             'Investimento Estrangeiro',
             'PIB',
             'Crescimento PIB (%)',
             'Importações',
             'Densidade Populacional',
             'Crescimento Populacional',
             'População Total',
             'Relação entre sexos'))+
  labs(x="Variável", y="Carga Fatorial")



  
## Plotando PC1 versus PC2 de acordo com os clusters já realizados
mdndc$cluster_K<-as.factor(mdndc$cluster_K)
ggplot(mdndc, aes(x=PC1, y= PC2, colour=cluster_K))+
  geom_point(size=2)+
  geom_text_repel(aes(label=iso3),label.size = 0.1)+
  labs(x = "PC1 (41.7%)", y = "PC2 (15.0%)")+
  theme_bw()



mdndc_clean <- mdndc %>% filter(mdndc$cluster_K %in% c("1","2","3","6","8"))

ggplot(mdndc_clean, aes(x=PC1, y= PC2, colour=cluster_K))+
  geom_point(size=2)+
  geom_text_repel(aes(label=iso3),label.size = 0.1)+
  labs(x = "PC1 (41.7%)", y = "PC2 (15.0%)")+
  theme_bw()


write.table(mdndc, "20230820_MasterDE9.csv",sep = ";", dec = ",")
