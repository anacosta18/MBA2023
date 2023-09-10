## CARREGANDO PACOTES NECESSÁRIOS PARA ANALISE

pacotes <- c('tidyverse',  
              'scales',    
             'shapr',
             'reshape',
             'sjPlot',
             'amap',
             'ade4',
             'knitr',
             'kableExtra',
             'ggrepel',
             'factoextra',
             'dplyr',
             'plotly')


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


## CARREGANDO O BANCO DE DADOS 

mdndc <- read.csv("20230820_MasterDE9.csv",  header = TRUE, sep = ";", dec = ",")


## SELEÇÃO DE VARIÁVEIS QUALITATIVAS E GARANTIA DE VARIÁVEIS NO FORMATO FACTOR

mdnc_acm <- mdndc[,c(1,3,14,18:30,34,36,38)]

mdnc_acm <- as.data.frame(unclass(mdnc_acm), stringsAsFactors=TRUE)
mdnc_acm$ndc_imp_st_y <- as.factor(mdnc_acm$ndc_imp_st_y)
mdnc_acm$ndc_imp_end_y <- as.factor(mdnc_acm$ndc_imp_end_y)
mdnc_acm$kyoto_signature_year <- as.factor(mdnc_acm$kyoto_signature_year)
mdnc_acm$kyoto_ratification_year <- as.factor(mdnc_acm$kyoto_ratification_year)
mdnc_acm$paris_ag_signature_date <- as.factor(mdnc_acm$paris_ag_signature_date)
mdnc_acm$cluster_K <- as.factor(mdnc_acm$cluster_K)
mdnc_acm$ndc_bl_y <- as.factor(mdnc_acm$ndc_bl_y)

#categOrizando o CO2 emitido

mdnc_acm <- mdnc_acm %>% 
  mutate(CO2_emmited = case_when(co2_eq_WDI < quantile(co2_eq_WDI, 0.25, na.rm = TRUE) ~ "<13 kton",
                                       co2_eq_WDI < quantile(co2_eq_WDI, 0.5, na.rm = TRUE) & co2_eq_WDI >= quantile(co2_eq_WDI, 0.25, na.rm = TRUE) ~ "13a42 kton",
                                       co2_eq_WDI < quantile(co2_eq_WDI, 0.75, na.rm = TRUE) & co2_eq_WDI >= quantile(co2_eq_WDI, 0.5, na.rm = TRUE) ~ "42a125 kton",
                                       co2_eq_WDI < quantile(co2_eq_WDI, 0.95, na.rm = TRUE) & co2_eq_WDI >= quantile(co2_eq_WDI, 0.75, na.rm = TRUE)~ "125a819 kton",
                                       co2_eq_WDI >= quantile(co2_eq_WDI, 0.95, na.rm = TRUE) ~ ">819 kton"
                                       ))
                                       
mdnc_acm$CO2_emmited<- as.factor(mdnc_acm$CO2_emmited)

# categorizando o ano base do NDC
mdnc_acm <- mdnc_acm %>% 
  mutate(NDC_baseline_year = case_when(ndc_bl_y == "1990" ~ "1990",
                                       ndc_bl_y == "2005" ~ "2005-2010",
                                       ndc_bl_y == "2006" ~ "2005-2010",
                                       ndc_bl_y == "2008" ~ "2005-2010",
                                       ndc_bl_y == "2010" ~ "2005-2010",
                                       ndc_bl_y == "2013" ~ "2011-2015",
                                       ndc_bl_y == "2014" ~ "2011-2015",
                                       ndc_bl_y == "2015" ~ "2011-2015",
                                       ndc_bl_y == "2018" ~ "2016-2020",
                                       ndc_bl_y == "2019" ~ "2016-2020",
                                       ndc_bl_y == "2020" ~ "2016-2020",
                                       ndc_bl_y == "BAU" ~ "BAU",
                                       is.na(ndc_bl_y) == TRUE ~ "not disclosed"))

mdnc_acm$NDC_baseline_year<- as.factor(mdnc_acm$NDC_baseline_year)

# organizando DF final para analise

mdnc_acm_final <- mdnc_acm[,c(16,18,20,21)]


## VERIFICAÇÃO ASSOCIAÇÃO SIGNIFICATIVA VIA QUI-QUADRADO

##NDC Mitigation Type versus Type of coverage

tabela_contingencia <- table(mdnc_acm_final$ndc_mitigation_type,
                             mdnc_acm_final$ndc_type_cove)

qui2 <- chisq.test(x = tabela_contingencia)
qui2
db <- data.frame(qui2$stdres)
  
ggplot(db, aes(x = fct_rev(Var1), y = Var2,fill = Freq, label = round(Freq, 3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "white", 
                       mid = "white", 
                       high = "purple",
                       midpoint = 1.96) +
  labs(x = 'Estratégia de Mitigação', y = "Tipo de cobertura", fill = "Res. Pad. Ajustados") +
  coord_flip() +
  theme_bw()


##NDC Mitigation Type versus Baseline_Year

tabela_contingencia2 <- table(mdnc_acm_final$ndc_mitigation_type,
                             mdnc_acm_final$NDC_baseline_year)
qui2 <- chisq.test(x = tabela_contingencia2)
qui2
db <- data.frame(qui2$stdres)

ggplot(db, aes(x = fct_rev(Var1), y = Var2,fill = Freq, label = round(Freq, 3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "white", 
                       mid = "white", 
                       high = "purple",
                       midpoint = 1.96) +
  labs(x = 'Estratégia de Mitigação', y = "Ano referência para cálculo de redução de emissão", fill = "Res. Pad. Ajustados") +
  coord_flip() +
  theme_bw()


#NDC Mitigation Type versus Baseline_Year

tabela_contingencia3 <- table(mdnc_acm_final$ndc_mitigation_type,
                             mdnc_acm_final$CO2_emmited)
qui2 <- chisq.test(x = tabela_contingencia3)
qui2
qui2$stdres
db <- data.frame(qui2$stdres)

ggplot(db, aes(x = fct_rev(Var1), y = Var2,fill = Freq, label = round(Freq, 3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "white", 
                       mid = "white", 
                       high = "purple",
                       midpoint = 1.96) +
  labs(x = 'Estratégia de Mitigação', y = "Faixa de emissão de GEE em 2019", fill = "Res. Pad. Ajustados") +
  coord_flip() +
  theme_bw()

summary(mdnc_acm_final$CO2_emmited)

## EXECUTANDO A ACM

ACM <- dudi.acm(mdnc_acm_final[,c(1,3,4)], scannf = FALSE)


# Visualização das coordenadas principais das categorias das variáveis
# Método da matriz de Burt B (componente 'co' do objeto 'ACM')
round(ACM$co, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Visualização das coordenadas-padrão das categorias das variáveis
# Método da matriz binária (componente 'c1' do objeto 'ACM')
round(ACM$c1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Massas das linhas e colunas (componente 'cw' do objeto 'ACM')

ACM$cw

# Inércias principais (componente 'eig' do objeto 'ACM')
ACM$eig

# Percentual de variância explicada por dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Visualização do percentual de variância explicada por dimensão

data.frame(Dimensão = paste("Dimensão", 1:length(perc_variancia)),
           Variância = perc_variancia) %>%
  ggplot(aes(x = Dimensão,
             y = Variância,
             label = paste0(round(Variância, 2),"%"))) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_text(vjust = 2.5, size = 5) +
  theme_bw()

# Mapa perceptual na ACM

# Definição da quantidade de categorias de cada variável qualitativa
quant_categorias <- apply( mdnc_acm_final[,c(1,3,4)],
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária ('c1')
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Visualizando as coordenadas
df_ACM <- df_ACM %>%
  rownames_to_column()

df_ACM <-df_ACM %>%
  mutate(rowname = gsub("ndc_mitigation_type.","", rowname))%>%
  mutate(rowname = gsub("ndc_type_cove.","", rowname))%>%
  mutate(rowname = gsub("CO2_emmited.","", rowname))%>%
  mutate(rowname = gsub("NDC_baseline_year.","", rowname))

df_ACM%>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

######################################################################




# Plotando o mapa perceptual
  ggplot(df_ACM, aes(x = CS1, y = CS2, label = rowname, color = Variável)) +
  geom_point()+ 
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_color_manual("Variável",
                     values = c("turquoise3", "springgreen4", "deeppink1", "blue")) +
  theme_bw()

# Coletando as coordenadas das observações
ACM_observacoes_df <- data.frame(ACM$li)

ACM_observacoes_df$cluster_k <- mdnc_acm$cluster_K

# Vamos acrescentar as informações das observacões ao mapa perceptual da ACM

ACM_observacoes_df %>% 
  filter(co2_emmited ==">819 kton")%>%
  ggplot(aes(x = Axis1, y = Axis2, color = country)) +
  geom_point(aes(shape = cluster_k), size = 3) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey48") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey48") +
  geom_label_repel(data = df_ACM, 
                   aes(x = CS1, y = CS2, 
                       label = rownames(df_ACM), 
                       fill = Variável), 
                   color = "white") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"))



