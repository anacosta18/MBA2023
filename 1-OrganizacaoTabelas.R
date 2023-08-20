##----------------------------- INSTALAÇÃO DE PACOTES--------------------

pacotes <- c("readxl", "plyr","nc","dplyr", "ggplot2","plotly","tidyverse","reshape2","knitr","kableExtra",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","gganimate",
             "ggridges","viridis","hrbrthemes")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


##------------------ DADOS DEMOGRÁFICOS ------------------

##Descrição dos dados presentes na tabela
  ## pop_index - indice
  ## pop_variant - tipo de estimativa realizada
  ## opo_country - Nome do país
  ## pop_country_code - código do país
  ## pop_iso3 - código do país pelo ISO3
  ## pop_year - ano
  ## pop_pop_million - População com base no dia 1 de julho, em milhões
  ## pop-density - densidade populacional, representada por número de pessoas por km²
  ## pop_sex_ratio - proporção entre sexos, representada por numero de homens para cada 100 mulheres
  ## pop_growth - crescimento populacional, em %



demo <- read_excel("WPP2022_DEMOGRAFIC_CLEAN.xlsx")
demo$pop_country<-as.factor(demo$pop_country)
demo$pop_country_loc <- as.factor(demo$pop_country_loc)
demo$pop_iso3 <- as.factor(demo$pop_iso3)
summary(demo)

##------------------ DADOS IMPORTAÇÃO ------------------

##Notas - dados foram limpos para conter apenas países que contem dados a partir de 2000, 1970-2021


##Descrição dos dados presentes na tabela
    ## import_country - Nome do país
    ## inc_iso3 - código do país ISO3
    ##year - ano
    ##import_million --importação de bens e serviços em milhoes de dólares


import <- read_excel("API_NE.IMP.GNFS.CD_DS2_en_excel_v2_5456194.xls")
import$inc_iso3 <-as.factor(import$inc_iso3)
summary(import)

import_tallskinny <- nc::capture_melt_single(import,
                                             year = list (
                                             "[0-9]{4}"),
                                             value.name = "import_dolar"
                                             )
rm(import)


##------------------ DADOS EXPORTAÇÃO ------------------

##Notas - dados foram limpos para conter apenas países que contem dados a partir de 2000, 1970-2021


##Descrição dos dados presentes na tabela
## export_country - Nome do país
## exp_iso3 - código do país ISO3
##year - ano
##exp_dolar --importação de bens e serviços em dólares


export <- read_excel("API_NE.EXP.GNFS.CD_DS2_en_excel_v2_5454955.xls")
export$inc_iso3 <-as.factor(export$inc_iso3)
summary(export)
export_tallskinny <- nc::capture_melt_single(export,
                                             year = list (
                                               "[0-9]{4}"),
                                             value.name = "export_dolar"
                                           )
rm(export)


##------------------ DADOS GPD ------------------

##Notas - dados foram limpos para conter apenas países que contem dados a partir de 2000, 1970-2021


##Descrição dos dados presentes na tabela
## country - Nome do país
## iso3 - código do país ISO3
##year - ano
##gdp_dolar --GDP at purchaser's prices is the sum of gross value added by all resident
##              producers in the economy plus any product taxes and minus any subsidies not   
##              included in the value of the products. It is calculated without making deductions
##              for depreciation of fabricated assets or for depletion and degradation of natural 
##              resources. Data are in current U.S. dollars. Dollar figures for GDP are converted from domestic currencies using single year official exchange rates.


gpd <- read_excel("API_NY.GDP.MKTP.CD_DS2_en_excel_v2_5454813(GPD em milhoes).xls")
gpd$iso3 <-as.factor(gpd$iso3)
summary(gpd)
gpd_tallskinny <- nc::capture_melt_single(gpd,
                                             year = list (
                                               "[0-9]{4}"),
                                             value.name = "gpd_dolar"
)

rm(gpd)

##------------------ DADOS GPD - CRESCIMENTO EM %------------------

##Notas - dados foram limpos para conter apenas países que contem dados a partir de 2000, 1970-2021


##Descrição dos dados presentes na tabela
## country - Nome do país
## iso3 - código do país ISO3
##year - ano
##gdp_perc -Annual percentage growth rate of GDP per capita based on constant local currenc

gpd_perc <- read_excel("API_NY.GDP.PCAP.KD.ZG_DS2_en_excel_v2_5455109 (gpd crescimento %).xlsx")
gpd_perc$iso3 <-as.factor(gpd_perc$iso3)
summary(gpd_perc)
gpd_perc_tallskinny <- nc::capture_melt_single(gpd_perc,
                                          year = list (
                                            "[0-9]{4}"),
                                          value.name = "gpd_perc"
)

rm(gpd_perc)




##------------------ COMPILANDO OS DADOS EM ÚNICA TABELA

names(demo)
names(export_tallskinny)
names(import_tallskinny)
names(gpd_perc_tallskinny)
names(gpd_tallskinny)


#criando uma coluna que individualiza pelo código do pais e ano
demo$key <- paste(demo$pop_iso3,demo$pop_year, sep= "-")
export_tallskinny$key <- paste(export_tallskinny$inc_iso3, export_tallskinny$year,sep= "-")
import_tallskinny$key <- paste(import_tallskinny$inc_iso3, import_tallskinny$year,sep= "-")
gpd_tallskinny$key <- paste(gpd_tallskinny$iso3, gpd_tallskinny$year, sep = "-")
gpd_perc_tallskinny$key <- paste(gpd_perc_tallskinny$iso3, gpd_perc_tallskinny$year, sep = "-")


# usando função merge para construir o banco master de dados demográficos e economicos    
support_1 <- merge(demo,export_tallskinny, by ="key")
support_2 <- merge(support_1, import_tallskinny, by="key")
support_3 <- merge(support_2, gpd_tallskinny, by = "key")
master_de <- merge(support_3, gpd_perc_tallskinny, by = "key")
rm(support_1, support_2,support_3)

##removendo as colunas que ficaram duplicadas pelo JOIN
names(master_de)
master_de <- subset(master_de, select = -c(iso3.y, inc_iso3.x, inc_iso3.y, iso3.x))
master_de <- subset(master_de, select = -c(imp_country.x, indicator_name.x, year.x, imp_country.y, indicator_name.y))
master_de <- subset(master_de, select = -c(indicator_name.y.1,year.y,country.x,indicator_name.x.1,year.x.1,year.y.1,country.y))
names(master_de)

# deixando a tabela master salva em excel
write.table(master_de, "20230513_MasterDE.csv",dec =",", sep=";")


##-------------------PARTE 2 - DADOS DE EMISSÕES ------------------

total_emissions <- read_excel("EDGARv7.0_FT2021_fossil_CO2_booklet_2022 (1).xlsx", 
                              sheet = "fossil_CO2_totals_by_country")
                              

totalemissions_tallskinny <- nc::capture_melt_single(total_emissions,
                                                year = list (
                                                  "[0-9]{4}"),
                                                value.name = "co2_emissions")


totalemissions_tallskinny$key <- paste(totalemissions_tallskinny$iso3,totalemissions_tallskinny$year, sep= "-")


master_de<-read.csv("20230513_MasterDE.csv",dec =",", sep=";")
master_de_2 <- merge(master_de, totalemissions_tallskinny, by ="key")
master_de_2 <- subset(master_de_2, select = -c(pop_year, pop_country,pop_iso3))
master_de_2 <- subset(master_de_2, select = -c(X))
master_de_2$year <- as.numeric(master_de_2$year)
master_de_2$iso3 <-as.factor(master_de_2$iso3)
summary(master_de_2)

ggplot(master_de_2, aes (x=gpd_dolar, y=co2_emissions, colour = year))+
  geom_point()

write.table(master_de, "20230520_MasterDE2.csv",dec =",", sep=";")


##-------------------PARTE 3 - Incluindo filtros geográficos para facilitar visualização ------------------

country_region <- read_excel("IGES_71_CLEAN.xlsx", 
                             sheet = "Planilha1 (2)")

master_de_3 <- merge (master_de_2, country_region, by ="iso3")

ggplot(master_de_3, aes (x=gpd_dolar, y=co2_emissions, colour = year))+
  geom_point()+
  facet_wrap(~region)


##-------------------PARTE 4 - VERIFICANDO DIVERGENCIAS DURANTE UNIAO DE TABELAS----------------------------
  
# 1- Contando o número de menções a cada país (a partir do código iso 3) nas duas operações de junção de tabelas 
dd_master <- master_de %>% 
  count(pop_iso3)%>%
  rename ("n.dd1" = "n")
dd_master$iso3 <- dd_master$pop_iso3

dd_master_2 <- master_de_2 %>%
  count(iso3) %>%
  rename ("n.dd2" = "n")

dd_master_3 <-master_de_3 %>%
  count(iso3)%>%
  rename ("n.dd3" = "n")


#2- Organizando estes dados em um dt único e calculando a diferença entre as menções na primera junção (perda1)
#   e na segunda junção (perda2)
dd<-join_all(list(dd_master, dd_master_2, dd_master_3), by ="iso3", type = "left")
dd$perda1 <- dd$n.dd1-dd$n.dd2
dd$perda2<-dd$n.dd2- dd$n.dd3


#3- Verificando a lista de países que tiveram variações de menção entre as junções e criando uma lista para excluí-los do db original
dd %>% 
  select(iso3,perda1, perda2)%>%
  filter(is.na(perda1)| is.na(perda2) | perda1 != 0 | perda2 !=0)%>%
  rename("País" = iso3, "Variação nos Dados operação 1" = perda1, "Variação nos Dados operação 2" = perda2)%>%
  kable() %>%
  kable_styling("striped", position="left")

paises_out <- dd %>% 
              filter(is.na(perda1)| is.na(perda2) | perda1 != 0 | perda2 !=0) %>%
              select(iso3)

#4 - removendo os países com divergências nas junções do master_de_3

master_de_4 <- master_de_4 %>%
               filter(!iso3%in%c("ABW","AGO","COG","HKG","KOR","MAC","MAR","MHL","NCL","PRI","PSE","SDN","SRB"))
               
##-------------------PARTE 5 - VERIFICANDO CONSISTENCIA NOS DADOS EM PERIODO DE TEMPO DEFINIDO

anos<-master_de_4 %>% 
  select(iso3, year)%>%
  group_by(iso3)%>%
  summarise(year = n())%>%
  rename("País" = iso3, "N°paises"=year)



  
ggplot(anos, aes(x = `N°paises`)) +
    geom_histogram(binwidth = 5, fill = "deeppink1", show.legend = FALSE) +
  xlab("Número de anos com dados")+
  ylab("Número de países")+
  theme_bw()

##Selecionado os dados apenas do intervalo de 30 anos - 1991 a 2021
master_de_5 <- master_de_4%>%
                filter(year >= 1991)
 
anos2<-master_de_5 %>% 
  select(region,iso3, year)%>%
  group_by(iso3,region)%>%
  summarise(year = n())
  

ggplot(anos2, aes(x = year)) +
  geom_histogram(binwidth = 5, fill = "deeppink1", show.legend = FALSE) +
  xlab("Número de anos com dados")+
  ylab("Número de países")+
  facet_wrap(~region)
  theme_bw()

##---------------------- SALVANDO PLANILHA FINAL PARA ANALISE ------------
write.table(master_de_5, "20230520_MasterDE5.csv",dec =",", sep=";")
  

##----------------Adição de variaveis a partir dos dados macro WDI
  wdi <- read.csv("20230702_WDI_CO2andFDIcsv")
  glimpse(wdi)
  
  
  ## Renomeando as colunas
  
  wdi <- wdi %>%
    rename(country = 1,
           country_code = 2,
           serie_name = 3,
           serie_code = 4)
  
  glimpse(wdi)
  
  wdi<-wdi[,c(1:4,11:42)]
  glimpse(wdi)
  wdi <- wdi %>%
    rename("1991" = 5,
           "1992" = 6,
           "1993" = 7,
           "1994" = 8,
           "1995" = 9,
           "1996" = 10,
           "1997" = 11,
           "1998" = 12,
           "1999" = 13,
           "2000" = 14,
           "2001" = 15,
           "2002" = 16,
           "2003" = 17,
           "2004" = 18,
           "2005" = 19,
           "2006" = 20,
           "2007" = 21,
           "2008" = 22,
           "2009" = 23,
           "2010" = 24,
           "2011" = 25,
           "2012" = 26,
           "2013" = 27,
           "2014" = 28,
           "2015" = 29,
           "2016" = 30,
           "2017" = 31,
           "2018" = 32,
           "2019" = 33,
           "2020" = 34,
           "2021" = 35,
           "2022" = 36
    )
  
  # Alterando dimensões de tabela para que tenha valores anuais
  
  glimpse(wdi)       
  unique(wdi$serie_name)
  
  wdi_tallskinny <- nc::capture_melt_single(wdi,
                                            year = list (
                                              "[0-9]{4}"),
                                            value.name = "serie_value"
  )                       
  
  glimpse(wdi_tallskinny)
  
  
  #removendo NA dos valores e alterando colunas chr para double
  
  wdi_tallskinny <- wdi_tallskinny %>% 
    mutate(serie_value = na_if(serie_value, ".."))%>%
    mutate(serie_value= as.double(serie_value),
           year = as.double(year))
  
  glimpse(wdi_tallskinny)
  
  wdi_tallskinny$key <- paste(wdi_tallskinny$country_code,wdi_tallskinny$year, sep= "-")
  
  #adicionando dados de emissões totais de CO2 via dados WDI
  
  wdi_co2 <-wdi_tallskinny %>%
    filter(serie_code=="EN.ATM.GHGT.KT.CE")%>%
    mutate(co2_eq_WDI = serie_value)
  wdi_co2 <- wdi_co2[,c(1,2,5,7,8)]           
  
  master_db2 <- left_join(master_db, wdi_co2, by = "key")
  
  
  #adicionando dados de emissões de investimentos via dados WDI
  
  wdi_fdi <-wdi_tallskinny %>%
    filter(serie_code=="BX.KLT.DINV.CD.WD")%>%
    mutate(FDI_WDI = serie_value)
  wdi_fdi <- wdi_fdi[,c(1,2,5,7,8)]           
  
  master_db3 <- left_join(master_db2, wdi_fdi, by = "key")
  
  master_db3 <- master_db3[,c(1:16,18,22,29)]
  names(master_db3)
  
  master_db3 <- master_db3%>%
    select(country.x, iso3, region, year.x, everything())
  master_db3 <- master_db3%>%
    rename(country = country.x,
           year = year.x)
  
  write.table(master_db3, "20230703_MasterDE6.csv",dec =",", sep=";")
  
  
masterdf<-read.csv(file = "20230703_MasterDE6.csv", header = TRUE, sep = ";", dec = ",")
masterdf$export_dolar <-(masterdf$export_dolar/1000000000) ##exportação em bilhões de doláres
masterdf$import_dolar <-(masterdf$import_dolar/1000000000) ##importação em bilhões de dólares
masterdf$gpd_dolar <-(masterdf$gpd_dolar/1000000000) ##gpd em bilhões de dólares
masterdf$pop_pop_million <-(masterdf$pop_pop_million/1000) ## população em milhões de habitantes - no DB está como milhares de habitantes
masterdf$co2percapita <- masterdf$co2_emissions/masterdf$pop_pop_million # ton CO2/pessoa
masterdf$co2pergpd <- masterdf$co2_emissions/masterdf$gpd_dolar # Mton CO2/bilhoes R$ > kg CO2/R$ GPD
summary(masterdf)
masterdf$co2_eq_WDI <- masterdf$co2_eq_WDI/1000 # MtonCO2 equivalente

md_2019 <- masterdf %>%
  filter(year == "2019")%>%
  filter(is.na(FDI_WDI) == FALSE)

md_2019 <- mutate(md_2019,
                  co2_emissions = NULL)



## including NDC data on MD_2019 TABLE ----------------------------------

ndc <- read_excel("IGES+NDC+Database_v7.7 (1) - Copia.xlsx", 
                  sheet = "NDC_Master_Sheet_TCCAna", col_types = c("text", 
                                                                   "text", "text", "numeric", "numeric", 
                                                                   "text", "text", "numeric", "numeric", 
                                                                   "text", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "text", "text", 
                                                                   "text", "text", "text", "text", "text", 
                                                                   "text"))

names(ndc)

ndc <- rename(ndc,
              "ndc_imp_st_y" = "implementation_start_year",
              "ndc_imp_end_y" = "implementation_end_year",
              "ndc_mitigation_type" = "mitigation_type",
              "ndc_bl_y" = "baseline_year",
              "ndc_tg_y" = "target_year",
              "ndc_type_cove" = "type_of_coverage",
              "ndc_financial_sup" = "financial_support_billions_usd")
      
names(ndc)

mdndc <- left_join(md_2019, ndc, by = "iso3")





names(mdndc)

mdndc <- rename(mdndc, "country" = 1, "region" = 3)
mdndc <- mutate(mdndc, 
                "country.y" = NULL,
                "region.y" = NULL,
                "pop_index" = NULL,
                "pop_variant" = NULL,
                "pop_country_loc" = NULL
                )
  
masterdf<-read.csv(file = "20230703_MasterDE6.csv", header = TRUE, sep = ";", dec = ",")
masterdf$export_dolar <-(masterdf$export_dolar/1000000000) ##exportação em bilhões de doláres
masterdf$import_dolar <-(masterdf$import_dolar/1000000000) ##importação em bilhões de dólares
masterdf$gpd_dolar <-(masterdf$gpd_dolar/1000000000) ##gpd em bilhões de dólares
masterdf$pop_pop_million <-(masterdf$pop_pop_million/1000) ## população em milhões de habitantes - no DB está como milhares de habitantes
masterdf$co2percapita <- masterdf$co2_emissions/masterdf$pop_pop_million # ton CO2/pessoa
masterdf$co2pergpd <- masterdf$co2_emissions/masterdf$gpd_dolar # Mton CO2/bilhoes R$ > kg CO2/R$ GPD
summary(masterdf)
masterdf$co2_eq_WDI <- masterdf$co2_eq_WDI/1000 # MtonCO2 equivalente

md_2019 <- masterdf %>%
  filter(year == "2019")%>%
  filter(is.na(FDI_WDI) == FALSE)

md_2019 <- mutate(md_2019,
                  co2_emissions = NULL)



## including NDC data on MD_2019 TABLE ----------------------------------

ndc <- read_excel("IGES+NDC+Database_v7.7 (1) - Copia.xlsx", 
                  sheet = "NDC_Master_Sheet_TCCAna", col_types = c("text", 
                                                                   "text", "text", "numeric", "numeric", 
                                                                   "text", "text", "numeric", "numeric", 
                                                                   "text", "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "text", "text", 
                                                                   "text", "text", "text", "text", "text", 
                                                                   "text"))

names(ndc)

ndc <- rename(ndc,
              "ndc_imp_st_y" = "implementation_start_year",
              "ndc_imp_end_y" = "implementation_end_year",
              "ndc_mitigation_type" = "mitigation_type",
              "ndc_bl_y" = "baseline_year",
              "ndc_tg_y" = "target_year",
              "ndc_type_cove" = "type_of_coverage",
              "ndc_financial_sup" = "financial_support_billions_usd")

names(ndc)

mdndc <- left_join(md_2019, ndc, by = "iso3")





names(mdndc)

mdndc <- rename(mdndc, "country" = 1, "region" = 3)
mdndc <- mutate(mdndc, 
                "country.y" = NULL,
                "region.y" = NULL,
                "pop_index" = NULL,
                "pop_variant" = NULL,
                "pop_country_loc" = NULL
)

view(mdndc)

write.table(mdndc, "20230723_MasterDE7.csv",dec =",", sep=";")

