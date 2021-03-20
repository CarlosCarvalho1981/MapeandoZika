# MINI PROJETO 3 - MAPEANDO A OCORRÊNCIA DO VÍRUS ZIKA
# Projeto desenvolvido como exercício do curso Formação Cientista de Dados, da DataScience Academy
# https://www.datascienceacademy.com.br/bundles?bundle_id=formacao-cientista-de-dados
# Utilizando ggmap
# D. Kahle and H. Wickham. ggmap: Spatial Visualization
# with ggplot2. The R Journal, 5(1), 144-161. URL
# http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

# Mapeando a ocorrência do virus Zika no Brasil.
# Carlos E. Carvalho
# https://www.linkedin.com/in/carlos-carvalho-93204b13/

setwd("D:/CIENTISTA_DADOS/1_BIG_DATA_R_AZURE/CAP19")
getwd()

# Carregando os pacotes
#devtools::install_github("wch/webshot")
library(dplyr)
library(ggplot2)

# Listando os arquivos e gerando uma lista com os nomes
temp_files <- list.files(pattern = ".csv")
temp_files

# Carregando todos os arquivos em um único objeto
myfiles <- lapply(temp_files, read.csv)

# Resumo dos arquivos
str(myfiles,1)
lapply(myfiles, names)[1]
lapply(myfiles, head)[1:2]

# Organizando o shape dos dados
brasil <- do.call(rbind, myfiles)
View(brasil)

brasil <- brasil %>%
  mutate(report_date = as.Date(report_date))

# Visualizando o dataset
glimpse(brasil)

# Transformando o dataframe em uma tabela dplyr e eliminando as colunas 5 a 7
brasil <- brasil %>% select(-(6:7))

# Visualizando as 20 primeiras linhas
brasil %>% slice(1:20)

# Para cada report date temos 5 regiões
brasil %>% filter(location_type == "region")
brasil %>% filter(location_type == "region") %>%
  ggplot(aes(x = report_date, y = value, group = location, color = location)) +
  geom_line() +
  geom_point() +
  ggtitle("Casos de Zika por Região do Brasil")

# Separando as regiões e visualizando os dados 
region <- brasil %>%
  filter(location_type == "region")

region %>%
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") + 
  ylab("Número de Casos Reportados") + xlab("Região") + 
  ggtitle("Casos Reportados no Brasil")

region %>%
  slice(1:length(unique(region$location))) %>%
  arrange(desc(value)) %>%
  mutate(location = factor(location, levels = location, ordered = TRUE)) %>%
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") + 
  ylab("Número de Casos Reportados") + xlab("Região") + 
  ggtitle("Casos Reportados no Brasil")

# Obtendo localidade únicas
region %>%
  slice(1:length(unique(region$location)))

# Organizando as localidades únicas por quantidade de casos
region %>%
  slice(1:length(unique(region$location))) %>%
  arrange(desc(value))

# Criando variáveis do tipo fator
region %>%
  slice(1:length(unique(region$location))) %>%
  arrange(desc(value)) %>%
  mutate(location = factor(location, levels = location, ordered = TRUE)) %>%
  glimpse()

# Agrupando e sumarizando
brasil_totals <- brasil %>% filter(location == "Brazil")
region_totals <- region %>% filter(location_type == "region") %>%
  group_by(report_date,location) %>%
  summarize(tot = sum(value))

# Parametrizar os dados e remover as sumarizações
regvec <- vector()
length(regvec) <- nrow(brasil)
for(i in 1:nrow(brasil)){
  if(brasil[i,]$location_type != "region"){
    regvec[i] <- newlab
  }
  else{
    newlab <- brasil[i,]$location
    regvec[i] <- newlab
    
  }
}

# Agregando o vetor de regiões aos datafram brasil
statedf <- cbind(brasil, regvec)

# Eliminar o sumário de linhas por regiões do brasil
statedf <- statedf %>% filter(location != "Brazil")
statedf <- statedf %>% filter(location_type != "region")

# Gerar o total por regiões a partir dos dados transformados
statedf %>% group_by(report_date, regvec) %>%
  summarize(tot = sum(value)) -> totals

# Gerando os mapas de cada estado do Brasil
install.packages("ggmap")
library(ggmap)
statedf$location <- as.character(statedf$location)

register_google(key = "YOUR API KEY")

longlat <- geocode(unique(statedf$location)) %>%
  mutate(loc = unique(as.character(statedf$location)))

# Salvando os geocodes do dataframe e salvando em um novo dataframe
statedf %>% filter(as.character(report_date) == "2016-06-11") %>%
  group_by(location) %>% summarize(cases = sum(value)) %>%
  inner_join(longlat, by = c("location" = "loc")) %>%
  mutate(LatLon = paste(lat, lon, sep = ":")) -> formapping

# Visualizando os dados
head(formapping)

# Formatando a saída e gerando um novo dataframe chamad long_formappin
num_of_times_to_repeat <- formapping$cases
lon_formapping <- formapping[rep(seq_len(nrow(formapping)),
                                 num_of_times_to_repeat),]
# Visualizando os dados
head(lon_formapping)

# Instalando o pactoe leaflet
install.packages("leaflet")
library(leaflet)

# Gerando o mapa com o dataframe
leaflet(lon_formapping) %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())


citation("ggmap")












