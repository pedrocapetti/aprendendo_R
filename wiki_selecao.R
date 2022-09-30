library(rvest)
library(tidyverse)
library(xml2)
library(dplyr)
library(tidyverse)
library(data.table)

#Urls
url_1 <- "https://en.wikipedia.org/wiki/"
url_2 <- "_FIFA_World_Cup_squads"
ano <- c(2018,2014,2010,2006,2002,1998,1994,1990,1986,1982,1978,1974,1970,1966,1962,1958,1954,1950,1934,1930)

#Cria uma tabela
ta <- tibble()

#Raspa as informações
for (a in ano){
  print(a)
  theurl <- paste0(url_1, a, url_2)
  tabela <- theurl %>% xml2::read_html() %>% rvest::html_table()
  tabela <- keep(tabela, ~ nrow(.x) > 5) 
  nome <- theurl %>% read_html() %>% 
    html_nodes(xpath = "//h3") %>% 
    html_text()
  nome <- gsub("\\[[^][]*]", "", nome) 
  i <- nome %>% match(x = "Brazil")
  if(is.na(i) == TRUE){
    nome <- theurl %>% read_html() %>% 
      html_nodes(xpath = "//h2") %>% 
      html_text()
    nome <- gsub("\\[[^][]*]", "", nome)
    nome <- nome[-1]
    i <- nome %>% match(x = "Brazil")
    } 
  else {
    i = i
  }
  df_loop <- as.data.frame(tabela[i]) %>% mutate(pais = nome[i],
                                                 ano = a)
  df_loop <- df_loop %>% select("Player", "Date.of.birth..age.", "Caps", "Club", "pais", "ano")
  ta <- rbind(ta, df_loop)
}

#Limpa as informações
ta$dt_nascimento <- as.Date(str_extract(ta$Date.of.birth..age., "\\d{4}-\\d{2}-\\d{2}"), format="%Y-%m-%d")
ta$idade_convocacao <- as.numeric(str_sub(ta$Date.of.birth..age., - 3, - 2))
ta$Caps <- NULL
ta$Date.of.birth..age. <- NULL
colnames(ta)[1] <- "jogador"
colnames(ta)[2] <- "clube"
