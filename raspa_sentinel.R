library(rvest)
library(data.table)
library(tidyverse)
library(xml2)
library(magrittr)

urlbase <- "https://botsentinel.com/trending-topics/top-hashtags?"
mes <- c(5:9)
dia <- c(1:31)
hora <- 23
dados <- data.frame()


for(d in dia){
    for(m in mes) {
      for(h in hora) {
        arquivo <- paste0(urlbase,"m=",m,"&d=",d,"&y=2020&h=",h,"&options=aggregate")
        data <- d
        month <- m
        tabela <- arquivo %>% xml2::read_html() %>% rvest::html_table()
        tabela <- as.data.frame(tabela) %>% mutate(day = data, months = month)
        dados <- rbind(dados, tabela)
        print(arquivo)
      }
    }
}

write.csv(dados, "dados_botsentinel.csv", row.names = FALSE)
