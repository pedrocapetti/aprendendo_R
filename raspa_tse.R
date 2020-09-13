#Importa as bibliotecas 
library(tidyverse)
library(data.table)
library(rvest)
library(plyr)
library(rjson)

#Importa o nome dos partidos e cria uma lista
#Dispinível em http://www.tse.jus.br/partidos/filiacao-partidaria/relacao-de-filiados
nome_partidos <- fread("filiados_partido_2020.csv")
nome_partidos <- unique(nome_partidos$Partido)
nome_partidos <- tolower(nome_partidos)

#Cria uma lista com a sigla de todos os estados brasileiros
uf <- c("ac", "al", "am", "ap", "ba", "ce", "df", "es", "go", "ma", "mg", "ms", 
        "mt", "pa", "pb", "pe", "pi", "pr", "rj", "rn", "ro","rr","rs","sc","se","sp","to")


#Cria um loop para fazer requisição no sistema do TSE
#Exemplo padrão: http://agencia.tse.jus.br/estatistica/sead/eleitorado/filiados/uf/filiados_avante_al.zip
for(partido in nome_partidos){
  for(estado in uf) {
    arquivo <- paste0(partido,"_",estado,".zip")
    download.file(paste0("http://agencia.tse.jus.br/estatistica/sead/eleitorado/filiados/uf/filiados_",arquivo),arquivo, mode = "wb")}}
