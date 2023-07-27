#Carrega as bibliotecas
library(rvest)
library(data.table)
library(tidyverse)
library(xml2)
library(magrittr)
library(httr)
library(stringr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyverse)

#Obtém a lista de deputados da API
legis <- c('57','56','55','54')

df <- data.table()

for (l in legis){
  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/deputados?idLegislatura=", l)
  print(url)
  response <- GET(url)
  xml_data <- fromJSON(content(response, "text"), flatten = TRUE)
  df_deputado <- xml_data$dados
  df <- rbind(df, df_deputado)
}

lista_id <- unique(df$id)

#Cria df final com os dados
final_df <- data.frame(
  id = character(),
  ano = integer(),
  partido = character(),
  nome = character(),
  funcao = character(),
  cargo = character(),
  link = character(),
  stringsAsFactors = FALSE
)


#Lista com os anos 
lista_ano <- c(2011:2023)

# Get the total number of iterations for the progress bar
total_iterations <- length(lista_id) * length(lista_ano)

# Create the progress bar
progress_bar <- txtProgressBar(min = 0, max = total_iterations, style = 3)

# Initialize counter variable for the progress
progress_counter <- 0


#Loop
for(i in lista_id) {
  for (y in lista_ano){
    arquivo <- paste0("https://www.camara.leg.br/deputados/", i, "/pessoal-gabinete?ano=", y)
  a <- http_status(GET(arquivo))
  if(a$category[[1]] == "Success"){
    
    # Increment the progress counter
    progress_counter <- progress_counter + 1
    
    # Update the progress bar
    setTxtProgressBar(progress_bar, progress_counter)
    
    nome <- arquivo %>% read_html() %>%
      html_nodes(css = "td.col-md-5") %>% html_text()
    
    funcao <- arquivo %>% read_html() %>%
      html_nodes(css = "td.alinhar-esquerda.col-md-2") %>% html_text()
    
    cargo <- arquivo %>% read_html() %>%
      html_nodes(css = "td.col-md-1") %>% html_text()
    
    periodo <-  arquivo %>% read_html() %>%
      html_nodes(css = "td:nth-of-type(4)") %>% html_text()
    
    link <-  arquivo %>% read_html() %>%
      html_nodes("td.text-center") %>%
      xml2::xml_find_all("a") %>% html_attr("href")
    
    id <- rep(i, length.out = length(nome))
    ano <- rep(y, length.out = length(nome))
    
    final_df <- rbind(final_df, 
                   data.frame(
                     id = id,
                     ano = ano,
                     nome = nome,
                     funcao = funcao,
                     cargo = cargo,
                     periodo = periodo,
                     link = link
                   ))
  }
  
  else{
    next
  }
  }
  Sys.sleep(2)
}





#Precisa reescrever daqui pra baixo






#Tirar as duplicatas
dados <- dados %>% select(nome, funcao, cargo, período, identif, party) %>% distinct()

#Agora vamos limpar as colunas de datas
dados$lenght <- str_remove_all(dados$período, "[a-zA-Z]")
dados$lenght <- str_trim(dados$lenght)

#Cria coluna de data de início e fim
dados$inicio <- substr(dados$lenght, 1, 10)
dados$fim <- substr(dados$lenght, 13, 23)

#Transformando em data
dados$inicio <- as.Date(dados$inicio, "%d/%m/%Y")
dados$fim <- as.Date(dados$fim, "%d/%m/%Y")

#Para termos uma ideia de quantos dias, vamos colocar a data de hoje para simularmo
today <- Sys.Date()
dados$fim <- replace_na(dados$fim, today)
dados$fim <- as.Date(dados$fim, "%d/%m/%Y")

dados$cont_dat <- as.Date(dados$fim) - as.Date(dados$inicio)

dados <- dados %>% distinct(nome, funcao, cargo, período, identif, lenght, inicio, fim, cont_dat)

#Atual legislatura
deputado_atual <- rbind(deputados_20)

#Filtra
deputado_atual <- deputado_atual %>% 
  select(txNomeParlamentar, ideCadastro, codLegislatura, sgUF, sgPartido, nuLegislatura) %>%
  filter(sgUF != "NA" & nuLegislatura == "2019") %>% distinct() %>% select(txNomeParlamentar, ideCadastro, sgUF, sgPartido)

#Pega apenas os deputados dessa legislatura

dep_leg <- inner_join(dados, deputado_atual, by = c("identif" = "ideCadastro"))

dep <- fread("dep_leg.csv", encoding = "Latin-1")


dep$nivel <- as.numeric(substr(dep$cargo, 3, 4))

dep <- dep %>% filter(nivel != 'NA')




#Teste
teste <- dep %>% group_by(nome, txNomeParlamentar, sgPartido) %>% summarise(abc = length(unique(cargo))) %>% arrange(desc(abc))


pedido_lai <- teste %>% filter(abc > 5)

write.csv(pedido_lai, "pedido_lai.csv", row.names = FALSE)

mais_trocas <- teste %>% group_by(txNomeParlamentar, sgPartido) %>% summarise(soma = sum(abc))

median(mais_trocas$soma)
