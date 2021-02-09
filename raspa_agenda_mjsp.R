library(rvest)

urlbase <- "https://www.gov.br/defesa/pt-br/acesso-a-informacao/agenda-de-autoridades/agenda-do-ministro/xxxxxxxxxx"
datas <- as.Date(as.Date("2020-01-01"):as.Date("2020-12-31"), origin="1970-01-01")
datas <- as.character.Date(datas)

dados <- tribble(
  ~agenda, ~data, ~titulo, ~inicio, ~local)

for (i in datas){
  url <- gsub("xxxxxxxxxx", i, urlbase)
  b <- http_status(GET(url))
  if(b$category[[1]] == "Success"){
    print(url)
    d <- i
    a <- url %>% 
      read_html() %>% 
      html_nodes(xpath = "//h4") %>% 
      html_text()
    tryCatch({
      i <- url %>% 
        read_html() %>% 
        html_nodes(css = ".compromisso-inicio") %>% 
        html_text()  
      t <- url %>% 
        read_html() %>% 
        html_nodes(css = ".compromisso-titulo") %>% 
        html_text()
      l <- url %>% 
        read_html() %>% 
        html_nodes(css = ".compromisso-local") %>% 
        html_text()
      dados %<>% add_row(agenda = a,
                         data = d,
                         titulo = t,
                         inicio = i,
                         local = l)},
      error = function(e){
      })}
  else {
  }
}