library(rvest)
library(stringr)
library(xml2)
library(tidyverse)

# URL do site
url_1 <- 'http://comprasnet.gov.br/livre/Pregao/ata2.asp?co_no_uasg='
url_2 <- "&numprp="
url_3 <- "&codigoModalidade=5&f_lstSrp=T&f_Uf=&f_numPrp=0&f_codUasg="
url_4 <- "&f_codMod=5&f_tpPregao=E&f_lstICMS=T&f_dtAberturaIni=&f_dtAberturaFim="


#Coloca UASG e Número do Pregão
uasg <- "153173"
#("195016", "195011", "195005", "195001","195004","195006","195002","195003","195015")

numpreg <- as.character(seq(from = 12021, to = 42021, by = 10000))
df_final <- data.frame()

#Obtém a URL do Resultado por Fornecedor
url_df <- data.frame(url_pregao = character(), 
                     url_compra = character(), 
                     cod_pregao = character(), 
                     licitacao = character(), 
                     uasg = character(), 
                     recurso = factor(),
                     url_recurso = character(),
                     url_ata = character())

#Loop de consulta
for (i in numpreg) {
  for (u in uasg) {
    url <- paste0(url_1, u, url_2, i, url_3, u, url_4)
    loading <- read_html(url) %>% html_nodes(css = "span:nth-of-type(5)") %>% html_text()
    error <- read_html(url) %>% html_nodes(css = "td > label") %>% html_text()
    if (loading != " - (Decreto Nº 10.024/2019)" || is_empty(loading)) {
      # Caso o número da licitação seja inexistente, pula para o próximo número
      next
    }
    cod_pregao <- read_html(url) %>% html_nodes(xpath = "//*[@id='btnResultadoFornecr']")
    cod_pregao <- str_match(cod_pregao, 'resultadoFornecedor\\((\\d+)\\);"')[, 2]
    if (is_empty(cod_pregao)) {
      # Caso o número da licitação seja inexistente, pula para o próximo número
      next
      }
    url_fornecedor <- paste0("http://comprasnet.gov.br/livre/Pregao/FornecedorResultadoDecreto.asp?prgcod=",
                             cod_pregao, 
                             '&f_lstSrp=T&f_Uf=&f_numPrp=0&f_coduasg=', 
                             u, 
                             '&f_codMod=5&f_tpPregao=E&f_lstICMS=T&f_dtAberturaIni=&f_dtAberturaFim=')
    u_ata <- paste0('http://comprasnet.gov.br/livre/Pregao/AtaEletronico.asp?co_no_uasg=',
                      u,
                      '&&uasg=',
                      u,
                      '&numprp=',
                      i,
                      '&codigoModalidade=5&Seq=1&f_lstSrp=T&f_Uf=&f_numPrp=0&f_coduasg=',
                      u,
                      '&f_codMod=5&f_tpPregao=E&f_lstICMS=T&f_dtAberturaIni=&f_dtAberturaFim=')
    rec <- read_html(url) %>% html_nodes(., "#btnRecursos")
    if (is_empty(rec)) {
      r <- 'nao'
    } else {
      r <- 'sim'
      url_r <- "http://comprasnet.gov.br/livre/Pregao/VisualizarRecursosPregao.asp?prgCod="
      url_r <- paste0(url_r, cod_pregao)
    }
    url_df <- rbind(url_df, 
                    data.frame(url_pregao = url,
                               url_compra = url_fornecedor,
                               cod_pregao = cod_pregao, 
                               licitacao = i, 
                               uasg = u,
                               recurso = r,
                               url_recurso = url_r,
                               url_ata = u_ata))
  }
}



#Raspa a página de Resultado por Fornecedor
for (i in 1:nrow(url_df)) {
  teste <- url_df$url[i]
  #Obtém o CNPJ
  cnpj_vencedor <- teste %>%
    read_html() %>% 
    html_nodes(css = ".tex3 td[align='left']") %>% html_text() %>% trimws()
  if (length(cnpj_vencedor) == 0) {
    # Caso o número da licitação seja inexistente, pula para o próximo número
    print(paste0("deu ruim", "-", url_df$url[i]))
    next
  }
  print("deu bom")
  #Obtém a posição do CNPJ
  posicao_cnpj <- teste %>%
    read_html() %>% 
    html_nodes(css = ".tex3 td[align='left']") %>%  xml2::xml_path()
  posicao_cnpj <- gsub("^.*tr\\[(\\d+)\\].*$", "\\1", posicao_cnpj)
  
  #Obtém o objeto
  objeto <-  teste %>%
    read_html() %>%
    html_nodes(css = "tr.tex3:nth-of-type(n+3) td:nth-of-type(2)") %>% html_text() %>%
    keep(!grepl("R\\$", .))
  
  posicao_objeto <- teste %>%
    read_html() %>%
    html_nodes(css = "tr.tex3:nth-of-type(n+3) td:nth-of-type(2)") %>% xml2::xml_path()
  posicao_objeto <- gsub("^.*tr\\[(\\d+)\\].*$", "\\1", posicao_objeto) %>%
    as.numeric() %>%
    grep(pattern = "\\b\\d*[13579]\\b", value = TRUE)
  
  #Obtém a Unidade de Fornecimento
  un_forne <-  teste %>%
    read_html() %>%
    html_nodes(css = "tr.tex3:nth-of-type(n+3) td[align='center']:nth-of-type(3)") %>% html_text()
  
  #Obtém a Quantidade
  qnt <- teste %>%
    read_html() %>%
    html_nodes(css = "tr.tex3:nth-of-type(n+3) td:nth-of-type(4)") %>% html_text()
  
  #Obtém o Critério de Valor
  crit_valor <- teste %>%
    read_html() %>%
    html_nodes(css = "tr.tex3:nth-of-type(n+3) td:nth-of-type(5)") %>% html_text() %>%
    str_extract("\\d{1,3}(\\.\\d{3})*(,\\d+)?") %>%
    str_remove_all("\\.") %>%
    str_replace(",", ".")
  
  #Obtém o Valor Unitário
  vl_unit <- teste %>%
    read_html() %>%
    html_nodes(css = "td:nth-of-type(2) span") %>% html_text() %>%
    { if (any(grepl("R\\$", .))) . 
      else { teste %>% read_html() %>% html_nodes(css = "tr.tex3:nth-of-type(n+3) td:nth-of-type(6)") %>% html_text() %>% trimws()} } %>%
    str_extract("\\d{1,3}(\\.\\d{3})*(,\\d+)?") %>%
    str_remove_all("\\.") %>%
    str_replace(",", ".")

  #Obtém o Valor Global
  vl_global <- teste %>%
    read_html() %>%
    html_nodes(css = "td:nth-of-type(3) span") %>% html_text() %>%
    { if (any(grepl("R\\$", .))) . 
      else { teste %>% read_html() %>% html_nodes(css = "tr.tex3:nth-of-type(n+3) td:nth-of-type(7)") %>% html_text() %>% trimws() } } %>%
    str_extract("\\d{1,3}(\\.\\d{3})*(,\\d+)?") %>%
    str_remove_all("\\.") %>%
    str_replace(",", ".")
  
  #Obtém o Desconto
  desconto <- teste %>%
    read_html() %>%
    html_nodes(css = "tr.tex3:nth-of-type(n+3) td:nth-of-type(7)") %>% html_text() %>% trimws() %>%
    ifelse(grepl("%", .), ., NA)
  
  #Obtém descrição, com condição de que se ela não for existente ou estiver pedaços em brancos na licitação, seja colocado um campo NA
  descricao <- teste %>%
    read_html() %>%
    html_nodes(css = "span:nth-of-type(n+7)") %>% html_text() %>% keep(!grepl("Descrição Detalhada do Objeto Ofertado:", .))
  if (length(descricao) == 0) {
    descricao <- NA
  }
  if (length(descricao) != length(posicao_objeto)) {
    descricao <- NA
  }
  #Cria uma tabela com os dados raspados
  dados <- data.frame(
    objeto = objeto,
    posicao_objeto = posicao_objeto,
    un_forne = un_forne,
    qnt = qnt,
    crit_valor = crit_valor,
    vl_unit = vl_unit,
    vl_global = vl_global,
    desconto = desconto,
    descricao = descricao
  )
  #Tabela com os dados de CNPJ raspados
  dados_cnpj <- data.frame(
    cod_pregao = url_df$cod_pregao[i],
    licitacao = url_df$licitacao[i],
    cnpj_vencedor = cnpj_vencedor,
    posicao_cnpj = as.numeric(posicao_cnpj)
  )
  dados_cnpj <- dados_cnpj %>% separate(cnpj_vencedor, into = c("cnpj", "razao_social"), sep = " - ") 
  
  #Junta em uma tabela para identificar o CNPJ vencedort
  dados_cnpj$limite <- c(dados_cnpj$posicao_cnpj[-1], Inf)
  resultado <- merge(dados_cnpj, dados, by = NULL, all = TRUE) %>%
    filter(posicao_objeto >= posicao_cnpj & posicao_objeto < limite) %>%
    select(-limite)
  resultado <- as.data.frame(resultado)
  
  df_final <- rbind(df_final, resultado)
}










#Recursos 



#codigo_html <- "http://comprasnet.gov.br/livre/Pregao/VisualizarRecursosPregao.asp?prgCod=898971#"

html <- read_html(codigo_html)

# Extrai todos os elementos <a> com atributo onclick
print(elementos_a)
urls <- html_attr(elementos_a, "onclick") %>%
  str_extract_all("javascript:window.open\\('([^']+)'") %>%
  unlist()
urls <- str_extract(urls, "'([^']+)'")
urls <- str_replace_all(urls, "'", "")
cnpj_reclamante <- codigo_html %>%
  read_html() %>% 
  html_nodes(css = "tr:nth-of-type(n+14) td.tex3b") %>% html_text() %>% trimws() %>%
  keep(grepl("CNPJ", .))

posicao_cnpj_reclamante <- codigo_html %>%
  read_html() %>% 
  html_nodes(css = "tr:nth-of-type(n+14) td.tex3b") %>%  xml2::xml_path()
posicao_cnpj <- gsub("\\d+", posicao_cnpj)

