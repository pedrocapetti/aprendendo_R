#Importando bibliotecas
library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)
library(stringi)
library(cepespR)
library(geobr)

#Importa as planilhas relacionadas a abril e maio, disponibilizadas pela CGU
#Disponívei em https://www.gov.br/cgu/pt-br/assuntos/noticias/2020/06/portal-da-transparencia-divulga-lista-de-beneficiarios-do-auxilio-emergencial

auxilio_abril <- fread("D:/Documentos/auxilio_emergencial/202005_AuxilioEmergencial/202004_AuxilioEmergencial.csv", sep = ";", encoding = "Latin-1", 
                       select = c("MÊS DISPONIBILIZAÇÃO",
                                  "CÓDIGO MUNICÍPIO IBGE",
                                  "NOME MUNICÍPIO", 
                                  "UF",
                                  "VALOR BENEFÍCIO",
                                  "ENQUADRAMENTO",
                                  "PARCELA"))

auxilio_maio <- fread("D:/Documentos/auxilio_emergencial/202005_AuxilioEmergencial/202005_AuxilioEmergencial.csv", sep = ";", encoding = "Latin-1", 
                      select = c("MÊS DISPONIBILIZAÇÃO",
                                 "CÓDIGO MUNICÍPIO IBGE",
                                 "NOME MUNICÍPIO", 
                                 "UF",
                                 "VALOR BENEFÍCIO",
                                 "ENQUADRAMENTO",
                                 "PARCELA"))

#Junta as planilhas relacionadas ao auxílio
auxilio <- rbind(auxilio_maio, auxilio_abril)

#Vamos agora renomear as colunas da planilha
colnames(auxilio)[1] <- "MES"
colnames(auxilio)[2] <- "COD_IBGE"
colnames(auxilio)[3] <- "NOME_MUN"
colnames(auxilio)[4] <- "SIGLA_UF"
colnames(auxilio)[5] <- "VALOR"
colnames(auxilio)[6] <- "ENQUADRAMENTO"
colnames(auxilio)[7] <- "PARCELA"

#Transforma o valor recebido em numeric e exclui os beneficiários que não informaram a cidade de moradia
auxilio <- auxilio %>% mutate(VALOR = as.numeric(gsub(",00", "", VALOR))) %>% filter (COD_IBGE != "NA")

#Retorna o número de beneficiários por cidade e o valor pago total para essas pessoas
auxilio_cidade <- auxilio %>% 
  group_by(COD_IBGE) %>% 
  summarise(qnt_beneficio = n(), valor_pago = sum(VALOR))

#Importa a tabela de população do IBGE
pop <- fread("https://raw.githubusercontent.com/turicas/covid19-br/master/data/populacao-estimada-2019.csv", encoding = "UTF-8")

#Junta as tabelas de população e do auxílio emergencial
auxilio_pop <- left_join(auxilio_cidade, pop, by = c("COD_IBGE" = "city_ibge_code"))

#Criar df com benefício médio pago por município e a cobertura populacional da cidade
auxilio_df <- auxilio_pop %>% 
  mutate(benef_medio = valor_pago/qnt_beneficio, 
         cobertura = (qnt_beneficio/estimated_population) * 100) %>%
  mutate(benef_medio = round(benef_medio))


#Importando os votos da eleição presidencial de 2018, a partir do pacote CepespR, da FGV (https://github.com/Cepesp-Fgv/cepesp-r)
#Dados disponíveis no http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais
votos <- get_votes(year = 2018, position = "Presidente", regional_aggregation = "Municipio")

#Filtra os votos por municípios (excluindo os votos do exterior (UF = ZZ) e também a votação para o segundo turno)
#Cria uma coluna com a porcentagem de votos válidos de cada candidato
votos_seg <- votos %>% 
  select(NUM_TURNO, NUMERO_CANDIDATO, COD_MUN_IBGE, NOME_MUNICIPIO, UF, QTDE_VOTOS) %>% filter(NUM_TURNO == 2, UF != "ZZ") %>%
  group_by(COD_MUN_IBGE) %>% 
  mutate(TOTAL = sum(QTDE_VOTOS), 
         VOTOS_VAL = (QTDE_VOTOS/TOTAL*100))

#Junta a tabela de auxílio com a votação eleitoral
votos_auxilio <- left_join(votos_cidades, auxilio_df, by = c("COD_MUN_IBGE" = "COD_IBGE"))

#Filtra os votos do candidato Jair Bolsonaro (17)
votos_bolso <- votos_auxilio %>% filter(NUMERO_CANDIDATO == "17")

#Cria gráfico para ver alguma relação entre votos em 2018 e a cobertura atual do auxílio emergencial
ggplot(data = votos_bolso, aes(cobertura, VOTOS_VAL)) + geom_point() + geom_smooth(method="lm") + xlim(0, 60)

#Salva um CSV com os resultados
write.csv(votos_auxilio, "votos_auxilio.csv", row.names = FALSE)

#Cria categorias para a cobertura do Auxílio Emergencial
votos_bolso$cat_auxi <- cut(votos_bolso$cobertura, 
                            breaks = c(0, 10, 20, 30, 40, Inf),
                            include.lowest = TRUE, 
                            right = FALSE,
                            c("Até 10%", "Entre 10% a 20%","Entre 20% e 30%", "Entre 30% e 40%","Acima de 40%"))


#Cria categorias para a cobertura para votação de 2018 de Bolsonaro
votos_bolso$cat_votos <- cut(votos_bolso$VOTOS_VAL, 
                             breaks = c(0, 25, 50, 75, 100),
                             include.lowest = TRUE, 
                             right = FALSE,
                             c("Até 25%", "Entre 25% a 50%","Entre 50% e 75%", "Mais de 75%"))

#Vamos agora importar o shapefile de município e estado do pacote GeoBR
munic <- read_municipality(code_muni="all", year=2017)
state <- read_state(code_state="all", year=2017)

#Tira os axis dos gráficos para qualquer visualização
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

mapa <- right_join(munic, votos_bolso, by = c("code_muni" = "COD_MUN_IBGE"))

mapa_auxilio <- ggplot() +
  geom_sf(data = mapa, 
          aes(fill = cat_votos), 
          color= NA, 
          size=.0001) +
  scale_fill_brewer(palette = "Spectral") +
  theme_void() +
  no_axis + geom_sf(data=state, fill=NA, color="#151414")
