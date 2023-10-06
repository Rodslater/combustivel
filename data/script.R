library(readr)
library(dplyr)
library(lubridate)
library(tidygeocoder)

link_gasolina_etanol <- paste0(
  "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/",
  "arquivos/shpc/qus/ultimas-4-semanas-gasolina-etanol.csv")

link_diesel_gnv <- paste0(
  "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/",
  "arquivos/shpc/qus/ultimas-4-semanas-diesel-gnv.csv")

link_glp <- paste0(
  "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/",
  "arquivos/shpc/qus/ultimas-4-semanas-glp.csv")

gasolina_etanol <- read_csv2(link_gasolina_etanol)
diesel_gnv <- read_csv2(link_diesel_gnv)
glp <- read_csv2(link_glp)

combustivel <- gasolina_etanol |>
  full_join(diesel_gnv) |>
  full_join(glp)



combustivel <- combustivel |>
  filter(`Estado - Sigla` == "SE") |>
  mutate(Data = dmy(`Data da Coleta`),
         Complemento = ifelse(Complemento == 0, NA, Complemento),
         Endereço = ifelse(is.na(Complemento), 
                           paste0(`Nome da Rua`, ', ', `Numero Rua`, ', ', Bairro,', ', Municipio,'-', `Estado - Sigla`, ', ', Cep),
                           paste0(`Nome da Rua`, ', ', `Numero Rua`, ', ', Complemento, ', ', Bairro,', ', Municipio,'-', `Estado - Sigla`, ', ', Cep)),
         `Endereço resumido` = ifelse(is.na(Complemento),
                                      paste0(`Nome da Rua`, ', ', Bairro,', ', Municipio,'-', `Estado - Sigla`, ', BRASIL'),
                                      paste0(`Nome da Rua`, ', ', Complemento, ', ', Bairro,', ', Municipio,'-', `Estado - Sigla`, ', BRASIL'))) |>
  select(-c("Valor de Compra",
            "CNPJ da Revenda",
            "Unidade de Medida",
            "Estado - Sigla", 
            "Regiao - Sigla",
            "Data da Coleta",
            "Nome da Rua",
            "Numero Rua",
            "Complemento",
            "Cep")) |> 
  rename(Município = Municipio,
         Posto = Revenda,
         Preço = `Valor de Venda`) |> 
  relocate(Preço, .after = Posto)
  geocode(`Endereço resumido`, lat = latitude , long = longitude)
