library(readr)
library(dplyr)
library(lubridate)

link_gasolina_etanol <- paste0(
  "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/",
  "arquivos/shpc/qus/ultimas-4-semanas-gasolina-etanol.csv")

link_diesel_gnv <- paste0(
  "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/",
  "arquivos/shpc/qus/ultimas-4-semanas-diesel-gnv.csv")

gasolina_etanol <- read_csv2(link_gasolina_etanol)
diesel_gnv <- read_csv2(link_diesel_gnv)

combustivel <- full_join(gasolina_etanol, diesel_gnv)


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
            "Unidade de Medida",
            "Estado - Sigla", 
            "Regiao - Sigla",
            "Data da Coleta",
            "Nome da Rua",
            "Numero Rua",
            "Complemento",
            "Cep")) |> 
  rename(Município = Municipio)

saveRDS(combustivel, 'combustivel.rds')
