library(readr)
library(dplyr)
library(lubridate)
library(tidygeocoder)
library(stringr)
library(jsonlite)

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
  filter(`Estado - Sigla` %in% c("SE", "AL", "BA")) |>
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
            "Regiao - Sigla",
            "Data da Coleta",
            "Nome da Rua",
            "Numero Rua",
            "Complemento",
            "Cep")) |> 
  rename(Estado = `Estado - Sigla`,
         Município = Municipio,
         Posto = Revenda,
         Preço = `Valor de Venda`) |> 
  relocate(Preço, .after = Posto) |>
  geocode(`Endereço resumido`, lat = latitude , long = longitude)



combustivel_n8n <- combustivel |> 
  select(Município, Combustível = Produto, Data, Preço) |> 
  group_by(Município, Combustível) |>
  filter(Data == max(Data)) |> 
  distinct() |> 
  slice_min(Preço) |> 
  mutate(Município = str_to_title(Município),
         Preço = format(Preço, decimal.mark = ",")) |> 
  mutate(
    Município = str_replace_all(
      Município,
      c("\\bDo\\b" = "do",
        "\\bDa\\b" = "da",
        "\\bDe\\b" = "de")
    )
  ) |> 
  mutate(Data = format(Data, "%d/%m/%Y")) |> 
  mutate(Combustível = case_when(
    Combustível == "DIESEL" ~ "Diesel",
    Combustível == "DIESEL S10" ~ "Diesel S10",
    Combustível == "ETANOL" ~ "Etanol",
    Combustível == "GASOLINA" ~ "Gasolina",
    Combustível == "GASOLINA ADITIVADA" ~ "Gasolina aditivada",
    TRUE ~ Combustível 
  ))


saveRDS(combustivel, 'data/combustivel.rds')

write_json(combustivel_n8n, "data/combustivel.json", pretty = TRUE, auto_unbox = TRUE)
