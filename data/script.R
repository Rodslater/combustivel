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
  relocate(Preço, .after = Posto) 


combustivel_app <- combustivel |>
  filter(Estado %in% c("SE", "AL", "BA")) |>
  geocode(`Endereço resumido`, lat = latitude , long = longitude)


combustivel <- combustivel |> 
  rename(Combustível = Produto) |> 
  mutate(Município = str_to_title(Município)) |> 
  mutate(Combustível = case_when(
    Combustível == "DIESEL" ~ "Diesel",
    Combustível == "DIESEL S10" ~ "Diesel S10",
    Combustível == "ETANOL" ~ "Etanol",
    Combustível == "GASOLINA" ~ "Gasolina",
    Combustível == "GASOLINA ADITIVADA" ~ "Gasolina aditivada",
    TRUE ~ Combustível 
  ))

combustivel_SEALBA <- combustivel |> 
  filter(Estado %in% c("SE", "AL", "BA")) |>
  select(Município, Combustível, Data, Preço) |> 
  group_by(Município, Combustível) |>
  filter(Data == max(Data)) |> 
  distinct() |> 
  slice_min(Preço) |> 
  mutate(Combustível = factor(Combustível, levels = c(
    "Diesel", "Diesel S10", "Etanol", "Gasolina", "Gasolina aditivada", "GLP", "GNV"
  ))) |> 
  arrange(Município, Combustível, Data, Preço) |> 
  mutate(Combustível = as.character(Combustível))


combustivel_BR <- combustivel |> 
  select(Município, Combustível, Data, Preço) |> 
  group_by(Município, Combustível) |>
  filter(Data == max(Data)) |> 
  distinct() |> 
  slice_min(Preço) |> 
  mutate(Combustível = factor(Combustível, levels = c(
    "Diesel", "Diesel S10", "Etanol", "Gasolina", "Gasolina aditivada", "GLP", "GNV"
  ))) |> 
  arrange(Município, Combustível, Data, Preço) |> 
  mutate(Combustível = as.character(Combustível))

saveRDS(combustivel_app, 'data/combustivel.rds')

write_json(combustivel_SEALBA, "data/combustivel_SEALBA.json", pretty = TRUE, auto_unbox = TRUE)
write_json(combustivel_BR, "data/combustivel_BR.json", pretty = TRUE, auto_unbox = TRUE)
