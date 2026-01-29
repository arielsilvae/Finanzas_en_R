# Importación y limpieza de mi Portafolio

## Objetivo

### Importar de excel de mi portafolio, limpiar y consolidar datos 
### (ya que lo tengo en 2 broker) para dejar el dataset listo 
### para el analisis financiero

library(readxl)
library(dplyr)
library(janitor)
library(tidyquant)
library(purrr)

### Importación y preparación del archivo de portafolio

ruta_excel <- "portafolio_proyecto.xlsx"

portafolio_raw <- read_excel(ruta_excel)
glimpse(portafolio_raw)

### Limpieza y  preparación de datos
tickers <- portafolio_raw %>%
  clean_names() %>%
  transmute(
    ticker = toupper(activo),
    sector = industria_sector
  )

glimpse(tickers)
  
### consolidación de acciones duplicadas
tickers_unicos <- tickers %>%
  distinct(ticker, sector)

tickers_vector <- tickers_unicos$ticker
length(tickers_vector)

### Precios
from <- "2021-01-01"
batch_size <- 50

precios <- tickers_vector %>%
  split(ceiling(seq_along(.) / batch_size)) %>%
  map_dfr(~ tq_get(.x, from = from))

glimpse(precios)

### resumen de precios
resumen_precios <- precios %>%
  group_by(symbol) %>%
  summarise(
    obs = n(),
    fecha_min = min(date),
    fecha_max = max(date),
    .groups = "drop"
  )

resumen_precios

