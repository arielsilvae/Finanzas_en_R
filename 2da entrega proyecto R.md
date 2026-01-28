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

ruta_excel <- "balance acciones.xlsx"

portafolio_raw <- read_excel(ruta_excel)
glimpse(portafolio_raw)

### Limpieza y  preparación de datos
portafolio <- portafolio_raw %>%
  clean_names() %>%
  select(
    ticker = activo,
    cantidad = total_de_acciones,
    precio_compra = precio_compra,
    sector = industria_sector
  ) %>%
  filter(!is.na(ticker)) %>%
  mutate(
    ticker = toupper(ticker),
    cantidad = as.numeric(cantidad),
    precio_compra = as.numeric(precio_compra)
  )

glimpse(portafolio)
  
### consolidación de acciones duplicadas

portafolio_consolidado <- portafolio %>%
  group_by(ticker, sector) %>%
  summarise(
    cantidad_total = sum(cantidad, na.rm = TRUE),
    precio_compra_prom = sum(cantidad * precio_compra, na.rm = TRUE) /
                          sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )
glimpse(portafolio_consolidado)

### preparación de tickers
tickers <- portafolio_consolidado$ticker

from <- "2021-01-01"
batch_size <- 50

precios <- tickers %>%
  split(ceiling(seq_along(.) / batch_size)) %>%
  map_dfr(~ tq_get(.x, from = from))

resumen_precios <- precios %>%
  group_by(symbol) %>%
  summarise(
    obs = n(),
    fecha_min = min(date),
    fecha_max = max(date),
    .groups = "drop"
  )

resumen_precios

