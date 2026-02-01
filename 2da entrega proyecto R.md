# importación y limpieza de mi Portafolio

## Objetivo

### Importar de excel de mi portafolio, limpiar y consolidar datos

### (ya que lo tengo en 2 broker) para dejar el dataset listo

### para el analisis financiero

library(readxl) 
library(dplyr) 
library(janitor) 
library(tidyquant) 
library(purrr)
library(ggplot2)
ls()

### Importación de Excel y preparación del archivo de portafolio

ruta_excel \<- "portafolio_proyecto.xlsx"

portafolio_raw \<- read_excel(ruta_excel) glimpse(portafolio_raw)

### Limpieza y preparación de datos

tickers \<- portafolio_raw %\>% clean_names()
%\>% transmute( ticker = toupper(activo), sector = industria_sector )

glimpse(tickers)

### consolidación de acciones duplicadas

tickers_unicos \<- tickers 
%\>% distinct(ticker, sector)

tickers_vector \<- tickers_unicos\$ticker length(tickers_vector)

### Descarga de precios

from \<- "2021-01-01" batch_size \<- 50

precios \<- tickers_vector %\>% split(ceiling(seq_along(.) / batch_size))
%\>% map_dfr(\~ tq_get(.x, from = from))

glimpse(precios)

### resumen de precios

resumen_precios \<- precios %\>% group_by(symbol) 
%\>% summarise( obs = n(), fecha_min = min(date), 
fecha_max = max(date), .groups = "drop" )

resumen_precios

### Retornos mensuales

retornos_mensuales \<- precios
%\>% group_by(symbol) 
%\>% tq_transmute( select = adjusted,
mutate_fun = periodReturn, 
period = "monthly", type = "log" )

glimpse(retornos_mensuales)

retornos_mensuales_limpios \<- retornos_mensuales
%\>% rename( retorno_mensual = monthly.returns ) 
%\>% filter(!is.na(retorno_mensual))

### Calculo estadisticos por acción

estadisticos_activos \<- retornos_mensuales_limpios
%\>% group_by(symbol)
%\>% summarise( retorno_promedio = mean(retorno_mensual), 
volatilidad = sd(retorno_mensual), 
observaciones = n(), 
.groups = "drop" ) glimpse(estadisticos_activos)

### Ranking de retorno/volatilidad

``` {
A partir de los precios ajustados de cada activo, se calcularon retornos 
logarítmicos mensuales.  Este enfoque permite capturar de mejor manera la 
variación porcentual continua de los precios, facilitando el análisis 
estadístico y la comparación entre activos con distintos niveles de precio.

Posteriormente, se estimaron estadísticos descriptivos por activo, tales como el 
retorno promedio mensual y la volatilidad, con el objetivo de caracterizar el
desempeño histórico y el nivel de riesgo 
asociado a cada instrumento del portafolio.
}
```

```{r ranking-retorno}
estadisticos_activos %>%
  arrange(desc(retorno_promedio))
```

```{r ranking-volatilidad}
estadisticos_activos %>%
  arrange(desc(volatilidad))
```
### Grafico riesgo v/s retorno
“El gráfico riesgo–retorno permite visualizar la relación entre el retorno 
promedio mensual y la volatilidad de cada activo del portafolio. Esta
representación facilita la identificación de activos con mayor nivel de riesgo y 
aquellos con un desempeño más estable, sirviendo como apoyo a la toma de 
decisiones y al análisis de diversificación.”

grafico_riesgo_retorno <- ggplot(
  estadisticos_activos,
  aes(
    x = volatilidad,
    y = retorno_promedio,
    label = symbol
  )
) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(
    title = "Relación Riesgo–Retorno de los Activos del Portafolio",
    x = "Volatilidad mensual",
    y = "Retorno promedio mensual"
  ) +
  theme_minimal()

grafico_riesgo_retorno

### correlación entre activos del portofalio

### Correlación entre activos

Con el objetivo de evaluar el grado de relación entre los retornos mensuales 
de los activos del portafolio, se calcula la matriz de correlación de Pearson. 
Este análisis permite identificar activos con movimientos similares
o divergentes en el tiempo, información clave para evaluar el nivel de
diversificación y el riesgo conjunto del portafolio.


retornos_wide <- retornos_mensuales_limpios %>%
  select(symbol, date, retorno_mensual) %>%
  tidyr::pivot_wider(
    names_from  = symbol,
    values_from = retorno_mensual
  )

glimpse(retornos_wide)

matriz_correlacion <- retornos_wide %>%
  select(-date) %>%
  cor(use = "complete.obs")

matriz_correlacion

library(ggcorrplot)

ggcorrplot(
  matriz_correlacion,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  lab_size = 2.5,
  title = "Matriz de correlación de retornos mensuales",
  ggtheme = theme_minimal()
)

### Value at Risk (VaR) del portafolio

Con el objetivo de cuantificar el riesgo agregado del portafolio, se calcula el
Value at Risk (VaR),
una medida que estima la pérdida máxima esperada bajo un determinado nivel de
confianza y horizonte temporal.

El VaR permite evaluar escenarios adversos de mercado y constituye una 
herramienta clave para la toma de decisiones informadas y el control del riesgo, 
reduciendo la influencia del sesgo emocional en el manejo del portafolio.

retornos_wide_limpios <- retornos_wide %>%

### Preparación de retornos para VaR

retornos_wide_limpios <- retornos_wide %>%
  na.omit()

str(retornos_wide_limpios)

### Pesos del portafolio (igual ponderación)

n_activos <- ncol(retornos_wide_limpios)

pesos <- rep(1 / n_activos, n_activos)

pesos

### Retorno mensual del portafolio

retornos_pf <- as.matrix(retornos_wide_limpios) %*% pesos

retornos_pf <- as.numeric(retornos_pf)

summary(retornos_pf)

### Value at Risk (VaR) histórico

alpha <- 0.95

VaR_historico <- quantile(retornos_pf, probs = 1 - alpha)

VaR_historico









