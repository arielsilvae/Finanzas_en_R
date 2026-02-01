#Proyecto de construcción y gestión de un portafolio de inversiones 

### carga de Librería

library(readxl) 
library(dplyr) 
library(janitor) 
library(tidyquant) 
library(purrr)
library(ggplot2)

### Importación de Excel y limpieza del portafolio

ruta_excel \<- "portafolio_proyecto.xlsx"
portafolio_raw \<- read_excel(ruta_excel) glimpse(portafolio_raw)

tickers \<- portafolio_raw %\>% clean_names()
%\>% transmute( ticker = toupper(activo), sector = industria_sector )

glimpse(tickers)

### consolidación de acciones duplicadas

tickers_unicos \<- tickers 
%\>% distinct(ticker, sector)

tickers_vector \<- tickers_unicos\$ticker length(tickers_vector)

### Descarga de precios historicos

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

### Analisís de retorno/volatilidad

estadisticos_activos %>%
  arrange(desc(retorno_promedio))

estadisticos_activos %>%
  arrange(desc(volatilidad))
  
### Grafico riesgo v/s retorno

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

retornos_wide_limpios <- retornos_wide %>%

### Preparación de retornos para VaR

retornos_wide <- retornos_mensuales %>%
  select(symbol, date, monthly.returns) %>%
  pivot_wider(
    names_from = symbol,
    values_from = monthly.returns
  )

glimpse(retornos_wide)

### Limpieza NA

retornos_wide_limpios <- retornos_wide %>%
  select(-date) %>%     # quitamos fecha
  na.omit()

str(retornos_wide_limpios)

### pesos del portafolio

retornos_wide_limpios <- retornos_wide %>%
  select(-date) %>%      # eliminar fecha si existe
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

str(retornos_wide_limpios)

n_activos <- ncol(retornos_wide_limpios)
pesos <- rep(1 / n_activos, n_activos)

length(pesos)
ncol(retornos_wide_limpios)

### Retornos mensuales del portafolio

retornos_pf <- as.matrix(retornos_wide_limpios) %*% matrix(pesos, ncol = 1)
retornos_pf <- as.numeric(retornos_pf)

summary(retornos_pf)

### Var 95%
# El VaR histórico estima la pérdida máxima esperada del portafolio
# bajo un nivel de confianza dado, utilizando la distribución empírica
# de los retornos históricos observados.

alpha <- 0.95
VaR_historico <- quantile(retornos_pf, probs = 1 - alpha)
VaR_historico

### VaR paramétrico (Normal)
# El VaR paramétrico asume que los retornos del portafolio siguen
# una distribución normal, estimando el riesgo extremo a partir
# de la media y desviación estándar de los retornos.

media_pf <- mean(retornos_pf)
sd_pf <- sd(retornos_pf)

VaR_parametrico <- qnorm(
  p = 1 - alpha,
  mean = media_pf,
  sd = sd_pf
)

VaR_parametrico

### VaR MonteCarlo
# El VaR Monte Carlo permite simular múltiples escenarios de retorno
# bajo supuestos estadísticos, capturando eventos extremos que
# no necesariamente ocurrieron en el historial observado.

set.seed(123)
n_sim <- 10000
retornos_simulados <- rnorm(
  n_sim,
  mean = media_pf,
  sd = sd_pf
)

VaR_montecarlo <- quantile(retornos_simulados, probs = 1 - alpha)

VaR_montecarlo

hist(
  retornos_simulados,
  breaks = 50,
  main = "Distribución simulada de retornos del portafolio",
  xlab = "Retorno mensual",
  col = "lightblue"
)

abline(v = VaR_montecarlo, col = "red", lwd = 2)

##Recomendación de herramienta de apoyo al rebalanceo de cartera de inversión

### Métrica simple de eficiencia riesgo-retorno

ranking_rebalanceo <- estadisticos_activos %>%
  mutate(
    ratio_riesgo_retorno = retorno_promedio / volatilidad
  ) %>%
  arrange(desc(ratio_riesgo_retorno))

ranking_rebalanceo

### Conclusión preliminar

´´´{En base al ranking riesgo–retorno, activos como NVDA, GOOGL y BCH presentan 
una mejor relación entre retorno esperado y volatilidad, lo que sugiere que
podrían incrementar su ponderación relativa en un eventual rebalanceo del
portafolio.

Por el contrario, activos con bajo ratio riesgo–retorno aportarían menor 
eficiencia marginal, siendo candidatos a mantener o reducir su exposición.}´´´
