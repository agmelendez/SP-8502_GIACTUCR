# Generar datos sintéticos (n=30) y guardarlos como data/raw/datos_crudos.csv
library(tidyverse)
library(here)
library(lubridate)

set.seed(8502)

n <- 30
comunidades <- c("Puerto Viejo","Cahuita","Manzanillo")
ocupaciones <- c("Pescador","Turismo","Comercio","Agricultura","Educacion")

df <- tibble(
  id = 1:n,
  fecha = as.Date("2026-03-12") + sample(0:14, n, replace = TRUE),
  comunidad = sample(comunidades, n, replace = TRUE, prob = c(0.35, 0.33, 0.32)),
  ocupacion = sample(ocupaciones, n, replace = TRUE),
  edad = sample(22:58, n, replace = TRUE),
  percepcion_erosion_1_5 = sample(1:5, n, replace = TRUE, prob = c(0.08, 0.15, 0.27, 0.30, 0.20)),
  visitas_playa_mes = pmax(0, round(rnorm(n, mean = 9, sd = 3))),
  ingreso_mensual_usd = pmax(250, round(rnorm(n, mean = 720, sd = 220))),
  participa_comite = sample(c("Si","No"), n, replace = TRUE, prob = c(0.45, 0.55))
) %>%
  mutate(
    # apoyo correlaciona débilmente con percepción de erosión (solo para hacerlo “realista”)
    apoya_medida_1_5 = pmin(5, pmax(1, round(percepcion_erosion_1_5 + rnorm(n, 0, 0.8))))
  ) %>%
  arrange(fecha, comunidad)

dir.create(here::here("data","raw"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(df, here::here("data","raw","datos_crudos.csv"))
df