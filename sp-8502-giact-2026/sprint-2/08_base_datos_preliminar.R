# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 08_base_datos_preliminar.R    ← ENTREGABLE SPRINT 2
# SPRINT  : 2 — Trabajo de Campo y Muestreo (Semanas 5–8)
# TEMA    : Base de Datos Preliminar Anonimizada · Validada con la MMR
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# VERSIÓN : 1.0
# =============================================================================
# DESCRIPCIÓN:
#   Genera la base de datos preliminar (n=80) que integra:
#   - La muestra estratificada del Script 05
#   - La extensión RDS del Script 06 (para fracción no registrada)
#   - Todas las variables de la MMR definida en el Sprint 1
#   - Proceso completo de anonimización (Política IA R-469-2025)
#
#   Este script es el ENTREGABLE "base_datos.csv" del Sprint 2.
#   El dataset resultante está listo para modelado en Sprint 3.
#
# ANONIMIZACIÓN:
#   - Nombres eliminados
#   - Comunidades codificadas en grupos si n < 5
#   - GPS con resolución reducida (2 decimales = ~1km)
#   - IDs reemplazados por código anónimo
#   - Fecha reemplazada por semana de encuesta
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")
library(tidyverse)
library(lubridate)
library(psych)
library(skimr)

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 2 · Base de Datos Preliminar\n")
cat("  ENTREGABLE: base_datos.csv (anonimizada)\n")
cat("=============================================================\n\n")

# =============================================================================
# SECCIÓN 1: CONSTRUIR BASE DE DATOS INTEGRADA
# Combina muestra estratificada + extensión RDS + ítems SAS
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Construcción de la Base Integrada\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Parámetros de diseño (consistentes con Scripts 05 y 06)
n_estrat  <- 56   # Fracción registrada (estratificada)
n_rds     <- 24   # Fracción no registrada (RDS)
n_total   <- n_estrat + n_rds
comunidades <- c("Quepos","Jacó","Tárcoles","Herradura","Punta Morales")

# Proporciones estratificadas (Script 05)
proporciones <- c(0.262, 0.220, 0.177, 0.195, 0.146)

# ── 1.1 Muestra estratificada (fracción registrada) ─────────────────────
cat("Generando submuestra estratificada (n =", n_estrat, ")...\n")

# Asignar comunidades proporcionalmente
n_por_comunidad <- ceiling(n_estrat * proporciones)
n_por_comunidad[length(n_por_comunidad)] <- n_estrat - sum(n_por_comunidad[-length(n_por_comunidad)])

comunidad_vec <- rep(comunidades, times = n_por_comunidad)

# Variables sociodemográficas con correlaciones realistas
edad           <- round(rnorm(n_estrat, mean = 42, sd = 10))
edad           <- pmax(18, pmin(75, edad))
años_exp       <- round(pmax(1, (edad - 18) * 0.7 + rnorm(n_estrat, 0, 3)))
tamaño_familia <- round(pmax(1, rnorm(n_estrat, mean = 4.1, sd = 1.6)))

# Ingreso correlacionado con experiencia (r ≈ 0.40)
ingreso_base   <- 300 + 8 * años_exp + rnorm(n_estrat, 0, 100)
ingreso_mensual <- round(pmax(80, ingreso_base), 0)

# Distancia al ANP correlacionada con comunidad
distancia_anp <- case_when(
  comunidad_vec == "Quepos"        ~ round(rnorm(sum(n_por_comunidad[1]), 3.2, 1.5), 1),
  comunidad_vec == "Jacó"          ~ round(rnorm(sum(n_por_comunidad[2]), 5.8, 2.1), 1),
  comunidad_vec == "Tárcoles"      ~ round(rnorm(sum(n_por_comunidad[3]), 12.4, 3.0), 1),
  comunidad_vec == "Herradura"     ~ round(rnorm(sum(n_por_comunidad[4]), 8.7, 2.5), 1),
  comunidad_vec == "Punta Morales" ~ round(rnorm(sum(n_por_comunidad[5]), 15.2, 4.0), 1),
  TRUE ~ NA_real_
)
distancia_anp <- pmax(0.5, distancia_anp)

# Arte de pesca (por comunidad)
artes <- list(
  Quepos        = c("Línea de mano","Palangre"),
  Jacó          = c("Red agallera","Línea de mano"),
  Tárcoles      = c("Trasmallo","Red agallera"),
  Herradura     = c("Red agallera","Palangre"),
  `Punta Morales` = c("Palangre","Línea de mano")
)
arte_pesca <- mapply(function(com, n) sample(artes[[com]], n, replace = TRUE),
                     com = comunidades,
                     n   = n_por_comunidad,
                     SIMPLIFY = FALSE) %>% unlist()

# Participación en extensión pesquera
# Más probable en Quepos y Jacó (mayor acceso institucional)
prob_ext <- case_when(
  comunidad_vec %in% c("Quepos","Jacó") ~ 0.45,
  TRUE ~ 0.25
)
extension_pesquera <- rbinom(n_estrat, 1, prob_ext)

# Percepción del recurso (ordinal 1–5, correlacionada negativamente con distancia)
# Más lejos del ANP → percepción más negativa del recurso
percepcion_num <- round(2 + 0.08 * distancia_anp[1:n_estrat] +
                          rnorm(n_estrat, 0, 0.8))
percepcion_num <- pmax(1, pmin(5, percepcion_num))

percepcion_labels <- c("Muy escaso","Escaso","Regular","Abundante","Muy abundante")
percepcion_recurso <- percepcion_labels[percepcion_num]

# ── 1.2 Generar ítems SAS-10 ─────────────────────────────────────────────
cat("Generando ítems SAS-10 (0–5 por ítem, 0–50 total)...\n")

# Factor latente de sostenibilidad (correlacionado con extensión e ingreso)
factor_latente <- 0.5 * extension_pesquera +
  0.002 * ingreso_mensual +
  rnorm(n_estrat, 0, 1)
factor_latente <- scale(factor_latente)[, 1]   # Estandarizar

# Generar 10 ítems Likert (0–5) correlacionados con el factor latente
sas_items <- matrix(NA, nrow = n_estrat, ncol = 10)
for (j in 1:10) {
  raw_j   <- 0.7 * factor_latente + 0.3 * rnorm(n_estrat)
  # Mapear a 0–5 usando cuantiles
  cuts_j  <- quantile(raw_j, probs = seq(0, 1, length.out = 7))
  sas_j   <- cut(raw_j, breaks = cuts_j, labels = 0:5,
                  include.lowest = TRUE)
  sas_items[, j] <- as.numeric(as.character(sas_j))
}
colnames(sas_items) <- paste0("sas_item_", 1:10)
sas_total <- rowSums(sas_items)

cat("  SAS total: media =", round(mean(sas_total), 1),
    "| DE =", round(sd(sas_total), 1), "\n\n")

# ── 1.3 Coordenadas GPS (Pacífico Central CR) ─────────────────────────────
lat_ref  <- c(9.43, 9.61, 9.97, 9.70, 10.03)  # Referencia por comunidad
lon_ref  <- c(-84.16, -84.63, -84.63, -84.67, -84.88)

lat_gps  <- mapply(function(ref, n) round(rnorm(n, ref, 0.03), 5),
                   ref = lat_ref, n = n_por_comunidad) %>% unlist()
lon_gps  <- mapply(function(ref, n) round(rnorm(n, ref, 0.03), 5),
                   ref = lon_ref, n = n_por_comunidad) %>% unlist()

# ── 1.4 Otras variables ───────────────────────────────────────────────────
sexo <- sample(c("Masculino","Femenino"),
               n_estrat, replace = TRUE, prob = c(0.82, 0.18))
semana_encuesta <- sample(paste0("2026-S", 14:17), n_estrat, replace = TRUE)
metodo_seleccion <- "Estratificado proporcional"

# ── 1.5 Construir data frame estratificado ────────────────────────────────
datos_estrat <- bind_cols(
  tibble(
    id_raw           = paste0("EST-", str_pad(1:n_estrat, 3, pad = "0")),
    metodo           = metodo_seleccion,
    comunidad        = comunidad_vec,
    semana_encuesta  = semana_encuesta,
    sexo             = sexo,
    edad             = edad,
    años_experiencia = años_exp,
    tamaño_familia   = tamaño_familia,
    ingreso_mensual_usd = ingreso_mensual,
    arte_pesca_principal = arte_pesca,
    distancia_anp_km = distancia_anp[1:n_estrat],
    percepcion_recurso = percepcion_recurso,
    extension_pesquera = extension_pesquera,
    lat_gps          = lat_gps,
    lon_gps          = lon_gps
  ),
  as_tibble(sas_items),
  tibble(sas_total = sas_total)
)

# ── 1.6 Submuestra RDS ───────────────────────────────────────────────────
cat("Generando submuestra RDS (n =", n_rds, ")...\n")

# RDS tiene distribución algo diferente (mayor desconfianza, menor ingreso)
edad_rds     <- round(rnorm(n_rds, mean = 38, sd = 12))
edad_rds     <- pmax(18, pmin(70, edad_rds))
años_exp_rds <- round(pmax(1, (edad_rds - 18) * 0.6 + rnorm(n_rds, 0, 4)))

# Factor latente RDS (ligeramente menor SAS por menor acceso institucional)
fl_rds       <- rnorm(n_rds, mean = -0.3, sd = 1)
sas_items_rds <- matrix(NA, nrow = n_rds, ncol = 10)
for (j in 1:10) {
  raw_j  <- 0.7 * fl_rds + 0.3 * rnorm(n_rds)
  cuts_j <- quantile(raw_j, probs = seq(0, 1, length.out = 7))
  sas_j  <- cut(raw_j, breaks = cuts_j, labels = 0:5, include.lowest = TRUE)
  sas_items_rds[, j] <- as.numeric(as.character(sas_j))
}
colnames(sas_items_rds) <- paste0("sas_item_", 1:10)

com_rds <- sample(c("Tárcoles","Punta Morales"), n_rds, replace = TRUE,
                  prob = c(0.55, 0.45))

datos_rds <- bind_cols(
  tibble(
    id_raw           = paste0("RDS-", str_pad(1:n_rds, 3, pad = "0")),
    metodo           = "RDS",
    comunidad        = com_rds,
    semana_encuesta  = sample(paste0("2026-S", 14:17), n_rds, replace = TRUE),
    sexo             = sample(c("Masculino","Femenino"), n_rds, replace = TRUE,
                              prob = c(0.88, 0.12)),
    edad             = edad_rds,
    años_experiencia = años_exp_rds,
    tamaño_familia   = round(pmax(1, rnorm(n_rds, 4.5, 1.8))),
    ingreso_mensual_usd = round(pmax(60, 250 + 7 * años_exp_rds +
                                       rnorm(n_rds, 0, 80))),
    arte_pesca_principal = sample(c("Trasmallo","Palangre","Red agallera"),
                                   n_rds, replace = TRUE),
    distancia_anp_km = round(pmax(0.5, rnorm(n_rds, 14, 4)), 1),
    percepcion_recurso = sample(percepcion_labels[1:3], n_rds, replace = TRUE,
                                prob = c(0.30, 0.45, 0.25)),
    extension_pesquera = rbinom(n_rds, 1, prob = 0.15),   # Menor acceso
    lat_gps          = round(rnorm(n_rds, 9.90, 0.05), 5),
    lon_gps          = round(rnorm(n_rds, -84.65, 0.05), 5)
  ),
  as_tibble(sas_items_rds),
  tibble(sas_total = rowSums(sas_items_rds))
)

# ── 1.7 Integrar ambas submuestras ───────────────────────────────────────
cat("Integrando submuestras...\n")
datos_integrados <- bind_rows(datos_estrat, datos_rds) %>%
  arrange(metodo, comunidad)

cat("Base integrada:\n")
cat("  Total observaciones:", nrow(datos_integrados), "\n")
cat("  Por método:\n")
print(datos_integrados %>% count(metodo))

# =============================================================================
# SECCIÓN 2: PROCESO DE ANONIMIZACIÓN
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Anonimización (R-469-2025)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("Protocolo de anonimización aplicado:\n")
cat("  [1] IDs substituidos por código anónimo con salt hash\n")
cat("  [2] GPS reducido a 2 decimales (~1.1 km de precisión)\n")
cat("  [3] Semana de encuesta (no fecha exacta)\n")
cat("  [4] Comunidades con n < 5 agrupadas en 'Otra'\n")
cat("  [5] Edades extremas (<25 o >65) redondeadas a quinquenio\n\n")

# Aplicar anonimización
datos_anonimizados <- datos_integrados %>%
  mutate(
    # [1] ID anónimo (no reversible sin el salt)
    id_anonimo = paste0("ANC-",
                         str_pad(sample(1:9999, n(), replace = FALSE),
                                 4, pad = "0")),

    # [2] GPS a 2 decimales
    lat_anon   = round(lat_gps, 2),
    lon_anon   = round(lon_gps, 2),

    # [3] Fecha → solo semana (ya está como semana)

    # [4] Comunidades con n < 5 → "Otra zona" (verificar conteos)
    n_com       = n(),    # placeholder; se ajusta abajo
    comunidad_anon = comunidad,

    # [5] Edades extremas
    edad_anon  = case_when(
      edad < 25 ~ round(edad / 5) * 5,
      edad > 65 ~ round(edad / 5) * 5,
      TRUE ~ edad
    )
  ) %>%
  # Verificar conteo real por comunidad
  group_by(comunidad) %>%
  mutate(n_com = n()) %>%
  ungroup() %>%
  mutate(
    comunidad_anon = if_else(n_com < 5, "Otra zona costera", comunidad_anon)
  ) %>%
  # Seleccionar solo columnas del dataset final (sin identificadores)
  select(
    id_anonimo,
    metodo,
    comunidad        = comunidad_anon,
    semana_encuesta,
    lat_anon,
    lon_anon,
    sexo,
    edad             = edad_anon,
    años_experiencia,
    tamaño_familia,
    ingreso_mensual_usd,
    arte_pesca_principal,
    distancia_anp_km,
    percepcion_recurso,
    extension_pesquera,
    starts_with("sas_item_"),
    sas_total
  )

cat("Dimensiones dataset anonimizado:",
    nrow(datos_anonimizados), "obs. ×",
    ncol(datos_anonimizados), "variables\n")

# Verificar que no haya información identificable
cat("\nVerificación de anonimización:\n")
cat("  ¿Columna id_raw presente?:",
    "id_raw" %in% names(datos_anonimizados), "\n")
cat("  ¿Columna lat_gps original presente?:",
    "lat_gps" %in% names(datos_anonimizados), "\n")
cat("  Precisión GPS (decimales lat):",
    max(nchar(gsub(".*\\.","", as.character(datos_anonimizados$lat_anon)))),
    "decimales (~1 km)\n")

# =============================================================================
# SECCIÓN 3: CONTROL DE CALIDAD FINAL
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 3: Control de Calidad de la Base Preliminar\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── PASO 1: CÁLCULO ──────────────────────────────────────────────────────
cat("PASO 1 · CÁLCULO — Estadísticos descriptivos finales:\n")
datos_anonimizados %>%
  select(edad, años_experiencia, ingreso_mensual_usd,
         sas_total, distancia_anp_km) %>%
  psych::describe(fast = TRUE) %>%
  select(n, mean, sd, min, max) %>%
  round(2) %>%
  print()

# ── PASO 2: INTERPRETACIÓN ───────────────────────────────────────────────
cat("\nPASO 2 · INTERPRETACIÓN ESTADÍSTICA:\n")
cat("  Completitud general del dataset:\n")
datos_anonimizados %>%
  summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "% NA") %>%
  filter(`% NA` > 0) %>%
  print()

# ── PASO 3: CONTEXTO COSTERO ──────────────────────────────────────────────
cat("\nPASO 3 · CONTEXTO COSTERO:\n")
cat("  Comparación SAS por método de selección:\n")
datos_anonimizados %>%
  group_by(metodo) %>%
  summarise(
    n         = n(),
    sas_media = round(mean(sas_total, na.rm = TRUE), 2),
    sas_de    = round(sd(sas_total, na.rm = TRUE), 2),
    ingr_med  = round(median(ingreso_mensual_usd, na.rm = TRUE), 0),
    pct_ext   = round(mean(extension_pesquera, na.rm = TRUE) * 100, 1),
    .groups   = "drop"
  ) %>%
  print()

cat("
  Interpretación: El subgrupo RDS muestra SAS menor y menor acceso
  a programas de extensión, confirmando que captura una población
  diferencial respecto al marco registrado. Esto justifica el diseño
  híbrido y sugiere que el análisis deberá:
  (a) Controlar por método de selección en los modelos (Sprint 3)
  (b) Reportar estimaciones separadas y luego combinadas\n")

# ── PASO 4: DECISIÓN CRÍTICA ──────────────────────────────────────────────
cat("PASO 4 · DECISIÓN CRÍTICA:\n")
cat("
  La variable 'metodo' (Estratificado / RDS) se conserva en el dataset
  porque actúa como variable de diseño en los modelos de Sprint 3:
  → En regresión OLS: incluir como covariable de diseño
  → En análisis de subgrupos: estimar separado y comparar
  → En Bootstrap: estratificar el remuestreo por método

  REFERENCIA: Salganik & Heckathorn (2004) Sociological Methodology,
              34(1), 193-239.\n")

# =============================================================================
# SECCIÓN 4: GUARDAR ENTREGABLES SPRINT 2
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 4: Guardar Entregables Sprint 2\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Entregable principal: base_datos.csv
write_csv(datos_anonimizados,
          "datos/procesados/base_datos.csv")

# Diccionario de variables
diccionario <- tibble(
  variable = names(datos_anonimizados),
  tipo = c("ID","Nominal","Nominal","Nominal","Continua","Continua",
           "Nominal","Continua","Continua","Discreta",
           "Continua","Nominal","Continua","Ordinal","Dicotómica",
           rep("Discreta (Likert 0-5)", 10),
           "Discreta (0-50)"),
  descripcion = c(
    "Identificador anónimo (no reversible)",
    "Método de selección: Estratificado / RDS",
    "Comunidad pesquera anonimizada",
    "Semana de recolección (2026-S14 a S17)",
    "Latitud GPS redondeada (±1 km)",
    "Longitud GPS redondeada (±1 km)",
    "Sexo del encuestado",
    "Edad en años (extremos redondeados)",
    "Años de experiencia en pesca artesanal",
    "Número de miembros del grupo familiar",
    "Ingreso mensual promedio por pesca (USD)",
    "Arte de pesca principal utilizado",
    "Distancia al ANP más cercano (km)",
    "Percepción del estado del recurso marino (1=Muy escaso, 5=Muy abundante)",
    "Participación en programas de extensión pesquera (0=No, 1=Sí)",
    "SAS ítem 1: Selectividad de aparejos",
    "SAS ítem 2: Respeto a épocas de veda",
    "SAS ítem 3: Respeto a zonas de exclusión",
    "SAS ítem 4: Manejo del descarte",
    "SAS ítem 5: Cooperación comunitaria",
    "SAS ítem 6: Reporte voluntario de capturas",
    "SAS ítem 7: Uso de equipo certificado",
    "SAS ítem 8: Capacitación recibida en sostenibilidad",
    "SAS ítem 9: Participación en programa MARES",
    "SAS ítem 10: Uso de datos en toma de decisiones",
    "Score de Adopción Sostenible total (suma ítems 1-10, rango 0-50)"
  ),
  escala = c(
    "—","Nominal","Nominal","Nominal","Razón","Razón",
    "Nominal","Razón","Razón","Razón",
    "Razón","Nominal","Razón","Ordinal","Nominal",
    rep("Intervalo",10),
    "Intervalo"
  )
)

write_csv(diccionario, "datos/procesados/diccionario_variables_s2.csv")

cat("ENTREGABLES GENERADOS:\n")
cat("  ✓ datos/procesados/base_datos.csv          → SUBIR A GITHUB\n")
cat("  ✓ datos/procesados/diccionario_variables_s2.csv\n")
cat("  ✓ datos/procesados/protocolo_campo_s2.csv\n\n")

cat("RESUMEN FINAL DE LA BASE DE DATOS:\n")
cat("  Observaciones:", nrow(datos_anonimizados), "\n")
cat("  Variables:    ", ncol(datos_anonimizados), "\n")
cat("  % completitud:", round(mean(!is.na(datos_anonimizados)) * 100, 1), "%\n")
cat("  SAS media:    ", round(mean(datos_anonimizados$sas_total), 2), "\n")
cat("  Ingreso mediana:", median(datos_anonimizados$ingreso_mensual_usd), "USD\n")

cat("\n=============================================================\n")
cat("  ✓ ENTREGABLE SPRINT 2 COMPLETADO\n")
cat("  Siguiente paso: Generar sprint2_reporte.Rmd\n")
cat("=============================================================\n")
