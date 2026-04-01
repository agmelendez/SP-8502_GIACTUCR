# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 02_datos_sinteticos_costeros.R
# SPRINT  : 1 — Diseño y Fundamentos de Datos
# TEMA    : Generación de Dataset Sintético · Comunidades Pesqueras Costa Rica
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   Genera un dataset sintético de 80 observaciones que simula una encuesta
#   a pescadores artesanales del Pacífico Central de Costa Rica.
#   El dataset incluye errores y problemas deliberados (pedagógicos) que
#   el script 03_script_limpieza.R deberá detectar y corregir.
#
# CONTEXTO COSTERO:
#   Comunidades: Quepos, Jacó, Tárcoles, Herradura, Punta Morales
#   Área marina: Parque Nacional Manuel Antonio + corredor marino Pacífico Central
#
# NOTA SOBRE PRIVACIDAD Y DATOS SINTÉTICOS:
#   Datos completamente ficticios. No representan personas reales.
#   Uso de set.seed() garantiza reproducibilidad exacta.
#   Prohibido usar datos reales de comunidades en plataformas de IA públicas
#   (Política IA SP-8502, R-469-2025).
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("00_setup_ambiente.R")
library(tidyverse)
library(lubridate)

# Semilla para reproducibilidad
set.seed(8502)   # Número del curso como semilla → siempre el mismo resultado

cat("Generando dataset sintético de pescadores artesanales...\n\n")

# =============================================================================
# SECCIÓN 1: PARÁMETROS DE SIMULACIÓN
# =============================================================================
# Se definen primero los parámetros poblacionales → buena práctica estadística.
# Permite modificar la simulación en un solo lugar.

n           <- 80        # Tamaño de muestra objetivo
comunidades <- c("Quepos", "Jacó", "Tárcoles", "Herradura", "Punta Morales")
artes_pesca <- c("Línea de mano", "Red agallera", "Trasmallo", "Palangre", "Cerco")

# =============================================================================
# SECCIÓN 2: GENERAR DATASET CRUDO (CON ERRORES DELIBERADOS)
# =============================================================================
# ¡ADVERTENCIA PEDAGÓGICA!
# Este dataset contiene 8 tipos de problemas de calidad intencionales.
# El estudiante debe identificarlos en el script de limpieza (Script 03).
# =============================================================================

datos_crudos <- tibble(
  # ── Identificadores ──────────────────────────────────────────────────────
  id_encuesta = paste0("ENC-", str_pad(1:n, width = 3, pad = "0")),
  fecha_encuesta = sample(
    seq(as.Date("2026-03-01"), as.Date("2026-04-15"), by = "day"),
    n, replace = TRUE
  ),

  # ── Variables de ubicación ────────────────────────────────────────────────
  # ERROR 1: Inconsistencia en mayúsculas/minúsculas y nombres
  comunidad = sample(
    c("Quepos", "quepos", "QUEPOS",          # Error: misma comunidad, 3 formas
      "Jacó", "Jaco", "jacó",               # Error: acento inconsistente
      "Tárcoles", "Tarcoles",
      "Herradura",
      "Punta Morales", "PuntaMorales"),      # Error: sin espacio
    n, replace = TRUE,
    prob = c(0.10, 0.05, 0.05,
             0.10, 0.05, 0.05,
             0.10, 0.10,
             0.10,
             0.15, 0.15)
  ),

  latitud  = round(runif(n, min = 9.40, max = 10.20), 5),   # Pacífico Central CR
  longitud = round(runif(n, min = -85.0, max = -84.5), 5),

  # ── Variables sociodemográficas ───────────────────────────────────────────
  edad = c(
    round(rnorm(n - 3, mean = 42, sd = 12)),   # Distribución realista
    -5, 150, NA                                 # ERROR 2: valores imposibles + NA
  ) %>% sample(n),

  sexo = sample(
    c("Masculino", "Femenino", "M", "F", "masculino", NA),   # ERROR 3: inconsistencia
    n, replace = TRUE,
    prob = c(0.35, 0.20, 0.20, 0.10, 0.10, 0.05)
  ),

  años_experiencia = round(pmax(0, rnorm(n, mean = 18, sd = 8))),
  tamaño_familia   = round(pmax(1, rnorm(n, mean = 4.2, sd = 1.8))),

  # ── Variables económicas ──────────────────────────────────────────────────
  # ERROR 4: Ingreso con outlier extremo y codificación especial
  ingreso_mensual_usd = c(
    round(rnorm(n - 4, mean = 480, sd = 150), 2),  # Distribución realista
    9999.99,   # ERROR: código de "no sabe/no responde" sin documentar
    -50.00,    # ERROR: valor negativo imposible
    0,         # Posible 0 real o error
    NA         # Dato faltante legítimo
  ) %>% sample(n),

  # ── Arte de pesca ──────────────────────────────────────────────────────────
  # ERROR 5: Mezcla de categorías en español/inglés
  arte_pesca_principal = sample(
    c("Línea de mano", "Red agallera", "Trasmallo",
      "Palangre", "long line",         # ERROR: inglés
      "Cerco", "cerco",                # ERROR: mayúscula
      "línea de mano"),                # ERROR: minúscula
    n, replace = TRUE
  ),

  # ── Variables ambientales ─────────────────────────────────────────────────
  distancia_anp_km = round(pmax(0.1, rnorm(n, mean = 12, sd = 6)), 1),

  percepcion_recurso = sample(
    c("Muy abundante", "Abundante", "Regular", "Escaso", "Muy escaso",
      "No sé", "NS/NR"),   # ERROR 6: dos formas de no respuesta
    n, replace = TRUE,
    prob = c(0.05, 0.15, 0.30, 0.35, 0.10, 0.03, 0.02)
  ),

  # ── Score de Adopción Sostenible (SAS) ───────────────────────────────────
  # Escala 0–50 (suma de 10 ítems Likert 0–5)
  # ERROR 7: algunos valores fuera de rango
  sas_total = c(
    round(rnorm(n - 4, mean = 28, sd = 9)),   # Escala válida 0–50
    55, 60, -3, NA                             # ERROR: fuera de rango + NA
  ) %>% sample(n) %>% pmin(70) %>% pmax(-10), # Permitir salir del rango

  # ── Participación en programas de extensión ───────────────────────────────
  extension_pesquera = sample(
    c("Sí", "No", "SI", "NO", "si", "1", "0", NA),   # ERROR 8: inconsistencia
    n, replace = TRUE,
    prob = c(0.25, 0.45, 0.08, 0.08, 0.05, 0.04, 0.04, 0.01)
  ),

  # ── Columnas irrelevantes (ruido) ─────────────────────────────────────────
  columna_vacia       = NA_character_,               # ERROR 9: columna completamente NA
  numero_encuestador  = sample(1:5, n, replace = TRUE),
  observaciones_libres = sample(
    c(NA, "pescador cooperativo", "no quiso responder ingreso",
      "posible error de campo", "revisar dato de edad"),
    n, replace = TRUE
  )
)

# =============================================================================
# SECCIÓN 3: INSPECCIÓN INICIAL DEL DATASET CRUDO
# =============================================================================

cat("=============================================================\n")
cat("INSPECCIÓN INICIAL — Dataset Crudo (antes de limpiar)\n")
cat("=============================================================\n\n")

cat("Dimensiones:", nrow(datos_crudos), "filas ×", ncol(datos_crudos), "columnas\n\n")

cat("Primeras 6 filas:\n")
print(head(datos_crudos))

cat("\n\nResumen de valores faltantes por variable:\n")
datos_crudos %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_NA") %>%
  filter(n_NA > 0) %>%
  arrange(desc(n_NA)) %>%
  print()

cat("\n\nUso de memoria:\n")
cat("  Dataset crudo:", format(object.size(datos_crudos), units = "Kb"), "\n")

# =============================================================================
# SECCIÓN 4: GUARDAR DATASET CRUDO
# =============================================================================

write_csv(datos_crudos, "datos/crudos/pescadores_pacifico_central_CRUDO.csv")
cat("\n✓ Dataset crudo guardado en: datos/crudos/pescadores_pacifico_central_CRUDO.csv\n")

# =============================================================================
# SECCIÓN 5: CATÁLOGO DE ERRORES PEDAGÓGICOS
# =============================================================================
# Este catálogo documenta los errores deliberados para discusión en clase.
# En un proyecto real, este catálogo vendría del proceso de campo.

catalogo_errores <- tibble(
  num_error    = paste0("E", 1:9),
  variable_afectada = c(
    "comunidad", "edad", "sexo",
    "ingreso_mensual_usd", "arte_pesca_principal",
    "percepcion_recurso", "sas_total",
    "extension_pesquera", "columna_vacia"
  ),
  tipo_error = c(
    "Inconsistencia léxica (caps/tildes)",
    "Valores imposibles y outliers extremos",
    "Heterogeneidad en codificación categórica",
    "Código especial no documentado + negativo",
    "Mezcla de idiomas en categorías",
    "Duplicidad de categoría 'no respuesta'",
    "Valores fuera de rango lógico de escala",
    "Múltiples codificaciones de variable binaria",
    "Columna completamente vacía (ruido)"
  ),
  impacto_potencial = c(
    "Duplicación de grupos en análisis descriptivo",
    "Sesgos en media, sd; afecta regresión",
    "Colapso incorrecto de categorías",
    "Sobreestimación/subestimación del ingreso promedio",
    "Frecuencias fragmentadas por inconsistencia",
    "Ambigüedad en tratamiento de missings",
    "Violación de supuestos de la escala Likert",
    "Factor binario mal construido en regresión",
    "Desperdicio de memoria; confusión analítica"
  ),
  tecnica_correccion = c(
    "str_to_title() + case_when() para unificación",
    "filter() con límites lógicos + imputación",
    "fct_collapse() o recode()",
    "Recodificar 9999.99 a NA; eliminar negativos",
    "case_when() con traducción de términos en inglés",
    "fct_collapse() unificando 'No sé' y 'NS/NR'",
    "Winsorización o eliminación según justificación",
    "case_when() mapeando todas las variantes a Sí/No",
    "select(-columna_vacia)"
  )
)

print(catalogo_errores, n = Inf)

write_csv(catalogo_errores, "datos/procesados/catalogo_errores_pedagogicos.csv")
cat("\n✓ Catálogo de errores guardado en: datos/procesados/catalogo_errores_pedagogicos.csv\n")

cat("\n=============================================================\n")
cat("  ✓ Script 02 completado. Dataset crudo generado.\n")
cat("  Errores deliberados: 9 tipos en 9 variables.\n")
cat("  Siguiente paso: ejecutar 03_script_limpieza.R\n")
cat("=============================================================\n")
