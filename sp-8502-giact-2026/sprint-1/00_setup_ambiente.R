# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 00_setup_ambiente.R
# SPRINT  : 1 — Diseño y Fundamentos de Datos (Semanas 1–4)
# AUTOR   : MSI. Agustín Gómez Meléndez · CIOdD · UCR
# VERSIÓN : 1.0 · Marzo 2026
# MARCO   : Resolución R-469-2025 · UCR
# =============================================================================
# PROPÓSITO:
#   Instalar y cargar todos los paquetes necesarios para el curso SP-8502.
#   Ejecutar ANTES de cualquier otro script del Sprint 1.
#   Verificar que el entorno R quede correctamente configurado.
# =============================================================================

# ── 0. Información del entorno ────────────────────────────────────────────────
cat("=============================================================\n")
cat("  SP-8502 · Setup del Ambiente R · Sprint 1\n")
cat("  GIACT · Universidad de Costa Rica · 2026-I\n")
cat("=============================================================\n\n")
cat("Versión de R:", R.version.string, "\n")
cat("Directorio de trabajo:", getwd(), "\n\n")

# ── 1. Función auxiliar: instalar si no existe ────────────────────────────────
instalar_si_falta <- function(pkgs) {
  nuevos <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(nuevos) > 0) {
    message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cran.rstudio.com/")
  } else {
    message("✓ Todos los paquetes ya están instalados.")
  }
}

# ── 2. Paquetes del curso ──────────────────────────────────────────────────────
# Organizados por sprint/módulo temático

paquetes_core <- c(
  # Núcleo Tidyverse (Sprint 1 y todos los sprints)
  "tidyverse",   # dplyr, ggplot2, tidyr, readr, stringr, forcats, purrr
  "lubridate",   # Manejo de fechas
  "janitor",     # Limpieza de nombres de columnas y tablas de frecuencia
  "skimr",       # Resumen estadístico rápido y elegante
  "here"         # Rutas relativas robustas al proyecto
)

paquetes_datos <- c(
  # Importación y manipulación de datos (Sprint 1 y 2)
  "readxl",      # Leer archivos Excel (.xlsx)
  "writexl",     # Escribir archivos Excel
  "haven",       # Importar SPSS, Stata, SAS
  "jsonlite",    # Leer/escribir JSON
  "openxlsx"    # Excel avanzado con formato
)

paquetes_estadistica <- c(
  # Estadística y modelos (Sprint 2, 3 y 4)
  "psych",       # Estadística descriptiva avanzada, alpha de Cronbach
  "nortest",     # Tests de normalidad (Lilliefors, Anderson-Darling)
  "car",         # Regression diagnostics, VIF
  "lmtest",      # Tests sobre modelos lineales
  "sandwich",    # Errores estándar robustos
  "broom"        # Tidying model outputs (tidy, glance, augment)
)

paquetes_viz <- c(
  # Visualización (Sprint 4 principalmente)
  "ggplot2",     # Ya en tidyverse, listado explícito para claridad
  "scales",      # Formateo de ejes en ggplot2
  "ggthemes",    # Temas adicionales para ggplot2
  "patchwork",   # Combinar múltiples gráficos
  "ggcorrplot",  # Matrices de correlación visuales
  "RColorBrewer",# Paletas de color accesibles
  "viridis"      # Paletas perceptualmente uniformes (daltonismo-safe)
)

paquetes_geo <- c(
  # Análisis espacial costero (Sprints 3 y 4)
  "sf",          # Simple Features: datos vectoriales (puntos, polígonos)
  "terra",       # Datos raster (reemplaza raster)
  "tmap"         # Mapas temáticos (sf-compatible)
)

paquetes_rmd <- c(
  # Reportería reproducible
  "rmarkdown",   # R Markdown base
  "knitr",       # Motor de tejido (knit)
  "kableExtra",  # Tablas HTML/LaTeX con formato
  "gt"           # Tablas de publicación de alta calidad
)

# ── 3. Instalar todos los paquetes ────────────────────────────────────────────
todos_los_paquetes <- c(
  paquetes_core,
  paquetes_datos,
  paquetes_estadistica,
  paquetes_viz,
  paquetes_geo,
  paquetes_rmd
)

cat("Verificando e instalando paquetes del curso...\n\n")
instalar_si_falta(todos_los_paquetes)

# ── 4. Cargar paquetes esenciales para el Sprint 1 ───────────────────────────
cat("\nCargando paquetes del Sprint 1...\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(skimr)
  library(here)
  library(readxl)
  library(writexl)
  library(psych)
  library(knitr)
  library(kableExtra)
})

# ── 5. Configuración global de opciones ──────────────────────────────────────
options(
  scipen       = 999,        # Desactivar notación científica
  digits       = 4,          # Decimales en salida de consola
  OutDec       = ".",        # Separador decimal (punto para R estándar)
  warn         = 1,          # Mostrar advertencias inmediatamente
  stringsAsFactors = FALSE   # No convertir strings a factores automáticamente
)

# Tema ggplot2 global del curso
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", colour = "#2c7fb8"),
      plot.subtitle = element_text(colour = "#636363"),
      plot.caption  = element_text(size = 8, colour = "#969696"),
      axis.title    = element_text(face = "bold"),
      legend.position = "bottom"
    )
)

# ── 6. Estructura de carpetas del proyecto ───────────────────────────────────
# Crear estructura local si no existe
dirs <- c(
  "datos/crudos",
  "datos/procesados",
  "datos/sinteticos",
  "outputs/figuras",
  "outputs/tablas",
  "outputs/reportes",
  "scripts",
  "bitacoras"
)

cat("\nVerificando estructura de carpetas del proyecto...\n")
for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("  ✓ Creada:", d, "\n")
  } else {
    cat("  · Ya existe:", d, "\n")
  }
}

# ── 7. Verificación final ─────────────────────────────────────────────────────
cat("\n=============================================================\n")
cat("  Verificación del entorno:\n")
cat("=============================================================\n")
cat("  R versión   :", paste(R.version$major, R.version$minor, sep = "."), "\n")
cat("  tidyverse   :", as.character(packageVersion("tidyverse")), "\n")
cat("  ggplot2     :", as.character(packageVersion("ggplot2")),   "\n")
cat("  dplyr       :", as.character(packageVersion("dplyr")),     "\n")
cat("  janitor     :", as.character(packageVersion("janitor")),   "\n")
cat("  skimr       :", as.character(packageVersion("skimr")),     "\n")
cat("=============================================================\n")
cat("  ✓ Entorno configurado para SP-8502 · Sprint 1\n")
cat("  Siguiente paso: ejecutar 01_planteamiento_y_matriz.R\n")
cat("=============================================================\n")
