# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable
# Maestría GIACT · Universidad de Costa Rica
# =============================================================================
# Script: 00_setup_ambiente.R
# Propósito: Configuración inicial del entorno R para el curso
# Instructor: MSI. Agustín Gómez Meléndez · CIOdD
# Versión: 1.0 · Marzo 2026
# =============================================================================
#
# INSTRUCCIONES:
#   1. Ejecute este script sección por sección (Ctrl+Enter línea a línea)
#   2. Si encuentra errores, documéntelos en su Bitácora de IA y use el
#      Tutor IA del curso para resolverlos:
#      https://chatgpt.com/g/g-698e27494b108191b92caa3f39a920c6-asistente-sp-8502-giact
#   3. Al final del script encontrará una verificación automática del entorno
#
# =============================================================================


# -----------------------------------------------------------------------------
# SECCIÓN 1: Directorio de trabajo
# -----------------------------------------------------------------------------
# Establezca su directorio de trabajo local.
# ⚠️ ERROR 1: Esta ruta está codificada para Windows y no funcionará en Mac/Linux.
#             Corrija la ruta según su sistema operativo y estructura de carpetas.

setwd("C:/Users/estudiante/Documentos/SP-8502")


# -----------------------------------------------------------------------------
# SECCIÓN 2: Instalación de paquetes
# -----------------------------------------------------------------------------
# Instalamos todos los paquetes necesarios para el curso.
# Si ya los tiene instalados, esta sección puede omitirse.

# ⚠️ ERROR 2: Uno de los nombres de paquete tiene un error tipográfico.
#             Identifique cuál es y corríjalo antes de ejecutar.

paquetes_requeridos <- c(
  "tidyverse",    # Ecosistema central: dplyr, ggplot2, tidyr, readr, purrr
  "readxl",       # Lectura de archivos Excel
  "janitorr",     # Limpieza de nombres de columnas y datos sucios  ← revisar
  "here",         # Rutas de archivo relativas y reproducibles
  "skimr",        # Estadísticas descriptivas rápidas
  "lubridate",    # Manejo de fechas
  "ggthemes",     # Temas adicionales para ggplot2
  "knitr"         # Generación de reportes reproducibles
)

install.packages(paquetes_requeridos)


# -----------------------------------------------------------------------------
# SECCIÓN 3: Carga de paquetes
# -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(skimr)
library(lubridate)
library(ggthemes)
library(knitr)

cat("✅ Paquetes cargados correctamente\n")


# -----------------------------------------------------------------------------
# SECCIÓN 4: Estructura de carpetas del proyecto
# -----------------------------------------------------------------------------
# Creamos la estructura de directorios estándar para los entregables del curso.

carpetas <- c(
  "datos/crudos",
  "datos/procesados",
  "scripts",
  "resultados/tablas",
  "resultados/figuras",
  "bitacora"
)

# ⚠️ ERROR 3: La función para crear directorios tiene un argumento incorrecto.
#             recursive = VERDADERO no es R válido. Corrija con el valor lógico
#             apropiado en R.

for (carpeta in carpetas) {
  if (!dir.exists(carpeta)) {
    dir.create(carpeta, recursive = VERDADERO)
  }
}

cat("✅ Estructura de carpetas creada\n")


# -----------------------------------------------------------------------------
# SECCIÓN 5: Datos de prueba y primera exploración
# -----------------------------------------------------------------------------
# Simulamos un dataset costero básico para verificar que el entorno funciona.

set.seed(2026)

datos_costeros <- data.frame(
  sitio        = sample(c("Golfo_Nicoya", "Pacifico_Sur", "Caribe"), 60, replace = TRUE),
  fecha        = seq(as.Date("2025-01-01"), by = "week", length.out = 60),
  temperatura  = round(rnorm(60, mean = 28, sd = 2.5), 1),
  salinidad    = round(rnorm(60, mean = 34, sd = 1.2), 2),
  capturas_kg  = round(abs(rnorm(60, mean = 45, sd = 18)), 1),
  actores      = sample(c("pescadores", "turismo", "conservacion"), 60, replace = TRUE)
)

# Vista rápida del dataset
glimpse(datos_costeros)
skim(datos_costeros)


# -----------------------------------------------------------------------------
# SECCIÓN 6: Limpieza con janitor
# -----------------------------------------------------------------------------

datos_limpios <- datos_costeros %>%
  clean_names() %>%
  mutate(sitio = as.factor(sitio),
         actores = as.factor(actores))

cat("✅ Datos de prueba generados y limpiados:", nrow(datos_limpios), "observaciones\n")


# -----------------------------------------------------------------------------
# SECCIÓN 7: Visualización de prueba con ggplot2
# -----------------------------------------------------------------------------
# Generamos un gráfico básico para confirmar que ggplot2 funciona correctamente.

# ⚠️ ERROR 4: El argumento de color dentro de aes() hace referencia a una
#             columna que no existe en el dataframe. Identifique la columna
#             correcta y corrija la llamada.

grafico_prueba <- ggplot(datos_limpios,
                         aes(x = sitio,
                             y = capturas_kg,
                             fill = zona_marina)) +       # ← columna inexistente
  geom_boxplot(alpha = 0.7) +
  labs(
    title    = "Distribución de capturas por sitio costero",
    subtitle = "Datos simulados · SP-8502 GIACT 2026",
    x        = "Sitio de monitoreo",
    y        = "Capturas (kg)",
    caption  = "Fuente: datos de prueba generados con set.seed(2026)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafico_prueba)


# -----------------------------------------------------------------------------
# SECCIÓN 8: Exportar figura de prueba
# -----------------------------------------------------------------------------
# ⚠️ ERROR 5: ggsave tiene los argumentos de ancho y alto en unidades incorrectas.
#             El argumento units admite "in", "cm", "mm" o "px".
#             "pulgadas" no es un valor válido. Corrija antes de ejecutar.

ggsave(
  filename = "resultados/figuras/prueba_setup.png",
  plot     = grafico_prueba,
  width    = 8,
  height   = 5,
  units    = "pulgadas",           # ← corregir
  dpi      = 300
)


# -----------------------------------------------------------------------------
# SECCIÓN 9: Verificación final del entorno
# -----------------------------------------------------------------------------

cat("\n")
cat("=============================================================\n")
cat("  VERIFICACIÓN DEL ENTORNO · SP-8502 · GIACT UCR 2026\n")
cat("=============================================================\n")
cat("  R version:     ", R.version$major, ".", R.version$minor, "\n", sep = "")
cat("  tidyverse:     ", as.character(packageVersion("tidyverse")), "\n")
cat("  readxl:        ", as.character(packageVersion("readxl")), "\n")
cat("  janitor:       ", as.character(packageVersion("janitor")), "\n")
cat("  here:          ", as.character(packageVersion("here")), "\n")
cat("  skimr:         ", as.character(packageVersion("skimr")), "\n")
cat("  Observaciones: ", nrow(datos_limpios), "\n")
cat("  Variables:     ", ncol(datos_limpios), "\n")
cat("=============================================================\n")
cat("  ✅ Si llega hasta aquí sin errores, su entorno está listo.\n")
cat("  📋 Documente este proceso en su Bitácora de IA.\n")
cat("=============================================================\n")

# =============================================================================
# FIN DEL SCRIPT · 00_setup_ambiente.R
# =============================================================================
