# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : script_limpieza.R      ← ENTREGABLE SPRINT 1
# SPRINT  : 1 — Diseño y Fundamentos de Datos (Semanas 1–4)
# TEMA    : Limpieza, Estructuración y Documentación de Datos
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# VERSIÓN : 1.0
# =============================================================================
# PEDAGOGICAL SEQUENCE (SP-8502):
#   PASO 1 · CÁLCULO        → Operaciones de limpieza y transformación
#   PASO 2 · INTERPRETACIÓN → ¿Qué revelan los diagnósticos sobre la calidad?
#   PASO 3 · NEGOCIO/CONTEXTO → Implicaciones para la investigación costera
#   PASO 4 · DECISIÓN CRÍTICA  → Justificación metodológica de cada decisión
#
# ENTREGABLE SPRINT 1:
#   Este script es el entregable "script_limpieza.R" del Sprint 1.
#   Debe producir: datos/procesados/pescadores_limpio.csv
#   y estar 100% reproducible (mismos resultados con set.seed = 8502).
#
# REPRODUCIBILIDAD:
#   Ejecutar en orden desde 00_setup_ambiente.R.
#   Documentado en: bitacoras/bitacora_ia_s1.xlsx
# =============================================================================

# ── 0. Cargar entorno y datos crudos ────────────────────────────────────────
if (!require("tidyverse")) source("00_setup_ambiente.R")
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 1 · Script de Limpieza de Datos\n")
cat("  Entregable: script_limpieza.R\n")
cat("=============================================================\n\n")

# Ejecutar script de generación si no existen los datos crudos
if (!file.exists("datos/crudos/pescadores_pacifico_central_CRUDO.csv")) {
  message("Generando datos crudos primero...")
  source("02_datos_sinteticos_costeros.R")
}

datos_crudos <- read_csv(
  "datos/crudos/pescadores_pacifico_central_CRUDO.csv",
  show_col_types = FALSE
)

cat("Dataset crudo cargado:", nrow(datos_crudos), "obs. ×",
    ncol(datos_crudos), "variables\n\n")

# =============================================================================
# ══ PASO 1: CÁLCULO ══════════════════════════════════════════════════════════
# Operaciones de limpieza sistemáticas, en secuencia lógica
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("PASO 1: CÁLCULO — Operaciones de Limpieza\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── 1.1 Limpiar nombres de columnas (janitor) ─────────────────────────────
cat("1.1 Estandarizando nombres de columnas...\n")
datos <- datos_crudos %>%
  clean_names()   # snake_case, sin tildes, sin espacios

cat("  Nombres originales → limpios:\n")
data.frame(
  original = names(datos_crudos),
  limpio   = names(datos)
) %>%
  filter(original != limpio) %>%
  print()

# ── 1.2 Eliminar columna completamente vacía (Error E9) ────────────────────
cat("\n1.2 Eliminando columnas completamente vacías...\n")
n_antes <- ncol(datos)
datos <- datos %>%
  select(where(~ !all(is.na(.))))
cat("  Columnas eliminadas:", n_antes - ncol(datos), "\n")
cat("  Columnas restantes:", ncol(datos), "\n")

# ── 1.3 Estandarizar variable 'comunidad' (Error E1) ─────────────────────
cat("\n1.3 Estandarizando 'comunidad' (Error E1)...\n")
cat("  Valores únicos ANTES de limpiar:\n")
print(sort(unique(datos$comunidad)))

datos <- datos %>%
  mutate(
    comunidad = str_to_title(      # Primer letra mayúscula
      str_trim(                    # Eliminar espacios al inicio/fin
        iconv(comunidad,           # Normalizar tildes y acentos
              from = "UTF-8", to = "ASCII//TRANSLIT")
      )
    ),
    # Unificar variantes residuales
    comunidad = case_when(
      comunidad %in% c("Quepos", "Quepos")     ~ "Quepos",
      comunidad %in% c("Jaco", "Jaco")         ~ "Jacó",
      comunidad %in% c("Tarcoles", "Tarcoles") ~ "Tárcoles",
      comunidad == "Puntamorales"              ~ "Punta Morales",
      TRUE ~ comunidad
    ),
    comunidad = factor(comunidad,
                       levels = c("Quepos", "Jacó", "Tárcoles",
                                  "Herradura", "Punta Morales"))
  )

cat("  Valores únicos DESPUÉS de limpiar:\n")
print(levels(datos$comunidad))
cat("  Distribución:\n")
print(table(datos$comunidad, useNA = "ifany"))

# ── 1.4 Limpiar 'edad' (Error E2: valores imposibles) ────────────────────
cat("\n1.4 Limpiando 'edad' — valores imposibles (Error E2)...\n")
cat("  Rango ANTES:", range(datos$edad, na.rm = TRUE), "\n")
cat("  N fuera de rango [15, 80]:", sum(datos$edad < 15 | datos$edad > 80, na.rm = TRUE), "\n")

datos <- datos %>%
  mutate(
    edad_flag = case_when(
      is.na(edad)          ~ "Faltante",
      edad < 15            ~ "Imposible (< 15)",
      edad > 80            ~ "Imposible (> 80)",
      TRUE                 ~ "Válido"
    ),
    edad = case_when(
      edad < 15 | edad > 80 ~ NA_real_,   # Reemplazar con NA documentado
      TRUE                  ~ edad
    )
  )

cat("  Diagnóstico de edad:\n")
print(table(datos$edad_flag, useNA = "ifany"))
cat("  Rango DESPUÉS:", range(datos$edad, na.rm = TRUE), "\n")

# ── 1.5 Estandarizar 'sexo' (Error E3) ───────────────────────────────────
cat("\n1.5 Estandarizando 'sexo' (Error E3)...\n")
cat("  Categorías originales:", sort(unique(datos$sexo)), "\n")

datos <- datos %>%
  mutate(
    sexo = case_when(
      str_to_lower(sexo) %in% c("masculino", "m", "hombre") ~ "Masculino",
      str_to_lower(sexo) %in% c("femenino",  "f", "mujer")  ~ "Femenino",
      TRUE ~ NA_character_
    ),
    sexo = factor(sexo, levels = c("Masculino", "Femenino"))
  )

cat("  Categorías limpias:\n")
print(table(datos$sexo, useNA = "ifany"))

# ── 1.6 Limpiar 'ingreso_mensual_usd' (Error E4) ─────────────────────────
cat("\n1.6 Limpiando 'ingreso_mensual_usd' (Error E4)...\n")
cat("  Estadísticas ANTES:\n")
cat("    Min:", min(datos$ingreso_mensual_usd, na.rm = TRUE), "\n")
cat("    Max:", max(datos$ingreso_mensual_usd, na.rm = TRUE), "\n")
cat("    N NA:", sum(is.na(datos$ingreso_mensual_usd)), "\n")

datos <- datos %>%
  mutate(
    ingreso_flag = case_when(
      is.na(ingreso_mensual_usd)          ~ "Faltante",
      ingreso_mensual_usd == 9999.99      ~ "Código especial (NS/NR)",
      ingreso_mensual_usd < 0             ~ "Negativo (imposible)",
      ingreso_mensual_usd == 0            ~ "Cero (revisar)",
      ingreso_mensual_usd > 3000          ~ "Posible outlier extremo",
      TRUE                                ~ "Válido"
    ),
    ingreso_mensual_usd = case_when(
      ingreso_mensual_usd == 9999.99 ~ NA_real_,   # NS/NR → NA
      ingreso_mensual_usd < 0        ~ NA_real_,   # Negativo → NA
      TRUE                           ~ ingreso_mensual_usd
    )
  )

cat("  Diagnóstico de ingreso:\n")
print(table(datos$ingreso_flag, useNA = "ifany"))
cat("  Estadísticas DESPUÉS:\n")
cat("    Min:", round(min(datos$ingreso_mensual_usd, na.rm = TRUE), 2), "\n")
cat("    Max:", round(max(datos$ingreso_mensual_usd, na.rm = TRUE), 2), "\n")
cat("    Media:", round(mean(datos$ingreso_mensual_usd, na.rm = TRUE), 2), "\n")

# ── 1.7 Unificar 'arte_pesca_principal' (Error E5) ────────────────────────
cat("\n1.7 Unificando 'arte_pesca_principal' (Error E5)...\n")
cat("  Categorías originales:", sort(unique(datos$arte_pesca_principal)), "\n")

datos <- datos %>%
  mutate(
    arte_pesca_principal = case_when(
      str_to_lower(arte_pesca_principal) %in%
        c("línea de mano", "linea de mano")     ~ "Línea de mano",
      str_to_lower(arte_pesca_principal) %in%
        c("red agallera")                        ~ "Red agallera",
      str_to_lower(arte_pesca_principal) %in%
        c("trasmallo")                           ~ "Trasmallo",
      str_to_lower(arte_pesca_principal) %in%
        c("palangre", "long line")               ~ "Palangre",    # Inglés → español
      str_to_lower(arte_pesca_principal) %in%
        c("cerco")                               ~ "Cerco",
      TRUE ~ NA_character_
    ),
    arte_pesca_principal = factor(
      arte_pesca_principal,
      levels = c("Línea de mano", "Red agallera", "Trasmallo", "Palangre", "Cerco")
    )
  )

cat("  Distribución limpia:\n")
print(table(datos$arte_pesca_principal, useNA = "ifany"))

# ── 1.8 Unificar 'percepcion_recurso' (Error E6) ─────────────────────────
cat("\n1.8 Unificando categorías de percepción del recurso (Error E6)...\n")

datos <- datos %>%
  mutate(
    percepcion_recurso = case_when(
      percepcion_recurso %in% c("No sé", "NS/NR") ~ NA_character_,   # Ambas → NA
      TRUE                                          ~ percepcion_recurso
    ),
    percepcion_recurso = factor(
      percepcion_recurso,
      levels = c("Muy abundante", "Abundante", "Regular", "Escaso", "Muy escaso"),
      ordered = TRUE   # Variable ordinal → factor ordenado
    )
  )

cat("  Distribución:\n")
print(table(datos$percepcion_recurso, useNA = "ifany"))

# ── 1.9 Validar y depurar 'sas_total' (Error E7) ─────────────────────────
cat("\n1.9 Validando 'sas_total' — Escala 0–50 (Error E7)...\n")
cat("  Rango ANTES:", range(datos$sas_total, na.rm = TRUE), "\n")
cat("  N fuera de [0, 50]:",
    sum(datos$sas_total < 0 | datos$sas_total > 50, na.rm = TRUE), "\n")

datos <- datos %>%
  mutate(
    sas_flag  = case_when(
      is.na(sas_total)                   ~ "Faltante",
      sas_total < 0 | sas_total > 50    ~ "Fuera de rango",
      TRUE                               ~ "Válido"
    ),
    sas_total = case_when(
      sas_total < 0 | sas_total > 50    ~ NA_real_,
      TRUE                               ~ sas_total
    )
  )

cat("  Diagnóstico SAS:\n")
print(table(datos$sas_flag, useNA = "ifany"))

# ── 1.10 Estandarizar 'extension_pesquera' (Error E8) ────────────────────
cat("\n1.10 Estandarizando 'extension_pesquera' — variable binaria (Error E8)...\n")
cat("  Valores únicos originales:", sort(unique(datos$extension_pesquera)), "\n")

datos <- datos %>%
  mutate(
    extension_pesquera = case_when(
      str_to_lower(extension_pesquera) %in% c("sí", "si", "1", "yes") ~ "Sí",
      str_to_lower(extension_pesquera) %in% c("no", "0")               ~ "No",
      TRUE                                                               ~ NA_character_
    ),
    extension_pesquera = factor(extension_pesquera, levels = c("No", "Sí"))
  )

cat("  Distribución limpia:\n")
print(table(datos$extension_pesquera, useNA = "ifany"))

# ── 1.11 Eliminar columnas auxiliares de flags (solo para análisis) ────────
# Las columnas *_flag se guardan en un archivo de trazabilidad separado
flags <- datos %>% select(id_encuesta, contains("_flag"))
datos <- datos %>% select(-contains("_flag"), -columna_vacia,
                           -observaciones_libres, -numero_encuestador)

cat("\n1.11 Dataset final después de limpieza:\n")
cat("  Dimensiones:", nrow(datos), "obs. ×", ncol(datos), "variables\n")

# =============================================================================
# ══ PASO 2: INTERPRETACIÓN ESTADÍSTICA ═══════════════════════════════════════
# ¿Qué revelan los diagnósticos sobre la calidad de los datos?
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASO 2: INTERPRETACIÓN ESTADÍSTICA — Diagnóstico de Calidad\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── 2.1 Resumen global con skimr ─────────────────────────────────────────
cat("Resumen estadístico del dataset limpio:\n\n")
skim_resultado <- skim(datos)
print(skim_resultado)

# ── 2.2 Reporte de completitud ────────────────────────────────────────────
cat("\n\nReporte de completitud por variable:\n")
completitud <- datos %>%
  summarise(across(everything(),
                   list(n_valido = ~ sum(!is.na(.)),
                        n_na     = ~ sum(is.na(.)),
                        pct_na   = ~ round(mean(is.na(.)) * 100, 1)))) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(.+)$"
  ) %>%
  arrange(desc(pct_na))

print(completitud, n = Inf)

# ── 2.3 Evaluación de distribuciones de variables clave ──────────────────
cat("\n\nEstadísticas descriptivas — Variables numéricas clave:\n")
datos %>%
  select(edad, años_experiencia, ingreso_mensual_usd, sas_total,
         distancia_anp_km, tamaño_familia) %>%
  psych::describe() %>%
  round(2) %>%
  print()

# =============================================================================
# ══ PASO 3: INTERPRETACIÓN DE CONTEXTO (GESTIÓN COSTERA) ═════════════════════
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASO 3: INTERPRETACIÓN DE CONTEXTO — Gestión Costera\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat(
  "HALLAZGOS DE CALIDAD RELEVANTES PARA LA INVESTIGACIÓN COSTERA:\n\n",

  "1. INGRESO: La presencia de 9999.99 como código no documentado sugiere",
  "   que en campo se usó un código ad-hoc para 'no sabe'. Esto es común",
  "   en encuestas pesqueras y debe quedar registrado en el protocolo.",
  "   → Implicación: El ingreso tiene ~5% de NA, suficientemente bajo para",
  "     análisis de regresión sin imputación compleja (Sprint 3).\n\n",

  "2. SAS: Los valores fuera de rango (>50) pueden indicar errores de captura",
  "   o suma incorrecta de ítems en campo. Con n=3 casos afectados (<4%),",
  "   la decisión de convertir a NA es conservadora y metodológicamente sólida.\n\n",

  "3. COMUNIDAD: La heterogeneidad tipográfica es un problema estructural",
  "   recurrente en trabajo de campo costero con múltiples encuestadores.",
  "   → Recomendación: En futuros sprints, usar ODK/KoboToolbox con",
  "     listas controladas para evitar entrada libre de texto.\n\n",

  "4. PERCEPCIÓN: El 5% de 'No sé/NS/NR' puede ser informativo per se:",
  "   podría indicar comunidades con menor exposición a discursos de",
  "   sostenibilidad. Analizar como variable latente en Sprint 3.\n",
  sep = "\n"
)

# =============================================================================
# ══ PASO 4: DECISIÓN CRÍTICA ══════════════════════════════════════════════════
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASO 4: DECISIÓN CRÍTICA — Justificación Metodológica\n")
cat("─────────────────────────────────────────────────────────────\n\n")

decisiones_limpeza <- tibble(
  decision = c(
    "Valores imposibles → NA (no eliminación de fila)",
    "Código 9999.99 → NA (no imputación en este sprint)",
    "Categorías textuales → factor ordenado (percepción)",
    "Columnas auxiliares de flag conservadas en archivo separado",
    "NO se imputan valores faltantes en este sprint"
  ),
  justificacion = c(
    "Eliminar la fila completa por 1 variable errónea descarta información
     válida de las demás variables. El NA preserva la observación y permite
     análisis de casos completos (listwise) o imputación (MICE) en S3.",
    "No existe justificación metodológica para imputar el ingreso en S1
     sin conocer la distribución completa. La imputación requiere el
     modelo de predicción que se construye en S3.",
    "La percepción del recurso es intrínsecamente ordinal (escala Likert
     conceptual). Usar factor ordenado permite análisis Spearman y
     modelos ordinales en S3 sin asumir equidistancia.",
    "Las flags documentan la trazabilidad de decisiones: qué observaciones
     fueron modificadas y por qué. Son esenciales para el audit trail
     exigido por la R-469-2025 de reproducibilidad.",
    "La imputación múltiple (MICE, missForest) requiere que el modelo
     esté especificado. Hacerla en S1 introduce sesgos no detectables.
     Se aplica en S3 una vez definida la estructura del modelo principal."
  ),
  referencia = c(
    "Van Buuren (2018) Flexible Imputation of Missing Data, Cap. 1",
    "Little & Rubin (2002) Statistical Analysis with Missing Data, Cap. 3",
    "Agresti (2018) Analysis of Ordinal Categorical Data, Cap. 2",
    "UCR Res. R-469-2025, Art. 8: Trazabilidad y reproducibilidad",
    "Sterne et al. (2009) BMJ, 338:b2393 — Multiple imputation guidance"
  )
)

print(decisiones_limpeza, n = Inf)

# =============================================================================
# SECCIÓN FINAL: GUARDAR DATASETS LIMPIOS
# =============================================================================

# Dataset principal limpio
write_csv(datos, "datos/procesados/pescadores_limpio.csv")

# Tabla de flags de trazabilidad
write_csv(flags, "datos/procesados/trazabilidad_correcciones.csv")

# Catálogo de decisiones de limpieza
write_csv(decisiones_limpeza, "datos/procesados/decisiones_limpieza.csv")

cat("\n=============================================================\n")
cat("ARCHIVOS GENERADOS POR ESTE SCRIPT:\n")
cat("  ✓ datos/procesados/pescadores_limpio.csv\n")
cat("  ✓ datos/procesados/trazabilidad_correcciones.csv\n")
cat("  ✓ datos/procesados/decisiones_limpieza.csv\n")
cat("=============================================================\n")
cat("  Dimensión final del dataset limpio:\n")
cat("   ", nrow(datos), "observaciones ×", ncol(datos), "variables\n")
cat("=============================================================\n")
cat("  ✓ ENTREGABLE SPRINT 1 COMPLETADO\n")
cat("  Siguiente paso: ejecutar 04_eda_sprint1.R\n")
cat("=============================================================\n")
