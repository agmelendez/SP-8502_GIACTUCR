# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 07_instrumento_encuesta.R
# SPRINT  : 2 — Trabajo de Campo y Muestreo (Semanas 5–8)
# TEMA    : Diseño y Validación del Instrumento · Encuesta + Escala SAS-10
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   Este script documenta el proceso de diseño y validación del instrumento
#   de recolección de datos, con énfasis en:
#
#   1. Estructura modular de la encuesta (por bloques temáticos)
#   2. Propiedades psicométricas de la escala SAS-10
#      (Alpha de Cronbach, omega de McDonald, análisis de ítem)
#   3. Cálculo del error de muestreo por variable
#   4. Protocolo de recolección de datos en campo costero
#   5. Piloto y tabla de decisión: ¿el instrumento es viable?
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")
library(tidyverse)
library(psych)
library(knitr)

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 2 · Instrumento de Encuesta\n")
cat("  Validación Psicométrica de la Escala SAS-10\n")
cat("=============================================================\n\n")

# =============================================================================
# SECCIÓN 1: ESTRUCTURA DE LA ENCUESTA
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Estructura Modular del Instrumento\n")
cat("─────────────────────────────────────────────────────────────\n\n")

estructura_encuesta <- tibble(
  bloque         = c("A","A","A","A","A",
                     "B","B","B","B",
                     "C","C","C","C","C","C","C","C","C","C","C",
                     "D","D","D",
                     "E","E","E"),
  num_item       = c(1:5, 6:9, 10:19, 20:22, 23:25),
  nombre_bloque  = c(
    rep("Identificación y Datos de Campo", 5),
    rep("Perfil Socioeconómico",            4),
    rep("Escala SAS-10 (Variable Depen.)", 10),
    rep("Percepción Ambiental",             3),
    rep("Participación e Institucionalidad", 3)
  ),
  variable_mmr   = c(
    "ID encuesta","Fecha","Comunidad","GPS-Lat","GPS-Lon",
    "Edad","Sexo","Años experiencia","Ingreso mensual USD",
    "SAS-1 Selectividad aparejos","SAS-2 Respeto vedas",
    "SAS-3 Zonas de exclusión","SAS-4 Descarte peces",
    "SAS-5 Cooperación comunal","SAS-6 Reporte capturas",
    "SAS-7 Uso equipo certificado","SAS-8 Capacitación recibida",
    "SAS-9 Participación MARES","SAS-10 Uso de datos en decisión",
    "Percepción recurso","Cambio percibido 5 años","Causa atribuida",
    "Membresía cooperativa","Participación extensión","Contacto INCOPESCA"
  ),
  escala_respuesta = c(
    rep("Abierto/GPS",  5),
    "Abierta","Nominal dicot.","Abierta","Continua (USD)",
    rep("Likert 0–5", 10),
    "Ordinal 5 cat.","Ordinal 3 cat.","Nominal múltiple",
    rep("Dicotómica", 3)
  ),
  tipo_variable = c(
    rep("Control", 5),
    rep("Predictor", 4),
    rep("Dependiente", 10),
    rep("Covariable", 3),
    rep("Moderadora", 3)
  ),
  tiempo_min = c(
    rep(0.5, 5), rep(1.0, 4), rep(0.5, 10), rep(1.0, 3), rep(0.5, 3)
  )
)

cat("Estructura del instrumento (", nrow(estructura_encuesta), "ítems):\n\n")
print(estructura_encuesta %>%
        group_by(bloque, nombre_bloque) %>%
        summarise(n_items = n(),
                  tiempo_total_min = sum(tiempo_min),
                  .groups = "drop"))

tiempo_total <- sum(estructura_encuesta$tiempo_min)
cat("\nTiempo total estimado de aplicación:",
    tiempo_total, "minutos (", round(tiempo_total / 60, 1), "horas)\n")
cat("Recomendación: aplicar en dos sesiones si > 30 minutos\n")

# =============================================================================
# SECCIÓN 2: GENERACIÓN DE DATOS PILOTO PARA VALIDACIÓN PSICOMÉTRICA
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Validación Psicométrica · Escala SAS-10\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Simular respuestas de un piloto (n=30) para la escala SAS-10
# Los ítems están en escala 0–5 (Likert)
n_piloto <- 30

# Generar datos con estructura de factor latente (correlación intra-escala)
# Usamos decomposición de Cholesky para inducir correlación real
cor_base <- matrix(0.45, nrow = 10, ncol = 10)  # Correlación inter-ítems media
diag(cor_base) <- 1.0

chol_mat <- chol(cor_base)
raw_data  <- matrix(rnorm(n_piloto * 10), nrow = n_piloto) %*% chol_mat

# Discretizar a escala Likert 0–5
piloto_sas <- as.data.frame(
  apply(raw_data, 2, function(x) {
    # Mapear distribución normal a {0, 1, 2, 3, 4, 5}
    cuts <- quantile(x, probs = c(0, 0.05, 0.20, 0.50, 0.80, 0.95, 1))
    cut(x, breaks = cuts, labels = 0:5, include.lowest = TRUE) %>%
      as.character() %>% as.numeric()
  })
)
names(piloto_sas) <- paste0("SAS_", 1:10)

cat("Primeras 5 respuestas del piloto (escala 0–5 por ítem):\n")
print(head(piloto_sas))
cat("\nTotal SAS por participante (suma, rango 0–50):\n")
piloto_sas$SAS_total <- rowSums(piloto_sas)
print(summary(piloto_sas$SAS_total))

# ── 2.1 Alpha de Cronbach ─────────────────────────────────────────────────
cat("\n\nANÁLISIS DE FIABILIDAD (Alpha de Cronbach):\n")
alpha_result <- alpha(piloto_sas %>% select(-SAS_total))

cat("  Alpha de Cronbach =",
    round(alpha_result$total$raw_alpha, 3), "\n")
cat("  Alpha estandarizado =",
    round(alpha_result$total$std.alpha, 3), "\n")
cat("  Omega de McDonald (si disponible):\n")

# Omega de McDonald (más robusto que alpha)
tryCatch({
  omega_result <- omega(piloto_sas %>% select(-SAS_total),
                        nfactors = 1, plot = FALSE)
  cat("  ω total =", round(omega_result$omega.tot, 3), "\n")
  cat("  ω jerárquico =", round(omega_result$omega_h, 3), "\n")
}, error = function(e) {
  cat("  (omega requiere más varianza; usar alpha en su lugar)\n")
})

# Interpretación
alpha_val <- alpha_result$total$raw_alpha
cat("\n  Interpretación (George & Mallery, 2003):\n")
cat("  α =", round(alpha_val, 3), "→",
    case_when(
      alpha_val >= 0.90 ~ "Excelente (α ≥ 0.90)",
      alpha_val >= 0.80 ~ "Bueno (0.80 ≤ α < 0.90)",
      alpha_val >= 0.70 ~ "Aceptable (0.70 ≤ α < 0.80)",
      alpha_val >= 0.60 ~ "Cuestionable (0.60 ≤ α < 0.70)",
      TRUE              ~ "Inaceptable (α < 0.60)"
    ), "\n")

# ── 2.2 Análisis de ítem por ítem ─────────────────────────────────────────
cat("\n\nANÁLISIS DE ÍTEM (alpha si se elimina el ítem):\n\n")
item_stats <- alpha_result$alpha.drop %>%
  as.data.frame() %>%
  rownames_to_column("Item") %>%
  select(Item, raw_alpha) %>%
  rename(`Alpha sin ítem` = raw_alpha) %>%
  mutate(
    `Alpha sin ítem` = round(`Alpha sin ítem`, 3),
    Acción = case_when(
      `Alpha sin ítem` > alpha_val + 0.02 ~ "⚠ Considerar eliminar",
      `Alpha sin ítem` < alpha_val - 0.02 ~ "✓ Retener (mejora α)",
      TRUE                                 ~ "~ Neutral"
    )
  )
print(item_stats)

# ── 2.3 Correlación ítem-total corregida ─────────────────────────────────
cat("\n\nCORRELACIÓN ÍTEM-TOTAL CORREGIDA (r > 0.30 deseable):\n")
item_total <- alpha_result$item.stats %>%
  as.data.frame() %>%
  rownames_to_column("Item") %>%
  select(Item, r.cor) %>%
  rename(`r ítem-total` = r.cor) %>%
  mutate(
    `r ítem-total` = round(`r ítem-total`, 3),
    Diagnóstico = case_when(
      `r ítem-total` >= 0.50 ~ "✓ Excelente discriminación",
      `r ítem-total` >= 0.30 ~ "✓ Discriminación aceptable",
      `r ítem-total` >= 0.20 ~ "⚠ Discriminación baja",
      TRUE                   ~ "✗ Ítem problemático"
    )
  )
print(item_total)

# ── 2.4 Distribución por ítem ─────────────────────────────────────────────
cat("\n\nESTADÍSTICOS DESCRIPTIVOS POR ÍTEM:\n")
piloto_sas %>%
  select(-SAS_total) %>%
  psych::describe(fast = TRUE) %>%
  select(mean, sd, min, max, skew) %>%
  round(3) %>%
  print()

# =============================================================================
# SECCIÓN 3: ERROR DE MUESTREO POR VARIABLE
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 3: Error de Muestreo por Variable de Interés\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Usando n=80 de la estrategia muestral del Sprint 2
n_muestra <- 80
N_pop     <- 820
Z_95      <- qnorm(0.975)

errores_muestreo <- tibble(
  Variable     = c("SAS total", "Ingreso mensual",
                   "Edad", "Años experiencia", "Arte sostenible (%)"),
  sigma_estimado = c(9.5, 150, 10, 8, sqrt(0.38 * 0.62) * 50),
  Tipo         = c("Continua","Continua","Continua","Continua","Proporción")
) %>%
  mutate(
    SE_sin_corr = round(sigma_estimado / sqrt(n_muestra), 3),
    FPC         = round(sqrt(1 - n_muestra / N_pop), 4),   # Factor corrección finitud
    SE_con_corr = round(SE_sin_corr * FPC, 3),
    ME_95       = round(Z_95 * SE_con_corr, 3),            # Margen de error 95%
    ME_pct      = round(ME_95 / sigma_estimado * 100, 1)   # Error como % de σ
  )

print(errores_muestreo)

cat("\nInterpretación:\n")
cat("  El FPC (Factor de Corrección por Finitud) =",
    round(sqrt(1 - n_muestra/N_pop), 4),
    "reduce el SE en", round((1 - sqrt(1 - n_muestra/N_pop)) * 100, 1), "%\n")
cat("  dado que n/N =", round(n_muestra/N_pop*100,1),
    "% > 5% → corrección obligatoria.\n")

# =============================================================================
# SECCIÓN 4: PROTOCOLO DE RECOLECCIÓN DE DATOS EN CAMPO COSTERO
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 4: Protocolo de Campo · Costas del Pacífico Central\n")
cat("─────────────────────────────────────────────────────────────\n\n")

protocolo <- tibble(
  Fase = c("Pre-campo","Pre-campo","Pre-campo",
           "Campo","Campo","Campo","Campo","Campo",
           "Post-campo","Post-campo","Post-campo"),
  Actividad = c(
    "Coordinación con líderes de cooperativas pesqueras",
    "Capacitación de encuestadores (protocolo CIDI-UCR)",
    "Prueba piloto n=5 por comunidad + ajuste de instrumento",
    "Identificación de encuestados en puntos de desembarco (5-8am)",
    "Consentimiento informado (oral + escrito bilingüe)",
    "Aplicación de encuesta (presencial o KoboToolbox offline)",
    "Georeferenciación del punto de entrevista (GPS ±5m)",
    "Cupones RDS para participantes sin registro INCOPESCA",
    "Revisión diaria de formularios (consistencia lógica)",
    "Digitalización y codificación de datos abiertos",
    "Control de calidad: duplicados, rangos, inconsistencias"
  ),
  Responsable = c(
    "Investigador principal","Investigador principal","Encuestadores + PI",
    "Encuestadores","Encuestadores","Encuestadores","Encuestadores","Encuestadores",
    "PI + asistente","Asistente","PI"
  ),
  Tiempo = c(
    "2 semanas antes","1 semana antes","3 días antes",
    "5-8am diario","5 min/encuesta","25-30 min","1 min","2 min",
    "6-8pm diario","6-8pm diario","Semanal"
  ),
  Herramienta = c(
    "Correo + WhatsApp cooperativa","Guía CIDI","Lista de cotejo piloto",
    "Registro asistencia","Formulario CI firmado","ODK/KoboToolbox","GPS smartphone","Cupón físico numerado",
    "Google Forms (revisión)","Excel/R","R: script_limpieza.R"
  )
)

print(protocolo, n = Inf)

# Guardar protocolo
write_csv(protocolo, "datos/procesados/protocolo_campo_s2.csv")

# =============================================================================
# SECCIÓN 5: DECISIÓN FINAL SOBRE VIABILIDAD DEL INSTRUMENTO
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 5: Tabla de Decisión — Viabilidad del Instrumento\n")
cat("─────────────────────────────────────────────────────────────\n\n")

viabilidad <- tibble(
  Criterio = c(
    "Alpha de Cronbach escala SAS-10",
    "Correlación ítem-total mínima",
    "Tiempo de aplicación",
    "Tasa de completitud piloto",
    "Comprensibilidad (≥ 90% sin repregunta)",
    "Error de muestreo SAS (ME < ±4 pts)"
  ),
  Umbral_aceptacion = c(
    "α ≥ 0.70", "r ≥ 0.30", "≤ 35 min", "≥ 90%", "≥ 90%", "ME < ±4"
  ),
  Valor_piloto = c(
    paste0("α = ", round(alpha_val, 3)),
    paste0("r_min = ", round(min(item_total$`r ítem-total`), 3)),
    paste0(tiempo_total, " min"),
    "Simulado: 94%",
    "Simulado: 91%",
    paste0("ME = ±", round(errores_muestreo$ME_95[1], 2))
  ),
  Decision = c(
    ifelse(alpha_val >= 0.70, "✓ APROBADO", "✗ REVISAR"),
    ifelse(min(item_total$`r ítem-total`) >= 0.30, "✓ APROBADO", "⚠ REVISAR"),
    ifelse(tiempo_total <= 35, "✓ APROBADO", "⚠ REDUCIR"),
    "✓ APROBADO",
    "✓ APROBADO",
    ifelse(errores_muestreo$ME_95[1] < 4, "✓ APROBADO", "⚠ AUMENTAR n")
  )
)

print(viabilidad, n = Inf)

cat("\nCONCLUSIÓN: El instrumento",
    ifelse(all(grepl("APROBADO", viabilidad$Decision)),
           "CUMPLE todos los criterios y puede aplicarse en campo.",
           "REQUIERE AJUSTES antes del trabajo de campo."),
    "\n")

cat("\n=============================================================\n")
cat("  ✓ Script 07 completado.\n")
cat("  Archivos generados:\n")
cat("    datos/procesados/protocolo_campo_s2.csv\n")
cat("  Siguiente paso: ejecutar 08_base_datos_preliminar.R\n")
cat("=============================================================\n")
