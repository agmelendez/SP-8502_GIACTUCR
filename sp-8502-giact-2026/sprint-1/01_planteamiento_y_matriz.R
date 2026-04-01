# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 01_planteamiento_y_matriz.R
# SPRINT  : 1 — Diseño y Fundamentos de Datos (Semanas 1–4)
# TEMA    : Planteamiento del Problema · Lógica de Investigación Mixta
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# VERSIÓN : 1.0
# =============================================================================
# DESCRIPCIÓN:
#   Este script formaliza el planteamiento del problema de investigación y
#   construye la Matriz Metodológica Mixta (MMR) que guiará los 4 sprints.
#   La matriz es el artefacto estructurador central del Proyecto Integrador.
#
# PREGUNTA ORIENTADORA DEL EJEMPLO:
#   "¿En qué medida los factores socioeconómicos y la percepción de las
#   comunidades pesqueras artesanales de la zona costera del Pacífico Central
#   de Costa Rica influyen sobre la adopción de prácticas sostenibles
#   de pesca, y qué narrativas culturales median dicha relación?"
#
# USO DE IA DOCUMENTADO EN: bitacoras/bitacora_ia_s1.xlsx  (Fila 1-3)
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
source("00_setup_ambiente.R")

# =============================================================================
# SECCIÓN 1: PLANTEAMIENTO DEL PROBLEMA
# =============================================================================
# En un script de investigación, el planteamiento NO es solo texto:
# se operacionaliza en objetos R que la matriz usará.
# =============================================================================

# ── 1.1 Definición de la pregunta de investigación ───────────────────────────
pregunta_central <- paste(
  "¿En qué medida los factores socioeconómicos y la percepción ambiental",
  "de comunidades pesqueras artesanales del Pacífico Central de Costa Rica",
  "predicen la adopción de prácticas de pesca sostenible,",
  "y qué narrativas culturales median dicha relación?"
)

# Preguntas específicas cuantitativas y cualitativas
preguntas_cuantitativas <- c(
  "PQ1: ¿Cuál es la asociación entre nivel de ingreso mensual por pesca y",
  "     score de adopción de prácticas sostenibles (SAS)?",
  "PQ2: ¿Qué variables socioeconómicas (años de experiencia, tamaño del grupo",
  "     familiar, distancia al ANP) predicen el SAS?",
  "PQ3: ¿Existe diferencia estadísticamente significativa en SAS entre",
  "     comunidades con y sin acceso a programas de extensión pesquera?"
)

preguntas_cualitativas <- c(
  "PQual1: ¿Qué narrativas culturales expresan los pescadores sobre la",
  "        relación entre práctica tradicional y recurso marino?",
  "PQual2: ¿Cómo se construye el concepto de 'pesca responsable' desde",
  "        la perspectiva emic de las comunidades costeras?"
)

cat("=============================================================\n")
cat("PREGUNTA CENTRAL DE INVESTIGACIÓN:\n")
cat("=============================================================\n")
cat(strwrap(pregunta_central, width = 65), sep = "\n")
cat("\n\nPREGUNTAS CUANTITATIVAS:\n")
invisible(lapply(preguntas_cuantitativas, cat, "\n"))
cat("\nPREGUNTAS CUALITATIVAS:\n")
invisible(lapply(preguntas_cualitativas, cat, "\n"))

# =============================================================================
# SECCIÓN 2: MATRIZ METODOLÓGICA MIXTA (MMR)
# =============================================================================
# Estructura: Variable → Tipo → Nivel Medición → Instrumento → Análisis
# La MMR es la columna vertebral del Proyecto Integrador
# =============================================================================

# ── 2.1 Construir la MMR como data frame ─────────────────────────────────────
mmr <- tibble(
  # ── Identificadores ──────────────────────────────────────────────────
  componente = c(
    rep("CUANTITATIVO", 7),
    rep("CUALITATIVO",  3),
    rep("MIXTO (integración)", 2)
  ),

  variable = c(
    # Bloque cuantitativo
    "Ingreso mensual por pesca (USD)",
    "Años de experiencia pesquera",
    "Score Adopción Sostenible (SAS)",
    "Distancia a ANP (km)",
    "Tamaño del grupo familiar",
    "Tipo de arte de pesca",
    "Participación en programas extensión",
    # Bloque cualitativo
    "Narrativas sobre recurso marino",
    "Percepción cultural pesca responsable",
    "Historia y tradición comunal",
    # Bloque integración
    "Convergencia cuanti-cuali (SAS + narrativas)",
    "Tipología de perfiles pesqueros"
  ),

  tipo_variable = c(
    "Continua", "Continua", "Continua (escala Likert)", "Continua",
    "Discreta", "Nominal (categórica)", "Dicotómica",
    "Texto (libre)", "Texto (libre)", "Texto (libre)",
    "Integración analítica", "Integración analítica"
  ),

  nivel_medicion = c(
    "Razón", "Razón", "Intervalo (0–50 pts)", "Razón",
    "Razón", "Nominal", "Nominal (Sí/No)",
    "Nominal (semántico)", "Nominal (semántico)", "Nominal (semántico)",
    "Mixto", "Mixto"
  ),

  instrumento = c(
    "Encuesta estructurada (ítem 12)",
    "Encuesta estructurada (ítem 4)",
    "Escala SAS-10 adaptada (ítems 20–29)",
    "GPS / GIS (SINAC)",
    "Encuesta estructurada (ítem 6)",
    "Encuesta estructurada (ítem 8, lista)",
    "Encuesta estructurada (ítem 15)",
    "Entrevista semiestructurada",
    "Entrevista semiestructurada",
    "Grupo focal (historia oral)",
    "Análisis convergente (QUAN+QUAL)",
    "Análisis clúster + análisis temático"
  ),

  tecnica_analisis = c(
    "Regresión lineal múltiple (Sprint 3)",
    "Regresión lineal múltiple (Sprint 3)",
    "Variable dependiente central",
    "Análisis espacial-sf (Sprint 4)",
    "Descriptiva + regresión (Sprint 3)",
    "Análisis de frecuencias + chi-cuadrado",
    "Prueba t de Student / Mann-Whitney",
    "Codificación abierta → axial (Atlas.ti / R-qualr)",
    "Codificación axial → selectiva",
    "Análisis narrativo",
    "Joint display matrix (Sprint 4)",
    "K-means + análisis temático (Sprint 3–4)"
  ),

  sprint_responsable = c(
    "S1-S3", "S1-S3", "S1-S3", "S2-S3", "S1-S3", "S1-S2", "S1-S2",
    "S2-S3", "S2-S3", "S2-S3", "S4", "S3-S4"
  )
)

# ── 2.2 Imprimir la MMR ───────────────────────────────────────────────────────
cat("\n=============================================================\n")
cat("MATRIZ METODOLÓGICA MIXTA (MMR) — SP-8502 Sprint 1\n")
cat("=============================================================\n\n")

# Vista por componente
mmr %>%
  select(componente, variable, nivel_medicion, instrumento, sprint_responsable) %>%
  print(n = Inf)

# ── 2.3 Guardar la MMR como CSV ───────────────────────────────────────────────
write_csv(mmr, "datos/procesados/mmr_sprint1.csv")
cat("\n✓ MMR guardada en: datos/procesados/mmr_sprint1.csv\n")

# =============================================================================
# SECCIÓN 3: HIPÓTESIS ESTADÍSTICAS FORMALIZADAS
# =============================================================================

cat("\n=============================================================\n")
cat("HIPÓTESIS ESTADÍSTICAS (Sprint 1 → Sprint 3)\n")
cat("=============================================================\n")

hipotesis <- tibble(
  id  = paste0("H", 1:4),
  tipo = c("Principal", "Secundaria", "Secundaria", "Nula"),
  formulacion = c(
    "El ingreso mensual, los años de experiencia y la participación en programas
     de extensión predicen significativamente el SAS (R² > 0.35, p < 0.05).",
    "Las comunidades con acceso a programas de extensión pesquera presentan
     un SAS significativamente mayor (d de Cohen > 0.5).",
    "Existe una correlación positiva moderada-alta entre distancia a ANP y
     SAS (r de Pearson ≥ 0.40, p < 0.05).",
    "Ninguna variable socioeconómica predice significativamente el SAS."
  ),
  prueba_estadistica = c(
    "Regresión lineal múltiple (OLS) + diagnósticos (VIF, residuales)",
    "Prueba t de Student independiente / Mann-Whitney U",
    "Correlación de Pearson + Spearman (robustez)",
    "Contraste con H1 principal"
  )
)

print(hipotesis)

# =============================================================================
# SECCIÓN 4: DISEÑO MIXTO — TIPOLOGÍA Y SECUENCIA
# =============================================================================
cat("\n=============================================================\n")
cat("DISEÑO MIXTO ADOPTADO\n")
cat("=============================================================\n")

diseño_mixto <- list(
  tipologia       = "Secuencial Explicativo (QUAN → qual)",
  notacion_MMR    = "QUAN(dominant) → qual",
  razon           = paste(
    "La fase cuantitativa (encuesta estructurada, n ≥ 60) genera resultados",
    "sobre patrones de adopción, los cuales son luego profundizados mediante",
    "entrevistas semiestructuradas y grupos focales (fase cualitativa, n ≈ 15–20).",
    "La integración ocurre en el Sprint 4 mediante Joint Display y Weaving."
  ),
  secuencia = data.frame(
    fase     = c("Fase 1 (QUAN)",     "Fase 2 (qual)",     "Fase 3 (Integración)"),
    sprint   = c("Sprint 1–3",        "Sprint 2–3",        "Sprint 4"),
    objetivo = c(
      "Describir y modelar patrones de adopción sostenible",
      "Explicar mecanismos culturales que median los patrones",
      "Convergencia y triangulación: joint display + tipología"
    ),
    n_muestra = c("n ≥ 60 pescadores", "n = 15–20 (informantes clave)", "—")
  )
)

cat("\nTipología:", diseño_mixto$tipologia, "\n")
cat("Notación MMR:", diseño_mixto$notacion_MMR, "\n\n")
cat("Fundamentación:\n", strwrap(diseño_mixto$razon, width = 70), sep = "\n")
cat("\nSecuencia de fases:\n")
print(diseño_mixto$secuencia)

# =============================================================================
# SECCIÓN 5: DECISIÓN CRÍTICA — PROMPT ENGINEERING DOCUMENTADO
# =============================================================
# NOTA PEDAGÓGICA:
#   El siguiente bloque documenta cómo se usó IA (Custom GPT) para
#   ASISTIR (no reemplazar) la construcción de la MMR.
#   Esto es obligatorio según la Bitácora de IA (Resolución R-469-2025).
# =============================================================

cat("\n=============================================================\n")
cat("DECISIÓN CRÍTICA · USO DE IA DOCUMENTADO (Bitácora Fila 3)\n")
cat("=============================================================\n")

ia_decision <- list(
  herramienta     = "Tutor IA SP-8502 (Custom GPT)",
  prompt_usado    = paste(
    "Actúa como metodólogo experto en MMR. Dado este contexto de investigación",
    "[se adjuntó descripción del problema], sugiere 3 diseños mixtos alternativos",
    "justificando cada uno según los criterios de Creswell & Plano Clark (2018)."
  ),
  salida_ia       = "Propuso: (1) Secuencial Explicativo, (2) Convergente Paralelo, (3) Transformativo",
  decision_humana = paste(
    "Se seleccionó el diseño Secuencial Explicativo porque:",
    "(a) la pregunta central es predominantemente cuantitativa;",
    "(b) el tiempo del programa (16 semanas) favorece la secuencia;",
    "(c) la exploración cualitativa DEPENDE de los hallazgos cuantitativos previos.",
    "La sugerencia del GPT para el diseño Convergente fue rechazada porque",
    "requeriría recolección simultánea que excede la capacidad del sprint."
  ),
  verificacion    = "Contrastado con: Creswell, J.W. & Plano Clark, V.L. (2018). Cap. 3, pp. 61-88."
)

cat("Herramienta IA:", ia_decision$herramienta, "\n\n")
cat("Prompt usado:\n  «", ia_decision$prompt_usado, "»\n\n")
cat("Salida de la IA:\n  →", ia_decision$salida_ia, "\n\n")
cat("Decisión HUMANA y justificación:\n")
cat(strwrap(ia_decision$decision_humana, width = 68, prefix = "  "), sep = "\n")
cat("\nVerificación:\n  →", ia_decision$verificacion, "\n")

cat("\n=============================================================\n")
cat("  ✓ Script 01 completado.\n")
cat("  Siguiente paso: ejecutar 02_datos_sinteticos_costeros.R\n")
cat("=============================================================\n")
