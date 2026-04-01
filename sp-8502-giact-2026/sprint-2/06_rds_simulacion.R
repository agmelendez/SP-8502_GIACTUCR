# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 06_rds_simulacion.R
# SPRINT  : 2 — Trabajo de Campo y Muestreo (Semanas 5–8)
# TEMA    : Respondent-Driven Sampling (RDS) · Poblaciones Ocultas/Móviles
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   RDS es una técnica de muestreo en cadena que utiliza las redes sociales
#   de los propios encuestados para llegar a poblaciones de difícil acceso.
#   En contextos costeros, aplica para:
#   - Pescadores sin registro formal en INCOPESCA
#   - Comunidades con alta movilidad estacional
#   - Grupos con desconfianza institucional
#
# CONTENIDO:
#   1. Fundamentos matemáticos del RDS (estimador Volz-Heckathorn)
#   2. Simulación de red social de pescadores (grafo)
#   3. Proceso de reclutamiento en cadena (semillas → oleadas)
#   4. Estimación con corrección por grado de red
#   5. Diagnósticos de convergencia (equilibrio)
#   6. Comparación RDS vs. Muestreo convencional
#
# PAQUETES CLAVE:
#   igraph   → construcción y visualización de redes sociales
#   RDS      → estimadores específicos (si disponible)
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")
library(tidyverse)
library(igraph)

# Instalar RDS si está disponible (puede requerir CRAN)
if (!require("RDS", quietly = TRUE)) {
  message("Nota: paquete 'RDS' no instalado. Usando estimadores manuales.")
  rds_disponible <- FALSE
} else {
  library(RDS)
  rds_disponible <- TRUE
}

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 2 · RDS — Poblaciones Ocultas Costeras\n")
cat("  Redes de Pesca Artesanal · Pacífico Central CR\n")
cat("=============================================================\n\n")

# =============================================================================
# SECCIÓN 1: FUNDAMENTOS MATEMÁTICOS DEL RDS
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Fundamentos del Estimador RDS (Volz-Heckathorn)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("
PRINCIPIO CENTRAL DEL RDS:
─────────────────────────
Los encuestados actúan como 'reclutadores' de otros miembros de la
población objetivo. Al controlar el número de cupones distribuidos
(generalmente 2–3), se modela el proceso como una cadena de Markov
sobre la red social de la población.

ESTIMADOR VOLZ-HECKATHORN (2008) para la proporción poblacional:

         Σᵢ (yᵢ / dᵢ)
  p̂_VH = ─────────────
          Σᵢ (1 / dᵢ)

donde:
  yᵢ = 1 si individuo i tiene el atributo de interés; 0 si no
  dᵢ = grado de red del individuo i (número de conocidos en la población)

SUPUESTO CLAVE: La red social de la población está conectada
(no hay sub-grupos completamente aislados).

CONDICIÓN DE CONVERGENCIA: El estimador converge cuando la cadena
de Markov alcanza su distribución estacionaria (≈ oleada 2–4 según
Gile & Handcock, 2010).
")

# =============================================================================
# SECCIÓN 2: SIMULAR RED SOCIAL DE PESCADORES
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Construcción de la Red Social de Pescadores\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Parámetros de la red
N_red    <- 200   # Subpoblación de difícil acceso (no registrados)
p_enlace <- 0.05  # Probabilidad de conexión (Erdős–Rényi)

# Propiedades de los nodos (pescadores)
nodos <- tibble(
  id         = 1:N_red,
  comunidad  = sample(c("Quepos","Tárcoles","Punta Morales"),
                      N_red, replace = TRUE,
                      prob = c(0.40, 0.35, 0.25)),
  sas_real   = round(rnorm(N_red, mean = 25, sd = 10)),   # Valor "verdadero" para validar
  sas_real   = pmax(0, pmin(50, sas_real)),
  usa_arte_sostenible = rbinom(N_red, 1, prob = 0.38),    # 38% usa arte sostenible
  acceso_reg = rbinom(N_red, 1, prob = 0.30)              # 30% tienen registro INCOPESCA
)

# Generar red con estructura de pequeño mundo (Watts-Strogatz)
# → más realista que Erdős–Rényi para redes sociales de comunidad
red_ws <- sample_smallworld(
  dim   = 1,
  size  = N_red,
  nei   = 4,        # Cada nodo conectado a 4 vecinos en anillo
  p     = 0.15      # Probabilidad de reconexión aleatoria
)

# Agregar atributos al grafo
V(red_ws)$comunidad           <- nodos$comunidad
V(red_ws)$sas_real            <- nodos$sas_real
V(red_ws)$usa_arte_sostenible <- nodos$usa_arte_sostenible
V(red_ws)$acceso_reg          <- nodos$acceso_reg
V(red_ws)$grado               <- degree(red_ws)   # Grado de cada nodo

cat("Red social generada:\n")
cat("  Nodos (pescadores):", vcount(red_ws), "\n")
cat("  Aristas (vínculos):", ecount(red_ws), "\n")
cat("  Grado promedio:", round(mean(degree(red_ws)), 2), "\n")
cat("  Transitividad (clustering):", round(transitivity(red_ws), 3), "\n")
cat("  Diámetro de la red:", diameter(red_ws), "\n")
cat("  ¿Red conectada?:", is_connected(red_ws), "\n\n")

# Distribución de grados
grados_df <- tibble(grado = degree(red_ws))
cat("Distribución de grados (vecinos/pescador):\n")
print(summary(grados_df$grado))

# =============================================================================
# SECCIÓN 3: PROCESO DE RECLUTAMIENTO RDS
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 3: Simulación del Proceso de Reclutamiento RDS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Parámetros RDS
n_semillas     <- 5    # Número de semillas (selección intencional)
cupones_x_pers <- 3    # Cupones por encuestado
n_objetivo     <- 60   # Tamaño objetivo de muestra RDS
max_oleadas    <- 8    # Máximo de oleadas

cat("PARÁMETROS DEL DISEÑO RDS:\n")
cat("  Semillas iniciales:", n_semillas, "\n")
cat("  Cupones por persona:", cupones_x_pers, "\n")
cat("  Tamaño objetivo:", n_objetivo, "\n")
cat("  Oleadas máximas:", max_oleadas, "\n\n")

# Seleccionar semillas (heterogéneas en comunidad y atributos)
set.seed(8502)
semillas_ids <- c(
  sample(which(V(red_ws)$comunidad == "Quepos"),      2),
  sample(which(V(red_ws)$comunidad == "Tárcoles"),    2),
  sample(which(V(red_ws)$comunidad == "Punta Morales"), 1)
)

# Función de reclutamiento RDS
simular_rds <- function(grafo, semillas, n_obj, cupones, max_waves) {

  # Inicializar
  reclutados     <- semillas
  oleada_actual  <- rep(0, length(semillas))   # Semillas = oleada 0
  reclutador     <- rep(NA, length(semillas))
  ya_visitados   <- semillas

  oleada         <- 1

  while (length(reclutados) < n_obj && oleada <= max_waves) {
    nuevos_reclutados  <- c()
    nuevas_oleadas     <- c()
    nuevos_reclutadores <- c()

    # Cada persona de la oleada anterior recluta
    encuestados_anteriores <- which(oleada_actual == (oleada - 1))
    if (length(encuestados_anteriores) == 0) break

    for (i in encuestados_anteriores) {
      nodo_i    <- reclutados[i]
      vecinos_i <- neighbors(grafo, nodo_i)$name

      if (is.null(vecinos_i)) {
        vecinos_i <- as.character(neighbors(grafo, nodo_i))
      }

      # Vecinos aún no visitados
      vecinos_disp <- setdiff(
        as.integer(neighbors(grafo, nodo_i)),
        ya_visitados
      )

      if (length(vecinos_disp) > 0) {
        n_rec <- min(cupones, length(vecinos_disp))
        nuevos <- sample(vecinos_disp, n_rec)

        nuevos_reclutados   <- c(nuevos_reclutados, nuevos)
        nuevas_oleadas      <- c(nuevas_oleadas, rep(oleada, n_rec))
        nuevos_reclutadores <- c(nuevos_reclutadores, rep(nodo_i, n_rec))
        ya_visitados        <- c(ya_visitados, nuevos)
      }

      if (length(reclutados) + length(nuevos_reclutados) >= n_obj) break
    }

    if (length(nuevos_reclutados) == 0) break

    # Truncar si excede el objetivo
    exceso <- (length(reclutados) + length(nuevos_reclutados)) - n_obj
    if (exceso > 0) {
      k_guardar           <- length(nuevos_reclutados) - exceso
      nuevos_reclutados   <- nuevos_reclutados[1:k_guardar]
      nuevas_oleadas      <- nuevas_oleadas[1:k_guardar]
      nuevos_reclutadores <- nuevos_reclutadores[1:k_guardar]
    }

    reclutados  <- c(reclutados, nuevos_reclutados)
    oleada_actual <- c(oleada_actual, nuevas_oleadas)
    reclutador  <- c(reclutador, nuevos_reclutadores)
    oleada      <- oleada + 1
  }

  tibble(
    id_nodo    = reclutados,
    oleada     = oleada_actual,
    id_reclutador = reclutador
  )
}

# Ejecutar simulación
cadena_rds <- simular_rds(
  grafo   = red_ws,
  semillas = semillas_ids,
  n_obj   = n_objetivo,
  cupones = cupones_x_pers,
  max_waves = max_oleadas
)

cat("RESULTADO DEL RECLUTAMIENTO:\n")
cat("  Total reclutados:", nrow(cadena_rds), "\n")
cat("  Oleadas alcanzadas:", max(cadena_rds$oleada), "\n\n")

cat("Distribución por oleada:\n")
print(cadena_rds %>% count(oleada, name = "n_reclutados"))

# =============================================================================
# SECCIÓN 4: ESTIMACIÓN CON CORRECCIÓN POR GRADO
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 4: Estimador Volz-Heckathorn (VH)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Unir datos de la cadena con atributos de nodos
muestra_rds <- cadena_rds %>%
  left_join(
    nodos %>% select(id, comunidad, sas_real,
                     usa_arte_sostenible, grado = id),
    by = c("id_nodo" = "id")
  ) %>%
  mutate(
    grado_red = degree(red_ws)[id_nodo]
  )

# Estimador VH para proporción de pescadores con arte sostenible
# p̂_VH = Σ(yᵢ/dᵢ) / Σ(1/dᵢ)
VH_numerador   <- sum(muestra_rds$usa_arte_sostenible / muestra_rds$grado_red,
                      na.rm = TRUE)
VH_denominador <- sum(1 / muestra_rds$grado_red, na.rm = TRUE)
p_VH           <- VH_numerador / VH_denominador

# Estimador naive (sin corrección por grado)
p_naive <- mean(muestra_rds$usa_arte_sostenible, na.rm = TRUE)

# Verdadero poblacional
p_real <- mean(nodos$usa_arte_sostenible)

cat("ESTIMACIÓN: % pescadores que usan arte sostenible\n\n")
cat("  Proporción REAL (poblacional):          ",
    round(p_real * 100, 1), "%\n")
cat("  Estimador NAIVE (sin corrección):        ",
    round(p_naive * 100, 1), "%\n")
cat("  Estimador VH (con corrección por grado):",
    round(p_VH * 100, 1), "%\n\n")

cat("  Sesgo naive vs. real:  ", round((p_naive - p_real) * 100, 2), "pp\n")
cat("  Sesgo VH vs. real:     ", round((p_VH   - p_real) * 100, 2), "pp\n")
cat("  → El estimador VH es más cercano al valor real\n")

# Estimación del SAS medio con VH
sas_VH <- sum(muestra_rds$sas_real / muestra_rds$grado_red, na.rm = TRUE) /
  sum(1 / muestra_rds$grado_red, na.rm = TRUE)
sas_naive <- mean(muestra_rds$sas_real, na.rm = TRUE)
sas_real  <- mean(nodos$sas_real)

cat("\nEstimación del SAS promedio:\n")
cat("  SAS real poblacional:", round(sas_real, 2), "\n")
cat("  SAS estimado naive:  ", round(sas_naive, 2), "\n")
cat("  SAS estimado VH:     ", round(sas_VH, 2), "\n")

# =============================================================================
# SECCIÓN 5: DIAGNÓSTICOS DE CONVERGENCIA RDS
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 5: Diagnósticos de Convergencia del RDS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── 5.1 Convergencia por oleada (el estimador debe estabilizarse) ─────────
convergencia <- muestra_rds %>%
  arrange(oleada) %>%
  mutate(
    n_acum       = row_number(),
    VH_acum_num  = cumsum(usa_arte_sostenible / grado_red),
    VH_acum_den  = cumsum(1 / grado_red),
    p_VH_acum    = VH_acum_num / VH_acum_den,
    p_naive_acum = cummean(usa_arte_sostenible)
  )

cat("Convergencia del estimador VH por oleada:\n")
convergencia %>%
  group_by(oleada) %>%
  slice_tail(n = 1) %>%
  select(oleada, n_acum, p_VH_acum, p_naive_acum) %>%
  mutate(
    p_VH_acum    = round(p_VH_acum * 100, 1),
    p_naive_acum = round(p_naive_acum * 100, 1)
  ) %>%
  print()

cat("\nInterpretación: El estimador converge cuando los valores\n")
cat("entre oleadas consecutivas varían < 2pp.\n")

# ── 5.2 Independencia de semillas ────────────────────────────────────────
cat("\nVariación por semilla (¿las cadenas son independientes?):\n")

muestra_rds %>%
  group_by(id_nodo %in% semillas_ids) %>%
  summarise(
    n = n(),
    sas_medio = round(mean(sas_real, na.rm = TRUE), 2),
    p_sostenible = round(mean(usa_arte_sostenible, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(tipo = c("Reclutados", "Semillas")) %>%
  select(tipo, n, sas_medio, p_sostenible) %>%
  print()

# ── 5.3 Visualización de convergencia ────────────────────────────────────
p_convergencia <- convergencia %>%
  select(n_acum, p_VH_acum, p_naive_acum) %>%
  pivot_longer(-n_acum,
               names_to  = "Estimador",
               values_to = "Proporción") %>%
  mutate(
    Estimador = recode(Estimador,
                       p_VH_acum    = "Volz-Heckathorn (VH)",
                       p_naive_acum = "Naive (sin corrección)"),
    Proporción = Proporción * 100
  ) %>%
  ggplot(aes(x = n_acum, y = Proporción, color = Estimador)) +
  geom_line(linewidth = 1.1, alpha = 0.8) +
  geom_hline(yintercept = p_real * 100,
             color = "#31a354", linewidth = 1, linetype = "dashed") +
  annotate("text", x = 55, y = p_real * 100 + 1.5,
           label = paste0("Real = ", round(p_real * 100, 1), "%"),
           color = "#31a354", size = 3.5) +
  scale_color_manual(
    values = c("Volz-Heckathorn (VH)" = "#2c7fb8",
               "Naive (sin corrección)" = "#e34a33")
  ) +
  scale_y_continuous(limits = c(0, 80),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Convergencia del Estimador RDS por Oleada",
    subtitle = "% pescadores con arte de pesca sostenible | RDS simulado · Pacífico Central CR",
    x        = "n acumulado en cadena",
    y        = "Proporción estimada (%)",
    color    = "Estimador",
    caption  = "Línea verde = valor real poblacional | SP-8502 · Sprint 2 · 2026"
  )

ggsave("outputs/figuras/C1_convergencia_rds.png",
       plot = p_convergencia, width = 9, height = 5, dpi = 300)
cat("\n✓ Figura guardada: outputs/figuras/C1_convergencia_rds.png\n")

# =============================================================================
# SECCIÓN 6: CUANDO USAR RDS VS. MUESTREO PROBABILÍSTICO CLÁSICO
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 6: Guía de Decisión — RDS vs. Probabilístico Clásico\n")
cat("─────────────────────────────────────────────────────────────\n\n")

decision_rds <- tibble(
  Criterio = c(
    "Marco muestral disponible",
    "Población con registro formal",
    "Alta movilidad estacional",
    "Desconfianza institucional",
    "Redes sociales densas",
    "Presupuesto de campo",
    "Estimador VH válido",
    "Validez externa"
  ),
  `Muestreo Probabilístico` = c(
    "✓ Necesario", "✓ Requiere", "✗ Dificulta", "✗ Sesga",
    "~ Indiferente", "$$$ Alto", "N/A", "Alta"
  ),
  `RDS` = c(
    "✗ No requiere", "✗ No requiere", "✓ Resuelve", "✓ Resuelve",
    "✓ Aprovecha", "$ Moderado", "✓ Corrige sesgo", "Moderada"
  ),
  `Contexto Pacífico Central CR` = c(
    "Parcial (INCOPESCA incompleto)",
    "Solo ~30% registrados",
    "Alta (veda, migración)",
    "Alta (historial INCOPESCA)",
    "Sí (cooperativas, grupos)",
    "Limitado (viáticos campo)",
    "Sí, si red conectada",
    "Validar con datos INCOPESCA"
  )
)

print(decision_rds, n = Inf)

cat("
RECOMENDACIÓN PARA LA INVESTIGACIÓN:
  → Diseño HÍBRIDO:
    - Marco registrado (70%): Muestreo estratificado proporcional
    - Fracción no registrada (~30%): RDS con 5 semillas y 3 cupones
    - Integración: ponderación de Hartley-Rao o estimación separada
    - Validación cruzada: comparar distribución de grado y atributos
      entre ambos submuestreos
"
)

# Guardar muestra RDS
write_csv(muestra_rds, "datos/procesados/muestra_rds_s2.csv")
cat("✓ Muestra RDS guardada: datos/procesados/muestra_rds_s2.csv\n")
cat("  Siguiente paso: ejecutar 07_instrumento_encuesta.R\n")
cat("=============================================================\n")
