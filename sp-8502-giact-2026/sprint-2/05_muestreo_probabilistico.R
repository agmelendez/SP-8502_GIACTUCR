# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 05_muestreo_probabilistico.R
# SPRINT  : 2 — Trabajo de Campo y Muestreo (Semanas 5–8)
# TEMA    : Estrategias de Muestreo Probabilístico · Marco Muestral Costero
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# VERSIÓN : 1.0
# =============================================================================
# DESCRIPCIÓN:
#   Implementa y compara tres diseños de muestreo probabilístico:
#     (1) Muestreo Aleatorio Simple (MAS)
#     (2) Muestreo Estratificado Proporcional y Óptimo (Neyman)
#     (3) Muestreo Sistemático
#
#   Todo referenciado a la POBLACIÓN de estudio: pescadores artesanales
#   del Pacífico Central de Costa Rica (N ≈ 820 según INCOPESCA 2025).
#
# SECUENCIA PEDAGÓGICA SP-8502:
#   Cálculo → Interpretación Estadística → Contexto Costero → Decisión Crítica
#
# CONEXIÓN CON SPRINT 1:
#   Usa la MMR (mmr_sprint1.csv) para alinear la estrategia muestral
#   con las variables operacionalizadas en S1.
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")
library(tidyverse)
library(knitr)

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 2 · Muestreo Probabilístico\n")
cat("  Pacífico Central de Costa Rica · INCOPESCA 2025\n")
cat("=============================================================\n\n")

# =============================================================================
# SECCIÓN 1: MARCO MUESTRAL — POBLACIÓN DE REFERENCIA
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Marco Muestral (Población objetivo)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Marco poblacional basado en registros INCOPESCA 2025 (datos referenciales)
marco_muestral <- tibble(
  comunidad      = c("Quepos", "Jacó", "Tárcoles", "Herradura", "Punta Morales"),
  N_pescadores   = c(215, 180, 145, 160, 120),   # Total por comunidad
  prop_comunidad = c(0.262, 0.220, 0.177, 0.195, 0.146),
  sd_sas_ref     = c(9.1, 8.8, 9.5, 8.2, 10.1),  # Desv. estándar SAS (piloto)
  acceso_anp     = c("Sí","Sí","No","No","Sí"),   # Proximidad a ANP (<5km)
  tipo_pesca_dom = c("Línea de mano","Red agallera","Trasmallo",
                     "Red agallera","Palangre")
)

N_total <- sum(marco_muestral$N_pescadores)

cat("Población total (N):", N_total, "pescadores registrados\n")
cat("Fuente: INCOPESCA, Registro Nacional de Pesca Artesanal 2025\n\n")
print(marco_muestral)

# Verificar proporciones
cat("\nVerificación: suma de proporciones =",
    round(sum(marco_muestral$prop_comunidad), 4), "\n")

# =============================================================================
# SECCIÓN 2: CÁLCULO DEL TAMAÑO DE MUESTRA
# =============================================================================
# ══ PASO 1: CÁLCULO ══════════════════════════════════════════════════════════
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Cálculo del Tamaño de Muestra\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Parámetros de diseño
alpha   <- 0.05      # Nivel de significancia
Z_alpha <- qnorm(1 - alpha / 2)   # Z crítico = 1.96
E       <- 3.5       # Error máximo aceptable en puntos SAS (escala 0–50)
sigma   <- 9.5       # Desviación estándar conservadora (mayor de la tabla)
p_sup   <- 0.50      # Proporción conservadora para variables categóricas

cat("PARÁMETROS DE DISEÑO:\n")
cat("  Nivel de confianza: 1-α =", (1 - alpha) * 100, "%\n")
cat("  Z(α/2)            =", round(Z_alpha, 4), "\n")
cat("  Error máximo (E)  =", E, "puntos SAS\n")
cat("  σ (conservadora)  =", sigma, "\n")
cat("  N total           =", N_total, "\n\n")

# ── 2.1 Fórmula de Cochran (población infinita) ───────────────────────────
n0_continua <- (Z_alpha^2 * sigma^2) / E^2
n0_prop     <- (Z_alpha^2 * p_sup * (1 - p_sup)) / (0.05^2)   # E=5% para proporciones

cat("Fórmula de Cochran (sin corrección por finitud):\n")
cat("  n₀ (variable continua SAS, E=3.5):",
    ceiling(n0_continua), "\n")
cat("  n₀ (proporción categórica, E=5%):",
    ceiling(n0_prop), "\n\n")

# ── 2.2 Corrección por finitud ────────────────────────────────────────────
# Se aplica cuando n/N > 5%
n_corregida <- function(n0, N) {
  ceiling(n0 / (1 + (n0 - 1) / N))
}

n_mas <- n_corregida(n0_continua, N_total)
cat("Corrección por finitud (N =", N_total, "):\n")
cat("  n_MAS final =", n_mas, "pescadores\n")
cat("  Fracción de muestreo f = n/N =",
    round(n_mas / N_total * 100, 1), "%\n\n")

# ══ PASO 2: INTERPRETACIÓN ESTADÍSTICA ═══════════════════════════════════════
cat("─────────────────────────────────────────────────────────────\n")
cat("PASO 2: INTERPRETACIÓN ESTADÍSTICA\n")
cat("─────────────────────────────────────────────────────────────\n")
cat("
  Con n =", n_mas, "observaciones:
  • Se puede estimar la media del SAS con error ±", E, "puntos
    al 95% de confianza.
  • La fracción de muestreo (", round(n_mas/N_total*100,1), "%) JUSTIFICA la
    corrección por finitud: el muestreo NO es despreciable.
  • La σ = 9.5 fue tomada del estrato más variable (Punta Morales)
    → esto es conservador: si σ real es menor, E real < 3.5.\n"
)

# ══ PASO 3: CONTEXTO COSTERO ════════════════════════════════════════════════
cat("─────────────────────────────────────────────────────────────\n")
cat("PASO 3: CONTEXTO COSTERO\n")
cat("─────────────────────────────────────────────────────────────\n")
cat("
  CONSIDERACIONES ESPECÍFICAS DE CAMPO COSTERO:
  1. ESTACIONALIDAD: El período de encuesta (abril–mayo 2026) coincide
     con el inicio de veda del pargo, lo que reduce disponibilidad
     de pescadores activos en ~20%. Ajuste recomendado: n_campo = n_mas × 1.25
  2. ACCESO GEOGRÁFICO: Tárcoles y Punta Morales tienen acceso por lancha
     → costos logísticos elevados → peso diferencial en diseño estratificado
  3. RECHAZO: Históricamente, ~8-12% de pescadores rechaza participar en
     encuestas institucionales por desconfianza con INCOPESCA
     → factor de no-respuesta = 1.12 a 1.15\n"
)

n_ajustado <- ceiling(n_mas * 1.15 * 1.20)   # Rechazo + estacionalidad
cat("  n final ajustado por rechazo + estacionalidad:", n_ajustado, "\n")

# ══ PASO 4: DECISIÓN CRÍTICA ═════════════════════════════════════════════════
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASO 4: DECISIÓN CRÍTICA\n")
cat("─────────────────────────────────────────────────────────────\n")
cat("
  DECISIÓN ADOPTADA: n = 80 (ligeramente superior al n_mas =", n_mas, ")
  JUSTIFICACIÓN:
  (a) Cubre el error estándar requerido con margen de seguridad
  (b) Permite análisis de regresión múltiple (regla empírica: 10-20
      obs./predictor; con 5 predictores → mínimo 50–100 obs.)
  (c) Es operacionalmente viable con 2 encuestadores × 4 semanas
  (d) Permite sub-análisis por comunidad (mínimo 12–15 por estrato)

  REFERENCIA: Hair et al. (2019) Multivariate Data Analysis, 8va ed., p.102
              Cochran (1977) Sampling Techniques, 3ra ed., Cap. 4\n"
)

# =============================================================================
# SECCIÓN 3: MUESTREO ALEATORIO SIMPLE (MAS)
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 3: Diseño 1 — Muestreo Aleatorio Simple (MAS)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Simular marco de unidades muestrales
marco_completo <- tibble(
  id_pescador = paste0("P-", str_pad(1:N_total, width = 4, pad = "0")),
  comunidad   = rep(marco_muestral$comunidad,
                    times = marco_muestral$N_pescadores),
  edad_aprox  = round(rnorm(N_total, mean = 42, sd = 10)),
  años_exp    = round(pmax(1, rnorm(N_total, mean = 18, sd = 7)))
)

# Seleccionar muestra MAS
n_seleccion <- n_ajustado
indices_mas  <- sample(1:N_total, size = n_seleccion, replace = FALSE)
muestra_mas  <- marco_completo[indices_mas, ] %>%
  mutate(metodo_seleccion = "MAS")

cat("Muestra MAS seleccionada (n =", n_seleccion, "):\n")
cat("Distribución por comunidad (MAS vs. Marco):\n")

comparacion_mas <- muestra_mas %>%
  count(comunidad, name = "n_muestra") %>%
  left_join(marco_muestral %>% select(comunidad, N_pescadores,
                                       prop_comunidad), by = "comunidad") %>%
  mutate(
    prop_muestra  = round(n_muestra / sum(n_muestra), 3),
    diferencia_pp = round((prop_muestra - prop_comunidad) * 100, 1)
  )

print(comparacion_mas)

cat("\nValor esperado (probabilístico):",
    "proporciones MAS ≈ proporciones marco\n")
cat("Error de representación promedio:",
    round(mean(abs(comparacion_mas$diferencia_pp)), 2), "pp\n")

# =============================================================================
# SECCIÓN 4: MUESTREO ESTRATIFICADO
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 4: Diseño 2 — Muestreo Estratificado\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── 4.1 Asignación proporcional ───────────────────────────────────────────
marco_muestral <- marco_muestral %>%
  mutate(
    # Asignación proporcional: n_h = n × (N_h / N)
    n_proporcional = ceiling(n_seleccion * (N_pescadores / N_total)),
    # Asignación óptima de Neyman: n_h = n × (N_h × σ_h) / Σ(N_h × σ_h)
    Nh_sigma_h     = N_pescadores * sd_sas_ref,
    n_neyman       = ceiling(n_seleccion * (Nh_sigma_h / sum(Nh_sigma_h)))
  )

# Ajustar para que sumen exactamente n_seleccion
diferencia_prop <- n_seleccion - sum(marco_muestral$n_proporcional)
diferencia_neym <- n_seleccion - sum(marco_muestral$n_neyman)

cat("Comparación de asignaciones (n total =", n_seleccion, "):\n\n")
marco_muestral %>%
  select(comunidad, N_pescadores, sd_sas_ref,
         n_proporcional, n_neyman) %>%
  mutate(
    dif_asignacion = n_neyman - n_proporcional
  ) %>%
  print()

cat("\nTotal asignación proporcional:", sum(marco_muestral$n_proporcional), "\n")
cat("Total asignación Neyman:      ", sum(marco_muestral$n_neyman), "\n")

# ── 4.2 Comparación de varianzas estimadas ───────────────────────────────
# Varianza del estimador de la media bajo cada diseño
var_mas  <- (sigma^2 / n_seleccion) * (1 - n_seleccion / N_total)
var_prop <- sum(marco_muestral$prop_comunidad^2 *
                  marco_muestral$sd_sas_ref^2 /
                  marco_muestral$n_proporcional)
var_neym <- (1 / n_seleccion^2) *
  sum((marco_muestral$N_pescadores * marco_muestral$sd_sas_ref)^2 /
        marco_muestral$n_neyman)

cat("\nCOMPARACIÓN DE EFICIENCIA DE DISEÑOS:\n")
cat("  V(ȳ) bajo MAS:              ", round(var_mas, 5),
    "| SE =", round(sqrt(var_mas), 3), "\n")
cat("  V(ȳ) bajo estratif. prop.:  ", round(var_prop, 5),
    "| SE =", round(sqrt(var_prop), 3), "\n")
cat("  V(ȳ) bajo estratif. Neyman:", round(var_neym, 5),
    "| SE =", round(sqrt(var_neym), 3), "\n\n")

cat("Ganancia relativa Neyman vs MAS (DEFF⁻¹):",
    round(var_mas / var_neym, 3), "\n")
cat("→ El muestreo Neyman es", round(var_mas / var_neym, 1),
    "veces más eficiente que el MAS.\n")

# ── 4.3 Selección de muestra estratificada ───────────────────────────────
muestra_estrat <- marco_completo %>%
  left_join(marco_muestral %>% select(comunidad, n_proporcional),
            by = "comunidad") %>%
  group_by(comunidad) %>%
  slice_sample(n = first(n_proporcional)) %>%
  ungroup() %>%
  mutate(metodo_seleccion = "Estratificado proporcional")

cat("\nMuestra estratificada seleccionada (n =",
    nrow(muestra_estrat), "):\n")
print(muestra_estrat %>% count(comunidad))

# =============================================================================
# SECCIÓN 5: MUESTREO SISTEMÁTICO
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 5: Diseño 3 — Muestreo Sistemático (1-en-k)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

k       <- floor(N_total / n_seleccion)   # Intervalo de salto
inicio  <- sample(1:k, 1)                 # Arranque aleatorio
indices_sist <- seq(from = inicio, by = k, length.out = n_seleccion)
indices_sist <- indices_sist[indices_sist <= N_total]
muestra_sist <- marco_completo[indices_sist, ] %>%
  mutate(metodo_seleccion = "Sistemático")

cat("Intervalo de salto k =", k, "\n")
cat("Arranque aleatorio r =", inicio, "\n")
cat("Índices seleccionados (primeros 10):",
    paste(head(indices_sist, 10), collapse = ", "), "...\n")
cat("Muestra sistemática n =", nrow(muestra_sist), "\n\n")

cat("Distribución por comunidad (sistemático):\n")
print(muestra_sist %>% count(comunidad))

# =============================================================================
# SECCIÓN 6: RESUMEN COMPARATIVO DE DISEÑOS
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 6: Resumen Comparativo de Diseños Muestrales\n")
cat("─────────────────────────────────────────────────────────────\n\n")

resumen_diseños <- tibble(
  Diseño = c("MAS", "Estratificado proporcional",
             "Estratificado Neyman", "Sistemático"),
  n_total = c(nrow(muestra_mas), nrow(muestra_estrat),
               n_seleccion, nrow(muestra_sist)),
  SE_estimado = c(round(sqrt(var_mas), 3),
                  round(sqrt(var_prop), 3),
                  round(sqrt(var_neym), 3),
                  round(sqrt(var_mas * 1.05), 3)),   # Sist ≈ MAS con DEFF~1.05
  Ventaja = c(
    "Simplicidad conceptual y operativa",
    "Representación proporcional garantizada",
    "Máxima eficiencia estadística",
    "Fácil implementación en campo lineal"
  ),
  Limitacion = c(
    "Ineficiente si hay varianza inter-estrato alta",
    "Sub-representa estratos más variables",
    "Requiere σ previas por estrato (piloto)",
    "Riesgo de periodicidad si lista tiene patrón"
  ),
  Recomendado_contexto_costero = c("No","Sí (base)","Sí (ideal)","Parcial")
)

print(resumen_diseños, n = Inf)

# Guardar resultados
write_csv(marco_muestral,  "datos/procesados/marco_muestral_s2.csv")
write_csv(muestra_estrat,  "datos/procesados/muestra_estratificada_s2.csv")
write_csv(resumen_diseños, "datos/procesados/comparacion_diseños_muestrales.csv")

cat("\n✓ Archivos guardados en datos/procesados/\n")
cat("  Siguiente paso: ejecutar 06_rds_simulacion.R\n")
cat("=============================================================\n")
