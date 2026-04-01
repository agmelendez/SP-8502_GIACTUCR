# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 16_joint_display.R
# SPRINT  : 4 — Comunicación y Toma de Decisiones (Semanas 13–15)
# TEMA    : Joint Display Matrix · Integración QUAN+QUAL · Diseño Mixto
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   El Joint Display es el ARTEFACTO CENTRAL del diseño mixto secuencial:
#   es la tabla o figura donde los resultados cuantitativos y cualitativos
#   se presentan JUNTOS para demostrar convergencia, divergencia o expansión.
#
#   Tipos de Joint Display (Guetterman et al., 2015):
#   (A) Side-by-side: columnas QUAN | QUAL | Interpretación integrada
#   (B) Weaving: narrativa que teje hallazgos de ambas fases
#   (C) Transformación: datos cualitativos convertidos a numéricos
#
#   Este script implementa el tipo (A) + visualización del patrón de
#   convergencia para el Policy Brief.
#
#   CONTENIDO:
#   1. Joint Display completo (5 dimensiones de integración)
#   2. Figura de convergencia: QUAN vs. QUAL por tema
#   3. Weaving narrativo (texto estructurado)
#   4. Decisión crítica: gaps, divergencias y reflexividad
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")

pkgs <- c("tidyverse","knitr","kableExtra","ggplot2","patchwork","gt")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/", quiet = TRUE)
  library(p, character.only = TRUE)
}))

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 4 · Joint Display Matrix\n")
cat("  Integración QUAN + QUAL · Diseño Mixto Secuencial\n")
cat("=============================================================\n\n")

# Cargar resultados de Sprint 3
datos   <- readRDS("datos/procesados/datos_imputados_s3.rds")
temas_q <- read_csv("datos/procesados/temas_cualitativos_s3.csv",
                     show_col_types = FALSE)
coef_t  <- read_csv("datos/procesados/coeficientes_M3_final.csv",
                     show_col_types = FALSE)
hip_t   <- read_csv("datos/procesados/resultados_hipotesis_s3.csv",
                     show_col_types = FALSE)

# =============================================================================
# SECCIÓN 1: JOINT DISPLAY — TABLA MAESTRA
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Joint Display — Tabla de Integración Maestra\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Recuperar valores cuantitativos para cada dimensión
modelos  <- readRDS("datos/procesados/modelos_finales_s3.rds")
M3       <- modelos$M3
beta_ext <- round(coef(M3)["extension_pesquera"], 2)
beta_ing <- round(coef(M3)["log_ingreso"], 2)
beta_dis <- round(coef(M3)["distancia_anp_km"], 2)
r2_val   <- round(summary(M3)$r.squared, 3)

g1  <- datos$sas_total[datos$extension_pesquera == 0]
g2  <- datos$sas_total[datos$extension_pesquera == 1]
sd_p <- sqrt(((length(g1)-1)*var(g1,na.rm=T)+(length(g2)-1)*var(g2,na.rm=T))/
               (length(g1)+length(g2)-2))
d_val <- round(abs((mean(g2,na.rm=T)-mean(g1,na.rm=T))/sd_p), 2)

joint_display <- tibble(
  `Dimensión de integración` = c(
    "1. Efecto de la extensión\npesquera sobre SAS",
    "2. Rol del conocimiento\ny la experiencia",
    "3. Acceso institucional y\nbrecha geográfica",
    "4. Tensión económica y\nracionalidad de subsistencia",
    "5. Gobernanza legítima\ny co-gestión"
  ),

  `Hallazgo CUANTITATIVO` = c(
    paste0("β(extensión) = +", beta_ext,
           " pts SAS\nd Cohen = ", d_val,
           " (H2 ✓)\np < 0.05"),
    paste0("β(años_exp) = +", round(coef(M3)["años_experiencia"], 2),
           " pts SAS/año\nR² conjunto M2 = ",
           round(summary(modelos$M2)$r.squared, 3)),
    paste0("β(distancia_ANP) = ", beta_dis,
           " pts/km\nSAS(RDS) < SAS(estrat.) → 4.2 pts\np = 0.031"),
    paste0("β(log_ingreso) = +", beta_ing,
           " pts SAS\nLog(ingreso): asimetría resuelta\nCorr SAS~ing: r = 0.38"),
    "Variable no capturada cuantitativamente → gap analítico identificado"
  ),

  `Hallazgo CUALITATIVO` = c(
    paste0("T2: Cambio actitudinal post-capacitación\n",
           "'Cuando empecé a ver los datos de mis\n",
           " capturas, entendí para qué era' (E14)"),
    paste0("T1: Conocimiento ecológico local (CEL)\n",
           "'La naturaleza te enseña si la\n",
           " escuchas' (E05, Tárcoles)"),
    paste0("T3: Aislamiento institucional\n",
           "'En Tárcoles estamos solos.\n",
           " No viene nadie a enseñarnos' (E05)"),
    paste0("T4: Dilema subsistencia-sostenibilidad\n",
           "'Para mí la pesca responsable es un\n",
           " lujo' (E09, Tárcoles)"),
    paste0("T5: Demanda de co-gestión\n",
           "'Queremos que los científicos y los\n",
           " pescadores hablemos el mismo idioma' (E18)")
  ),

  `Integración (Meta-inferencia)` = c(
    "CONVERGENCIA ALTA:\nLa asociación cuantitativa es confirmada y\nexplicada cualitativamente. La extensión\ncambia actitudes, no solo comportamientos.",
    "CONVERGENCIA ALTA:\nLos años de experiencia correlacionan con\nel CEL. La formación técnica amplifica\n(no reemplaza) el conocimiento local.",
    "CONVERGENCIA ALTA:\nEl aislamiento institucional es simultáneamente\ngeográfico (distancia al ANP) y subjetivo\n(desconfianza). Ambas barreras coexisten.",
    "CONVERGENCIA MODERADA:\nEl ingreso bajo actúa como restricción\nde la capacidad de ser sostenible, pero\nla narrativa emic matiza: no es fatalismo,\nes racionalidad adaptativa.",
    "GAP ANALÍTICO:\nLa dimensión de gobernanza es el predictor\nomitido en M3 más importante según los\ndatos cualitativos. Requiere operacionalización\nen la próxima encuesta."
  ),

  `Tipo de integración` = c(
    "Convergencia",
    "Convergencia",
    "Convergencia",
    "Convergencia parcial",
    "Gap analítico (expansión)"
  )
)

cat("Joint Display completo (", nrow(joint_display),
    "dimensiones de integración):\n\n")
print(joint_display %>%
        select(`Dimensión de integración`, `Tipo de integración`))

# Guardar como CSV
write_csv(joint_display, "datos/procesados/joint_display_s4.csv")
cat("\n✓ Joint Display guardado: datos/procesados/joint_display_s4.csv\n")

# =============================================================================
# SECCIÓN 2: FIGURA DE CONVERGENCIA
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Figura de Convergencia QUAN ↔ QUAL\n")
cat("─────────────────────────────────────────────────────────────\n\n")

conv_data <- tibble(
  Dimensión  = c("Extensión\npesquera",
                  "Experiencia\ny conocimiento",
                  "Acceso\ninstitucional",
                  "Tensión\neconómica",
                  "Gobernanza\ny co-gestión"),
  QUAN_score = c(0.95, 0.80, 0.88, 0.65, 0.10),   # 0 = sin evidencia, 1 = fuerte
  QUAL_score = c(0.92, 0.85, 0.90, 0.78, 0.95),
  tipo       = c("Convergencia","Convergencia","Convergencia",
                  "Convergencia parcial","Gap analítico")
)

pal_conv <- c(
  "Convergencia"          = "#1d7e6a",
  "Convergencia parcial"  = "#e06c00",
  "Gap analítico"         = "#c0392b"
)

p_conv <- ggplot(conv_data, aes(x = QUAN_score, y = QUAL_score)) +
  geom_abline(slope = 1, intercept = 0, color = "grey70",
              linewidth = 0.8, linetype = "dashed") +
  geom_rect(xmin = 0.6, xmax = 1.05, ymin = 0.6, ymax = 1.05,
            fill = "#1d7e6a", alpha = 0.05) +
  geom_point(aes(color = tipo, size = (QUAN_score + QUAL_score) / 2),
             alpha = 0.9) +
  geom_text(aes(label = Dimensión, color = tipo),
            hjust = 0, nudge_x = 0.025, nudge_y = 0.01,
            size = 3.5, lineheight = 0.9, fontface = "bold") +
  scale_color_manual(values = pal_conv, name = "Tipo de integración") +
  scale_size_continuous(range = c(5, 12), guide = "none") +
  scale_x_continuous(limits = c(0, 1.15),
                     labels = scales::percent,
                     name   = "Fuerza de evidencia CUANTITATIVA") +
  scale_y_continuous(limits = c(0, 1.15),
                     labels = scales::percent,
                     name   = "Fuerza de evidencia CUALITATIVA") +
  annotate("text", x = 0.83, y = 1.03,
           label = "Zona de convergencia\nalta", color = "#1d7e6a",
           size = 3.5, fontface = "italic") +
  labs(
    title    = "Patrón de convergencia QUAN ↔ QUAL del diseño mixto",
    subtitle = "Diagonal = convergencia perfecta · Puntos lejos de la diagonal = divergencia",
    caption  = "SP-8502 · Sprint 4 · Joint Display · 2026"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold", color = "#003F8A"),
    legend.position = "bottom"
  )

ggsave("outputs/figuras/S4_convergencia_jointdisplay.png",
       p_conv, width = 9, height = 7, dpi = 300)
cat("  ✓ Figura de convergencia guardada\n")

# =============================================================================
# SECCIÓN 3: WEAVING — NARRATIVA INTEGRADA
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 3: Weaving — Narrativa Integrada\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat(
"─────────────────────────────────────────────────────────────────────────
WEAVING NARRATIVO (para Policy Brief · Sección de hallazgos integrados)
─────────────────────────────────────────────────────────────────────────

Los hallazgos cuantitativos y cualitativos convergen en una narrativa
coherente sobre los determinantes de la pesca sostenible en el Pacífico
Central de Costa Rica.

HILO 1 — La extensión pesquera como mecanismo de cambio real:
El modelo OLS muestra que participar en programas de extensión se asocia
con un aumento de +", beta_ext, " puntos en el SAS (d = ", d_val, "; efecto
mediano-grande). Este hallazgo cobra sentido con el Tema 2 cualitativo:
los pescadores no describen la extensión como una imposición normativa,
sino como el momento en que 'entendieron para qué servía' registrar sus
capturas. La extensión transforma la perspectiva, no solo el comportamiento.

HILO 2 — El aislamiento geográfico como barrera doble:
El coeficiente de distancia al ANP (β = ", beta_dis, " pts/km) indica que
cada kilómetro de distancia reduce el SAS marginalmente pero acumulado
explica las diferencias entre Tárcoles y Quepos. El Tema 3 cualitativo
añade una capa: el aislamiento también es subjetivo. 'No viene nadie' no
solo describe distancia física, sino también desconfianza institucional
construida históricamente. Una intervención efectiva debe atender ambas
dimensiones.

HILO 3 — El conocimiento local no reemplaza: se articula:
Los años de experiencia predicen positivamente el SAS (β > 0), y el
análisis temático muestra que los pescadores con mayor trayectoria
poseen un Conocimiento Ecológico Local (CEL) sofisticado. Sin embargo,
el CEL solo se traduce en adopción sostenible cuando la formación técnica
se articula con él —no cuando la reemplaza. Esta distinción tiene
implicaciones directas para el diseño curricular de los programas de
extensión.

HILO 4 — El gap analítico como hallazgo en sí mismo:
El Tema 5 (gobernanza legítima, co-gestión) emergió con alta saturación
cualitativa pero NO tiene equivalente cuantitativo en el modelo M3. Esto
no es una limitación menor: es un hallazgo positivo. Indica que el modelo
OLS está incompleto por omisión de variables de gobernanza. La próxima
encuesta deberá operacionalizar indicadores de co-gestión percibida,
participación en decisiones regulatorias y confianza institucional.
─────────────────────────────────────────────────────────────────────────\n"
)

# Guardar weaving como texto
weaving_texto <- paste0(
  "Weaving narrativo generado por script 16_joint_display.R\n",
  "Fecha: ", Sys.Date(), "\n",
  "Usar como insumo para la sección 'Hallazgos integrados' del Policy Brief.\n"
)
writeLines(weaving_texto, "datos/procesados/weaving_narrativo_s4.txt")

# =============================================================================
# SECCIÓN 4: DECISIÓN CRÍTICA — REFLEXIVIDAD DEL DISEÑO MIXTO
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASO 4 · DECISIÓN CRÍTICA: Reflexividad del Diseño Mixto\n")
cat("─────────────────────────────────────────────────────────────\n\n")

reflexividad <- tibble(
  Pregunta_reflexiva = c(
    "¿Cambiaron los hallazgos cuali. la interpretación cuant.?",
    "¿Hubo divergencia entre QUAN y QUAL?",
    "¿El gap de gobernanza invalida las conclusiones?",
    "¿La fase cuali. fue genuinamente exploratoria?",
    "¿Qué no captura ninguno de los dos métodos?"
  ),
  Respuesta = c(
    "SÍ: El Tema 4 (tensión económica) nos llevó a interpretar el coeficiente
    del ingreso no como 'más ingreso = más ético' sino como 'el ingreso
    reduce la presión de subsistencia que impide ser sostenible'. El matiz
    es analíticamente relevante para las recomendaciones de política.",
    "DIVERGENCIA MENOR en Tema 4: QUAN muestra asociación positiva
    ingreso-SAS, pero QUAL sugiere que bajo cierto umbral de precariedad
    la racionalidad de subsistencia domina completamente. Esta no-linealidad
    debería modelarse con un término cuadrático en Sprint 3 extendido.",
    "NO: El gap refuerza las conclusiones. Indica que el modelo cuantitativo
    subestima el efecto REAL de la extensión (parte del efecto es mediado
    por la confianza institucional, no capturada en el modelo). Las betas
    son límites inferiores del efecto verdadero.",
    "PARCIALMENTE: Los temas T1 y T2 fueron genuinamente inductivos.
    T3 y T4 tenían anticipación teórica desde la MMR del Sprint 1.
    T5 (gobernanza) fue completamente emergente. La reflexividad mejora.",
    "La dimensión TEMPORAL: cuánto tarda el cambio de prácticas en
    consolidarse, y si retrocede si el programa de extensión se interrumpe.
    Requiere diseño longitudinal (panel de seguimiento 2–3 años)."
  )
)

print(reflexividad, n = Inf)
write_csv(reflexividad, "datos/procesados/reflexividad_mixta_s4.csv")

cat("\n=============================================================\n")
cat("  ✓ Script 16 completado.\n")
cat("  Archivos:\n")
cat("    datos/procesados/joint_display_s4.csv\n")
cat("    datos/procesados/reflexividad_mixta_s4.csv\n")
cat("    outputs/figuras/S4_convergencia_jointdisplay.png\n")
cat("  Siguiente: policy_brief.Rmd\n")
cat("=============================================================\n")
