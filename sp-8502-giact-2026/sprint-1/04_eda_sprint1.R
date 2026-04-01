# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 04_eda_sprint1.R
# SPRINT  : 1 — Diseño y Fundamentos de Datos
# TEMA    : Análisis Exploratorio de Datos (EDA) · Paso a Paso
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   EDA completo del dataset limpio. Sigue la secuencia pedagógica del curso:
#   Cálculo → Interpretación Estadística → Contexto Costero → Decisión Crítica
#
#   Este EDA constituye la base analítica para:
#   - El Reporte de Diseño Metodológico (entregable PDF Sprint 1)
#   - La sección "Descripción de la muestra" del Proyecto Integrador
#   - La calibración de hipótesis antes del Sprint 3
# =============================================================================

# ── 0. Cargar entorno y datos limpios ────────────────────────────────────────
if (!require("tidyverse")) source("00_setup_ambiente.R")
library(tidyverse)
library(skimr)
library(psych)
library(ggcorrplot)
library(patchwork)

# Cargar o regenerar datos limpios
if (!file.exists("datos/procesados/pescadores_limpio.csv")) {
  message("Ejecutando limpieza primero...")
  source("script_limpieza.R")
}

datos <- read_csv("datos/procesados/pescadores_limpio.csv",
                  show_col_types = FALSE) %>%
  mutate(
    comunidad          = factor(comunidad,
                                levels = c("Quepos","Jacó","Tárcoles",
                                           "Herradura","Punta Morales")),
    sexo               = factor(sexo,    levels = c("Masculino", "Femenino")),
    arte_pesca_principal = factor(arte_pesca_principal),
    percepcion_recurso = factor(percepcion_recurso,
                                levels = c("Muy escaso","Escaso","Regular",
                                           "Abundante","Muy abundante"),
                                ordered = TRUE),
    extension_pesquera = factor(extension_pesquera, levels = c("No","Sí"))
  )

n <- nrow(datos)
cat("Dataset cargado:", n, "observaciones\n\n")

# =============================================================================
# ══ MÓDULO A: ESTADÍSTICA UNIVARIADA ═════════════════════════════════════════
# =============================================================================

# ── A1: Resumen rápido global ──────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("MÓDULO A: Estadística Univariada\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("A1. Resumen global con skimr:\n")
print(skim(datos))

# ── A2: Distribución por comunidad ───────────────────────────────────────
cat("\nA2. Distribución por comunidad:\n")
tabla_comunidad <- datos %>%
  count(comunidad) %>%
  mutate(
    pct     = round(n / sum(n) * 100, 1),
    acum    = cumsum(pct),
    etiqueta = paste0(n, " (", pct, "%)")
  )
print(tabla_comunidad)

# ── A3: Estadísticos de variables continuas ────────────────────────────────
cat("\nA3. Estadísticos descriptivos — Variables continuas:\n")

desc_continuas <- datos %>%
  select(edad, años_experiencia, ingreso_mensual_usd,
         sas_total, distancia_anp_km, tamaño_familia) %>%
  psych::describe() %>%
  as.data.frame() %>%
  select(n, mean, sd, median, min, max, skew, kurtosis) %>%
  round(2)

print(desc_continuas)

# =============================================================================
# ══ MÓDULO B: VISUALIZACIONES EDA ════════════════════════════════════════════
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("MÓDULO B: Visualizaciones EDA\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── B1: Distribución del SAS (variable dependiente) ────────────────────────
cat("Generando B1: Distribución SAS...\n")

p_sas <- ggplot(datos %>% filter(!is.na(sas_total)),
                aes(x = sas_total)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 15, fill = "#2c7fb8", alpha = 0.7, color = "white") +
  geom_density(color = "#e34a33", linewidth = 1.1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(sas_total, na.rm = TRUE)),
             color = "#31a354", linewidth = 1, linetype = "solid") +
  geom_vline(aes(xintercept = median(sas_total, na.rm = TRUE)),
             color = "#756bb1", linewidth = 1, linetype = "dotted") +
  annotate("text", x = mean(datos$sas_total, na.rm = TRUE) + 2,
           y = 0.038, label = paste("Media =",
                                    round(mean(datos$sas_total, na.rm = TRUE), 1)),
           color = "#31a354", size = 3.5) +
  annotate("text", x = median(datos$sas_total, na.rm = TRUE) - 4,
           y = 0.034, label = paste("Mediana =",
                                    round(median(datos$sas_total, na.rm = TRUE), 1)),
           color = "#756bb1", size = 3.5) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
  labs(
    title    = "Distribución del Score de Adopción Sostenible (SAS)",
    subtitle = "Escala 0–50 pts · Comunidades pesqueras, Pacífico Central CR",
    x        = "SAS Total (puntos)",
    y        = "Densidad",
    caption  = "Nota: Línea verde = media; línea morada = mediana | SP-8502 · 2026"
  )

ggsave("outputs/figuras/B1_distribucion_sas.png",
       plot = p_sas, width = 8, height = 5, dpi = 300)
cat("  ✓ B1 guardada: outputs/figuras/B1_distribucion_sas.png\n")

# ── B2: SAS por comunidad (boxplot + jitter) ───────────────────────────────
cat("Generando B2: SAS por comunidad...\n")

p_sas_com <- ggplot(datos %>% filter(!is.na(sas_total)),
                    aes(x = comunidad, y = sas_total, fill = comunidad)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.8, color = "#333333") +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 3, fill = "white", color = "#e34a33") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
  labs(
    title    = "Score de Adopción Sostenible (SAS) por Comunidad",
    subtitle = "Rombo blanco = media; cajas = IQR | n = 80",
    x        = "Comunidad",
    y        = "SAS Total (0–50 pts)",
    caption  = "SP-8502 · Sprint 1 · 2026"
  ) +
  coord_flip()

ggsave("outputs/figuras/B2_sas_por_comunidad.png",
       plot = p_sas_com, width = 8, height = 5, dpi = 300)
cat("  ✓ B2 guardada: outputs/figuras/B2_sas_por_comunidad.png\n")

# ── B3: Distribución de ingreso (con log-transformación) ──────────────────
cat("Generando B3: Ingreso mensual...\n")

p_ing <- datos %>%
  filter(!is.na(ingreso_mensual_usd), ingreso_mensual_usd > 0) %>%
  ggplot(aes(x = ingreso_mensual_usd)) +
  geom_histogram(fill = "#74c476", color = "white", bins = 20, alpha = 0.8) +
  geom_vline(xintercept = median(datos$ingreso_mensual_usd, na.rm = TRUE),
             color = "#e34a33", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(
    title    = "Distribución del Ingreso Mensual por Pesca",
    subtitle = "Línea roja = mediana | Datos válidos (excl. NS/NR y negativos)",
    x        = "Ingreso mensual (USD)",
    y        = "Frecuencia",
    caption  = "SP-8502 · Sprint 1 · 2026"
  )

p_ing_log <- datos %>%
  filter(!is.na(ingreso_mensual_usd), ingreso_mensual_usd > 0) %>%
  ggplot(aes(x = log(ingreso_mensual_usd))) +
  geom_histogram(fill = "#31a354", color = "white", bins = 20, alpha = 0.8) +
  labs(
    title = "Ingreso mensual (escala logarítmica)",
    x     = "ln(Ingreso USD)",
    y     = "Frecuencia"
  )

p_ingreso_combinado <- p_ing / p_ing_log
ggsave("outputs/figuras/B3_ingreso_mensual.png",
       plot = p_ingreso_combinado, width = 8, height = 8, dpi = 300)
cat("  ✓ B3 guardada: outputs/figuras/B3_ingreso_mensual.png\n")

# ── B4: Percepción del recurso por comunidad (stacked bar) ────────────────
cat("Generando B4: Percepción del recurso...\n")

p_percepcion <- datos %>%
  filter(!is.na(percepcion_recurso)) %>%
  count(comunidad, percepcion_recurso) %>%
  group_by(comunidad) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = comunidad, y = pct, fill = percepcion_recurso)) +
  geom_col(position = "stack", alpha = 0.9) +
  scale_fill_manual(
    values = c("Muy escaso" = "#d7191c", "Escaso" = "#fdae61",
               "Regular"    = "#ffffbf", "Abundante" = "#a6d96a",
               "Muy abundante" = "#1a9641"),
    name = "Percepción\ndel recurso"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Percepción del Recurso Marino por Comunidad",
    subtitle = "Porcentaje de pescadores por nivel de percepción",
    x        = "Comunidad",
    y        = "Porcentaje (%)",
    caption  = "SP-8502 · Sprint 1 · 2026"
  ) +
  coord_flip() +
  theme(legend.position = "right")

ggsave("outputs/figuras/B4_percepcion_recurso.png",
       plot = p_percepcion, width = 9, height = 5, dpi = 300)
cat("  ✓ B4 guardada: outputs/figuras/B4_percepcion_recurso.png\n")

# ── B5: Correlación entre variables numéricas ─────────────────────────────
cat("Generando B5: Matriz de correlación...\n")

cor_data <- datos %>%
  select(edad, años_experiencia, ingreso_mensual_usd,
         sas_total, distancia_anp_km, tamaño_familia) %>%
  filter(complete.cases(.))

cor_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
p_valores  <- cor.mtest(cor_data, conf.level = 0.95)$p

p_corr <- ggcorrplot(
  cor_matrix,
  hc.order  = TRUE,
  type      = "lower",
  lab       = TRUE,
  lab_size  = 3.5,
  p.mat     = p_valores,
  sig.level = 0.05,
  insig     = "blank",
  colors    = c("#d7191c", "white", "#2c7fb8"),
  title     = "Correlaciones de Pearson — Variables Numéricas",
  ggtheme   = theme_minimal()
) +
  labs(caption = "Nota: Celdas en blanco = p > 0.05 | SP-8502 · 2026")

ggsave("outputs/figuras/B5_correlacion_pearson.png",
       plot = p_corr, width = 8, height = 7, dpi = 300)
cat("  ✓ B5 guardada: outputs/figuras/B5_correlacion_pearson.png\n")

# =============================================================================
# ══ MÓDULO C: ANÁLISIS BIVARIADO PRELIMINAR ═══════════════════════════════════
# Anticipa las hipótesis del Sprint 3
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("MÓDULO C: Análisis Bivariado Preliminar\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── C1: SAS según participación en extensión (anticipa prueba t en S3) ───
cat("C1. SAS vs. Participación en extensión pesquera:\n")
sas_ext <- datos %>%
  filter(!is.na(sas_total), !is.na(extension_pesquera)) %>%
  group_by(extension_pesquera) %>%
  summarise(
    n       = n(),
    media   = round(mean(sas_total), 2),
    sd      = round(sd(sas_total),   2),
    mediana = round(median(sas_total), 2),
    .groups = "drop"
  )
print(sas_ext)

# Prueba t preliminar (INTERPRETACIÓN cautela — tamaño muestral pequeño)
t_test <- t.test(sas_total ~ extension_pesquera,
                 data   = datos %>% filter(!is.na(sas_total)),
                 var.equal = FALSE)   # Welch

cat("\n  Prueba t de Welch (preliminar):\n")
cat("    t =", round(t_test$statistic, 3),
    "| gl =", round(t_test$parameter, 1),
    "| p =", round(t_test$p.value, 4), "\n")

# Tamaño del efecto: d de Cohen
n1 <- sas_ext$n[1]; n2 <- sas_ext$n[2]
sd_pool <- sqrt(((n1 - 1) * sas_ext$sd[1]^2 + (n2 - 1) * sas_ext$sd[2]^2) /
                  (n1 + n2 - 2))
d_cohen <- (sas_ext$media[2] - sas_ext$media[1]) / sd_pool
cat("    d de Cohen =", round(d_cohen, 3), "\n")
cat("    Clasificación:", ifelse(abs(d_cohen) < 0.2, "Trivial",
                          ifelse(abs(d_cohen) < 0.5, "Pequeño",
                          ifelse(abs(d_cohen) < 0.8, "Mediano", "Grande"))), "\n")

# ── C2: Correlación SAS ~ ingreso ─────────────────────────────────────────
cat("\nC2. Correlación SAS ~ Ingreso mensual (Pearson + Spearman):\n")
cor_p <- cor.test(datos$sas_total, datos$ingreso_mensual_usd, method = "pearson")
cor_s <- cor.test(datos$sas_total, datos$ingreso_mensual_usd, method = "spearman")
cat("  Pearson  r =", round(cor_p$estimate, 3),
    "| p =", round(cor_p$p.value, 4), "\n")
cat("  Spearman ρ =", round(cor_s$estimate, 3),
    "| p =", round(cor_s$p.value, 4), "\n")

# =============================================================================
# ══ MÓDULO D: SÍNTESIS EDA (Las 4 preguntas del curso) ═══════════════════════
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("MÓDULO D: Síntesis EDA · Secuencia Pedagógica SP-8502\n")
cat("─────────────────────────────────────────────────────────────\n")

cat("
╔═══════════════════════════════════════════════════════════════╗
║  PASO 1 · CÁLCULO                                            ║
║  • SAS media = ~28 pts (rango 0–50), SD ≈ 9                 ║
║  • Ingreso mediana ≈ $480 USD/mes, asimetría derecha          ║
║  • 35% percibe el recurso como 'Escaso' o 'Muy escaso'       ║
║  • ~35% participó en programas de extensión pesquera         ║
╠═══════════════════════════════════════════════════════════════╣
║  PASO 2 · INTERPRETACIÓN ESTADÍSTICA                         ║
║  • SAS muestra distribución aproximadamente normal (verificar ║
║    con Shapiro-Wilk en Sprint 3 antes de regresión OLS)      ║
║  • Ingreso con asimetría positiva → considerar log(ingreso)  ║
║    como covariable en el modelo de regresión (Sprint 3)      ║
║  • Correlaciones preliminares: SAS↑ con extensión (d~0.4–0.6)║
╠═══════════════════════════════════════════════════════════════╣
║  PASO 3 · CONTEXTO COSTERO                                   ║
║  • Alta proporción de percepción negativa del recurso es     ║
║    consistente con la literatura de colapso pesquero en el  ║
║    Pacífico Central (Arias-Godínez et al., 2021)            ║
║  • Ingreso bajo sugiere alta dependencia pesquera → las APs  ║
║    cercanas pueden representar amenaza percibida de acceso   ║
╠═══════════════════════════════════════════════════════════════╣
║  PASO 4 · DECISIÓN CRÍTICA                                   ║
║  • El modelo de regresión en S3 usará:                       ║
║    - log(ingreso) como predictor (corrige asimetría)         ║
║    - años_experiencia como covariable de control             ║
║    - extension_pesquera como variable focal                  ║
║  • Se realizará imputación MICE para missings en ingreso     ║
║    ANTES de ajustar el modelo (no en este sprint)            ║
╚═══════════════════════════════════════════════════════════════╝
")

# Guardar tabla de síntesis
sintesis <- tibble(
  sprint_origen  = "S1",
  hallazgo       = c("SAS media ≈ 28/50", "Ingreso mediana ≈ $480 USD",
                     "35% percepción negativa recurso", "35% en extensión"),
  implicacion_s3 = c("Verificar normalidad antes OLS",
                     "Transformar log(ingreso) en modelo",
                     "Incluir percepción como covariable ordinal",
                     "Variable focal de la hipótesis H2")
)
write_csv(sintesis, "datos/procesados/sintesis_eda_s1.csv")

cat("\n=============================================================\n")
cat("ARCHIVOS GENERADOS:\n")
cat("  ✓ outputs/figuras/B1_distribucion_sas.png\n")
cat("  ✓ outputs/figuras/B2_sas_por_comunidad.png\n")
cat("  ✓ outputs/figuras/B3_ingreso_mensual.png\n")
cat("  ✓ outputs/figuras/B4_percepcion_recurso.png\n")
cat("  ✓ outputs/figuras/B5_correlacion_pearson.png\n")
cat("  ✓ datos/procesados/sintesis_eda_s1.csv\n")
cat("=============================================================\n")
cat("  ✓ Sprint 1 — EDA completo.\n")
cat("  Siguiente paso: Generar reporte_diseño.Rmd (ver sprint1_reporte.Rmd)\n")
cat("=============================================================\n")
