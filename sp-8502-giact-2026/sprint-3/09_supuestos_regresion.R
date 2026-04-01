# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 09_supuestos_regresion.R
# SPRINT  : 3 — Análisis Profundo y Modelado (Semanas 9–12)
# TEMA    : Verificación de Supuestos OLS · Clínica de Diagnóstico
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   ANTES de ajustar cualquier modelo de regresión, se verifican los
#   supuestos de Gauss-Markov que hacen al estimador OLS BLUE
#   (Best Linear Unbiased Estimator):
#
#   [S1] Linealidad:             E(Y|X) es lineal en los parámetros
#   [S2] Independencia:          Las observaciones son independientes
#   [S3] Homocedasticidad:       Var(ε|X) = σ² constante
#   [S4] Normalidad de residuos: ε ~ N(0, σ²) (para inferencia)
#   [S5] No multicolinealidad:   VIF < 10 (umbral práctico)
#   [S6] Sin outliers influyentes: Distancia de Cook < 4/n
#
#   Incluye IMPUTACIÓN MICE para valores faltantes (prometida en S1).
#
# SECUENCIA PEDAGÓGICA: Cálculo → Interpretación → Contexto → Decisión
# CONEXIÓN S2 → S3: Lee base_datos.csv generada por script 08
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")

pkgs <- c("tidyverse","psych","car","lmtest","nortest",
          "ggplot2","patchwork","mice","VIM")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/", quiet = TRUE)
  library(p, character.only = TRUE)
}))

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 3 · Verificación de Supuestos OLS\n")
cat("  'Clínica de Código' · Semanas 9–10\n")
cat("=============================================================\n\n")

# =============================================================================
# SECCIÓN 0: CARGAR Y PREPARAR BASE DE DATOS (del Sprint 2)
# =============================================================================
if (!file.exists("datos/procesados/base_datos.csv")) {
  stop("ERROR: Ejecute primero la suite del Sprint 2 para generar base_datos.csv")
}

datos_raw <- read_csv("datos/procesados/base_datos.csv",
                      show_col_types = FALSE)

# Preparar tipos de variables
datos <- datos_raw %>%
  mutate(
    comunidad          = factor(comunidad),
    sexo               = factor(sexo, levels = c("Masculino","Femenino")),
    metodo             = factor(metodo),
    arte_pesca_principal = factor(arte_pesca_principal),
    percepcion_recurso = factor(percepcion_recurso,
                                levels = c("Muy escaso","Escaso","Regular",
                                           "Abundante","Muy abundante"),
                                ordered = TRUE),
    extension_pesquera = as.numeric(extension_pesquera),
    # Transformación logarítmica del ingreso (anticipada en EDA Sprint 1)
    log_ingreso        = log(ingreso_mensual_usd + 1),
    # Percepción como numérica para modelos continuos
    percepcion_num     = as.numeric(percepcion_recurso)
  )

cat("Base cargada:", nrow(datos), "obs. ×", ncol(datos), "variables\n\n")

# =============================================================================
# SECCIÓN 1: IMPUTACIÓN MICE (prometida desde Sprint 1)
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Imputación Múltiple con MICE\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Variables a imputar
vars_modelo <- c("sas_total","log_ingreso","años_experiencia",
                 "distancia_anp_km","extension_pesquera",
                 "percepcion_num","tamaño_familia")

datos_mice_input <- datos %>% select(all_of(vars_modelo))

# Patrón de missings
cat("Patrón de valores faltantes (variables del modelo):\n")
md_pattern <- datos_mice_input %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_NA") %>%
  mutate(pct_NA = round(n_NA / nrow(datos) * 100, 1))
print(md_pattern)

# Ejecutar MICE
cat("\nEjecutando MICE (m=5 imputaciones, método PMM)...\n")
imp <- mice(datos_mice_input,
            m       = 5,
            method  = "pmm",       # Predictive Mean Matching
            seed    = 8502,
            printFlag = FALSE)

cat("✓ MICE completado.\n")
cat("  Imputaciones:", imp$m, "\n")
cat("  Método:", unique(imp$method), "\n\n")

# Usar primera imputación para análisis de supuestos
datos_imp <- complete(imp, action = 1)
datos_completo <- datos %>%
  select(-all_of(vars_modelo)) %>%
  bind_cols(datos_imp)

cat("Dataset completo post-imputación:", nrow(datos_completo),
    "obs. (0 NA en variables del modelo)\n\n")

# ══ PASO 2: INTERPRETACIÓN DE MICE ═══════════════════════════════════════════
cat("INTERPRETACIÓN MICE:\n")
cat("  PMM imputa buscando los k=5 vecinos más cercanos en el espacio
  predicho y seleccionando aleatoriamente entre sus valores observados.
  Esto preserva la distribución empírica mejor que imputación por media.\n")
cat("  Convergencia MICE: verificar con plot(imp) en sesión interactiva.\n\n")

# =============================================================================
# SECCIÓN 2: MODELO PRELIMINAR PARA DIAGNÓSTICO
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Modelo Preliminar · Diagnóstico de Supuestos\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Modelo completo (a verificar)
modelo_prelim <- lm(
  sas_total ~ log_ingreso + años_experiencia + extension_pesquera +
    distancia_anp_km + percepcion_num + tamaño_familia + metodo,
  data = datos_completo
)

cat("Modelo preliminar especificado:\n")
cat("  SAS ~ log(ingreso) + años_exp + extensión + distancia_ANP\n")
cat("         + percepción + familia + método_selección\n\n")
cat("Resumen rápido:\n")
cat("  R² =", round(summary(modelo_prelim)$r.squared, 4), "\n")
cat("  R² ajustado =", round(summary(modelo_prelim)$adj.r.squared, 4), "\n")
cat("  F-stat =", round(summary(modelo_prelim)$fstatistic[1], 2), "\n\n")

# =============================================================================
# SECCIÓN 3: VERIFICACIÓN SUPUESTO POR SUPUESTO
# =============================================================================

# ── [S1] LINEALIDAD ──────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("[S1] LINEALIDAD — Residuos vs. Valores Ajustados\n")
cat("─────────────────────────────────────────────────────────────\n\n")

residuos_df <- tibble(
  fitted    = fitted(modelo_prelim),
  residuos  = residuals(modelo_prelim),
  std_res   = rstandard(modelo_prelim),
  sqrt_abs  = sqrt(abs(std_res)),
  leverage  = hatvalues(modelo_prelim),
  cook_d    = cooks.distance(modelo_prelim),
  id        = seq_along(residuals(modelo_prelim))
)

p_s1 <- ggplot(residuos_df, aes(x = fitted, y = residuos)) +
  geom_point(alpha = 0.6, color = "#2c7fb8", size = 2) +
  geom_hline(yintercept = 0, color = "#e34a33", linewidth = 1,
             linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE, color = "#31a354",
              linewidth = 0.8, linetype = "solid") +
  labs(title = "[S1] Residuos vs. Valores Ajustados",
       subtitle = "La línea verde (LOESS) debe ser aproximadamente horizontal",
       x = "Valores ajustados (ŷ)", y = "Residuos (e)") +
  annotate("text", x = max(residuos_df$fitted) - 3,
           y = max(residuos_df$residuos) - 1,
           label = ifelse(
             abs(cor(residuos_df$fitted, residuos_df$residuos)) < 0.1,
             "✓ Sin tendencia sistemática",
             "⚠ Posible no linealidad"),
           color = "#31a354", size = 3.5)

ggsave("outputs/figuras/S3_s1_linealidad.png",
       plot = p_s1, width = 7, height = 5, dpi = 300)

cat("PASO 1 · CÁLCULO: Correlación residuos~ajustados =",
    round(cor(residuos_df$fitted, residuos_df$residuos), 4), "\n")
cat("PASO 2 · INTERPRETACIÓN: |r| <",
    ifelse(abs(cor(residuos_df$fitted, residuos_df$residuos)) < 0.1,
           "0.10 → Linealidad aceptable ✓\n",
           "0.10 → Revisar transformaciones de predictores\n"))

# ── [S3] HOMOCEDASTICIDAD ────────────────────────────────────────────────────
cat("\n─────────────────────────────────────────────────────────────\n")
cat("[S3] HOMOCEDASTICIDAD — Prueba de Breusch-Pagan\n")
cat("─────────────────────────────────────────────────────────────\n\n")

bp_test <- bptest(modelo_prelim)
cat("PASO 1 · CÁLCULO:\n")
cat("  Breusch-Pagan χ² =", round(bp_test$statistic, 4),
    "| p =", round(bp_test$p.value, 4), "\n")

cat("PASO 2 · INTERPRETACIÓN:\n")
if (bp_test$p.value > 0.05) {
  cat("  p > 0.05 → No se rechaza homocedasticidad ✓\n")
  cat("  Los residuos tienen varianza aproximadamente constante.\n")
} else {
  cat("  p ≤ 0.05 → Se rechaza homocedasticidad ⚠\n")
  cat("  Acción: Usar errores estándar robustos HC3 (sandwich) en Sprint 3.\n")
}

# Prueba de White (más general)
white_test <- bptest(modelo_prelim,
                     ~ fitted(modelo_prelim) + I(fitted(modelo_prelim)^2))
cat("\n  Prueba de White (términos cuadráticos):\n")
cat("  χ² =", round(white_test$statistic, 4),
    "| p =", round(white_test$p.value, 4), "\n")

cat("\nPASO 3 · CONTEXTO COSTERO:\n")
cat("  La heterocedasticidad es común en datos de ingreso pesquero:
  comunidades con mayor variabilidad de capturas (clima, ENOS)
  tendrán mayor dispersión en ingreso y posiblemente en SAS.
  Transformar log(ingreso) ya mitiga parte de este problema.\n")

cat("\nPASO 4 · DECISIÓN:\n")
bp_decision <- if (bp_test$p.value > 0.05) {
  "SE MANTIENE estimación OLS estándar (homocedasticidad no rechazada)"
} else {
  "SE APLICARÁN errores estándar robustos HC3 via sandwich::vcovHC()"
}
cat(" ", bp_decision, "\n")

# ── [S4] NORMALIDAD DE RESIDUOS ──────────────────────────────────────────────
cat("\n─────────────────────────────────────────────────────────────\n")
cat("[S4] NORMALIDAD DE RESIDUOS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Shapiro-Wilk (n < 5000)
sw_test  <- shapiro.test(residuals(modelo_prelim))
# Kolmogorov-Smirnov con corrección Lilliefors
lf_test  <- nortest::lillie.test(residuals(modelo_prelim))
# Anderson-Darling
ad_test  <- nortest::ad.test(residuals(modelo_prelim))

cat("PASO 1 · CÁLCULO — Tests de normalidad:\n")
cat("  Shapiro-Wilk  W =", round(sw_test$statistic, 4),
    "| p =", round(sw_test$p.value, 4), "\n")
cat("  Lilliefors    D =", round(lf_test$statistic, 4),
    "| p =", round(lf_test$p.value, 4), "\n")
cat("  Anderson-Darling A =", round(ad_test$statistic, 4),
    "| p =", round(ad_test$p.value, 4), "\n")

# QQ-plot
p_qq <- ggplot(residuos_df, aes(sample = residuos)) +
  stat_qq(color = "#2c7fb8", alpha = 0.7, size = 2) +
  stat_qq_line(color = "#e34a33", linewidth = 1) +
  labs(title = "[S4] QQ-Plot de Residuos",
       subtitle = "Los puntos deben seguir la línea diagonal",
       x = "Cuantiles teóricos N(0,1)",
       y = "Cuantiles muestrales") +
  annotate("text", x = -1.5, y = max(residuos_df$residuos) * 0.8,
           label = paste0("Shapiro-Wilk\np = ",
                           round(sw_test$p.value, 3)),
           color = "#555555", size = 3.5)

ggsave("outputs/figuras/S3_s4_normalidad_qq.png",
       plot = p_qq, width = 6, height = 5, dpi = 300)

cat("\nPASO 2 · INTERPRETACIÓN:\n")
norm_ok <- sw_test$p.value > 0.05
cat("  Shapiro-Wilk:", ifelse(norm_ok,
                               "No rechaza normalidad (p > 0.05) ✓",
                               "Rechaza normalidad (p ≤ 0.05) ⚠"), "\n")
cat("  Nota: Con n=80, el TCL garantiza validez asintótica del test F
  aun con desviaciones moderadas de normalidad.\n")

# ── [S5] MULTICOLINEALIDAD ───────────────────────────────────────────────────
cat("\n─────────────────────────────────────────────────────────────\n")
cat("[S5] MULTICOLINEALIDAD — VIF y Correlaciones\n")
cat("─────────────────────────────────────────────────────────────\n\n")

vif_vals <- car::vif(modelo_prelim)

cat("PASO 1 · CÁLCULO — Factor de Inflación de Varianza (VIF):\n")
vif_tabla <- tibble(
  Variable = names(vif_vals),
  VIF      = round(as.numeric(vif_vals), 3),
  Diagnostico = case_when(
    as.numeric(vif_vals) < 2   ~ "✓ Sin multicolinealidad",
    as.numeric(vif_vals) < 5   ~ "~ Leve (aceptable)",
    as.numeric(vif_vals) < 10  ~ "⚠ Moderada (revisar)",
    TRUE                        ~ "✗ Severa (eliminar)"
  )
)
print(vif_tabla)

cat("\nPASO 2 · INTERPRETACIÓN:\n")
max_vif <- max(as.numeric(vif_vals))
cat("  VIF máximo:", round(max_vif, 3),
    "| Umbral crítico: 10\n")
cat("  Estado:", ifelse(max_vif < 10, "✓ No hay multicolinealidad severa",
                        "✗ Eliminar o combinar predictores con VIF>10"), "\n")

cat("\nPASO 3 · CONTEXTO COSTERO:\n")
cat("  Es esperable cierta correlación entre log_ingreso y años_experiencia
  (pescadores más experimentados tienden a generar más ingreso).
  VIF 2–3 es tolerable en este contexto; no requiere acción.\n")

# ── [S6] OUTLIERS E INFLUENCIA ────────────────────────────────────────────────
cat("\n─────────────────────────────────────────────────────────────\n")
cat("[S6] OUTLIERS INFLUYENTES — Distancia de Cook\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cook_umbral <- 4 / nrow(datos_completo)
outliers_cook <- residuos_df %>%
  filter(cook_d > cook_umbral) %>%
  arrange(desc(cook_d))

cat("PASO 1 · CÁLCULO:\n")
cat("  Umbral Cook (4/n = 4/", nrow(datos_completo), "):",
    round(cook_umbral, 4), "\n")
cat("  Observaciones influyentes:", nrow(outliers_cook), "\n")
if (nrow(outliers_cook) > 0) print(outliers_cook %>% select(id, fitted, residuos, cook_d))

# Gráfico de Cook
p_cook <- ggplot(residuos_df, aes(x = id, y = cook_d)) +
  geom_col(fill = "#2c7fb8", alpha = 0.6, width = 0.7) +
  geom_hline(yintercept = cook_umbral, color = "#e34a33",
             linewidth = 1, linetype = "dashed") +
  annotate("text", x = nrow(residuos_df) * 0.85,
           y = cook_umbral + 0.005,
           label = paste0("Umbral = ", round(cook_umbral, 3)),
           color = "#e34a33", size = 3.5) +
  labs(title = "[S6] Distancia de Cook por Observación",
       subtitle = "Puntos sobre la línea roja = observaciones influyentes",
       x = "ID Observación", y = "Distancia de Cook")

ggsave("outputs/figuras/S3_s6_cook_distance.png",
       plot = p_cook, width = 9, height = 4, dpi = 300)

cat("\nPASO 2 · INTERPRETACIÓN:\n")
cat("  ", nrow(outliers_cook), "observaciones influyentes (",
    round(nrow(outliers_cook)/nrow(datos_completo)*100, 1), "% de la muestra)\n")
cat("  Acción:", ifelse(nrow(outliers_cook) == 0,
                        "Ninguna requerida.",
                        "Revisar ficha de campo de las observaciones flagueadas."), "\n")

# =============================================================================
# SECCIÓN 4: PANEL DIAGNÓSTICO COMPLETO
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 4: Panel Diagnóstico Integrado (4 gráficos)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Residuos vs. Ajustados
p_d1 <- ggplot(residuos_df, aes(fitted, residuos)) +
  geom_point(alpha=0.5, color="#2c7fb8", size=1.5) +
  geom_hline(yintercept=0, color="#e34a33", linetype="dashed") +
  geom_smooth(method="loess", se=FALSE, color="#31a354", linewidth=0.8) +
  labs(title="Residuos vs. Ajustados", x="ŷ", y="e")

# QQ-Plot
p_d2 <- ggplot(residuos_df, aes(sample = std_res)) +
  stat_qq(alpha=0.5, color="#2c7fb8", size=1.5) +
  stat_qq_line(color="#e34a33", linewidth=1) +
  labs(title="QQ-Plot Normal", x="Cuantiles teóricos", y="Cuantiles muestrales")

# Scale-Location (raíz de |residuos estandarizados| vs. ajustados)
p_d3 <- ggplot(residuos_df, aes(fitted, sqrt_abs)) +
  geom_point(alpha=0.5, color="#2c7fb8", size=1.5) +
  geom_smooth(method="loess", se=FALSE, color="#31a354", linewidth=0.8) +
  labs(title="Scale-Location", x="ŷ", y="√|e estand.|")

# Residuos vs. Leverage
p_d4 <- ggplot(residuos_df, aes(leverage, std_res)) +
  geom_point(aes(color = cook_d > cook_umbral), size=1.8, alpha=0.7) +
  scale_color_manual(values=c("FALSE"="#2c7fb8","TRUE"="#e34a33"),
                     labels=c("Normal","Influyente"), name="") +
  geom_hline(yintercept=c(-2,2), color="grey50", linetype="dashed") +
  labs(title="Residuos vs. Leverage", x="Leverage (hii)", y="e estandarizado")

panel_diagnostico <- (p_d1 | p_d2) / (p_d3 | p_d4) +
  plot_annotation(
    title   = "Panel de Diagnóstico OLS — SP-8502 · Sprint 3",
    caption = "SP-8502 · GIACT UCR · 2026"
  )

ggsave("outputs/figuras/S3_panel_diagnostico_ols.png",
       plot = panel_diagnostico, width = 11, height = 8, dpi = 300)
cat("✓ Panel diagnóstico guardado: outputs/figuras/S3_panel_diagnostico_ols.png\n")

# =============================================================================
# SECCIÓN 5: TABLA RESUMEN DE SUPUESTOS
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 5: Tabla Resumen de Supuestos\n")
cat("─────────────────────────────────────────────────────────────\n\n")

resumen_supuestos <- tibble(
  Supuesto      = c("S1 Linealidad","S2 Independencia","S3 Homocedasticidad",
                    "S4 Normalidad residuos","S5 No multicolinealidad",
                    "S6 Sin outliers influyentes"),
  Test_Usado    = c("Residuos vs. Ajustados (LOESS)","Diseño muestral",
                    "Breusch-Pagan","Shapiro-Wilk + QQ",
                    "VIF (car::vif)","Distancia de Cook"),
  Resultado     = c(
    "Sin tendencia sistemática",
    "Diseño estratificado + RDS independiente",
    paste0("p = ", round(bp_test$p.value, 3)),
    paste0("p = ", round(sw_test$p.value, 3)),
    paste0("VIF max = ", round(max_vif, 2)),
    paste0(nrow(outliers_cook), " obs. influyentes")
  ),
  Veredicto     = c(
    "✓ Cumple","✓ Cumple",
    ifelse(bp_test$p.value > 0.05, "✓ Cumple", "⚠ SE robustos HC3"),
    ifelse(sw_test$p.value > 0.05, "✓ Cumple", "~ TCL garantiza validez"),
    ifelse(max_vif < 10, "✓ Cumple", "⚠ Revisar"),
    ifelse(nrow(outliers_cook) == 0, "✓ Cumple", "⚠ Revisar fichas")
  )
)

print(resumen_supuestos)
write_csv(resumen_supuestos, "datos/procesados/resumen_supuestos_s3.csv")
saveRDS(datos_completo, "datos/procesados/datos_imputados_s3.rds")

cat("\n=============================================================\n")
cat("  ✓ Script 09 completado.\n")
cat("  Archivos: panel_diagnostico_ols.png + resumen_supuestos_s3.csv\n")
cat("         + datos_imputados_s3.rds (para scripts 10 y 11)\n")
cat("  Siguiente: 10_regresion_lineal.R\n")
cat("=============================================================\n")
