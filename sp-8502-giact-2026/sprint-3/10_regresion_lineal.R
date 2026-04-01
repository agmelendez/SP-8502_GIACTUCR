# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 10_regresion_lineal.R
# SPRINT  : 3 — Análisis Profundo y Modelado (Semanas 9–12)
# TEMA    : Regresión Lineal Múltiple OLS · Modelo Principal del Proyecto
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   Ajusta y compara modelos de regresión OLS para responder H1–H3:
#
#   H1: SAS ~ log(ingreso) + años_exp + extensión + otras covariables
#       (R² ≥ 0.35, p < 0.05)
#   H2: SAS(extensión=Sí) > SAS(extensión=No)  (d Cohen > 0.5)
#   H3: Correlación SAS ~ distancia_ANP (r ≥ 0.40)
#
#   Estrategia de modelado: construcción secuencial
#     M0: Modelo nulo (solo intercepto)
#     M1: Solo variable focal (extensión)
#     M2: Modelo base (variables sociodemográficas)
#     M3: Modelo completo (todas las variables teóricas)
#     M4: Modelo parsimonioso (selección stepwise + criterio AIC/BIC)
#
# SECUENCIA PEDAGÓGICA: Cálculo → Interpretación → Contexto → Decisión
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")

pkgs <- c("tidyverse","broom","car","lmtest","sandwich",
          "patchwork","ggcorrplot","knitr")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/", quiet = TRUE)
  library(p, character.only = TRUE)
}))

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 3 · Regresión Lineal Múltiple OLS\n")
cat("  Variable dependiente: SAS Total (0–50 pts)\n")
cat("=============================================================\n\n")

# Cargar datos imputados del Script 09
if (!file.exists("datos/procesados/datos_imputados_s3.rds")) {
  message("Ejecutando script 09 primero...")
  source("09_supuestos_regresion.R")
}
datos <- readRDS("datos/procesados/datos_imputados_s3.rds")
cat("Datos cargados:", nrow(datos), "obs.\n\n")

# =============================================================================
# SECCIÓN 1: ESTRATEGIA DE CONSTRUCCIÓN SECUENCIAL DE MODELOS
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Construcción Secuencial de Modelos (M0 → M4)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── M0: Modelo nulo (referencia) ─────────────────────────────────────────
M0 <- lm(sas_total ~ 1, data = datos)

# ── M1: Solo variable focal (extensión pesquera) ─────────────────────────
M1 <- lm(sas_total ~ extension_pesquera, data = datos)

# ── M2: Variables sociodemográficas ──────────────────────────────────────
M2 <- lm(sas_total ~ extension_pesquera + log_ingreso + años_experiencia +
            tamaño_familia, data = datos)

# ── M3: Modelo teóricamente completo ─────────────────────────────────────
M3 <- lm(sas_total ~ extension_pesquera + log_ingreso + años_experiencia +
            tamaño_familia + distancia_anp_km + percepcion_num + metodo,
          data = datos)

# ── M4: Selección stepwise (AIC) ─────────────────────────────────────────
M4 <- step(M3, direction = "both", trace = 0)

cat("Modelos especificados:\n")
cat("  M0: SAS ~ 1\n")
cat("  M1: SAS ~ extensión\n")
cat("  M2: SAS ~ extensión + log_ingreso + años_exp + familia\n")
cat("  M3: SAS ~ (todas las variables teóricas)\n")
cat("  M4: SAS ~ (selección stepwise AIC, formula final abajo)\n")
cat("  M4 fórmula:", deparse(formula(M4)), "\n\n")

# =============================================================================
# SECCIÓN 2: TABLA COMPARATIVA DE MODELOS
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Comparación de Modelos (R², AIC, BIC, F)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

comparar_modelos <- function(lista_modelos, nombres) {
  map2_dfr(lista_modelos, nombres, function(m, nombre) {
    s   <- summary(m)
    tibble(
      Modelo    = nombre,
      k         = length(coef(m)) - 1,  # predictores (sin intercepto)
      R2        = round(s$r.squared, 4),
      R2_adj    = round(s$adj.r.squared, 4),
      AIC       = round(AIC(m), 2),
      BIC       = round(BIC(m), 2),
      RMSE      = round(sqrt(mean(residuals(m)^2)), 3),
      F_stat    = round(s$fstatistic[1], 2),
      F_p       = round(pf(s$fstatistic[1], s$fstatistic[2],
                           s$fstatistic[3], lower.tail = FALSE), 4)
    )
  })
}

tabla_modelos <- comparar_modelos(
  list(M0, M1, M2, M3, M4),
  c("M0: Nulo", "M1: Focal", "M2: Sociodemog.",
    "M3: Completo", "M4: Parsimonioso")
)
print(tabla_modelos)

# ══ PASO 2: INTERPRETACIÓN ═══════════════════════════════════════════════════
cat("\nPASO 2 · INTERPRETACIÓN ESTADÍSTICA:\n")
mejor_aic <- tabla_modelos$Modelo[which.min(tabla_modelos$AIC)]
mejor_bic <- tabla_modelos$Modelo[which.min(tabla_modelos$BIC)]
cat("  Mejor AIC:", mejor_aic, "\n")
cat("  Mejor BIC:", mejor_bic, "\n")
cat("  DECISIÓN: Se reporta M3 (teóricamente motivado) y M4",
    "(parsimonioso) en paralelo.\n")
cat("  La inferencia sustantiva se basa en M3; M4 confirma robustez.\n")

# Test ANOVA entre modelos anidados
cat("\nTest ANOVA para modelos anidados:\n")
print(anova(M0, M1, M2, M3))

# =============================================================================
# SECCIÓN 3: MODELO PRINCIPAL — RESULTADOS DETALLADOS (M3)
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 3: Resultados del Modelo Principal (M3)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("══ PASO 1: CÁLCULO ══════════════════════════════════════\n\n")
print(summary(M3))

# Tabla de coeficientes con IC 95%
coef_m3 <- tidy(M3, conf.int = TRUE, conf.level = 0.95) %>%
  mutate(
    across(c(estimate, std.error, statistic, conf.low, conf.high), round, 4),
    p.value = round(p.value, 4),
    significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ "ns"
    ),
    Interpretacion = case_when(
      term == "(Intercept)"       ~ "Valor esperado SAS cuando todo = 0 (referencia)",
      term == "extension_pesquera" ~ "Diferencia SAS: participantes vs. no participantes en extensión",
      term == "log_ingreso"       ~ "Cambio en SAS por 1% de aumento en ingreso (×log)",
      term == "años_experiencia"  ~ "Cambio en SAS por año adicional de experiencia",
      term == "tamaño_familia"    ~ "Cambio en SAS por miembro adicional del grupo familiar",
      term == "distancia_anp_km"  ~ "Cambio en SAS por km adicional de distancia al ANP",
      term == "percepcion_num"    ~ "Cambio en SAS por nivel adicional de percepción positiva",
      term == "metodoRDS"         ~ "Diferencia SAS entre submuestra RDS y estratificada",
      TRUE ~ ""
    )
  )

cat("\nTabla de coeficientes con IC 95%:\n")
print(coef_m3 %>%
        select(term, estimate, std.error, conf.low, conf.high,
               p.value, significancia))

# =============================================================================
# SECCIÓN 4: COEFICIENTES ESTANDARIZADOS (β*)
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 4: Coeficientes Estandarizados (Beta*) — Importancia relativa\n")
cat("─────────────────────────────────────────────────────────────\n\n")
cat("PASO 1 · CÁLCULO:\n")

# Estandarizar todas las variables numéricas
datos_z <- datos %>%
  mutate(across(c(sas_total, log_ingreso, años_experiencia,
                  tamaño_familia, distancia_anp_km, percepcion_num),
                ~ as.numeric(scale(.))))

M3_z <- lm(sas_total ~ extension_pesquera + log_ingreso + años_experiencia +
              tamaño_familia + distancia_anp_km + percepcion_num + metodo,
            data = datos_z)

beta_std <- tidy(M3_z, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    beta_abs   = abs(estimate),
    ranking    = rank(-beta_abs),
    estimate   = round(estimate, 3),
    conf.low   = round(conf.low, 3),
    conf.high  = round(conf.high, 3),
    p.value    = round(p.value, 4)
  ) %>%
  arrange(desc(beta_abs))

cat("\nCoeficientes Beta estandarizados (ordenados por magnitud):\n")
print(beta_std %>% select(term, estimate, conf.low, conf.high, p.value, ranking))

cat("\nPASO 2 · INTERPRETACIÓN:\n")
top_predictor <- beta_std$term[1]
top_beta      <- beta_std$estimate[1]
cat("  Predictor más importante:", top_predictor,
    "(β* =", top_beta, ")\n")
cat("  Interpretación: Un aumento de 1 DE en", top_predictor,
    "se asocia con un cambio de", top_beta,
    "desviaciones estándar en el SAS.\n")

cat("\nPASO 3 · CONTEXTO COSTERO:\n")
cat("  La magnitud relativa de los predictores indica las palancas de
  intervención más efectivas para programas de pesca sostenible.
  El predictor dominante orienta las recomendaciones del Policy Brief
  del Sprint 4.\n")

# Gráfico de coeficientes (forest plot)
p_forest <- coef_m3 %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "grey50",
             linewidth = 0.8, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3, color = "#636363", linewidth = 0.8) +
  geom_point(aes(color = p.value < 0.05), size = 3.5) +
  scale_color_manual(values = c("TRUE" = "#2c7fb8", "FALSE" = "#bdbdbd"),
                     labels = c("p ≥ 0.05", "p < 0.05"),
                     name   = "Significancia") +
  labs(
    title    = "Coeficientes OLS con IC 95% — Modelo M3",
    subtitle = paste0("Variable dependiente: SAS Total | R² = ",
                       round(summary(M3)$r.squared, 3),
                       " | n = ", nrow(datos)),
    x        = "Coeficiente (pts SAS)",
    y        = NULL,
    caption  = "SP-8502 · Sprint 3 · 2026"
  )

ggsave("outputs/figuras/S3_forest_plot_ols.png",
       plot = p_forest, width = 9, height = 6, dpi = 300)
cat("\n✓ Forest plot guardado: outputs/figuras/S3_forest_plot_ols.png\n")

# =============================================================================
# SECCIÓN 5: PRUEBA DE HIPÓTESIS H1–H3
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 5: Prueba de Hipótesis H1–H3\n")
cat("─────────────────────────────────────────────────────────────\n\n")

r2_m3   <- summary(M3)$r.squared
f_m3    <- summary(M3)$fstatistic
p_f_m3  <- pf(f_m3[1], f_m3[2], f_m3[3], lower.tail = FALSE)

cat("── H1: Modelo predice ≥ 35% varianza del SAS ──\n")
cat("   R² =", round(r2_m3, 4), "(",
    ifelse(r2_m3 >= 0.35, "✓ SE CONFIRMA H1", "✗ No se confirma H1"), ")\n")
cat("   F(", round(f_m3[2]), ",", round(f_m3[3]), ") =",
    round(f_m3[1], 2), "| p =", format(p_f_m3, scientific = TRUE), "\n\n")

cat("── H2: Extensión pesquera → mayor SAS (d Cohen > 0.5) ──\n")
ext_coef <- coef_m3 %>% filter(term == "extension_pesquera")
sas_x_ext <- datos %>%
  group_by(extension_pesquera) %>%
  summarise(m = mean(sas_total, na.rm = TRUE),
            s = sd(sas_total, na.rm = TRUE),
            n = n(), .groups = "drop")

# d de Cohen (sin el sesgo de regresión)
sd_pool <- sqrt(((sas_x_ext$n[1]-1)*sas_x_ext$s[1]^2 +
                   (sas_x_ext$n[2]-1)*sas_x_ext$s[2]^2) /
                  (sum(sas_x_ext$n) - 2))
d_cohen_h2 <- (sas_x_ext$m[2] - sas_x_ext$m[1]) / sd_pool
cat("   Coeficiente extensión =", round(ext_coef$estimate, 3),
    "(p =", round(ext_coef$p.value, 4), ")\n")
cat("   d de Cohen =", round(d_cohen_h2, 3),
    "(umbral 0.5 =",
    ifelse(abs(d_cohen_h2) >= 0.5, "✓ SE CONFIRMA H2", "✗ No se confirma H2"),
    ")\n\n")

cat("── H3: Correlación SAS ~ distancia ANP (r ≥ 0.40) ──\n")
cor_h3_p <- cor.test(datos$sas_total, datos$distancia_anp_km,
                     method = "pearson")
cor_h3_s <- cor.test(datos$sas_total, datos$distancia_anp_km,
                     method = "spearman", exact = FALSE)
cat("   r Pearson =", round(cor_h3_p$estimate, 3),
    "(p =", round(cor_h3_p$p.value, 4), ")\n")
cat("   ρ Spearman =", round(cor_h3_s$estimate, 3),
    "(p =", round(cor_h3_s$p.value, 4), ")\n")
cat("   Umbral 0.40:",
    ifelse(abs(cor_h3_p$estimate) >= 0.40,
           "✓ SE CONFIRMA H3", "✗ No se confirma H3"), "\n\n")

# Tabla resumen de hipótesis
hip_resultado <- tibble(
  Hipótesis   = c("H1","H2","H3"),
  Predicción  = c("R² ≥ 0.35","d Cohen > 0.5","r ≥ 0.40"),
  Valor_obs   = c(round(r2_m3, 3),
                  round(d_cohen_h2, 3),
                  round(cor_h3_p$estimate, 3)),
  p_value     = c(round(p_f_m3, 4),
                  round(ext_coef$p.value, 4),
                  round(cor_h3_p$p.value, 4)),
  Conclusion  = c(
    ifelse(r2_m3 >= 0.35, "SE CONFIRMA ✓","No confirmada ✗"),
    ifelse(abs(d_cohen_h2) >= 0.5, "SE CONFIRMA ✓","No confirmada ✗"),
    ifelse(abs(cor_h3_p$estimate) >= 0.40, "SE CONFIRMA ✓","No confirmada ✗")
  )
)
print(hip_resultado)

# =============================================================================
# SECCIÓN 6: GUARDAR RESULTADOS
# =============================================================================
write_csv(tabla_modelos,  "datos/procesados/comparacion_modelos_s3.csv")
write_csv(coef_m3,        "datos/procesados/coeficientes_M3.csv")
write_csv(beta_std,       "datos/procesados/betas_estandarizados_M3.csv")
write_csv(hip_resultado,  "datos/procesados/resultados_hipotesis_s3.csv")
saveRDS(list(M0=M0, M1=M1, M2=M2, M3=M3, M4=M4),
        "datos/procesados/modelos_ols_s3.rds")

cat("\n=============================================================\n")
cat("  ✓ Script 10 completado.\n")
cat("  Archivos clave: coeficientes_M3.csv · comparacion_modelos_s3.csv\n")
cat("                 S3_forest_plot_ols.png\n")
cat("  Siguiente: 11_regresion_logistica.R\n")
cat("=============================================================\n")
