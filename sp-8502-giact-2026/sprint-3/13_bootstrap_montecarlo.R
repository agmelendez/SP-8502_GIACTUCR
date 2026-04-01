# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 13_bootstrap_montecarlo.R
# SPRINT  : 3 — Análisis Profundo y Modelado (Semanas 9–12)
# TEMA    : Bootstrap · Simulación Monte Carlo · Inferencia por Remuestreo
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   Bootstrap y Monte Carlo son técnicas de simulación que permiten:
#   (a) BOOTSTRAP: Estimar la distribución muestral de un estadístico
#       sin asumir normalidad. Útil cuando n es pequeño (n=80) o los
#       supuestos del modelo son cuestionables.
#   (b) MONTE CARLO: Simular distribuciones de resultados bajo diferentes
#       escenarios hipotéticos (¿qué pasa si aumenta el ingreso 20%?).
#
#   MÓDULOS:
#   1. Bootstrap no paramétrico del R² y coeficientes OLS
#   2. Bootstrap del d de Cohen (H2)
#   3. Bootstrap de la correlación (H3)
#   4. Simulación Monte Carlo: escenario de política (extensión +20%)
#   5. Análisis de sensibilidad: ¿cuánto varía el modelo si cambia n?
#
# PEDAGOGÍA:
#   El Bootstrap no "inventa" datos: remuestrea CON REEMPLAZO los datos
#   reales. Con B=2000 repeticiones, la distribución de los estadísticos
#   converge a la distribución muestral verdadera.
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")
library(tidyverse)
library(patchwork)

set.seed(8502)
B <- 2000   # Número de réplicas bootstrap

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 3 · Bootstrap y Monte Carlo\n")
cat("  B =", B, "réplicas · set.seed(8502)\n")
cat("=============================================================\n\n")

datos <- readRDS("datos/procesados/datos_imputados_s3.rds")
n     <- nrow(datos)
cat("Datos:", n, "obs.\n\n")

# =============================================================================
# SECCIÓN 1: BOOTSTRAP NO PARAMÉTRICO — COEFICIENTES OLS
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Bootstrap del Modelo OLS (R² y β)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("Ejecutando", B, "réplicas bootstrap (puede tardar ~15 seg)...\n")

boot_ols <- replicate(B, {
  idx    <- sample(1:n, n, replace = TRUE)   # Remuestrear CON reemplazo
  dat_b  <- datos[idx, ]
  m_b    <- lm(sas_total ~ extension_pesquera + log_ingreso +
                  años_experiencia + distancia_anp_km +
                  percepcion_num + metodo,
                data = dat_b)
  c(r2   = summary(m_b)$r.squared,
    beta_ext  = coef(m_b)["extension_pesquera"],
    beta_ing  = coef(m_b)["log_ingreso"],
    beta_dist = coef(m_b)["distancia_anp_km"])
}, simplify = TRUE)

boot_df <- as_tibble(t(boot_ols)) %>%
  rename(r2 = r2, beta_ext = `beta_ext.extension_pesquera`,
         beta_ing = `beta_ing.log_ingreso`,
         beta_dist = `beta_dist.distancia_anp_km`)

cat("✓ Bootstrap completado.\n\n")
cat("PASO 1 · CÁLCULO — Estadísticos bootstrap:\n")
boot_summary <- boot_df %>%
  summarise(across(everything(),
                   list(media = mean, se = sd,
                        ic_low  = ~ quantile(., 0.025),
                        ic_high = ~ quantile(., 0.975)),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to  = c("parametro",".value"),
               names_pattern = "(.+)_(media|se|ic_low|ic_high)") %>%
  mutate(across(c(media, se, ic_low, ic_high), round, 4))

print(boot_summary)

# Modelo OLS original para comparar
m_orig <- lm(sas_total ~ extension_pesquera + log_ingreso +
               años_experiencia + distancia_anp_km +
               percepcion_num + metodo, data = datos)

cat("\nPASO 2 · INTERPRETACIÓN ESTADÍSTICA:\n")
cat("  Comparación IC Bootstrap vs. IC Analítico (OLS estándar):\n\n")

# IC analítico del R²
r2_orig <- summary(m_orig)$r.squared
# IC analítico del coeficiente extensión
ci_ext_analitico <- confint(m_orig)["extension_pesquera", ]
ci_ext_boot      <- c(
  quantile(boot_df$beta_ext, 0.025),
  quantile(boot_df$beta_ext, 0.975)
)

cat("  R² bootstrap media:", round(mean(boot_df$r2), 4),
    "| IC 95% [", round(quantile(boot_df$r2, 0.025), 4), ",",
    round(quantile(boot_df$r2, 0.975), 4), "]\n")
cat("  R² original:", round(r2_orig, 4), "\n\n")

cat("  β(extensión) analítico IC [",
    round(ci_ext_analitico[1], 3), ",",
    round(ci_ext_analitico[2], 3), "]\n")
cat("  β(extensión) bootstrap IC [",
    round(ci_ext_boot[1], 3), ",",
    round(ci_ext_boot[2], 3), "]\n")
cat("  Convergencia IC:", ifelse(
  sign(ci_ext_boot[1]) == sign(ci_ext_analitico[1]),
  "✓ Ambos IC del mismo signo → resultado robusto",
  "⚠ Discrepancia → revisar distribución de residuos"), "\n")

# Gráfico distribución bootstrap de R²
p_boot_r2 <- ggplot(boot_df, aes(x = r2)) +
  geom_histogram(bins = 50, fill = "#2c7fb8", alpha = 0.7,
                  color = "white") +
  geom_vline(xintercept = r2_orig, color = "#e34a33",
             linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = c(quantile(boot_df$r2, 0.025),
                              quantile(boot_df$r2, 0.975)),
             color = "#31a354", linewidth = 0.8, linetype = "dotted") +
  labs(title    = "Distribución Bootstrap del R²",
       subtitle = paste0("B = ", B, " réplicas | Línea roja = R² original"),
       x        = "R²", y = "Frecuencia")

# Gráfico distribución bootstrap de β(extensión)
p_boot_ext <- ggplot(boot_df, aes(x = beta_ext)) +
  geom_histogram(bins = 50, fill = "#74c476", alpha = 0.7,
                  color = "white") +
  geom_vline(xintercept = coef(m_orig)["extension_pesquera"],
             color = "#e34a33", linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey40",
             linewidth = 0.8, linetype = "solid") +
  labs(title = "Distribución Bootstrap de β(extensión)",
       x = "Coeficiente β", y = "Frecuencia")

p_boot_panel <- p_boot_r2 | p_boot_ext
ggsave("outputs/figuras/S3_bootstrap_ols.png",
       plot = p_boot_panel, width = 11, height = 5, dpi = 300)
cat("\n✓ Figura bootstrap: outputs/figuras/S3_bootstrap_ols.png\n")

# =============================================================================
# SECCIÓN 2: BOOTSTRAP DEL d DE COHEN (H2)
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Bootstrap del d de Cohen (H2)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

boot_d <- replicate(B, {
  idx   <- sample(1:n, n, replace = TRUE)
  dat_b <- datos[idx, ]
  g1    <- dat_b$sas_total[dat_b$extension_pesquera == 0]
  g2    <- dat_b$sas_total[dat_b$extension_pesquera == 1]
  sd_pool <- sqrt(((length(g1)-1)*var(g1) + (length(g2)-1)*var(g2)) /
                    (length(g1) + length(g2) - 2))
  (mean(g2) - mean(g1)) / sd_pool
})

cat("PASO 1 · CÁLCULO:\n")
cat("  d bootstrap media:", round(mean(boot_d), 3), "\n")
cat("  SE bootstrap:", round(sd(boot_d), 3), "\n")
cat("  IC 95% bootstrap: [",
    round(quantile(boot_d, 0.025), 3), ",",
    round(quantile(boot_d, 0.975), 3), "]\n\n")

cat("PASO 2 · INTERPRETACIÓN:\n")
d_boot_low  <- quantile(boot_d, 0.025)
cat("  H2 (d > 0.5):", ifelse(d_boot_low > 0.5,
                               "✓ IC inferior > 0.5: H2 SE CONFIRMA robustamente",
                               ifelse(d_boot_low > 0,
                                      "~ IC inferior ≤ 0.5 pero positivo: efecto real pero pequeño",
                                      "✗ IC incluye 0: H2 no confirmada")), "\n")

# =============================================================================
# SECCIÓN 3: BOOTSTRAP DE LA CORRELACIÓN (H3)
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 3: Bootstrap de la Correlación SAS ~ Distancia ANP (H3)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

boot_r <- replicate(B, {
  idx   <- sample(1:n, n, replace = TRUE)
  dat_b <- datos[idx, ]
  cor(dat_b$sas_total, dat_b$distancia_anp_km,
      use = "complete.obs", method = "pearson")
})

r_orig <- cor(datos$sas_total, datos$distancia_anp_km, use = "complete.obs")
cat("  r Pearson original:", round(r_orig, 3), "\n")
cat("  r bootstrap media:", round(mean(boot_r), 3), "\n")
cat("  IC 95% bootstrap: [",
    round(quantile(boot_r, 0.025), 3), ",",
    round(quantile(boot_r, 0.975), 3), "]\n\n")
cat("  H3 (r ≥ 0.40):", ifelse(
  quantile(boot_r, 0.025) >= 0.40,
  "✓ IC inferior ≥ 0.40: H3 SE CONFIRMA",
  ifelse(r_orig >= 0.40,
         "~ r puntual ≥ 0.40 pero IC incluye <0.40: confirmación débil",
         "✗ r < 0.40: H3 no confirmada")), "\n")

# =============================================================================
# SECCIÓN 4: SIMULACIÓN MONTE CARLO — ESCENARIO DE POLÍTICA
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 4: Simulación Monte Carlo · Escenario de Política\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("PREGUNTA DE POLÍTICA:
  '¿Cuál sería el SAS promedio esperado si el programa de
   extensión pesquera aumenta su cobertura del 35% actual al 55%?'
  Esta pregunta NO puede responderse con el modelo OLS directamente
  (eso requiere inferencia causal). Se simula para ilustrar el rango
  de resultados BAJO EL SUPUESTO de que el OR observado es causal.\n\n")

# Coeficientes del modelo M3 (script 10)
modelos <- tryCatch(
  readRDS("datos/procesados/modelos_ols_s3.rds"),
  error = function(e) list(M3 = m_orig)
)
M3 <- modelos$M3 %||% m_orig

coef_m3      <- coef(M3)
sigma_modelo <- sigma(M3)

n_sim   <- 5000   # Simulaciones Monte Carlo
cob_obs <- mean(datos$extension_pesquera)   # Cobertura observada
cob_pol <- 0.55                              # Cobertura bajo política

cat("Cobertura observada:", round(cob_obs * 100, 1), "%\n")
cat("Cobertura hipotética (política):", cob_pol * 100, "%\n\n")

# Simular SAS bajo los dos escenarios
simular_sas <- function(datos_base, coef, sigma, cobertura, n_rep = 5000) {
  replicate(n_rep, {
    # Reasignar extensión_pesquera con nueva cobertura
    n_b      <- nrow(datos_base)
    dat_sim  <- datos_base %>%
      mutate(extension_pesquera = rbinom(n_b, 1, cobertura))
    # Predicción + ruido estocástico
    X_pred   <- model.matrix(
      ~ extension_pesquera + log_ingreso + años_experiencia +
        distancia_anp_km + percepcion_num + metodo,
      data = dat_sim
    )
    # Alinear columnas con coeficientes
    cols_com <- intersect(names(coef), colnames(X_pred))
    pred_lin <- X_pred[, cols_com] %*% coef[cols_com]
    mean(pred_lin + rnorm(n_b, 0, sigma))
  })
}

cat("Corriendo simulación Monte Carlo (n_sim =", n_sim, ")...\n")
sas_obs_sim <- simular_sas(datos, coef_m3, sigma_modelo, cob_obs, n_sim)
sas_pol_sim <- simular_sas(datos, coef_m3, sigma_modelo, cob_pol, n_sim)

mc_df <- tibble(
  sas_sim  = c(sas_obs_sim, sas_pol_sim),
  escenario = rep(c(paste0("Observado (", round(cob_obs*100,0), "%)"),
                     paste0("Política (", round(cob_pol*100,0), "%)")),
                   each = n_sim)
)

cat("\nPASO 1 · CÁLCULO — Resultados simulación Monte Carlo:\n")
mc_summary <- mc_df %>%
  group_by(escenario) %>%
  summarise(
    SAS_medio  = round(mean(sas_sim), 3),
    SE         = round(sd(sas_sim), 3),
    IC_low     = round(quantile(sas_sim, 0.025), 3),
    IC_high    = round(quantile(sas_sim, 0.975), 3),
    .groups    = "drop"
  ) %>%
  mutate(
    Diferencia   = round(SAS_medio - SAS_medio[1], 3),
    Dif_IC_low   = round(IC_low  - mean(sas_obs_sim), 3),
    Dif_IC_high  = round(IC_high - mean(sas_obs_sim), 3)
  )

print(mc_summary)

cat("\nPASO 2 · INTERPRETACIÓN:\n")
delta_sas <- mc_summary$SAS_medio[2] - mc_summary$SAS_medio[1]
cat("  Aumento esperado en SAS si cobertura ↑20pp:",
    round(delta_sas, 2), "puntos\n")
cat("  IC 95% del diferencial: [",
    round(mc_summary$Dif_IC_low[2], 2), ",",
    round(mc_summary$Dif_IC_high[2], 2), "]\n")
cat("  ¿IC incluye 0?:", ifelse(
  mc_summary$Dif_IC_low[2] < 0 & mc_summary$Dif_IC_high[2] > 0,
  "Sí → diferencia no es estadísticamente discernible",
  "No → diferencia es estadísticamente discernible"), "\n")

cat("\nPASO 3 · CONTEXTO COSTERO:\n")
cat("  Bajo el SUPUESTO de que la asociación extensión→SAS es causal:
  ampliar el programa de extensión pesquera al 55% de cobertura
  se asociaría con un aumento de ~", round(delta_sas, 1), "pts en el SAS
  promedio de las comunidades. En política pesquera, esto representaría
  un impacto moderado-alto en adopción de prácticas sostenibles.
  ADVERTENCIA: Este resultado es indicativo, no causal.\n")

cat("\nPASO 4 · DECISIÓN CRÍTICA:\n")
cat("  La simulación MC usa los coeficientes OLS como parámetros 'verdaderos'.
  Si el modelo está mal especificado (variables omitidas, endogeneidad),
  el escenario es igualmente sesgado.
  Uso recomendado: comunicación ejecutiva de rangos de incertidumbre,
  NO para presupuestos de política (requiere evaluación experimental).\n")

# Gráfico Monte Carlo
p_mc <- ggplot(mc_df, aes(x = sas_sim, fill = escenario)) +
  geom_density(alpha = 0.5, linewidth = 0.8) +
  scale_fill_manual(values = c("#2c7fb8","#e34a33")) +
  labs(
    title    = "Simulación Monte Carlo — SAS Promedio Esperado",
    subtitle = paste0("n_sim = ", n_sim, " | Escenario actual vs. política +20pp extensión"),
    x        = "SAS Promedio Simulado",
    y        = "Densidad",
    fill     = "Escenario",
    caption  = "Nota: Resultado predicho, no causal | SP-8502 · Sprint 3 · 2026"
  )

ggsave("outputs/figuras/S3_montecarlo_escenario.png",
       plot = p_mc, width = 9, height = 5, dpi = 300)
cat("\n✓ Figura MC: outputs/figuras/S3_montecarlo_escenario.png\n")

# =============================================================================
# SECCIÓN 5: TABLA RESUMEN BOOTSTRAP
# =============================================================================
resumen_boot <- tibble(
  Estadístico  = c("R² OLS","β(extensión)","d Cohen (H2)","r Pearson (H3)"),
  Valor_puntual = c(round(r2_orig, 3),
                    round(coef(m_orig)["extension_pesquera"], 3),
                    round(mean(boot_d), 3),
                    round(r_orig, 3)),
  SE_bootstrap = c(round(sd(boot_df$r2), 3),
                   round(sd(boot_df$beta_ext), 3),
                   round(sd(boot_d), 3),
                   round(sd(boot_r), 3)),
  IC_95_bajo   = c(round(quantile(boot_df$r2, 0.025), 3),
                   round(quantile(boot_df$beta_ext, 0.025), 3),
                   round(quantile(boot_d, 0.025), 3),
                   round(quantile(boot_r, 0.025), 3)),
  IC_95_alto   = c(round(quantile(boot_df$r2, 0.975), 3),
                   round(quantile(boot_df$beta_ext, 0.975), 3),
                   round(quantile(boot_d, 0.975), 3),
                   round(quantile(boot_r, 0.975), 3))
)

print(resumen_boot)
write_csv(resumen_boot, "datos/procesados/resumen_bootstrap_s3.csv")
write_csv(mc_summary,   "datos/procesados/montecarlo_escenario_s3.csv")

cat("\n=============================================================\n")
cat("  ✓ Script 13 completado. Bootstrap y MC concluidos.\n")
cat("  Siguiente: script_analisis.R (entregable integrado)\n")
cat("=============================================================\n")
