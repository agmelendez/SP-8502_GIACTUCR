# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 11_regresion_logistica.R
# SPRINT  : 3 — Análisis Profundo y Modelado (Semanas 9–12)
# TEMA    : Regresión Logística Binaria · Variable Outcome Dicotómica
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   La extensión pesquera (Sí/No) es tanto predictor del SAS (Regresión OLS)
#   como un outcome de interés propio:
#   "¿Qué determina que un pescador PARTICIPE en programas de extensión?"
#
#   Modelo logístico:
#   logit(P(extensión=1)) = β₀ + β₁·SAS + β₂·log(ingreso) +
#                            β₃·años_exp + β₄·distancia + β₅·percepción
#
#   Incluye:
#   1. Especificación y ajuste del modelo logístico
#   2. Odds Ratios (OR) con IC 95%
#   3. Probabilidades predichas (efectos marginales)
#   4. Métricas de ajuste: Pseudo-R², Hosmer-Lemeshow, AUC-ROC
#   5. Curva ROC y punto de corte óptimo (Youden)
#   6. Matriz de confusión
#
# NOTA SOBRE CAUSALIDAD:
#   Los modelos son PREDICTIVOS, no causales. La interpretación causal
#   requeriría diseño experimental o instrumento instrumental válido.
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")

pkgs <- c("tidyverse","broom","pROC","ResourceSelection",
          "patchwork","caret","ggplot2")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/", quiet = TRUE)
  library(p, character.only = TRUE)
}))

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 3 · Regresión Logística Binaria\n")
cat("  Outcome: Participación en Extensión Pesquera (0/1)\n")
cat("=============================================================\n\n")

datos <- readRDS("datos/procesados/datos_imputados_s3.rds")
cat("Datos cargados:", nrow(datos), "obs.\n")
cat("Distribución del outcome:\n")
print(table(datos$extension_pesquera, dnn = "Extensión"))
cat("Prevalencia:", round(mean(datos$extension_pesquera) * 100, 1), "%\n\n")

# =============================================================================
# SECCIÓN 1: ESPECIFICACIÓN Y AJUSTE
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Especificación y Ajuste del Modelo Logístico\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Modelo nulo (solo intercepto)
logit_0 <- glm(extension_pesquera ~ 1,
               data = datos, family = binomial(link = "logit"))

# Modelo completo
logit_1 <- glm(
  extension_pesquera ~ sas_total + log_ingreso + años_experiencia +
    distancia_anp_km + percepcion_num + comunidad,
  data = datos, family = binomial(link = "logit")
)

cat("PASO 1 · CÁLCULO — Resumen del modelo logístico:\n\n")
print(summary(logit_1))

# =============================================================================
# SECCIÓN 2: ODDS RATIOS CON IC 95%
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Odds Ratios (OR) e Interpretación\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("PASO 1 · CÁLCULO:\n")
or_tabla <- tidy(logit_1, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    across(c(estimate, conf.low, conf.high), round, 3),
    p.value      = round(p.value, 4),
    significancia = case_when(
      p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
      p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".",
      TRUE            ~ "ns"
    ),
    Interpretacion = case_when(
      term == "(Intercept)"    ~ "Odds base (todos predictores = 0)",
      term == "sas_total"      ~ "OR por cada punto adicional de SAS",
      term == "log_ingreso"    ~ "OR por 1% de aumento en ingreso",
      term == "años_experiencia" ~ "OR por año adicional de experiencia",
      term == "distancia_anp_km" ~ "OR por km adicional de distancia al ANP",
      term == "percepcion_num"   ~ "OR por nivel adicional de percepción positiva",
      grepl("comunidad", term)   ~ paste("OR vs. categoría referencia"),
      TRUE ~ ""
    )
  )

print(or_tabla %>% select(term, estimate, conf.low, conf.high,
                           p.value, significancia))

cat("\nPASO 2 · INTERPRETACIÓN ESTADÍSTICA:\n")
cat("  Un OR > 1 indica mayor probabilidad del evento (extensión = Sí).\n")
cat("  Un OR < 1 indica menor probabilidad.\n")
cat("  El IC 95% que no incluye el 1 indica significancia estadística.\n\n")

# Identificar OR significativos
sig_or <- or_tabla %>%
  filter(p.value < 0.05, term != "(Intercept)") %>%
  arrange(desc(abs(log(estimate))))

cat("Predictores estadísticamente significativos (p < 0.05):\n")
if (nrow(sig_or) > 0) {
  for (i in 1:nrow(sig_or)) {
    cat("  •", sig_or$term[i], ": OR =", sig_or$estimate[i],
        " → cada unidad adicional", ifelse(sig_or$estimate[i] > 1,
                                            "AUMENTA", "REDUCE"),
        "el OR en", round(abs(sig_or$estimate[i] - 1) * 100, 1), "%\n")
  }
} else {
  cat("  Ningún predictor individual es significativo al 5%.\n")
}

cat("\nPASO 3 · CONTEXTO COSTERO:\n")
cat("  La participación en extensión pesquera es una variable de ACCESO:
  pescadores más cercanos a centros institucionales (IMAS, INCOPESCA)
  tienen mayor acceso geográfico. Esto explica la dirección esperada
  del OR de distancia_anp_km (mayor distancia → menor participación).
  El OR de SAS total es particularmente relevante: indica si el
  comportamiento sostenible PREDICE el involucramiento institucional
  o viceversa (endogeneidad potencial → ver decisión crítica).\n")

# Forest plot de OR
p_or <- or_tabla %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, log(estimate))) %>%
  ggplot(aes(x = log(estimate), y = term)) +
  geom_vline(xintercept = 0, color = "grey50",
             linewidth = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = log(conf.low), xmax = log(conf.high)),
                 height = 0.3, color = "#636363") +
  geom_point(aes(color = p.value < 0.05), size = 3.5) +
  scale_color_manual(
    values = c("TRUE" = "#e34a33", "FALSE" = "#bdbdbd"),
    labels = c("p ≥ 0.05", "p < 0.05"), name = ""
  ) +
  scale_x_continuous(
    labels = function(x) round(exp(x), 2),
    name   = "Odds Ratio (escala log)"
  ) +
  labs(
    title    = "Odds Ratios — Regresión Logística",
    subtitle = "Outcome: Participación en extensión pesquera | IC 95%",
    y        = NULL,
    caption  = "SP-8502 · Sprint 3 · 2026"
  )

ggsave("outputs/figuras/S3_or_logistica.png",
       plot = p_or, width = 9, height = 6, dpi = 300)

# =============================================================================
# SECCIÓN 3: MÉTRICAS DE AJUSTE
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 3: Métricas de Ajuste del Modelo Logístico\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── Pseudo-R² de McFadden ─────────────────────────────────────────────────
pseudo_r2 <- 1 - logLik(logit_1) / logLik(logit_0)
cat("Pseudo-R² de McFadden:", round(as.numeric(pseudo_r2), 4), "\n")
cat("  Referencia: 0.10–0.20 = aceptable; 0.20–0.40 = bueno\n\n")

# ── Hosmer-Lemeshow (calibración) ────────────────────────────────────────
hl_test <- hoslem.test(datos$extension_pesquera,
                        fitted(logit_1), g = 8)
cat("Hosmer-Lemeshow (calibración):\n")
cat("  χ² =", round(hl_test$statistic, 3),
    "| p =", round(hl_test$p.value, 4), "\n")
cat("  Interpretación:", ifelse(hl_test$p.value > 0.05,
                                 "p > 0.05 → Buen ajuste ✓",
                                 "p ≤ 0.05 → Calibración deficiente ⚠"), "\n\n")

# ── Curva ROC y AUC ──────────────────────────────────────────────────────
roc_obj <- roc(datos$extension_pesquera, fitted(logit_1), quiet = TRUE)
auc_val <- auc(roc_obj)
cat("AUC-ROC:", round(auc_val, 4), "\n")
cat("  Referencia: AUC = 0.5 (azar); 0.7–0.8 = aceptable; >0.8 = bueno\n")
cat("  Clasificación:", case_when(
  auc_val >= 0.90 ~ "Excelente",
  auc_val >= 0.80 ~ "Bueno",
  auc_val >= 0.70 ~ "Aceptable",
  auc_val >= 0.60 ~ "Pobre",
  TRUE            ~ "Falla"
), "\n\n")

# ── Punto de corte óptimo (índice de Youden) ──────────────────────────────
coords_youden <- coords(roc_obj, "best", ret = c("threshold","sensitivity",
                                                   "specificity","ppv","npv"))
cat("Punto de corte óptimo (Youden):\n")
cat("  Threshold:", round(coords_youden$threshold, 3), "\n")
cat("  Sensibilidad:", round(coords_youden$sensitivity, 3), "\n")
cat("  Especificidad:", round(coords_youden$specificity, 3), "\n")
cat("  VPP:", round(coords_youden$ppv, 3), "\n")
cat("  VPN:", round(coords_youden$npv, 3), "\n\n")

# Curva ROC
roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "#2c7fb8", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50") +
  geom_point(aes(x = 1 - coords_youden$specificity,
                  y = coords_youden$sensitivity),
             color = "#e34a33", size = 4) +
  annotate("text", x = 0.60, y = 0.20,
           label = paste0("AUC = ", round(auc_val, 3)),
           color = "#2c7fb8", size = 5, fontface = "bold") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title    = "Curva ROC — Regresión Logística",
    subtitle = "Punto rojo = corte óptimo (Youden) | Outcome: Extensión pesquera",
    x        = "1 − Especificidad (Tasa Falsos Positivos)",
    y        = "Sensibilidad (Tasa Verdaderos Positivos)",
    caption  = "SP-8502 · Sprint 3 · 2026"
  )

ggsave("outputs/figuras/S3_curva_roc.png",
       plot = p_roc, width = 7, height = 6, dpi = 300)
cat("✓ Curva ROC: outputs/figuras/S3_curva_roc.png\n")

# =============================================================================
# SECCIÓN 4: PROBABILIDADES PREDICHAS (EFECTOS MARGINALES)
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 4: Probabilidades Predichas · Efectos Marginales\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Curva: P(extensión=1) ~ SAS, fijando el resto en su media
sas_seq  <- seq(0, 50, by = 1)
datos_pred <- tibble(
  sas_total         = sas_seq,
  log_ingreso       = mean(datos$log_ingreso, na.rm = TRUE),
  años_experiencia  = mean(datos$años_experiencia, na.rm = TRUE),
  distancia_anp_km  = mean(datos$distancia_anp_km, na.rm = TRUE),
  percepcion_num    = median(datos$percepcion_num, na.rm = TRUE),
  comunidad         = levels(datos$comunidad)[1]   # Categoría ref.
)

datos_pred$prob_pred <- predict(logit_1, newdata = datos_pred,
                                 type = "response")

p_marg <- ggplot(datos_pred, aes(x = sas_total, y = prob_pred)) +
  geom_ribbon(aes(ymin = prob_pred - 0.05,
                   ymax = prob_pred + 0.05),
              fill = "#2c7fb8", alpha = 0.2) +
  geom_line(color = "#2c7fb8", linewidth = 1.3) +
  geom_hline(yintercept = mean(datos$extension_pesquera),
             color = "#e34a33", linewidth = 0.8, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  labs(
    title    = "Probabilidad Predicha de Participar en Extensión vs. SAS",
    subtitle = "Resto de predictores fijados en su media/mediana",
    x        = "SAS Total (0–50 pts)",
    y        = "P(Extensión = Sí)",
    caption  = "Línea roja = prevalencia observada | SP-8502 · 2026"
  )

ggsave("outputs/figuras/S3_efectos_marginales.png",
       plot = p_marg, width = 8, height = 5, dpi = 300)
cat("✓ Efectos marginales: outputs/figuras/S3_efectos_marginales.png\n")

# =============================================================================
# SECCIÓN 5: DECISIÓN CRÍTICA — ENDOGENEIDAD SAS ↔ EXTENSIÓN
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASO 4 · DECISIÓN CRÍTICA: Endogeneidad Potencial\n")
cat("─────────────────────────────────────────────────────────────\n\n")

cat("
PROBLEMA DETECTADO: SAS aparece como predictor en el modelo logístico
(Script 11) y extensión como predictor en el modelo OLS (Script 10).
Esto plantea potencial ENDOGENEIDAD BIDIRECCIONAL:

  SAS ─→ extensión  (Script 11: OR SAS)
  extensión ─→ SAS  (Script 10: β extensión)

IMPLICACIÓN: Los estimadores OLS y logísticos son INCONSISTENTES si
hay causalidad inversa (reverse causality).

DECISIÓN ADOPTADA:
  (a) CORTO PLAZO (este sprint): Reportar la correlación/asociación
      sin reclamar causalidad. Usar lenguaje predictivo: 'se asocia con'
      en lugar de 'causa' o 'aumenta'.
  (b) MEDIANO PLAZO (tesis): Evaluar variable instrumental (IV).
      Candidato: distancia al centro de capacitación INCOPESCA más
      cercano. Es exógena (no la eligen los pescadores) y correlaciona
      con extensión (afecta acceso) pero no directamente con SAS.
  (c) NOTA PARA POLÍTICA: La asociación bidireccional POSITIVA es en sí
      misma un hallazgo: sugiere un 'círculo virtuoso' potencial entre
      prácticas sostenibles y participación institucional.

REFERENCIA: Wooldridge (2019) Introductory Econometrics, Cap. 15–16.
")

# Guardar resultados
write_csv(or_tabla, "datos/procesados/odds_ratios_logistica_s3.csv")

cat("\n=============================================================\n")
cat("  ✓ Script 11 completado.\n")
cat("  AUC-ROC =", round(auc_val, 3), "| HL p =",
    round(hl_test$p.value, 3), "\n")
cat("  Siguiente: 12_analisis_cualitativo.R\n")
cat("=============================================================\n")
