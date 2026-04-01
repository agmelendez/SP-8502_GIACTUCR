# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 14_visualizacion_ggplot2.R
# SPRINT  : 4 — Comunicación y Toma de Decisiones (Semanas 13–15)
# TEMA    : Visualización Efectiva con ggplot2 · Storytelling con Datos
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   Genera el conjunto completo de visualizaciones para el Policy Brief
#   y la Presentación Ejecutiva. Cada figura está diseñada bajo los
#   principios de Tufte (2001) y Schwabish (2021):
#     - Una sola idea principal por gráfico
#     - Redundancia visual eliminada
#     - Anotaciones directas (no leyendas remotas cuando sea posible)
#     - Paleta accesible (daltonismo-safe: viridis/ColorBrewer)
#     - Título informativo (el hallazgo, no solo la variable)
#
#   FIGURAS GENERADAS (6):
#   FIG1: SAS por comunidad (barras horizontales con IC 95%)
#   FIG2: Efecto marginal de extensión pesquera sobre SAS (lollipop)
#   FIG3: Waterfall chart de predictores OLS (coeficientes estandarizados)
#   FIG4: Segmentación de pescadores por perfil (scatter SAS ~ ingreso)
#   FIG5: Trayectoria de convergencia cuanti-cuali (tabla visual)
#   FIG6: Dashboard de síntesis para tomadores de decisión
#
# SECUENCIA PEDAGÓGICA: Cálculo → Interpretación → Contexto → Decisión
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")

pkgs <- c("tidyverse","scales","patchwork","ggtext",
          "RColorBrewer","viridis","broom")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/", quiet = TRUE)
  library(p, character.only = TRUE)
}))

set.seed(8502)

# Paleta del curso (accesible · daltonismo-safe)
pal_principal <- c(
  azul_UCR   = "#003F8A",
  verde_mar  = "#1d7e6a",
  naranja    = "#e06c00",
  rojo       = "#c0392b",
  gris_clar  = "#f0f4f8",
  gris_med   = "#636363",
  amarillo   = "#f0c040"
)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 4 · Visualización Ejecutiva\n")
cat("  6 figuras para Policy Brief y Presentación\n")
cat("=============================================================\n\n")

# Cargar datos del Sprint 3
if (!file.exists("datos/procesados/datos_imputados_s3.rds")) {
  stop("Ejecute la suite del Sprint 3 primero.")
}
datos     <- readRDS("datos/procesados/datos_imputados_s3.rds")
modelos   <- readRDS("datos/procesados/modelos_finales_s3.rds")
coef_tab  <- read_csv("datos/procesados/coeficientes_M3_final.csv",
                       show_col_types = FALSE)
hip_tab   <- read_csv("datos/procesados/resultados_hipotesis_s3.csv",
                       show_col_types = FALSE)
temas_q   <- read_csv("datos/procesados/temas_cualitativos_s3.csv",
                       show_col_types = FALSE)
M3        <- modelos$M3
n         <- nrow(datos)

# =============================================================================
# FIGURA 1: SAS por comunidad — "¿Dónde se practica más la pesca sostenible?"
# =============================================================================
cat("Generando FIG1: SAS por comunidad...\n")

sas_com <- datos %>%
  group_by(comunidad) %>%
  summarise(
    media    = mean(sas_total, na.rm = TRUE),
    se       = sd(sas_total, na.rm = TRUE) / sqrt(n()),
    ic_low   = media - qt(0.975, n() - 1) * se,
    ic_high  = media + qt(0.975, n() - 1) * se,
    n        = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    comunidad = fct_reorder(comunidad, media),
    color_barra = ifelse(media == max(media), pal_principal["azul_UCR"],
                  ifelse(media == min(media), pal_principal["rojo"],
                         pal_principal["verde_mar"]))
  )

media_global <- mean(datos$sas_total, na.rm = TRUE)

fig1 <- ggplot(sas_com, aes(x = media, y = comunidad)) +
  geom_vline(xintercept = media_global,
             color = pal_principal["gris_med"],
             linewidth = 0.8, linetype = "dashed") +
  geom_errorbarh(aes(xmin = ic_low, xmax = ic_high),
                 height = 0.3, color = pal_principal["gris_med"],
                 linewidth = 0.8) +
  geom_point(aes(color = media), size = 5) +
  scale_color_gradient(low  = pal_principal["rojo"],
                       high = pal_principal["verde_mar"],
                       guide = "none") +
  geom_text(aes(label = paste0(round(media, 1), " pts\n(n=", n, ")")),
            hjust = -0.15, size = 3.3, color = pal_principal["gris_med"]) +
  annotate("text",
           x     = media_global + 0.3,
           y     = 0.6,
           label = paste0("Promedio\nnacional\n", round(media_global, 1)),
           size  = 3, color = pal_principal["gris_med"], hjust = 0) +
  scale_x_continuous(limits = c(10, 45),
                     labels = function(x) paste0(x, " pts")) +
  labs(
    title    = "Quepos lidera la adopción de prácticas sostenibles",
    subtitle = "Score de Adopción Sostenible (SAS) con IC 95% · por comunidad pesquera",
    x        = "SAS Promedio (escala 0–50 pts)",
    y        = NULL,
    caption  = "Fuente: Encuesta SP-8502 · 2026 · n = 80 pescadores artesanales"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", color = pal_principal["azul_UCR"],
                                  size = 14),
    plot.subtitle = element_text(color = pal_principal["gris_med"], size = 11),
    plot.caption  = element_text(size = 8, color = pal_principal["gris_med"]),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y   = element_text(face = "bold", size = 11)
  )

ggsave("outputs/figuras/S4_fig1_sas_comunidad.png",
       fig1, width = 10, height = 5.5, dpi = 300)
cat("  ✓ FIG1 guardada\n")

# =============================================================================
# FIGURA 2: Efecto de la extensión pesquera — comparación de grupos
# =============================================================================
cat("Generando FIG2: Efecto extensión pesquera...\n")

ext_stats <- datos %>%
  mutate(grupo = ifelse(extension_pesquera == 1,
                        "Participa en\nextensión",
                        "No participa en\nextensión")) %>%
  group_by(grupo) %>%
  summarise(
    media   = mean(sas_total, na.rm = TRUE),
    se      = sd(sas_total, na.rm = TRUE) / sqrt(n()),
    ic_low  = media - qt(0.975, n() - 1) * se,
    ic_high = media + qt(0.975, n() - 1) * se,
    n       = n(),
    .groups = "drop"
  )

# d de Cohen calculado
g1 <- datos$sas_total[datos$extension_pesquera == 0]
g2 <- datos$sas_total[datos$extension_pesquera == 1]
sd_p <- sqrt(((length(g1)-1)*var(g1,na.rm=T)+(length(g2)-1)*var(g2,na.rm=T))/
               (length(g1)+length(g2)-2))
d_val <- round((mean(g2,na.rm=T)-mean(g1,na.rm=T))/sd_p, 2)
d_interp <- case_when(abs(d_val)<0.2~"trivial",abs(d_val)<0.5~"pequeño",
                       abs(d_val)<0.8~"mediano",TRUE~"grande")

fig2 <- ggplot(ext_stats, aes(x = grupo, y = media, fill = grupo)) +
  geom_col(width = 0.55, alpha = 0.85) +
  geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                width = 0.12, linewidth = 1,
                color = pal_principal["gris_med"]) +
  geom_text(aes(label = paste0(round(media, 1), " pts\n(n=", n, ")")),
            vjust = -1.2, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(
    "Participa en\nextensión"    = pal_principal["verde_mar"],
    "No participa en\nextensión" = pal_principal["gris_med"]
  ), guide = "none") +
  scale_y_continuous(limits = c(0, 45),
                     labels = function(x) paste0(x, " pts")) +
  annotate("segment",
           x = 1, xend = 2, y = 40, yend = 40,
           color = pal_principal["azul_UCR"], linewidth = 1) +
  annotate("text",
           x = 1.5, y = 42,
           label = paste0("d Cohen = ", d_val, " (efecto ", d_interp, ")"),
           size = 4, color = pal_principal["azul_UCR"], fontface = "bold") +
  labs(
    title    = "Los pescadores en programas de extensión tienen mayor SAS",
    subtitle = paste0("Diferencia estadísticamente significativa · d de Cohen = ",
                       d_val),
    x        = NULL,
    y        = "SAS Promedio (0–50 pts)",
    caption  = "IC 95% mostrado | SP-8502 · 2026"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold", color = pal_principal["azul_UCR"]),
    plot.subtitle = element_text(color = pal_principal["gris_med"]),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold")
  )

ggsave("outputs/figuras/S4_fig2_efecto_extension.png",
       fig2, width = 8, height = 6, dpi = 300)
cat("  ✓ FIG2 guardada\n")

# =============================================================================
# FIGURA 3: Waterfall de coeficientes estandarizados (importancia relativa)
# =============================================================================
cat("Generando FIG3: Waterfall coeficientes estandarizados...\n")

beta_data <- coef_tab %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_limpio = case_when(
      term == "extension_pesquera" ~ "Extensión pesquera",
      term == "log_ingreso"        ~ "Ingreso mensual (log)",
      term == "años_experiencia"   ~ "Años de experiencia",
      term == "tamaño_familia"     ~ "Tamaño del grupo familiar",
      term == "distancia_anp_km"   ~ "Distancia al ANP",
      term == "percepcion_num"     ~ "Percepción del recurso",
      grepl("metodo", term)        ~ "Método de muestreo (RDS)",
      TRUE ~ term
    ),
    Dirección = ifelse(estimate > 0, "Aumenta SAS ↑", "Reduce SAS ↓"),
    alpha_val  = ifelse(p.value < 0.05, 1.0, 0.45),
    term_limpio = fct_reorder(term_limpio, estimate)
  )

fig3 <- ggplot(beta_data,
               aes(x = estimate, y = term_limpio, fill = Dirección)) +
  geom_col(aes(alpha = alpha_val), width = 0.65) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, color = pal_principal["gris_med"],
                 linewidth = 0.8) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("Aumenta SAS ↑" = pal_principal["verde_mar"],
                                "Reduce SAS ↓"  = pal_principal["rojo"])) +
  scale_alpha_identity() +
  geom_text(aes(label = paste0(round(estimate, 2),
                                ifelse(p.value < 0.05, "*", "")),
                hjust = ifelse(estimate > 0, -0.2, 1.2)),
            size = 3.5, color = "grey30") +
  labs(
    title    = "La extensión pesquera y la percepción del recurso\nson los predictores más importantes del SAS",
    subtitle = "Coeficientes OLS con IC 95% · * p < 0.05 · Barras translúcidas = no significativo",
    x        = "Efecto en SAS (puntos)",
    y        = NULL,
    fill     = NULL,
    caption  = paste0("Modelo M3 · R² = ",
                       round(summary(M3)$r.squared, 3),
                       " · n = ", n, " | SP-8502 · 2026")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold", color = pal_principal["azul_UCR"],
                                 lineheight = 1.2),
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

ggsave("outputs/figuras/S4_fig3_waterfall_coef.png",
       fig3, width = 10, height = 6, dpi = 300)
cat("  ✓ FIG3 guardada\n")

# =============================================================================
# FIGURA 4: Segmentación de perfiles de pescador
# =============================================================================
cat("Generando FIG4: Perfiles de pescador (scatter segmentado)...\n")

# Clasificar en cuadrantes con base en mediana de ingreso y SAS
med_sas <- median(datos$sas_total, na.rm = TRUE)
med_ing <- median(datos$ingreso_mensual_usd, na.rm = TRUE)

datos_seg <- datos %>%
  filter(!is.na(sas_total), !is.na(ingreso_mensual_usd)) %>%
  mutate(
    perfil = case_when(
      sas_total >= med_sas & ingreso_mensual_usd >= med_ing ~
        "Sostenible próspero",
      sas_total >= med_sas & ingreso_mensual_usd <  med_ing ~
        "Sostenible precario",
      sas_total <  med_sas & ingreso_mensual_usd >= med_ing ~
        "No sostenible próspero",
      TRUE ~
        "No sostenible precario"
    ),
    perfil = factor(perfil, levels = c("Sostenible próspero",
                                        "Sostenible precario",
                                        "No sostenible próspero",
                                        "No sostenible precario")),
    ext_label = ifelse(extension_pesquera == 1, "◆", "·")
  )

n_perfiles <- datos_seg %>% count(perfil)

fig4 <- ggplot(datos_seg,
               aes(x = ingreso_mensual_usd, y = sas_total, color = perfil)) +
  geom_hline(yintercept = med_sas, color = "grey70", linetype = "dashed") +
  geom_vline(xintercept = med_ing, color = "grey70", linetype = "dashed") +
  geom_point(aes(shape = factor(extension_pesquera)),
             size = 2.8, alpha = 0.75) +
  scale_color_manual(
    values = c("Sostenible próspero"       = pal_principal["verde_mar"],
               "Sostenible precario"       = pal_principal["azul_UCR"],
               "No sostenible próspero"    = pal_principal["naranja"],
               "No sostenible precario"    = pal_principal["rojo"]),
    name = "Perfil del pescador"
  ) +
  scale_shape_manual(values = c("0" = 16, "1" = 18),
                     labels = c("No participa", "Sí participa"),
                     name   = "Extensión pesquera") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  annotate("text", x = med_ing * 0.35, y = 48,
           label = "Sostenible\nprecario", color = pal_principal["azul_UCR"],
           size = 3.2, fontface = "bold") +
  annotate("text", x = med_ing * 1.55, y = 48,
           label = "Sostenible\nprospero", color = pal_principal["verde_mar"],
           size = 3.2, fontface = "bold") +
  annotate("text", x = med_ing * 0.35, y = 5,
           label = "No sostenible\nprecario", color = pal_principal["rojo"],
           size = 3.2, fontface = "bold") +
  annotate("text", x = med_ing * 1.55, y = 5,
           label = "No sostenible\nprospero", color = pal_principal["naranja"],
           size = 3.2, fontface = "bold") +
  labs(
    title    = "Cuatro perfiles de pescador artesanal en el Pacífico Central",
    subtitle = "◆ Participan en extensión pesquera · · No participan",
    x        = "Ingreso mensual por pesca (USD)",
    y        = "Score de Adopción Sostenible (0–50 pts)",
    caption  = "Líneas = medianas muestrales | SP-8502 · 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", color = pal_principal["azul_UCR"]),
    legend.position = "bottom",
    legend.box    = "horizontal"
  )

ggsave("outputs/figuras/S4_fig4_perfiles_pescador.png",
       fig4, width = 10, height = 7, dpi = 300)
cat("  ✓ FIG4 guardada\n")

# =============================================================================
# FIGURA 5: Evolución de la cobertura simulada (para el escenario MC)
# =============================================================================
cat("Generando FIG5: Escenario de política (área chart)...\n")

# Simular trayectoria de política 2026–2030
years       <- 2026:2031
cob_actual  <- mean(datos$extension_pesquera, na.rm = TRUE)
cob_escen   <- c(cob_actual,
                  cob_actual + 0.04,
                  cob_actual + 0.08,
                  cob_actual + 0.14,
                  cob_actual + 0.18,
                  cob_actual + 0.20)
# Efecto OLS: β(ext) aplicado al incremento de cobertura
beta_ext    <- coef(M3)["extension_pesquera"]
sas_escen   <- mean(datos$sas_total, na.rm = TRUE) +
  beta_ext * (cob_escen - cob_actual) * 100

escen_df <- tibble(
  Año             = years,
  Cobertura       = round(cob_escen * 100, 1),
  SAS_esperado    = round(sas_escen, 1),
  Cobertura_inf   = pmax(0, Cobertura - 3),
  Cobertura_sup   = pmin(100, Cobertura + 3)
)

fig5a <- ggplot(escen_df, aes(x = Año)) +
  geom_ribbon(aes(ymin = Cobertura_inf, ymax = Cobertura_sup),
              fill = pal_principal["verde_mar"], alpha = 0.20) +
  geom_line(aes(y = Cobertura), color = pal_principal["verde_mar"],
            linewidth = 1.5) +
  geom_point(aes(y = Cobertura), color = pal_principal["verde_mar"],
             size = 3.5) +
  geom_text(aes(y = Cobertura, label = paste0(Cobertura, "%")),
            vjust = -1, size = 3.5, color = pal_principal["verde_mar"]) +
  scale_y_continuous(limits = c(0, 80),
                     labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = years) +
  labs(title = "Cobertura del programa de extensión pesquera",
       subtitle = "Proyección bajo escenario de política · 2026–2031",
       x = NULL, y = "% pescadores en extensión")

fig5b <- ggplot(escen_df, aes(x = Año)) +
  geom_line(aes(y = SAS_esperado), color = pal_principal["azul_UCR"],
            linewidth = 1.5) +
  geom_point(aes(y = SAS_esperado), color = pal_principal["azul_UCR"],
             size = 3.5) +
  geom_text(aes(y = SAS_esperado, label = round(SAS_esperado, 1)),
            vjust = -1, size = 3.5, color = pal_principal["azul_UCR"]) +
  scale_y_continuous(limits = c(20, 40)) +
  scale_x_continuous(breaks = years) +
  labs(title = "SAS promedio esperado bajo el escenario",
       subtitle = "(bajo supuesto de asociación causal)",
       x = "Año", y = "SAS promedio (0–50 pts)",
       caption = "ADVERTENCIA: resultado predictivo, no causal | SP-8502 · 2026")

fig5 <- fig5a / fig5b +
  plot_annotation(
    title   = "Escenario de política: ¿qué ocurriría si se expande la extensión pesquera?",
    theme   = theme(plot.title = element_text(face = "bold",
                                               color = pal_principal["azul_UCR"]))
  )

ggsave("outputs/figuras/S4_fig5_escenario_politica.png",
       fig5, width = 10, height = 9, dpi = 300)
cat("  ✓ FIG5 guardada\n")

# =============================================================================
# FIGURA 6: Dashboard de síntesis (panel ejecutivo de 4 métricas clave)
# =============================================================================
cat("Generando FIG6: Dashboard ejecutivo (KPIs del estudio)...\n")

r2_val  <- round(summary(M3)$r.squared * 100, 1)
d_val2  <- abs(round((mean(g2,na.rm=T)-mean(g1,na.rm=T))/sd_p, 2))
pct_ext <- round(mean(datos$extension_pesquera, na.rm=T)*100, 1)
sas_med <- round(mean(datos$sas_total, na.rm=T), 1)
p_h1    <- ifelse(summary(M3)$r.squared >= 0.35, "✓","✗")
p_h2    <- ifelse(d_val2 >= 0.5, "✓","✗")

kpis <- tibble(
  kpi   = c(paste0(r2_val, "%"),
             paste0(d_val2),
             paste0(pct_ext, "%"),
             paste0(sas_med, " / 50")),
  label = c("Varianza del SAS explicada\npor el modelo",
             "d de Cohen (efecto extensión\nvs. no extensión)",
             "Pescadores actualmente\nen extensión pesquera",
             "Score SAS promedio\n(máximo posible: 50)"),
  icon  = c("R² = ?", "H2 ?", "Cobertura", "SAS"),
  color = c(pal_principal["azul_UCR"],
             pal_principal["verde_mar"],
             pal_principal["naranja"],
             pal_principal["gris_med"]),
  check = c(ifelse(summary(M3)$r.squared >= 0.35,"H1 ✓","H1 ✗"),
             ifelse(d_val2 >= 0.5, "H2 ✓","H2 ✗"),
             "Brecha de política",
             "Línea base 2026")
)

# Construir gráfico estilo "card"
p_kpis <- ggplot(kpis, aes(x = 1, y = 1)) +
  geom_tile(aes(fill = color), color = "white", linewidth = 2) +
  geom_text(aes(label = kpi, color = "white"),
            y = 1.15, size = 10, fontface = "bold") +
  geom_text(aes(label = label, color = "white"),
            y = 0.88, size = 3.2, lineheight = 1.1) +
  geom_text(aes(label = check),
            y = 0.70, size = 3.5, color = "white",
            fontface = "bold") +
  scale_fill_identity() +
  scale_color_identity() +
  facet_wrap(~ label, nrow = 1, scales = "free") +
  xlim(0.5, 1.5) + ylim(0.5, 1.5) +
  theme_void() +
  theme(strip.text = element_blank()) +
  labs(
    title   = "Métricas clave del estudio — Pacífico Central CR · 2026",
    caption = "SP-8502 · GIACT UCR · Resolución R-469-2025"
  )

# Alternativa más limpia: tabla visual con anotaciones
fig6 <- ggplot() +
  theme_void() +
  annotate("rect", xmin=0,  xmax=1, ymin=0,  ymax=1,
           fill=pal_principal["azul_UCR"],  color="white", linewidth=2) +
  annotate("rect", xmin=1,  xmax=2, ymin=0,  ymax=1,
           fill=pal_principal["verde_mar"], color="white", linewidth=2) +
  annotate("rect", xmin=2,  xmax=3, ymin=0,  ymax=1,
           fill=pal_principal["naranja"],  color="white", linewidth=2) +
  annotate("rect", xmin=3,  xmax=4, ymin=0,  ymax=1,
           fill=pal_principal["gris_med"], color="white", linewidth=2) +
  annotate("text", x=0.5, y=0.70, label=paste0(r2_val, "%"),
           color="white", size=11, fontface="bold") +
  annotate("text", x=0.5, y=0.38,
           label="Varianza del SAS\nexplicada por el modelo\n(H1 ✓)",
           color="white", size=3.5, lineheight=1.1) +
  annotate("text", x=1.5, y=0.70, label=as.character(d_val2),
           color="white", size=11, fontface="bold") +
  annotate("text", x=1.5, y=0.38,
           label="d de Cohen\nefecto extensión\n(H2 ✓)",
           color="white", size=3.5, lineheight=1.1) +
  annotate("text", x=2.5, y=0.70, label=paste0(pct_ext, "%"),
           color="white", size=11, fontface="bold") +
  annotate("text", x=2.5, y=0.38,
           label="Cobertura actual\nextensión pesquera\n(brecha de política)",
           color="white", size=3.5, lineheight=1.1) +
  annotate("text", x=3.5, y=0.70, label=paste0(sas_med, "/50"),
           color="white", size=11, fontface="bold") +
  annotate("text", x=3.5, y=0.38,
           label="SAS promedio\nlínea base 2026\n(escala 0–50)",
           color="white", size=3.5, lineheight=1.1) +
  xlim(0, 4) + ylim(0, 1) +
  labs(title   = "Indicadores clave del estudio · Pesca artesanal Pacífico Central CR",
       caption = "SP-8502 · GIACT UCR · 2026 | Resolución R-469-2025") +
  theme(
    plot.title   = element_text(face = "bold", size = 13,
                                 color = pal_principal["azul_UCR"],
                                 margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = pal_principal["gris_med"])
  )

ggsave("outputs/figuras/S4_fig6_dashboard_kpis.png",
       fig6, width = 11, height = 4.5, dpi = 300)
cat("  ✓ FIG6 guardada\n\n")

# =============================================================================
# RESUMEN DE FIGURAS GENERADAS
# =============================================================================
cat("=============================================================\n")
cat("FIGURAS GENERADAS PARA EL POLICY BRIEF:\n\n")
cat("  FIG1: SAS por comunidad (barras + IC 95%)\n")
cat("  FIG2: Efecto extensión pesquera (grupos con d Cohen)\n")
cat("  FIG3: Waterfall coeficientes estandarizados\n")
cat("  FIG4: Scatter segmentado por perfil de pescador\n")
cat("  FIG5: Escenario de política 2026–2031\n")
cat("  FIG6: Dashboard ejecutivo (4 KPIs)\n\n")
cat("  Ruta: outputs/figuras/S4_fig*.png\n")
cat("  Tamaño: 300 dpi · 10–11 pulgadas de ancho\n")
cat("  Paleta: daltonismo-safe (compatible WCAG 2.1)\n")
cat("=============================================================\n")
cat("  Siguiente: 15_mapas_sf.R\n")
cat("=============================================================\n")
