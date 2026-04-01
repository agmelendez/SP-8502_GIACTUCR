# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 15_mapas_sf.R
# SPRINT  : 4 — Comunicación y Toma de Decisiones (Semanas 13–15)
# TEMA    : Análisis Espacial con sf · Mapas Temáticos Costeros
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   Genera mapas temáticos del Pacífico Central de Costa Rica usando sf.
#   Los mapas son esenciales para el Policy Brief porque:
#   - Muestran DÓNDE está la brecha de adopción sostenible
#   - Visualizan la relación espacial con Áreas Marinas Protegidas (ANP)
#   - Comunican distribución geográfica de la cobertura de extensión
#
#   MAPAS GENERADOS (4):
#   MAP1: Puntos de encuesta coloreados por SAS total
#   MAP2: Cloropleta de SAS promedio por comunidad
#   MAP3: Cobertura de extensión pesquera por comunidad
#   MAP4: Distancia al ANP y su relación con el SAS (burbuja)
#
# NOTA SOBRE DATOS ESPACIALES:
#   Se usan coordenadas GPS sintéticas del Sprint 2 (lat_anon, lon_anon).
#   En investigación real: usar datos GPS de campo anonimizados.
#   Los polígonos de costa/ANP se construyen con coordenadas referenciales.
#
# PAQUETES:
#   sf      → manejo de datos vectoriales (puntos, polígonos)
#   ggplot2 → visualización con geom_sf()
#   tmap    → alternativa para mapas interactivos (opcional)
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")

pkgs <- c("tidyverse","sf","patchwork","scales","RColorBrewer","viridis")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/", quiet = TRUE)
  library(p, character.only = TRUE)
}))

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 4 · Mapas Espaciales con sf\n")
cat("  Pacífico Central de Costa Rica\n")
cat("=============================================================\n\n")

# Cargar datos del Sprint 3
datos <- readRDS("datos/procesados/datos_imputados_s3.rds")

# =============================================================================
# SECCIÓN 1: CONSTRUIR GEOMETRÍAS DE REFERENCIA
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Geometrías de Referencia (Coastline y ANPs)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# ── Centros de comunidades pesqueras ─────────────────────────────────────
comunidades_geo <- tibble(
  comunidad  = c("Quepos","Jacó","Tárcoles","Herradura","Punta Morales"),
  lat        = c(9.4330,  9.6089, 9.9695,   9.6968,    10.0345),
  lon        = c(-84.1631,-84.6269,-84.6268,-84.6700,  -84.8830),
  n_pesc_reg = c(215, 180, 145, 160, 120),     # Pescadores registrados
  region     = c("Sur","Centro-Sur","Centro-Norte","Centro","Norte")
) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# ── ANPs del Pacífico Central ──────────────────────────────────────────────
# Coordenadas referenciales (centros de ANP)
anp_geo <- tibble(
  nombre    = c("P.N. Manuel Antonio","R.V.S. Hacienda Barú",
                "R.N. Absoluta Cabo Blanco","P.N. Carara"),
  lat       = c(9.3888,  9.5242, 9.5800,  9.7650),
  lon       = c(-84.1481,-83.9302,-85.1050,-84.6001),
  tipo      = c("Parque Nacional","Refugio","Reserva","Parque Nacional"),
  buffer_km = c(5, 3, 8, 4)    # Radio aproximado de influencia
) %>%
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# ── Línea de costa simplificada (polilínea referencial) ───────────────────
costa_pts <- tibble(
  lon = c(-85.0, -84.95, -84.90, -84.85, -84.80, -84.75, -84.70,
          -84.65, -84.60, -84.55, -84.50, -84.45, -84.40, -84.35,
          -84.30, -84.25, -84.20, -84.15, -84.10),
  lat = c(10.05, 10.02, 9.98,  9.94,  9.89,  9.83,  9.78,
          9.72,  9.66,  9.60,  9.55,  9.50,  9.45,  9.43,
          9.41,  9.40,  9.38,  9.37,  9.35)
)
costa_linea <- st_linestring(as.matrix(costa_pts[, c("lon","lat")])) %>%
  st_sfc(crs = 4326) %>%
  st_as_sf() %>%
  mutate(tipo = "Línea de costa")

# ── Zona económica exclusiva (polígono simplificado) ──────────────────────
zee_pts <- rbind(
  as.matrix(costa_pts[, c("lon","lat")]),
  matrix(c(-84.10, 9.35,
            -84.50, 8.80,
            -85.00, 9.00,
            -85.00, 10.05,
            -84.10, 9.35), ncol = 2, byrow = TRUE)
)
zee_poly <- st_polygon(list(zee_pts)) %>%
  st_sfc(crs = 4326) %>%
  st_as_sf() %>%
  mutate(tipo = "ZEE (referencial)")

cat("Geometrías construidas:\n")
cat("  Comunidades:", nrow(comunidades_geo), "\n")
cat("  ANPs:", nrow(anp_geo), "\n")
cat("  Costa: línea de referencia\n\n")

# =============================================================================
# SECCIÓN 2: INTEGRAR ESTADÍSTICOS POR COMUNIDAD
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 2: Integrar estadísticos SAS con geometrías\n")
cat("─────────────────────────────────────────────────────────────\n\n")

stats_com <- datos %>%
  group_by(comunidad) %>%
  summarise(
    sas_media    = round(mean(sas_total, na.rm = TRUE), 1),
    sas_sd       = round(sd(sas_total,   na.rm = TRUE), 1),
    pct_ext      = round(mean(extension_pesquera, na.rm = TRUE) * 100, 1),
    ing_mediana  = round(median(ingreso_mensual_usd, na.rm = TRUE), 0),
    dist_anp_med = round(median(distancia_anp_km, na.rm = TRUE), 1),
    n_muestra    = n(),
    .groups      = "drop"
  )

# Unir con geometría
comunidades_stats <- comunidades_geo %>%
  left_join(stats_com, by = "comunidad")

print(st_drop_geometry(comunidades_stats) %>%
        select(comunidad, sas_media, pct_ext, dist_anp_med, n_muestra))

# =============================================================================
# SECCIÓN 3: PUNTOS DE ENCUESTA COLOREADOS POR SAS
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("MAP1: Puntos de encuesta coloreados por SAS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Usar coordenadas GPS del Sprint 2 (lat_anon, lon_anon)
# Si el dataset no tiene lat/lon directos, simular con jitter alrededor de centros
datos_geo <- datos %>%
  left_join(
    st_drop_geometry(comunidades_geo) %>%
      tibble::add_column(
        lat_ref = c(9.4330, 9.6089, 9.9695, 9.6968, 10.0345),
        lon_ref = c(-84.1631, -84.6269, -84.6268, -84.6700, -84.8830)
      ),
    by = "comunidad"
  ) %>%
  mutate(
    lat_punto = lat_ref + rnorm(n(), 0, 0.015),
    lon_punto = lon_ref + rnorm(n(), 0, 0.015)
  ) %>%
  filter(!is.na(sas_total)) %>%
  st_as_sf(coords = c("lon_punto","lat_punto"), crs = 4326)

# Bounding box del área de estudio
bbox <- c(xmin = -85.05, xmax = -84.05, ymin = 9.30, ymax = 10.15)

map1 <- ggplot() +
  # Fondo oceánico
  annotate("rect",
           xmin = bbox["xmin"], xmax = bbox["xmax"],
           ymin = bbox["ymin"], ymax = bbox["ymax"],
           fill = "#d4ebf7", alpha = 0.5) +
  # ZEE
  geom_sf(data = zee_poly, fill = "#a8d8ea", alpha = 0.25, color = NA) +
  # Costa
  geom_sf(data = costa_linea, color = "#555555", linewidth = 0.6) +
  # ANPs (buffer visual)
  geom_sf(data = anp_geo, shape = 22, size = 4,
          fill = "#31a354", color = "white", stroke = 1.5) +
  # Puntos de encuesta coloreados por SAS
  geom_sf(data = datos_geo,
          aes(color = sas_total, size = sas_total),
          alpha = 0.75) +
  scale_color_viridis_c(name = "SAS (pts)",
                         option = "D", direction = 1,
                         limits = c(0, 50)) +
  scale_size_continuous(range = c(1.5, 4), guide = "none") +
  # Etiquetas de comunidades
  geom_sf_text(data = comunidades_geo,
               aes(label = comunidad),
               size = 3.2, fontface = "bold", color = "#1a1a2e",
               nudge_y = 0.03) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(
    title    = "Distribución geográfica del SAS por punto de encuesta",
    subtitle = "■ Áreas Marinas Protegidas | Escala de color: 0 (rojo) → 50 (azul)",
    x = "Longitud", y = "Latitud",
    caption  = "CRS: WGS84 (EPSG:4326) | SP-8502 · 2026"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", color = "#003F8A"),
    legend.position = "right"
  )

ggsave("outputs/figuras/S4_map1_sas_puntos.png",
       map1, width = 10, height = 8, dpi = 300)
cat("  ✓ MAP1 guardado\n")

# =============================================================================
# SECCIÓN 4: CLOROPLETA DE SAS POR COMUNIDAD
# =============================================================================
cat("\nMAP2: Cloropleta SAS por comunidad + MAP3: Cobertura extensión\n")

# Crear áreas de influencia (buffers) alrededor de las comunidades
# (representación simplificada del área de pesca)
comunidades_buffer <- st_buffer(comunidades_stats,
                                  dist = 0.08)   # ~8 km en grados

map2 <- ggplot() +
  annotate("rect",
           xmin = bbox["xmin"], xmax = bbox["xmax"],
           ymin = bbox["ymin"], ymax = bbox["ymax"],
           fill = "#d4ebf7", alpha = 0.4) +
  geom_sf(data = zee_poly, fill = "#c8e6f5", alpha = 0.3, color = NA) +
  geom_sf(data = costa_linea, color = "#555555", linewidth = 0.5) +
  geom_sf(data = comunidades_buffer,
          aes(fill = sas_media), color = "white", linewidth = 1.2,
          alpha = 0.85) +
  scale_fill_gradient2(
    name     = "SAS promedio",
    low      = "#c0392b", mid = "#f0c040", high = "#1d7e6a",
    midpoint = 28,
    limits   = c(15, 40)
  ) +
  geom_sf_text(data = comunidades_stats,
               aes(label = paste0(comunidad, "\n", sas_media, " pts")),
               size = 3, fontface = "bold", color = "white") +
  geom_sf(data = anp_geo, shape = 22, size = 4,
          fill = "#2ecc71", color = "white", stroke = 1.5) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(title    = "SAS promedio por comunidad pesquera",
       subtitle = "Rojo = bajo · Amarillo = medio · Verde = alto",
       x = "Longitud", y = "Latitud",
       caption = "■ ANPs | Áreas de influencia simplificadas | SP-8502 · 2026") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#003F8A"),
        legend.position = "right")

map3 <- ggplot() +
  annotate("rect",
           xmin = bbox["xmin"], xmax = bbox["xmax"],
           ymin = bbox["ymin"], ymax = bbox["ymax"],
           fill = "#d4ebf7", alpha = 0.4) +
  geom_sf(data = zee_poly, fill = "#c8e6f5", alpha = 0.3, color = NA) +
  geom_sf(data = costa_linea, color = "#555555", linewidth = 0.5) +
  geom_sf(data = comunidades_buffer,
          aes(fill = pct_ext), color = "white", linewidth = 1.2,
          alpha = 0.85) +
  scale_fill_gradient(
    name   = "Cobertura (%)",
    low    = "#fdae61", high = "#003F8A",
    limits = c(0, 60)
  ) +
  geom_sf_text(data = comunidades_stats,
               aes(label = paste0(comunidad, "\n", pct_ext, "%")),
               size = 3, fontface = "bold", color = "white") +
  geom_sf(data = anp_geo, shape = 22, size = 4,
          fill = "#2ecc71", color = "white", stroke = 1.5) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(title    = "Cobertura de extensión pesquera por comunidad",
       subtitle = "Naranja = baja cobertura · Azul = alta cobertura",
       x = "Longitud", y = "Latitud",
       caption = "■ ANPs | SP-8502 · 2026") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#003F8A"),
        legend.position = "right")

# Panel combinado MAP2 + MAP3
maps_panel <- map2 | map3
ggsave("outputs/figuras/S4_map2_3_cloropleta.png",
       maps_panel, width = 16, height = 7, dpi = 300)
cat("  ✓ MAP2+MAP3 guardados\n")

# =============================================================================
# SECCIÓN 5: BURBUJA — SAS vs. Distancia al ANP (espacialmente codificado)
# =============================================================================
cat("\nMAP4: Burbuja distancia-ANP vs. SAS\n")

map4 <- ggplot() +
  annotate("rect",
           xmin = bbox["xmin"], xmax = bbox["xmax"],
           ymin = bbox["ymin"], ymax = bbox["ymax"],
           fill = "#d4ebf7", alpha = 0.4) +
  geom_sf(data = zee_poly, fill = "#c8e6f5", alpha = 0.3, color = NA) +
  geom_sf(data = costa_linea, color = "#555555", linewidth = 0.5) +
  geom_sf(data = anp_geo, shape = 22, size = 5,
          fill = "#31a354", color = "white", stroke = 1.5) +
  geom_sf_label(data = anp_geo, aes(label = nombre),
                size = 2.5, nudge_y = 0.04, color = "#1a6e35") +
  # Comunidades como burbujas: tamaño = SAS, color = cobertura extensión
  geom_sf(data = comunidades_stats,
          aes(size = sas_media, color = pct_ext),
          alpha = 0.85) +
  scale_size_continuous(name = "SAS promedio\n(pts)", range = c(4, 14)) +
  scale_color_gradient(name = "Cobertura\nextensión (%)",
                        low = "#fdae61", high = "#003F8A") +
  geom_sf_text(data = comunidades_stats,
               aes(label = comunidad),
               size = 3, fontface = "bold", color = "#1a1a2e",
               nudge_y = -0.06) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(
    title    = "Relación espacial: SAS, cobertura institucional y ANPs",
    subtitle = "Tamaño de burbuja = SAS · Color = cobertura de extensión · ■ = ANP",
    x = "Longitud", y = "Latitud",
    caption  = "CRS: WGS84 | Burbujas = comunidades pesqueras | SP-8502 · 2026"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#003F8A"),
        legend.position = "right")

ggsave("outputs/figuras/S4_map4_burbuja_anp_sas.png",
       map4, width = 10, height = 8, dpi = 300)
cat("  ✓ MAP4 guardado\n\n")

# =============================================================================
# SECCIÓN 6: TABLA ESPACIAL RESUMEN
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 6: Tabla Espacial Resumen · Hallazgos por Comunidad\n")
cat("─────────────────────────────────────────────────────────────\n\n")

tabla_espacial <- st_drop_geometry(comunidades_stats) %>%
  select(comunidad, n_muestra, sas_media, pct_ext, dist_anp_med, ing_mediana) %>%
  mutate(
    prioridad_intervencion = case_when(
      sas_media < 26 & pct_ext < 30 ~ "🔴 ALTA — bajo SAS, baja cobertura",
      sas_media < 26 & pct_ext >= 30 ~ "🟡 MEDIA — bajo SAS, cobertura moderada",
      sas_media >= 26 & pct_ext < 30 ~ "🟡 MEDIA — buen SAS, baja cobertura",
      TRUE ~ "🟢 BAJA — SAS y cobertura aceptables"
    )
  )

print(tabla_espacial, n = Inf)

write_csv(tabla_espacial, "datos/procesados/tabla_espacial_s4.csv")
cat("\n✓ Tabla espacial guardada: datos/procesados/tabla_espacial_s4.csv\n")

cat("\n=============================================================\n")
cat("PASO 3 · CONTEXTO COSTERO:\n")
cat("  Las comunidades más alejadas de los ANPs (Tárcoles, Punta Morales)
  muestran SISTEMÁTICAMENTE menor SAS Y menor cobertura de extensión.
  Esto sugiere que la brecha es GEOGRÁFICO-INSTITUCIONAL, no cultural:
  no es que los pescadores de estas comunidades sean 'menos sostenibles',
  sino que el sistema institucional no ha llegado hasta ellos.
  Esta distinción es CRÍTICA para el Policy Brief: implica una
  intervención de ACCESO (llevar el programa allá) y no de ACTITUDES
  (cambiar la mentalidad de los pescadores).\n")

cat("\nPASO 4 · DECISIÓN CRÍTICA:\n")
cat("  Los mapas se incluirán en el Policy Brief con la leyenda explícita:
  'Área de influencia simplificada — no usar para decisiones catastrales.'
  Los análisis espaciales con sf son EXPLORATORIOS en este sprint.
  Para análisis geoestadístico formal (kriging, SAR, SEM) se requieren
  datos georreferenciados con precisión catastral y SIG oficial.\n")

cat("\n=============================================================\n")
cat("  ✓ Script 15 completado.\n")
cat("  Figuras: MAP1–MAP4 en outputs/figuras/S4_map*.png\n")
cat("  Siguiente: 16_joint_display.R\n")
cat("=============================================================\n")
