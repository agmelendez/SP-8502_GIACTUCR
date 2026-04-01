# SP-8502 · Sprint 4 — Guía Paso a Paso

**Curso:** Métodos Cuanti-Cuali con IA Responsable  
**Programa:** Maestría GIACT · Universidad de Costa Rica · 2026-I  
**Sprint:** 4 — Comunicación y Toma de Decisiones (Semanas 13–15, 20%)  
**Docente:** MSI. Agustín Gómez Meléndez · CIOdD · UCR

---

## ¿Qué produce este Sprint?

| Archivo a entregar | Generado por | Peso |
|---|---|---|
| `policy_brief.pdf` | `policy_brief.Rmd` | Principal |
| `presentacion_ejecutiva.pdf` | `presentacion_ejecutiva.Rmd` | Principal |
| `visualizaciones/` (carpeta con figuras) | Scripts 14–16 | Complementario |
| `bitacora_ia_s4.xlsx` | Manual (plantilla) | Obligatorio |

---

## Prerrequisitos: Sprints 1–3 Completados

El Sprint 4 requiere los siguientes archivos generados en sprints anteriores:

```
datos/procesados/
├── datos_imputados_s3.rds         ← Script 09 (Sprint 3)
├── modelos_finales_s3.rds         ← script_analisis.R (Sprint 3)
├── coeficientes_M3_final.csv      ← script_analisis.R (Sprint 3)
├── resultados_hipotesis_s3.csv    ← script_analisis.R (Sprint 3)
├── temas_cualitativos_s3.csv      ← Script 12 (Sprint 3)
├── bootstrap_resumen_s3.csv       ← Script 13 (Sprint 3)
└── montecarlo_escenario_s3.csv    ← Script 13 (Sprint 3)
```

Si no los tiene, ejecute primero `script_analisis.R` del Sprint 3.

---

## Secuencia de Ejecución — Paso a Paso

### PASO 0 — Instalar paquetes nuevos del Sprint 4

```r
install.packages(c("sf","viridis","ggtext","gt","scales"))
# Para presentación Beamer (requiere LaTeX):
install.packages("tinytex")
tinytex::install_tinytex()
```

---

### PASO 1 — Visualizaciones ggplot2 (Script 14)

```r
source("14_visualizacion_ggplot2.R")
```

**Genera 6 figuras para el Policy Brief:**

| Figura | Archivo | Mensaje central |
|--------|---------|-----------------|
| FIG1 | `S4_fig1_sas_comunidad.png` | "Quepos lidera la adopción sostenible" |
| FIG2 | `S4_fig2_efecto_extension.png` | "La extensión aumenta el SAS (d = ?" |
| FIG3 | `S4_fig3_waterfall_coef.png` | "Importancia relativa de predictores" |
| FIG4 | `S4_fig4_perfiles_pescador.png` | "4 tipos de pescador artesanal" |
| FIG5 | `S4_fig5_escenario_politica.png` | "¿Qué pasa si expandimos el programa?" |
| FIG6 | `S4_fig6_dashboard_kpis.png` | "4 indicadores clave del estudio" |

**Principios aplicados (Schwabish 2021 / Tufte 2001):**
```
✓ Una idea por gráfico (no sobrecargar)
✓ Título = el hallazgo (no "Gráfico de barras de SAS por comunidad")
✓ Anotaciones directas en el gráfico
✓ Paleta daltonismo-safe (viridis / ColorBrewer)
✓ 300 dpi para impresión
```

**Secuencia pedagógica en el script:**
```
CÁLCULO         → geom_col(), geom_errorbarh(), scale_*
INTERPRETACIÓN  → ¿qué muestra la figura? ¿qué NO muestra?
CONTEXTO        → ¿qué significa para la gestión costera?
DECISIÓN        → ¿por qué este tipo de gráfico y no otro?
```

---

### PASO 2 — Mapas espaciales con sf (Script 15)

```r
source("15_mapas_sf.R")
```

**Genera 4 mapas del Pacífico Central CR:**

| Mapa | Archivo | Contenido |
|------|---------|-----------|
| MAP1 | `S4_map1_sas_puntos.png` | Puntos de encuesta coloreados por SAS |
| MAP2+3 | `S4_map2_3_cloropleta.png` | Cloropleta SAS + cobertura extensión |
| MAP4 | `S4_map4_burbuja_anp_sas.png` | Relación espacial ANPs–SAS–cobertura |

**Conceptos clave de sf que demostrar en la Defensa:**
```r
# Crear objeto sf desde coordenadas
datos_sf <- st_as_sf(datos, coords = c("lon","lat"), crs = 4326)

# Buffer alrededor de puntos
buffer <- st_buffer(puntos_sf, dist = 0.08)  # ~8 km

# Mapa básico con ggplot2
ggplot() + geom_sf(data = datos_sf, aes(color = sas_total))
```

**Advertencia metodológica documentada en el script:**
> Los mapas son exploratorios. Para análisis geoestadístico formal (kriging, modelos de autocorrelación espacial), se requieren datos con precisión catastral y SIG oficial de SINAC/MINAE.

---

### PASO 3 — Joint Display (Script 16)

```r
source("16_joint_display.R")
```

**¿Qué es el Joint Display?**  
Es la tabla donde los resultados cuantitativos y cualitativos se presentan **simultáneamente** para demostrar convergencia, divergencia o expansión. Es el artefacto central del diseño mixto.

**Genera:**
- `datos/procesados/joint_display_s4.csv` → tabla maestra de integración
- `outputs/figuras/S4_convergencia_jointdisplay.png` → figura de convergencia
- Narrativa de Weaving en consola → base del texto para el Policy Brief

**Los 3 tipos de resultado en el Joint Display:**

| Tipo | Qué significa | Ejemplo en este estudio |
|------|--------------|------------------------|
| Convergencia | QUAN y QUAL dicen lo mismo | Efecto extensión: β > 0 + "entendí para qué era" |
| Divergencia | Contradicción que requiere explicación | — (no encontrada en este estudio) |
| Gap analítico | QUAL captura algo que QUAL no pudo | Gobernanza: saturado en entrevistas, ausente en M3 |

---

### PASO 4 — Generar el Policy Brief (ENTREGABLE)

```r
rmarkdown::render(
  "policy_brief.Rmd",
  output_file   = "outputs/reportes/policy_brief.pdf",
  output_format = "pdf_document"
)
```

**Estructura del Policy Brief (máximo 4 páginas):**

```
┌─────────────────────────────────────────┐
│  ENCABEZADO + 4 KPIs en dashboard       │
├─────────────────────────────────────────┤
│  EL PROBLEMA (½ página)                 │
│  → La paradoja del Pacífico Central     │
├─────────────────────────────────────────┤
│  HALLAZGOS PRINCIPALES (1.5 páginas)    │
│  → FIG efecto extensión                 │
│  → FIG SAS por comunidad                │
│  → FIG waterfall coeficientes           │
├─────────────────────────────────────────┤
│  RECOMENDACIONES (1 página)             │
│  → Tabla 4 recomendaciones con actor    │
│  → Priorización geográfica              │
├─────────────────────────────────────────┤
│  INTEGRACIÓN MIXTA (½ página)           │
│  → Qué añade lo cualitativo             │
│  → Gap de gobernanza como hallazgo      │
├─────────────────────────────────────────┤
│  LIMITACIONES + CITA SUGERIDA          │
└─────────────────────────────────────────┘
```

---

### PASO 5 — Generar la Presentación Ejecutiva (ENTREGABLE)

```r
# Versión Beamer (PDF de diapositivas)
rmarkdown::render(
  "presentacion_ejecutiva.Rmd",
  output_file   = "outputs/reportes/presentacion_ejecutiva.pdf",
  output_format = "beamer_presentation"
)

# Versión HTML (más fácil sin LaTeX)
rmarkdown::render(
  "presentacion_ejecutiva.Rmd",
  output_file   = "outputs/reportes/presentacion_ejecutiva.html",
  output_format = "ioslides_presentation"
)
```

**Estructura de la presentación (12 diapositivas):**
```
1. Título
2. Contexto y paradoja (gráfico SAS por comunidad)
3. Diseño del estudio (tabla QUAN/QUAL)
4. Modelo OLS: R² y forest plot
5. Hallazgo central: efecto extensión
6. Hipótesis H1–H3 (tabla resumen)
7. 5 temas cualitativos
8. Joint Display: convergencia
9–10. Recomendaciones (3 bloques)
11. Priorización geográfica
12. Conclusiones + créditos
```

---

## Mapa Conceptual del Sprint 4

```
 SPRINTS 1–3 (resultados ya disponibles)
 ═══════════════════════════════════════
     ↓              ↓              ↓
  M3 OLS       5 Temas Q      Bootstrap
  β, R², H1-H3  Weaving        IC robustos

 SPRINT 4: COMUNICACIÓN
 ══════════════════════

 SEM 13                 SEM 14
 ─────────────────      ─────────────────
 14_visualizacion.R     15_mapas_sf.R
 • 6 figuras policy     • 4 mapas costeros
 • Storytelling         • sf + geom_sf()
 • Schwabish/Tufte      • Priorización geo.
       ↓                       ↓
 FIG1–FIG6              MAP1–MAP4

 SEM 15: INTEGRACIÓN Y ENTREGABLES
 ──────────────────────────────────
 16_joint_display.R
 • 5 dimensiones QUAN+QUAL
 • Figura de convergencia
 • Weaving narrativo
       ↓
 policy_brief.Rmd  →  policy_brief.pdf    ← ENTREGABLE
 presentacion.Rmd  →  presentacion.pdf   ← ENTREGABLE
 visualizaciones/  →  carpeta de figuras ← ENTREGABLE
 bitacora_ia_s4    →  manual             ← ENTREGABLE
```

---

## Estructura de Carpetas Sprint 4

```
sprint4/
├── 14_visualizacion_ggplot2.R
├── 15_mapas_sf.R
├── 16_joint_display.R
├── policy_brief.Rmd               ← ENTREGABLE
├── presentacion_ejecutiva.Rmd     ← ENTREGABLE
├── README.md
│
├── datos/
│   └── procesados/
│       ├── joint_display_s4.csv
│       ├── tabla_espacial_s4.csv
│       ├── reflexividad_mixta_s4.csv
│       └── weaving_narrativo_s4.txt
│
├── outputs/
│   ├── figuras/
│   │   ├── S4_fig1_sas_comunidad.png
│   │   ├── S4_fig2_efecto_extension.png
│   │   ├── S4_fig3_waterfall_coef.png
│   │   ├── S4_fig4_perfiles_pescador.png
│   │   ├── S4_fig5_escenario_politica.png
│   │   ├── S4_fig6_dashboard_kpis.png
│   │   ├── S4_map1_sas_puntos.png
│   │   ├── S4_map2_3_cloropleta.png
│   │   ├── S4_map4_burbuja_anp_sas.png
│   │   └── S4_convergencia_jointdisplay.png
│   └── reportes/
│       ├── policy_brief.pdf        ← ENTREGABLE
│       └── presentacion_ejecutiva.pdf ← ENTREGABLE
│
└── bitacoras/
    └── bitacora_ia_s4.xlsx         ← ENTREGABLE
```

---

## Checklist de Verificación — Sprint 4

### Policy Brief
- [ ] Máximo 4 páginas (sin contar referencias)
- [ ] El TÍTULO de cada figura es el HALLAZGO, no la variable
- [ ] Tabla de recomendaciones incluye actor, plazo y fundamento
- [ ] La sección de integración mixta menciona convergencias Y el gap de gobernanza
- [ ] Figuras a 300 dpi (no pixeladas al imprimir)
- [ ] Declaración de uso de IA al pie

### Presentación Ejecutiva
- [ ] Máximo 12–15 diapositivas
- [ ] Cada diapositiva tiene UNA idea principal
- [ ] Los resultados de H1, H2, H3 están explícitos
- [ ] El Joint Display está explicado visualmente
- [ ] Las recomendaciones tienen actor y plazo

### Visualizaciones
- [ ] Paleta daltonismo-safe (verificar con Coblis o Sim Daltonism)
- [ ] Eje Y del gráfico de barras comienza en 0
- [ ] Los IC 95% están en todos los gráficos de comparación de grupos
- [ ] Todos los gráficos tienen caption con fuente y año

### Bitácora IA Sprint 4
- [ ] ≥ 4 filas documentadas
- [ ] Incluye interacciones de REDACCIÓN del Policy Brief
- [ ] Incluye interacciones de DISEÑO de figuras
- [ ] Cada fila tiene: prompt completo + respuesta IA + decisión humana + verificación

---

## Principios de Storytelling con Datos (para la Defensa)

El estudiante debe poder explicar en R en vivo (Semana 16):

1. Por qué el título de una figura debe ser el hallazgo, no la variable
2. Cómo usar `fct_reorder()` para ordenar categorías por magnitud
3. Por qué incluir IC 95% en gráficos de comparación de grupos
4. Qué es el Joint Display y cómo demuestra la integración del diseño mixto
5. Cómo `geom_sf()` permite mapear datos espaciales con ggplot2
6. Por qué los mapas son "exploratorios" y no "catastrales"

---

## La Conexión con la Defensa Final (Semana 16)

El Sprint 4 prepara directamente la Defensa Final:

| Componente Sprint 4 | Uso en Defensa |
|---|---|
| `policy_brief.pdf` | Resumen ejecutivo de una página |
| `presentacion_ejecutiva.pdf` | Presentación de 12–15 min |
| FIG2 (efecto extensión) | Demo en vivo: reproducir con datos reales |
| Joint Display | Demostrar comprensión del diseño mixto |
| Mapas sf | Demostrar que puede replicar análisis espacial |

**En la Defensa, sin IA en tiempo real, el estudiante debe:**
- Reproducir al menos una figura clave desde cero
- Explicar por qué se eligió `log(ingreso)` como transformación
- Describir los 5 temas cualitativos y su relación con el modelo OLS
- Justificar las 4 recomendaciones de política con evidencia específica

---

## Acumulado del Curso: S1 + S2 + S3 + S4

| Sprint | Archivos | Líneas | Entregables |
|--------|----------|--------|------------|
| Sprint 1 | 7 | 2,134 | script_limpieza.R · reporte_diseño.pdf |
| Sprint 2 | 6 | 2,291 | base_datos.csv · estrategia_muestreo.pdf |
| Sprint 3 | 8 | 2,877 | script_analisis.R · informe_hallazgos.pdf |
| Sprint 4 | 7 | ~2,600 | policy_brief.pdf · presentacion.pdf |
| **TOTAL** | **28** | **~9,900** | **12 entregables principales** |

---

*SP-8502 · GIACT · UCR · 2026-I · Sprint 4 · v1.0*  
*Resolución R-469-2025 · Integridad académica, IA responsable y reproducibilidad*
