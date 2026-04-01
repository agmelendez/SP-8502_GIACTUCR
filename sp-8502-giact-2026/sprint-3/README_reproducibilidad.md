# README de Reproducibilidad — Sprint 3

**Archivo:** `README_reproducibilidad.md`  
**Curso:** SP-8502 · Métodos Cuanti-Cuali con IA Responsable  
**Sprint:** 3 — Análisis Profundo y Modelado (Semanas 9–12, 25%)  
**Autor:** [Nombre] · Carné: [XXXXXXX]  
**Fecha:** [DD/MM/YYYY]  
**Marco:** Resolución R-469-2025 · UCR · Principio de reproducibilidad

---

## ¿Qué entrega el Sprint 3?

| Archivo a entregar | Generado por | Subir a GitHub |
|---|---|---|
| `informe_hallazgos.pdf` | `sprint3_reporte.Rmd` | ✓ |
| `script_analisis.R` | es el archivo mismo | ✓ |
| `README_reproducibilidad.md` | este archivo | ✓ |
| `bitacora_ia_s3.xlsx` | manual (plantilla) | ✓ |

---

## Entorno de Reproducibilidad

### Versión de R y SO

```
R version: ≥ 4.3.0
Sistema operativo: Windows 10/11, macOS 14+, o Ubuntu 22.04
RStudio: ≥ 2023.09 (recomendado)
```

### Paquetes requeridos y versiones mínimas

| Paquete | Versión mínima | Función en el sprint |
|---------|---------------|---------------------|
| `tidyverse` | 2.0.0 | Manipulación y visualización |
| `broom` | 1.0.5 | Tidying de modelos |
| `psych` | 2.3.6 | Estadística descriptiva |
| `car` | 3.1.2 | VIF, diagnóstico OLS |
| `lmtest` | 0.9.40 | Breusch-Pagan, tests de modelos |
| `sandwich` | 3.0.2 | SE robustos HC3 |
| `nortest` | 1.0.4 | Shapiro-Wilk, Lilliefors, Anderson-Darling |
| `mice` | 3.16.0 | Imputación múltiple |
| `pROC` | 1.18.4 | Curva ROC, AUC |
| `ResourceSelection` | 0.3.5 | Test Hosmer-Lemeshow |
| `patchwork` | 1.2.0 | Composición de gráficos |
| `tidytext` | 0.4.1 | Análisis de texto |
| `igraph` | 1.6.0 | Redes (usado en S2, disponible aquí) |
| `knitr` | 1.45 | Reportería |
| `kableExtra` | 1.3.4 | Tablas formateadas |

Para instalar todos de una vez:
```r
install.packages(c("tidyverse","broom","psych","car","lmtest","sandwich",
                   "nortest","mice","pROC","ResourceSelection","patchwork",
                   "tidytext","igraph","knitr","kableExtra"))
```

---

## Secuencia de Ejecución — Paso a Paso

### PASO 0 — Prerequisitos

```
sprint1/datos/procesados/mmr_sprint1.csv     ← generado por Script 01
sprint2/datos/procesados/base_datos.csv      ← generado por Script 08
```

Si no los tiene, ejecute la suite completa de Sprint 1 y Sprint 2 primero.

### PASO 1 — Supuestos OLS (Script 09)

```r
source("09_supuestos_regresion.R")
```

**Produce:**
- `datos/procesados/datos_imputados_s3.rds` ← usado por todos los scripts siguientes
- `datos/procesados/resumen_supuestos_s3.csv`
- `outputs/figuras/S3_panel_diagnostico_ols.png`
- `outputs/figuras/S3_s4_normalidad_qq.png`
- `outputs/figuras/S3_s6_cook_distance.png`

**Conceptos verificados:**
```
[S1] Linealidad           → Residuos vs. Ajustados (LOESS)
[S2] Independencia        → Diseño muestral
[S3] Homocedasticidad     → Breusch-Pagan + White
[S4] Normalidad residuos  → Shapiro-Wilk + Lilliefors + Anderson-Darling
[S5] No multicolinealidad → VIF (umbral: 10)
[S6] Sin influyentes      → Distancia de Cook (umbral: 4/n)
```

**Decisión crítica:** Si Breusch-Pagan p ≤ 0.05, el `script_analisis.R` activa automáticamente SE robustos HC3 (`sandwich::vcovHC`).

---

### PASO 2 — Regresión Lineal (Script 10)

```r
source("10_regresion_lineal.R")
```

**Produce:**
- `datos/procesados/comparacion_modelos_s3.csv`
- `datos/procesados/coeficientes_M3.csv`
- `datos/procesados/betas_estandarizados_M3.csv`
- `datos/procesados/resultados_hipotesis_s3.csv`
- `datos/procesados/modelos_ols_s3.rds`
- `outputs/figuras/S3_forest_plot_ols.png`

**Estrategia de modelado:**

```
M0 → M1 → M2 → M3 → M4(stepwise)
 ↑        ↑         ↑
nulo  focal  teórico  parsimonioso
```

**Pruebas de hipótesis ejecutadas:**
```
H1: R² ≥ 0.35   → Test F del modelo M3
H2: d > 0.5     → Prueba t de Welch + d Cohen
H3: r ≥ 0.40    → Pearson + Spearman (robustez)
```

---

### PASO 3 — Regresión Logística (Script 11)

```r
source("11_regresion_logistica.R")
```

**Produce:**
- `datos/procesados/odds_ratios_logistica_s3.csv`
- `outputs/figuras/S3_or_logistica.png`
- `outputs/figuras/S3_curva_roc.png`
- `outputs/figuras/S3_efectos_marginales.png`

**Métricas de ajuste:**
```
Pseudo-R² de McFadden  → bondad de ajuste global
Hosmer-Lemeshow        → calibración (p > 0.05 deseable)
AUC-ROC                → poder discriminante
Punto de corte Youden  → maximiza sensibilidad + especificidad
```

---

### PASO 4 — Análisis Cualitativo (Script 12)

```r
source("12_analisis_cualitativo.R")
```

**Produce:**
- `datos/procesados/diccionario_codigos_s3.csv`
- `datos/procesados/frecuencia_codigos_s3.csv`
- `datos/procesados/temas_cualitativos_s3.csv`
- `datos/procesados/matriz_codificacion_s3.csv`
- `outputs/figuras/S3_frecuencia_temas.png`

**Proceso de codificación:**
```
Paso 1: Familiarización       (frecuencia de palabras, tokens)
Paso 2: Códigos iniciales     (diccionario inductivo-deductivo, 17 códigos)
Paso 3: Construcción temas    (agrupación jerárquica → 5 temas)
Paso 4: Revisión-saturación   (frecuencia y % de aparición)
Paso 5: Definición densa      (descripción + cita ilustrativa)
Paso 6: Conexión cuantitativa (articulación MMR → Joint Display S4)
```

---

### PASO 5 — Bootstrap y Monte Carlo (Script 13)

```r
source("13_bootstrap_montecarlo.R")
```

**Produce:**
- `datos/procesados/resumen_bootstrap_s3.csv`
- `datos/procesados/montecarlo_escenario_s3.csv`
- `outputs/figuras/S3_bootstrap_ols.png`
- `outputs/figuras/S3_montecarlo_escenario.png`

**Parámetros de simulación:**
```
Bootstrap:    B = 2,000 réplicas · set.seed(8502)
Monte Carlo:  n_sim = 5,000 escenarios de política
Escenario:    cobertura extensión: 35% → 55%
```

---

### PASO 6 — ENTREGABLE: Script Integrado

```r
# Limpiar workspace y ejecutar desde cero
rm(list = ls())
source("script_analisis.R")
```

**Este es el entregable principal.** Ejecuta el pipeline completo en ~3 minutos y produce todos los archivos de resultados.

---

### PASO 7 — Generar Reporte PDF

```r
rmarkdown::render(
  "sprint3_reporte.Rmd",
  output_file   = "outputs/reportes/informe_hallazgos.pdf",
  output_format = "pdf_document"
)
```

---

## Mapa del Pipeline Sprint 3

```
BASE_DATOS.CSV (S2)
      │
      ▼
09_supuestos_regresion.R
  │  Imputación MICE (m=5, PMM)
  │  Tests: BP · SW · VIF · Cook
  └──► datos_imputados_s3.rds
           │
           ├──► 10_regresion_lineal.R
           │      M0→M1→M2→M3→M4
           │      Forest plot · H1-H3
           │      Bootstrap B=2000
           │
           ├──► 11_regresion_logistica.R
           │      OR · AUC · HL · Youden
           │      Efectos marginales
           │
           ├──► 12_analisis_cualitativo.R
           │      17 códigos · 5 temas
           │      Convergencia cuanti↔cuali
           │
           └──► 13_bootstrap_montecarlo.R
                  IC bootstrap R², d, r
                  MC escenario política

TODOS LOS ANTERIORES
      │
      ▼
script_analisis.R  ← ENTREGABLE
      │
      ▼
sprint3_reporte.Rmd
      │
      ▼
informe_hallazgos.pdf  ← ENTREGABLE
```

---

## Estructura de Carpetas Sprint 3

```
sprint3/
├── 09_supuestos_regresion.R
├── 10_regresion_lineal.R
├── 11_regresion_logistica.R
├── 12_analisis_cualitativo.R
├── 13_bootstrap_montecarlo.R
├── script_analisis.R              ← ENTREGABLE
├── sprint3_reporte.Rmd
├── README_reproducibilidad.md     ← ENTREGABLE
│
├── datos/
│   └── procesados/
│       ├── datos_imputados_s3.rds
│       ├── modelos_finales_s3.rds
│       ├── coeficientes_M3_final.csv
│       ├── comparacion_modelos_s3.csv
│       ├── resultados_hipotesis_s3.csv
│       ├── bootstrap_resumen_s3.csv
│       ├── OR_logistica_s3.csv
│       ├── temas_cualitativos_s3.csv
│       ├── diccionario_codigos_s3.csv
│       └── frecuencia_codigos_s3.csv
│
├── outputs/
│   ├── figuras/
│   │   ├── S3_panel_diagnostico.png
│   │   ├── S3_coeficientes_OLS.png
│   │   ├── S3_forest_plot_ols.png
│   │   ├── S3_curva_roc.png
│   │   ├── S3_or_logistica.png
│   │   ├── S3_efectos_marginales.png
│   │   ├── S3_frecuencia_temas.png
│   │   ├── S3_bootstrap_ols.png
│   │   └── S3_montecarlo_escenario.png
│   └── reportes/
│       └── informe_hallazgos.pdf  ← ENTREGABLE
│
└── bitacoras/
    └── bitacora_ia_s3.xlsx        ← ENTREGABLE (manual)
```

---

## Checklist de Verificación — Sprint 3

- [ ] `script_analisis.R` se ejecuta desde cero sin errores (limpiar workspace primero)
- [ ] `set.seed(8502)` presente → resultados reproducibles exactos
- [ ] El panel de diagnóstico OLS está incluido en el reporte
- [ ] Los supuestos están documentados con tests numéricos (no solo gráficos)
- [ ] Se reportan AMBOS modelos M3 (teórico) y M4 (parsimonioso) con justificación
- [ ] H1, H2, H3 tienen resultado explícito: "SE CONFIRMA / No se confirma"
- [ ] El Bootstrap usa B ≥ 1,000 y documenta IC 95%
- [ ] El análisis cualitativo tiene ≥ 3 temas con cita ilustrativa cada uno
- [ ] La tabla de convergencia cuanti-cuali está en el reporte
- [ ] `README_reproducibilidad.md` incluye versiones de paquetes
- [ ] `bitacora_ia_s3.xlsx` documenta ≥ 4 interacciones con IA
- [ ] Los archivos están en `sprint-3/entregas/[su-carné]/` en GitHub

---

## Advertencias de Reproducibilidad

### ⚠ Semillas y aleatoriedad
- `set.seed(8502)` debe ejecutarse al inicio de CADA sesión R nueva
- Los resultados de MICE pueden variar mínimamente con diferentes versiones del paquete
- Bootstrap con B < 1,000 puede producir IC inestables

### ⚠ Dependencias entre scripts
- El Script 10, 11, 12 y 13 requieren `datos_imputados_s3.rds` generado por Script 09
- El `script_analisis.R` es autocontenido: regenera todo internamente

### ⚠ Paquete `pROC`
- Requiere `exactranks = FALSE` en `cor.test` para Spearman con n=80 (avisos)
- Usar `quiet = TRUE` en `roc()` para suprimir mensajes de formato

---

## Conexión con Sprint 4

Los siguientes archivos de este sprint son entradas directas del Sprint 4:

| Archivo S3 | Uso en Sprint 4 |
|---|---|
| `coeficientes_M3_final.csv` | Tabla de resultados del Policy Brief |
| `temas_cualitativos_s3.csv` | Componente del Joint Display |
| `resultados_hipotesis_s3.csv` | Sección de hallazgos del informe ejecutivo |
| `modelos_finales_s3.rds` | Predicciones para gráficos sf/ggplot2 |

---

*SP-8502 · GIACT · UCR · 2026-I · Sprint 3 · v1.0*  
*Resolución R-469-2025 · Integridad académica y reproducibilidad*
