# SP-8502 · Sprint 1 — Guía Paso a Paso

**Curso:** Métodos Cuanti-Cuali con IA Responsable  
**Programa:** Maestría GIACT · Universidad de Costa Rica · 2026-I  
**Sprint:** 1 — Diseño y Fundamentos de Datos (Semanas 1–4, 20%)  
**Docente:** MSI. Agustín Gómez Meléndez · CIOdD · UCR

---

## ¿Qué produce este Sprint?

| Archivo a entregar | Script que lo genera | Carpeta destino |
|---|---|---|
| `reporte_diseño.pdf` | `sprint1_reporte.Rmd` | `sprint-1/entregas/[carné]/` |
| `script_limpieza.R` | es el script mismo | `sprint-1/entregas/[carné]/` |
| `bitacora_ia_s1.xlsx` | Manual (plantilla dada) | `sprint-1/entregas/[carné]/` |

---

## Secuencia de Ejecución — Paso a Paso

### PASO 0 — Configurar el entorno (una sola vez)

```r
# Ejecutar primero, siempre
source("00_setup_ambiente.R")
```

**¿Qué hace?**  
- Instala todos los paquetes del curso.  
- Crea la estructura de carpetas del proyecto.  
- Configura opciones globales de R y el tema `ggplot2`.

**¿Cómo sé que funcionó?**  
Verás en la consola:  
```
✓ Entorno configurado para SP-8502 · Sprint 1
```

---

### PASO 1 — Formular el problema (Script 01)

```r
source("01_planteamiento_y_matriz.R")
```

**¿Qué hace?**  
- Formaliza la **pregunta de investigación** como objeto R.  
- Construye la **Matriz Metodológica Mixta (MMR)** como `data.frame`.  
- Declara las **hipótesis estadísticas** en notación formal.  
- Documenta el diseño mixto adoptado (Secuencial Explicativo).  
- Muestra la decisión de IA con justificación humana (Sección 5).

**Archivo generado:**  
`datos/procesados/mmr_sprint1.csv`

**Decisión crítica aquí:**  
> ¿Por qué un diseño Secuencial Explicativo y no un Convergente Paralelo?  
> Respuesta: porque la fase cualitativa DEPENDE de los hallazgos cuantitativos previos.

---

### PASO 2 — Generar datos sintéticos (Script 02)

```r
source("02_datos_sinteticos_costeros.R")
```

**¿Qué hace?**  
- Simula una encuesta a `n = 80` pescadores artesanales del Pacífico Central CR.  
- **Inyecta 9 errores deliberados** en el dataset:

| Error | Variable | Tipo |
|-------|----------|------|
| E1 | `comunidad` | Inconsistencia tipográfica (caps/tildes) |
| E2 | `edad` | Valores imposibles (-5, 150) |
| E3 | `sexo` | Múltiples codificaciones (M, masculino, Masculino) |
| E4 | `ingreso_mensual_usd` | Código especial no documentado (9999.99) |
| E5 | `arte_pesca_principal` | Mezcla español/inglés |
| E6 | `percepcion_recurso` | Duplicidad de "no respuesta" |
| E7 | `sas_total` | Valores fuera de rango (55, 60, -3) |
| E8 | `extension_pesquera` | Binaria codificada en 5 formas distintas |
| E9 | `columna_vacia` | Columna completamente NA |

**Pregunta de discusión en clase:**  
> ¿Cuál de estos errores tiene mayor impacto en el análisis de regresión del Sprint 3?

**Archivo generado:**  
`datos/crudos/pescadores_pacifico_central_CRUDO.csv`

---

### PASO 3 — Limpiar los datos (Script 03 = Entregable)

```r
source("script_limpieza.R")
```

**Este es el entregable principal del Sprint 1.**

El script aplica la secuencia pedagógica de 4 pasos para cada error:

```
PASO 1 · CÁLCULO
  → case_when(), str_to_lower(), fct_collapse(), filter()

PASO 2 · INTERPRETACIÓN ESTADÍSTICA  
  → ¿Cuántos valores se corrigieron? ¿Qué revela la distribución?

PASO 3 · CONTEXTO COSTERO
  → ¿Por qué este error es especialmente riesgoso en investigación pesquera?

PASO 4 · DECISIÓN CRÍTICA
  → ¿Por qué NA en lugar de imputación? ¿Por qué factor ordenado?
```

**Archivos generados:**
```
datos/procesados/pescadores_limpio.csv         ← Dataset principal
datos/procesados/trazabilidad_correcciones.csv ← Audit trail
datos/procesados/decisiones_limpieza.csv       ← Justificaciones
```

**Lo que NO hace este script (y es intencional):**  
- NO imputa valores faltantes (eso es Sprint 3).  
- NO transforma variables para modelos (eso es Sprint 3).  
- NO genera gráficos de análisis (eso es el script 04).

---

### PASO 4 — Analizar exploratoriamente (Script 04)

```r
source("04_eda_sprint1.R")
```

**¿Qué hace?**  
- Estadística descriptiva univariada completa (`skimr`, `psych`).  
- 5 visualizaciones `ggplot2` guardadas en `outputs/figuras/`.  
- Análisis bivariado preliminar (anticipa hipótesis del Sprint 3).  
- Síntesis en 4 pasos pedagógicos.

**Gráficos generados:**
| Archivo | Contenido |
|---------|-----------|
| `B1_distribucion_sas.png` | Histograma + densidad del SAS |
| `B2_sas_por_comunidad.png` | Boxplot + jitter por comunidad |
| `B3_ingreso_mensual.png` | Distribución original y log-transformada |
| `B4_percepcion_recurso.png` | Stacked bar por comunidad |
| `B5_correlacion_pearson.png` | Matriz de correlación (p < 0.05) |

---

### PASO 5 — Generar el reporte PDF (Rmd)

```r
rmarkdown::render(
  "sprint1_reporte.Rmd",
  output_file   = "outputs/reportes/reporte_diseño.pdf",
  output_format = "pdf_document"
)
```

**Requiere:** LaTeX instalado (`tinytex::install_tinytex()`) o usar `html_document`.

**Alternativa HTML:**
```r
rmarkdown::render(
  "sprint1_reporte.Rmd",
  output_file   = "outputs/reportes/reporte_diseño.html",
  output_format = "html_document"
)
```

---

## Estructura de Carpetas Generada

```
proyecto_sp8502/
├── 00_setup_ambiente.R
├── 01_planteamiento_y_matriz.R
├── 02_datos_sinteticos_costeros.R
├── script_limpieza.R                  ← ENTREGABLE
├── 04_eda_sprint1.R
├── sprint1_reporte.Rmd
│
├── datos/
│   ├── crudos/
│   │   └── pescadores_pacifico_central_CRUDO.csv
│   └── procesados/
│       ├── pescadores_limpio.csv      ← Dataset principal
│       ├── mmr_sprint1.csv
│       ├── trazabilidad_correcciones.csv
│       ├── decisiones_limpieza.csv
│       ├── catalogo_errores_pedagogicos.csv
│       └── sintesis_eda_s1.csv
│
├── outputs/
│   ├── figuras/
│   │   ├── B1_distribucion_sas.png
│   │   ├── B2_sas_por_comunidad.png
│   │   ├── B3_ingreso_mensual.png
│   │   ├── B4_percepcion_recurso.png
│   │   └── B5_correlacion_pearson.png
│   └── reportes/
│       └── reporte_diseño.pdf         ← ENTREGABLE
│
└── bitacoras/
    └── bitacora_ia_s1.xlsx            ← ENTREGABLE (manual)
```

---

## Checklist de Verificación — Sprint 1

Antes de subir a GitHub, verificar que su entrega cumple:

- [ ] `script_limpieza.R` se ejecuta sin errores desde cero (reiniciar R, ejecutar todo)
- [ ] `set.seed(8502)` está presente y produce resultados idénticos
- [ ] Las 4 secciones pedagógicas están en el script (Cálculo, Interpretación, Contexto, Decisión)
- [ ] `reporte_diseño.pdf` fue generado con `rmarkdown::render()`
- [ ] La MMR tiene ≥ 8 variables con nivel de medición y sprint responsable
- [ ] `bitacora_ia_s1.xlsx` documenta ≥ 3 interacciones significativas con IA
- [ ] Las decisiones metodológicas citan fuentes académicas
- [ ] Ningún dato real de campo fue subido a plataformas de IA públicas
- [ ] Los archivos están en `sprint-1/entregas/[su-carné]/` en GitHub

---

## Política de Uso de IA — Recordatorio (R-469-2025)

| Nivel | Permitido | Documentación requerida |
|-------|-----------|------------------------|
| Exploratorio | Lluvia de ideas, comprensión conceptual | Bitácora + verificación académica |
| Técnico | Debug de código R, sugerencia de funciones | Bitácora + capacidad de explicar el código |
| Redacción | Revisión gramatical/estilo | Bitácora + contenido sustantivo original |
| ⛔ PROHIBIDO | Generar reporte completo; fabricar datos | Sanción académica UCR |

---

## Conexión con Sprints Posteriores

```
Sprint 1 (este)          Sprint 2              Sprint 3            Sprint 4
─────────────────        ─────────────         ──────────────      ────────────
MMR definida         →   Instrumento final  →  Modelo OLS      →  Policy Brief
Dataset limpio       →   Datos primarios    →  Imputación MICE →  Visualización
Hipótesis formuladas →   Muestra calculada  →  Prueba H1–H3    →  Joint Display
EDA preliminar       →   Trabajo de campo   →  Hallazgos       →  Defensa oral
```

---

*SP-8502 · GIACT · UCR · 2026-I · v1.0*  
*Resolución R-469-2025 · Integridad académica y IA responsable*
