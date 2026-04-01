# SP-8502 · Sprint 2 — Guía Paso a Paso

**Curso:** Métodos Cuanti-Cuali con IA Responsable  
**Programa:** Maestría GIACT · Universidad de Costa Rica · 2026-I  
**Sprint:** 2 — Trabajo de Campo y Muestreo (Semanas 5–8, 20%)  
**Docente:** MSI. Agustín Gómez Meléndez · CIOdD · UCR

---

## ¿Qué produce este Sprint?

| Archivo a entregar | Script que lo genera | Ruta en GitHub |
|---|---|---|
| `estrategia_muestreo.pdf` | `sprint2_reporte.Rmd` | `sprint-2/entregas/[carné]/` |
| `base_datos.csv` (anonimizada) | `08_base_datos_preliminar.R` | `sprint-2/entregas/[carné]/` |
| `bitacora_ia_s2.xlsx` | Manual (plantilla del curso) | `sprint-2/entregas/[carné]/` |

---

## Prerrequisito: Sprint 1 Completado

El Sprint 2 requiere los siguientes archivos del Sprint 1:

```
sprint1/
├── 00_setup_ambiente.R              ← Se llama con source()
├── datos/procesados/mmr_sprint1.csv ← Marco de variables
└── datos/procesados/pescadores_limpio.csv ← EDA de referencia
```

Si no los tiene, ejecute primero la suite completa del Sprint 1.

---

## Secuencia de Ejecución — Paso a Paso

### PASO 0 — Verificar entorno (cargar setup del Sprint 1)

```r
source("../sprint1/00_setup_ambiente.R")
# O si está en la misma carpeta raíz:
source("00_setup_ambiente.R")
```

Además, instalar el paquete `igraph` si no está presente:

```r
install.packages("igraph")
library(igraph)
```

---

### PASO 1 — Muestreo probabilístico (Script 05)

```r
source("05_muestreo_probabilistico.R")
```

**¿Qué hace?** En 6 secciones:

| Sección | Contenido |
|---------|-----------|
| §1 Marco muestral | Tabla INCOPESCA con N por comunidad y σ SAS piloto |
| §2 Cálculo n | Fórmula de Cochran + corrección por finitud |
| §3 MAS | Selección aleatoria simple y comparación con marco |
| §4 Estratificado | Asignación proporcional vs. óptima de Neyman |
| §5 Sistemático | Muestreo 1-en-k con arranque aleatorio |
| §6 Comparación | Tabla de eficiencia: V(ȳ) bajo cada diseño |

**Secuencia pedagógica aplicada:**
```
CÁLCULO         → Fórmula Cochran, corrección por finitud
INTERPRETACIÓN  → Ganancia relativa Neyman vs. MAS (DEFF)
CONTEXTO        → Estacionalidad veda, rechazo INCOPESCA
DECISIÓN        → Justificación de n=80 y diseño estratificado
```

**Archivos generados:**
```
datos/procesados/marco_muestral_s2.csv
datos/procesados/muestra_estratificada_s2.csv
datos/procesados/comparacion_diseños_muestrales.csv
```

**Pregunta de examen posible:**
> "¿Por qué la asignación óptima de Neyman es estadísticamente superior
>  pero se adopta la proporcional en este Sprint?"
>
> Respuesta esperada: Porque la óptima requiere σ_h confiables de cada
> estrato, disponibles solo después de un piloto robusto.

---

### PASO 2 — Simulación RDS (Script 06)

```r
source("06_rds_simulacion.R")
```

**¿Qué hace?**

1. **Construye una red social** de 200 pescadores no registrados usando el modelo de pequeño mundo de Watts-Strogatz (más realista que Erdős–Rényi para comunidades costeras).
2. **Simula el reclutamiento** en cadena desde 5 semillas heterogéneas.
3. **Calcula el estimador VH** y lo compara con el estimador naive y el valor real poblacional.
4. **Diagnostica la convergencia** (el estimador debe estabilizarse antes de la oleada 4).
5. **Genera figura** `outputs/figuras/C1_convergencia_rds.png`.

**Concepto clave para la Defensa (Semana 16):**

| Estimador | Fórmula | Cuándo usar |
|-----------|---------|-------------|
| Naive (media simple) | $\bar{y} = \frac{1}{n}\sum y_i$ | Muestra aleatoria simple |
| Volz-Heckathorn (VH) | $\hat{p}_{VH} = \frac{\sum y_i/d_i}{\sum 1/d_i}$ | Siempre en RDS |

> El estimador naive **sobreestima** la proporción de comportamiento
> sostenible porque los encuestados con más conexiones (d_i alto)
> tienen mayor probabilidad de ser reclutados.

**Archivo generado:**
```
datos/procesados/muestra_rds_s2.csv
outputs/figuras/C1_convergencia_rds.png
```

---

### PASO 3 — Instrumento de encuesta (Script 07)

```r
source("07_instrumento_encuesta.R")
```

**¿Qué hace?**

1. **Mapea la estructura** del instrumento (25 ítems, 5 bloques).
2. **Valida psicométricamente** la escala SAS-10 con datos piloto simulados:
   - Alpha de Cronbach ($\alpha \geq 0.70$)
   - Omega de McDonald ($\omega \geq 0.70$)
   - Correlación ítem-total ($r \geq 0.30$)
   - Alpha si se elimina el ítem
3. **Calcula el error de muestreo** específico por cada variable.
4. **Genera protocolo de campo** con fases, responsables y herramientas.
5. **Produce tabla de decisión** sobre viabilidad del instrumento.

**Decisión crítica documentada:**
> Si algún ítem tiene $r_{ítem-total} < 0.20$: → eliminar y recalcular $\alpha$  
> Si $\alpha < 0.60$ con 10 ítems: → revisar redacción o reorganizar dimensiones

**Archivo generado:**
```
datos/procesados/protocolo_campo_s2.csv
```

---

### PASO 4 — Base de datos preliminar (Script 08 = Entregable)

```r
source("08_base_datos_preliminar.R")
```

**Este es el entregable `base_datos.csv` del Sprint 2.**

**Arquitectura del dataset generado (n=80, k=26 variables):**

```
┌─────────────────────────────────────────────────────────┐
│  Variables de identificación (anonimizadas)             │
│  id_anonimo · metodo · comunidad · semana_encuesta       │
│  lat_anon · lon_anon                                     │
├─────────────────────────────────────────────────────────┤
│  Variables sociodemográficas (predictores)               │
│  sexo · edad · años_experiencia · tamaño_familia         │
│  ingreso_mensual_usd · arte_pesca_principal              │
├─────────────────────────────────────────────────────────┤
│  Variables ambientales (covariables)                     │
│  distancia_anp_km · percepcion_recurso                   │
│  extension_pesquera                                      │
├─────────────────────────────────────────────────────────┤
│  Escala SAS-10 (variable dependiente)                    │
│  sas_item_1 a sas_item_10 · sas_total (0–50 pts)        │
└─────────────────────────────────────────────────────────┘
```

**Proceso de anonimización aplicado (5 capas):**

| Capa | Dato original | Dato anonimizado |
|------|--------------|-----------------|
| 1 | ID de encuesta (`EST-001`) | Código hash anónimo (`ANC-XXXX`) |
| 2 | GPS exacto (6 decimales) | GPS a 2 decimales (~1.1 km) |
| 3 | Fecha exacta | Semana de encuesta (2026-S14) |
| 4 | Comunidad con n<5 | "Otra zona costera" |
| 5 | Edades extremas (<25, >65) | Redondeadas al quinquenio |

---

### PASO 5 — Generar el reporte PDF (Rmd)

```r
rmarkdown::render(
  "sprint2_reporte.Rmd",
  output_file   = "outputs/reportes/estrategia_muestreo.pdf",
  output_format = "pdf_document"
)
```

**Alternativa HTML (si no tiene LaTeX):**
```r
rmarkdown::render(
  "sprint2_reporte.Rmd",
  output_file   = "outputs/reportes/estrategia_muestreo.html",
  output_format = "html_document"
)
```

---

## Mapa Conceptual del Sprint 2

```
 SPRINT 2: TRABAJO DE CAMPO Y MUESTREO
 ═══════════════════════════════════════

  SEMANA 5                    SEMANA 6
  ─────────────────           ─────────────────
  05_muestreo_prob.R    →     06_rds_simulacion.R
  • Marco muestral            • Red social Watts-Strogatz
  • Fórmula Cochran           • Reclutamiento en cadena
  • MAS vs Estratificado      • Estimador VH
  • Comparación DEFF          • Diagnóstico convergencia
        ↓                           ↓
  Decisión: n=80              Decisión: 5 semillas,
  Diseño estratificado        3 cupones, ≤8 oleadas

  SEMANA 7                    SEMANA 8
  ─────────────────           ─────────────────
  07_instrumento.R      →     08_base_datos.R
  • Estructura 25 ítems       • Integrar submuestras
  • Alpha Cronbach            • Anonimizar (5 capas)
  • Correlación ítem-total    • Control de calidad
  • Protocolo de campo        • ENTREGABLE: base_datos.csv
        ↓                           ↓
  Decisión: instrumento       Decisión: metodo como
  aprobado/rechazado          covariable en S3
```

---

## Estructura de Carpetas Sprint 2

```
sprint2/
├── 05_muestreo_probabilistico.R
├── 06_rds_simulacion.R
├── 07_instrumento_encuesta.R
├── 08_base_datos_preliminar.R       ← ENTREGABLE (genera base_datos.csv)
├── sprint2_reporte.Rmd
│
├── datos/
│   └── procesados/
│       ├── base_datos.csv           ← ENTREGABLE principal
│       ├── diccionario_variables_s2.csv
│       ├── marco_muestral_s2.csv
│       ├── muestra_estratificada_s2.csv
│       ├── muestra_rds_s2.csv
│       ├── comparacion_diseños_muestrales.csv
│       └── protocolo_campo_s2.csv
│
├── outputs/
│   ├── figuras/
│   │   └── C1_convergencia_rds.png
│   └── reportes/
│       └── estrategia_muestreo.pdf  ← ENTREGABLE
│
└── bitacoras/
    └── bitacora_ia_s2.xlsx          ← ENTREGABLE (manual)
```

---

## Checklist de Verificación — Sprint 2

Antes de subir a GitHub:

- [ ] `08_base_datos_preliminar.R` se ejecuta desde cero sin errores
- [ ] `base_datos.csv` NO contiene nombres, IDs originales ni GPS exacto
- [ ] El `diccionario_variables_s2.csv` documenta todas las variables
- [ ] `estrategia_muestreo.pdf` incluye tabla de marco muestral + justificación n
- [ ] Se documenta por qué el diseño RDS es necesario (fracción no registrada)
- [ ] Alpha de Cronbach de la escala SAS-10 está documentado (≥ 0.70)
- [ ] La figura de convergencia RDS está en el reporte
- [ ] `bitacora_ia_s2.xlsx` tiene ≥ 3 filas con prompt + decisión humana + verificación
- [ ] Los archivos están en `sprint-2/entregas/[su-carné]/` en GitHub

---

## Conexión con los demás Sprints

```
Sprint 1              Sprint 2 (este)           Sprint 3              Sprint 4
──────────────        ─────────────────         ──────────────        ──────────────
MMR definida     →    Marco muestral        →   Modelo OLS        →   Policy Brief
Hipótesis H1-H3  →    n=80 con poder        →   Prueba H1-H3      →   Hallazgos
EDA exploratorio →    base_datos.csv        →   Imputación MICE   →   Joint Display
Dataset crudo    →    Dataset anonimizado   →   Diagnósticos      →   Visualización sf
```

---

## Conceptos clave para la Defensa Final (Semana 16)

El estudiante debe poder **demostrar en vivo** en R:

1. Calcular el tamaño de muestra con `qnorm()` y la fórmula de Cochran
2. Mostrar la diferencia entre estimador naive y estimador VH en datos RDS
3. Calcular e interpretar el Alpha de Cronbach con `psych::alpha()`
4. Demostrar que el dataset está anonimizado (sin identificadores directos)
5. Explicar por qué la variable `metodo` debe incluirse en el modelo de Sprint 3

---

*SP-8502 · GIACT · UCR · 2026-I · Sprint 2 · v1.0*  
*Resolución R-469-2025 · Integridad académica y IA responsable*
