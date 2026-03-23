# ==============================================================================
# TALLER PRÁCTICO: Limpieza de datos reales con tidyr
# Gestión Marino-Costera en Centroamérica y República Dominicana
# ==============================================================================
# Curso    : R Básico, Tidyverse y Data Wrangling
# Enfoque  : Técnicas cuantitativas y cualitativas
# Autor    : Agustín Gómez Meléndez
# Fecha    : 2026
# ------------------------------------------------------------------------------
# OBJETIVOS DE APRENDIZAJE:
#   1. Diagnosticar calidad de datos: valores faltantes (NA) y outliers
#   2. Construir un script reproducible de limpieza de datos
#   3. Dominar pivot_longer() y pivot_wider() de tidyr
#   4. Integrar variables cualitativas (texto, categorías) con cuantitativas
#.  5. Fuentes: Informe Estado de la Region CR 2024
#.  6. Código optimizado con Claude Opus 4.6 Extend
# ------------------------------------------------------------------------------
# INSTRUCCIONES PARA EL ESTUDIANTE:
#   - Ejecute el código SECCIÓN POR SECCIÓN (no todo de una vez).
#   - Lea los mensajes que aparecen en la consola: explican qué hace cada paso.
#   - Observe los resultados y pregúntese: ¿tiene sentido lo que veo?
#   - Los ejercicios al final son para practicar por su cuenta.
# ==============================================================================


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  PARTE 0: CONFIGURACIÓN DEL ENTORNO                                      ║
# ╚════════════════════════════════════════════════════════════════════════════╝

# ¿QUÉ VAMOS A HACER AQUÍ?
# Antes de trabajar con datos necesitamos asegurarnos de que R tenga instalados
# los "paquetes" (bibliotecas de funciones) que vamos a usar. Piense en los
# paquetes como cajas de herramientas especializadas: no vienen con R por
# defecto, hay que descargarlas una vez e invocarlas cada vez que las necesitemos.

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║  PASO 0: Verificando e instalando paquetes necesarios                      ║
╠══════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  Vamos a usar tres paquetes:                                               ║
║                                                                            ║
║  1. tidyverse → Es un 'meta-paquete' que instala varios paquetes juntos:   ║
║     - dplyr   : manipulación de datos (filter, mutate, summarise, etc.)    ║
║     - tidyr   : transformación de estructura (pivot_longer, pivot_wider)   ║
║     - ggplot2 : visualización de datos                                     ║
║     - readr   : lectura de archivos CSV/TSV                                ║
║     - stringr : manipulación de texto (str_to_title, str_detect, etc.)     ║
║     - tibble  : tablas de datos mejoradas                                  ║
║     - purrr   : programación funcional                                     ║
║                                                                            ║
║  2. naniar → Herramientas especializadas para analizar datos faltantes     ║
║     (NA = Not Available). Nos permite visualizar PATRONES de faltantes.    ║
║                                                                            ║
║  3. visdat → Diagnóstico visual rápido: ¿qué tipo es cada variable?       ║
║     ¿dónde están los NAs? Lo muestra todo en un solo gráfico.              ║
║                                                                            ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# Este bloque revisa si los paquetes ya están instalados.
# Si alguno falta, lo descarga automáticamente desde CRAN (el repositorio
# oficial de paquetes de R). Así no tiene que instalarlos manualmente.
paquetes_necesarios <- c("tidyverse", "naniar", "visdat")
paquetes_faltantes <- paquetes_necesarios[
  !paquetes_necesarios %in% installed.packages()[, "Package"]
]
if (length(paquetes_faltantes) > 0) {
  cat("→ Instalando paquetes faltantes:", paste(paquetes_faltantes, collapse = ", "), "\n")
  install.packages(paquetes_faltantes, repos = "https://cran.r-project.org")
} else {
  cat("→ Todos los paquetes ya están instalados. ¡Perfecto!\n")
}

# library() CARGA el paquete en la sesión actual.
# Diferencia clave:
#   install.packages() = descargar e instalar (se hace UNA vez)
#   library()          = activar para usar (se hace CADA vez que abre R)
library(tidyverse)
library(naniar)
library(visdat)

cat("\n✓ Paquetes cargados exitosamente. Estamos listos para trabajar.\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  PARTE 1: CREACIÓN DEL DATASET SIMULADO                                  ║
# ╚════════════════════════════════════════════════════════════════════════════╝

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║  PASO 1: Creando el dataset simulado de monitoreo costero                  ║
╠══════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  ¿POR QUÉ SIMULAMOS DATOS?                                                ║
║  En un proyecto real usted leería datos de un archivo CSV o Excel.         ║
║  Aquí los simulamos para que el taller sea autocontenido y para que        ║
║  podamos CONTROLAR qué problemas de calidad existen (así sabemos qué      ║
║  buscar). En su proyecto final usará datos reales.                         ║
║                                                                            ║
║  ¿QUÉ REPRESENTAN ESTOS DATOS?                                            ║
║  Simulan un programa de monitoreo costero en 5 comunidades de la región   ║
║  SICA que enfrentan los 5 problemas críticos del cambio climático:         ║
║                                                                            ║
║  P1: Aumento del nivel del mar y erosión costera                           ║
║  P2: Intensificación de ciclones tropicales y marejadas                    ║
║  P3: Intrusión salina en acuíferos                                         ║
║  P4: Pérdida de playas y destrucción de ecosistemas protectores            ║
║  P5: Desplazamiento forzado (movilidad climática)                          ║
║                                                                            ║
║  Las comunidades incluyen los 3 CASOS REALES documentados:                 ║
║  • Cartí Sugdup, Panamá  → Reubicación masiva Guna Yala (2024)            ║
║  • Iztapa, Guatemala     → Pérdida de infraestructura y emigración         ║
║  • Tela, Honduras        → Erosión acelerada y sacos de arena              ║
║  Más 2 comunidades adicionales para ampliar la muestra regional.           ║
║                                                                            ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# --- 1.1 set.seed(): Garantizar reproducibilidad ---

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 1.1 SEMILLA ALEATORIA (set.seed)                                           │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ R genera números 'aleatorios' usando algoritmos matemáticos. Si no fijamos │
│ una semilla, cada vez que ejecutemos el script los datos serán DIFERENTES.  │
│                                                                            │
│ set.seed(2026) fija el punto de partida del generador aleatorio.           │
│ Resultado: TODOS los estudiantes obtendrán EXACTAMENTE los mismos datos.   │
│ Esto es fundamental para la REPRODUCIBILIDAD científica: si alguien más    │
│ corre su código, debe obtener los mismos resultados.                       │
│                                                                            │
│ El número 2026 es arbitrario (podría ser cualquiera). Usamos el año del   │
│ curso como convención.                                                     │
└──────────────────────────────────────────────────────────────────────────────┘
")

set.seed(2026)

n <- 60  # Número total de observaciones (estaciones × periodos de monitoreo)

cat("→ Semilla fijada en 2026. Se generarán", n, "registros simulados.\n")

# --- 1.2 Construcción del tibble principal ---

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 1.2 CONSTRUCCIÓN DEL DATASET CON tibble()                                  │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ Un 'tibble' es la versión mejorada del data.frame clásico de R.            │
│ Ventajas del tibble sobre data.frame:                                      │
│   - Muestra solo las primeras filas al imprimirlo (no inunda la consola)   │
│   - Indica el tipo de cada columna (chr, dbl, int, etc.)                   │
│   - No convierte texto a factores automáticamente                          │
│   - Advierte si accede a una columna que no existe                         │
│                                                                            │
│ Vamos a crear 15 variables organizadas en 3 bloques:                       │
│   A) Identificadores   → quién, dónde, cuándo                             │
│   B) Vars cuantitativas → mediciones numéricas de cada problema            │
│   C) Vars cualitativas → percepciones, categorías y narrativas             │
│                                                                            │
│ IMPORTANTE: Este dataset tiene PROBLEMAS DE CALIDAD INTENCIONALES.         │
│ Más adelante los vamos a diagnosticar y corregir. Así es la vida real.     │
└──────────────────────────────────────────────────────────────────────────────┘
")

datos_costeros <- tibble(
  
  # ─── BLOQUE A: IDENTIFICADORES ───────────────────────────────────────────
  # Estas variables ubican cada registro en el espacio y el tiempo.
  # Sin identificadores claros, no podríamos saber DE DÓNDE viene cada dato.
  
  id_registro = 1:n,
  # ^ Número secuencial único para cada fila. Permite rastrear registros
  #   individuales durante la limpieza ("el registro 22 tiene un outlier").
  
  comunidad = sample(
    c("Cartí Sugdup, Panamá",     # Caso 1: Reubicación Guna Yala
      "Iztapa, Guatemala",         # Caso 2: Pérdida de infraestructura
      "Tela, Honduras",            # Caso 3: Erosión acelerada
      "Limón, Costa Rica",         # Contexto regional adicional
      "Boca Chica, Rep. Dom."),    # Contexto regional adicional
    n, replace = TRUE
    # sample() toma una muestra aleatoria. replace = TRUE permite repeticiones
    # (una comunidad puede aparecer varias veces, como en un monitoreo real
    # donde se mide la misma comunidad en múltiples periodos).
  ),
  
  anio = sample(2018:2025, n, replace = TRUE),
  # ^ Año de la medición. sample() elige aleatoriamente entre 2018 y 2025.
  #   Esto simula un programa de monitoreo de 8 años.
  
  trimestre = sample(1:4, n, replace = TRUE),
  # ^ Trimestre del año (1=ene-mar, 2=abr-jun, 3=jul-sep, 4=oct-dic).
  #   Importante porque los ciclones son estacionales (junio-noviembre).
  
  
  # ─── BLOQUE B: VARIABLES CUANTITATIVAS ───────────────────────────────────
  # Cada variable mide un aspecto de los 5 problemas críticos.
  # Usamos rnorm() para generar datos con distribución normal (campana de Gauss).
  # rnorm(n, mean, sd) genera n valores con media = mean y desviación = sd.
  
  # → PROBLEMA 1: Aumento del nivel del mar
  nivel_mar_mm = round(rnorm(n, mean = 35, sd = 12), 1),
  # ^ Elevación del nivel del mar en milímetros sobre la línea base de 2010.
  #   Media = 35 mm, desviación estándar = 12 mm.
  #   round(..., 1) redondea a 1 decimal.
  #   Contexto: el IPCC estima ~3.6 mm/año de aumento global promedio,
  #   así que 35 mm acumulados en ~10 años es plausible regionalmente.
  
  # → PROBLEMA 4: Pérdida de playas y ecosistemas
  erosion_m_anio = round(abs(rnorm(n, mean = 1.8, sd = 0.9)), 2),
  # ^ Tasa de erosión costera en metros por año (retroceso de línea de costa).
  #   abs() convierte a valor absoluto porque la erosión no puede ser negativa.
  #   Media = 1.8 m/año es consistente con tasas reportadas en el Caribe.
  
  # → PROBLEMA 3: Intrusión salina
  salinidad_ppm = round(rnorm(n, mean = 450, sd = 150), 0),
  # ^ Concentración de cloruro en partes por millón (ppm) del acuífero.
  #   La OMS recomienda <250 ppm para agua potable.
  #   Media = 450 ppm indica que ya hay intrusión salina en la zona.
  #   round(..., 0) redondea a entero (las mediciones típicas son enteras).
  
  # → PROBLEMA 2: Intensificación de ciclones
  viento_max_kmh = round(rnorm(n, mean = 95, sd = 35), 0),
  # ^ Velocidad máxima de viento registrada en temporada ciclónica (km/h).
  #   Media = 95 km/h corresponde a una tormenta tropical.
  #   Un huracán categoría 1 empieza en 119 km/h; cat. 5 llega a ~280 km/h.
  
  # → PROBLEMA 5: Desplazamiento forzado
  familias_desplazadas = round(abs(rnorm(n, mean = 40, sd = 25))),
  # ^ Número acumulado de familias que tuvieron que reubicarse.
  #   abs() porque no puede haber familias desplazadas negativas.
  #   Referencia: en Cartí Sugdup fueron 285 familias en un solo evento.
  
  # → INDICADOR TRANSVERSAL: Pérdida económica
  perdida_econ_kusd = round(abs(rnorm(n, mean = 320, sd = 180)), 1),
  # ^ Pérdida económica estimada en miles de dólares (kUSD).
  #   Incluye daños a infraestructura, pérdida de producción pesquera,
  #   costos de reubicación, etc. Es un indicador agregado.
  
  
  # ─── BLOQUE C: VARIABLES CUALITATIVAS ────────────────────────────────────
  # En investigación mixta, las variables cualitativas capturan percepciones,
  # significados y experiencias que los números solos no pueden expresar.
  # Aquí simulamos respuestas típicas de encuestas y entrevistas.
  
  percepcion_riesgo = sample(
    c("Muy bajo", "Bajo", "Moderado", "Alto", "Muy alto", "No responde"),
    n, replace = TRUE,
    prob = c(0.03, 0.07, 0.20, 0.35, 0.30, 0.05)
    # ^ prob = define la probabilidad de cada opción. Noten que "Alto" y
    #   "Muy alto" suman 65%: la mayoría percibe riesgo alto, lo cual es
    #   coherente con comunidades que ya están viviendo los impactos.
    #   "No responde" (5%) simula la no-respuesta real en encuestas.
  ),
  # ^ Escala Likert de percepción comunitaria del riesgo climático.
  #   Las escalas Likert son ordinales: tienen un orden lógico pero la
  #   distancia entre categorías no es necesariamente igual.
  
  ecosistema_protector = sample(
    c("Manglar", "Arrecife coralino", "Pradera marina",
      "Humedal costero", "Ninguno identificado"),
    n, replace = TRUE,
    prob = c(0.30, 0.25, 0.15, 0.15, 0.15)
    # ^ Los manglares y arrecifes son los más frecuentes como barreras
    #   naturales en la región centroamericana y caribeña.
  ),
  # ^ Tipo de ecosistema que actúa como barrera natural contra oleaje,
  #   marejadas y erosión. Son los "amortiguadores" del Problema 4.
  
  estado_ecosistema = sample(
    c("Saludable", "Degradado", "Críticamente degradado",
      "En recuperación", "Sin datos"),
    n, replace = TRUE,
    prob = c(0.10, 0.35, 0.25, 0.15, 0.15)
    # ^ Solo 10% saludable; 60% degradado o críticamente degradado.
    #   "Sin datos" (15%) refleja la realidad de que muchas comunidades
    #   no tienen monitoreo formal de sus ecosistemas.
  ),
  # ^ Evaluación cualitativa del estado del ecosistema protector,
  #   típicamente basada en observación experta o comunitaria.
  
  narrativa_informante = sample(c(
    "El mar se come la playa cada año más rápido, ya perdimos tres casas",
    "Los pozos están salados, no podemos regar ni tomar esa agua",
    "Después del último huracán no quedó nada, tuvimos que empezar de cero",
    "La gente joven se va porque aquí ya no hay de qué vivir",
    "El manglar nos protegía pero lo cortaron para hacer camaroneras",
    "Nos dijeron que nos tenemos que ir pero no sabemos a dónde",
    "Cada temporada de lluvias el agua sube más, ya no es como antes",
    "Los arrecifes están blancos, los peces se fueron",
    "Pusimos sacos de arena pero el mar los arranca en una noche",
    NA_character_
    # ^ NA_character_ es un valor faltante de tipo texto.
    #   Simula que el informante no quiso responder o que no se pudo
    #   realizar la entrevista en esa visita de campo.
  ), n, replace = TRUE),
  # ^ Fragmentos de entrevistas con informantes clave locales.
  #   En investigación cualitativa, estas narrativas son DATOS tan válidos
  #   como los números. Capturan la experiencia vivida del cambio climático.
  
  estrategia_adaptacion = sample(
    c("Reubicación planificada", "Infraestructura de contención",
      "Restauración de ecosistemas", "Diversificación productiva",
      "Ninguna", "No sabe / No responde"),
    n, replace = TRUE
  )
  # ^ ¿Qué está haciendo la comunidad para enfrentar el problema?
  #   Esta variable permite analizar las respuestas adaptativas locales.
)

cat("→ Dataset base creado:", nrow(datos_costeros), "registros ×",
    ncol(datos_costeros), "variables.\n")
cat("→ Pero OJO: este dataset tiene PROBLEMAS DE CALIDAD que debemos corregir.\n")
cat("  Vamos a inyectarlos ahora de forma controlada...\n\n")


# --- 1.3 Inyección controlada de problemas de calidad -----------------------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 1.3 INYECCIÓN DE PROBLEMAS DE CALIDAD                                      │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ ¿POR QUÉ HACEMOS ESTO?                                                    │
│ En la vida real, los datos SIEMPRE llegan con problemas. Los instrumentos  │
│ fallan, los encuestadores cometen errores de digitación, los informantes   │
│ no responden, y los fenómenos extremos generan valores atípicos.           │
│                                                                            │
│ Vamos a inyectar 3 tipos de problemas:                                     │
│   a) Valores faltantes (NA) → simulan fallos de instrumentos/no-respuesta  │
│   b) Outliers extremos → simulan errores de digitación o eventos extremos  │
│   c) Inconsistencias de texto → simulan errores de captura en encuestas    │
│                                                                            │
│ NOTA PEDAGÓGICA: Como nosotros controlamos qué problemas inyectamos,       │
│ después podremos verificar si nuestro diagnóstico los detecta todos.       │
└──────────────────────────────────────────────────────────────────────────────┘
")

# a) Valores faltantes (NA) dispersos
# sample(1:n, 8) elige 8 posiciones aleatorias donde pondremos NA.
# En la realidad, esto pasa cuando un sensor se descalibra, cuando una
# estación de monitoreo se inunda, o cuando un encuestado se niega a responder.

indices_na_nivel   <- sample(1:n, 8)   # 8 NAs en nivel del mar
indices_na_sal     <- sample(1:n, 6)   # 6 NAs en salinidad
indices_na_erosion <- sample(1:n, 5)   # 5 NAs en erosión
indices_na_perdida <- sample(1:n, 4)   # 4 NAs en pérdida económica

datos_costeros$nivel_mar_mm[indices_na_nivel]       <- NA
datos_costeros$salinidad_ppm[indices_na_sal]        <- NA
datos_costeros$erosion_m_anio[indices_na_erosion]   <- NA
datos_costeros$perdida_econ_kusd[indices_na_perdida] <- NA

cat("→ a) Valores faltantes inyectados:\n")
cat("     nivel_mar_mm:      8 NAs (sensor no registró)\n")
cat("     salinidad_ppm:     6 NAs (pozo inaccesible)\n")
cat("     erosion_m_anio:    5 NAs (medición no realizada)\n")
cat("     perdida_econ_kusd: 4 NAs (dato no reportado)\n")
cat("     narrativa:         ~6 NAs (entrevista no realizada)\n\n")

# b) Outliers extremos
# Cada outlier tiene una HISTORIA diferente. Parte del trabajo del analista
# es distinguir si un valor extremo es un ERROR o un DATO REAL pero inusual.

datos_costeros$nivel_mar_mm[3]          <- 250
# ^ ERROR DE DIGITACIÓN: alguien escribió 250 en vez de 25.0 mm.
#   Esto es muy común cuando los datos se digitan manualmente.

datos_costeros$salinidad_ppm[15]        <- 5000
# ^ AMBIGUO: ¿Error o evento real? Una intrusión salina severa tras un
#   huracán PUEDE generar 5000 ppm. Pero también podría ser un error.
#   Aquí el conocimiento del dominio es crucial para decidir.

datos_costeros$erosion_m_anio[22]       <- 15.5
# ^ POSIBLE ERROR DE UNIDADES: ¿15.5 metros/año o 1.55 metros/año?
#   ¿Alguien reportó en centímetros pero el campo espera metros?
#   Sin embargo, en un evento extremo 15 m de retroceso es posible.

datos_costeros$viento_max_kmh[40]       <- 380
# ^ ERROR CLARO: 380 km/h es FÍSICAMENTE IMPOSIBLE en la Tierra.
#   El viento más rápido jamás registrado fue 407 km/h en un tornado.
#   Un huracán categoría 5 máximo alcanza ~280 km/h.

datos_costeros$familias_desplazadas[50] <- -12
# ^ ERROR LÓGICO: No pueden existir -12 familias desplazadas.
#   Los conteos son siempre ≥ 0. Probablemente un error de signo.

cat("→ b) Outliers inyectados:\n")
cat("     Registro  3: nivel_mar_mm = 250 (error digitación: debió ser 25.0)\n")
cat("     Registro 15: salinidad_ppm = 5000 (¿error o evento extremo real?)\n")
cat("     Registro 22: erosion_m_anio = 15.5 (¿error de unidades?)\n")
cat("     Registro 40: viento_max_kmh = 380 (imposible físicamente)\n")
cat("     Registro 50: familias_desplazadas = -12 (error lógico: negativo)\n\n")

# c) Inconsistencias en texto cualitativo
# En encuestas reales, diferentes digitadores escriben de diferente manera.
# "Alto" vs "alto" vs "ALTO" son la misma respuesta pero R las trata como
# tres categorías distintas si no las estandarizamos.

datos_costeros$percepcion_riesgo[c(5, 18)] <- c("alto", "MUY ALTO")
# ^ Mismas respuestas pero con mayúsculas/minúsculas inconsistentes.

datos_costeros$comunidad[7] <- "Carti Sugdup, Panama"
# ^ Mismo lugar pero sin tildes ni acentos. R lo trata como una
#   comunidad DIFERENTE a "Cartí Sugdup, Panamá".

cat("→ c) Inconsistencias de texto inyectadas:\n")
cat("     percepcion_riesgo: 'alto' y 'MUY ALTO' (debería ser 'Alto' y 'Muy alto')\n")
cat("     comunidad: 'Carti Sugdup, Panama' (sin acentos, debería ser 'Cartí Sugdup, Panamá')\n")

cat("\n✓ Dataset con problemas de calidad listo:", nrow(datos_costeros),
    "registros ×", ncol(datos_costeros), "variables.\n")
cat("  Ahora vamos a DIAGNOSTICAR estos problemas como lo haría con datos reales.\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  PARTE 2: DIAGNÓSTICO DE CALIDAD DE DATOS                                ║
# ╚════════════════════════════════════════════════════════════════════════════╝

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║  PASO 2: Diagnóstico de calidad                                            ║
╠══════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  ¿QUÉ ES UN DIAGNÓSTICO DE CALIDAD?                                       ║
║  Antes de analizar datos, SIEMPRE debemos revisar su calidad. Es como      ║
║  revisar los ingredientes antes de cocinar: si la harina tiene gorgojos,   ║
║  el resultado será malo sin importar la receta.                            ║
║                                                                            ║
║  Un diagnóstico de calidad responde 4 preguntas:                           ║
║  1. ¿Qué estructura tienen los datos? (tipos, dimensiones)                 ║
║  2. ¿Cuántos valores faltan y dónde? (patrón de NAs)                       ║
║  3. ¿Hay valores imposibles o sospechosos? (outliers)                      ║
║  4. ¿Las variables cualitativas son consistentes? (ortografía, categorías) ║
║                                                                            ║
║  REGLA DE ORO: Nunca analice datos que no ha diagnosticado primero.        ║
╚══════════════════════════════════════════════════════════════════════════════╝
")


# --- 2.1 Exploración estructural del dataset --------------------------------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 2.1 EXPLORACIÓN ESTRUCTURAL                                                │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ glimpse() es la primera función que debe ejecutar con datos nuevos.         │
│ Le muestra:                                                                │
│   - Cuántas filas (registros) y columnas (variables) tiene                 │
│   - El nombre de cada variable                                             │
│   - El TIPO de cada variable:                                              │
│       <dbl> = numérico con decimales (double)                              │
│       <int> = número entero (integer)                                      │
│       <chr> = texto/caracteres (character)                                 │
│       <lgl> = lógico: TRUE/FALSE (logical)                                 │
│       <fct> = factor: categoría con niveles definidos                      │
│   - Los primeros valores de cada variable                                  │
│                                                                            │
│ ¿POR QUÉ IMPORTA EL TIPO?                                                 │
│ Porque R trata diferente un "3" (texto) que un 3 (número).                 │
│ Si un número se leyó como texto, no podrá calcular promedios con él.       │
└──────────────────────────────────────────────────────────────────────────────┘
")

glimpse(datos_costeros)

cat("\n→ Observe los tipos <dbl>, <int>, <chr>. ¿Coinciden con lo esperado?\n")
cat("  - Las mediciones deben ser numéricas (dbl/int)\n")
cat("  - Las categorías y narrativas deben ser texto (chr)\n")

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ Ahora usamos summary() para ver estadísticas descriptivas rápidas.         │
│ Para variables numéricas muestra: mínimo, cuartiles, media, máximo y NAs.  │
│ Para variables de texto muestra: longitud y clase.                         │
│                                                                            │
│ PRESTE ATENCIÓN A:                                                         │
│   - Los valores mínimos y máximos (¿son plausibles?)                       │
│   - La cantidad de NAs por variable                                        │
│   - Si la media y la mediana son muy diferentes (señal de asimetría)       │
└──────────────────────────────────────────────────────────────────────────────┘
")

summary(datos_costeros)

cat("\n→ ¿Notó algo raro en el summary?\n")
cat("  Pistas: revise el máximo de nivel_mar_mm, viento_max_kmh,\n")
cat("  el mínimo de familias_desplazadas, y los conteos de NA.\n")


# --- 2.2 Análisis de valores faltantes (NA) ----------------------------------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 2.2 ANÁLISIS DE VALORES FALTANTES (NA)                                     │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ ¿QUÉ ES UN NA?                                                            │
│ NA significa 'Not Available' (No Disponible). Es la forma que tiene R      │
│ de representar un dato que FALTA. No es lo mismo que un cero:              │
│   - NA = 'no sabemos cuánto es' (el dato no existe)                        │
│   - 0  = 'sabemos que es cero' (el dato existe y vale 0)                   │
│                                                                            │
│ ¿POR QUÉ IMPORTA?                                                         │
│ 1. Muchas funciones de R devuelven NA si hay algún NA en los datos.        │
│    Ejemplo: mean(c(1, 2, NA)) devuelve NA, no 1.5.                        │
│    Por eso usamos na.rm = TRUE (remove NAs) en funciones estadísticas.     │
│                                                                            │
│ 2. Los NAs pueden ser INFORMATIVOS. Si todas las salinidades faltan en     │
│    Tela pero no en Iztapa, quizás Tela no tiene pozo de monitoreo.        │
│    El PATRÓN de NAs cuenta una historia.                                   │
│                                                                            │
│ ESTRATEGIA: Primero contamos cuántos hay, luego analizamos el patrón.     │
│                                                                            │
│ ★ AQUÍ USAMOS pivot_longer() POR PRIMERA VEZ ★                            │
│ Necesitamos esta función para transformar el resumen de NAs de formato     │
│ ancho (una columna por variable) a formato largo (una fila por variable).  │
│ Esto facilita filtrar y ordenar los resultados.                            │
└──────────────────────────────────────────────────────────────────────────────┘
")

cat("\n--- Conteo absoluto de NAs por variable ---\n\n")

# EXPLICACIÓN PASO A PASO de este bloque:
#
# 1. datos_costeros %>%
#    El operador %>% se llama 'pipe' (tubería). Toma el resultado de la
#    izquierda y lo pasa como primer argumento a la función de la derecha.
#    Se lee como: "toma datos_costeros Y LUEGO..."
#
# 2. summarise(across(everything(), ~ sum(is.na(.))))
#    summarise() resume datos. across() aplica una función a varias columnas.
#    everything() selecciona TODAS las columnas.
#    ~ sum(is.na(.)) es una función anónima (lambda) que:
#      - is.na(.) → pregunta "¿es NA?" para cada valor (TRUE/FALSE)
#      - sum()    → cuenta cuántos TRUE hay (TRUE = 1, FALSE = 0)
#    Resultado: una fila con el conteo de NAs de cada variable.
#
# 3. pivot_longer(cols = everything(), names_to = "variable", values_to = "n_faltantes")
#    TRANSFORMA de formato ancho a largo:
#    ANTES (ancho): | nivel_mar_mm | salinidad_ppm | erosion_m_anio | ...
#                   |      8       |       6       |        5       | ...
#    DESPUÉS (largo): | variable       | n_faltantes |
#                     | nivel_mar_mm   |      8      |
#                     | salinidad_ppm  |      6      |
#                     | erosion_m_anio |      5      |
#
#    ¿POR QUÉ HACEMOS ESTO?
#    Porque en formato largo podemos filter() para quedarnos solo con las
#    variables que tienen NAs, y arrange() para ordenar de mayor a menor.
#    En formato ancho esto sería mucho más difícil de hacer.
#
# 4. filter(n_faltantes > 0) → solo conserva variables con al menos 1 NA
# 5. arrange(desc(n_faltantes)) → ordena de más NAs a menos NAs

datos_costeros %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "variable",
    values_to = "n_faltantes"
  ) %>%
  filter(n_faltantes > 0) %>%
  arrange(desc(n_faltantes)) %>%
  print()

cat("\n→ Interprete: ¿cuáles variables tienen más datos faltantes?\n")
cat("  ¿Es un problema grave o manejable para el análisis?\n")

# Ahora calculamos la PROPORCIÓN (porcentaje) de NAs
cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ La proporción de NAs es más informativa que el conteo absoluto.             │
│ Regla general:                                                             │
│   < 5%  → probablemente no afecta el análisis                              │
│   5-20% → hay que evaluar si el patrón es aleatorio o sistemático          │
│   > 20% → cuidado: la variable puede no ser confiable para inferencia      │
└──────────────────────────────────────────────────────────────────────────────┘
")

cat("\n--- Proporción de NAs (%) por variable ---\n\n")

# La lógica es idéntica al bloque anterior, pero en vez de sum(is.na(.))
# usamos mean(is.na(.)) * 100:
#   mean() de TRUE/FALSE calcula la proporción (ej: 8/60 = 0.133)
#   * 100 lo convierte a porcentaje (13.3%)

datos_costeros %>%
  summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "variable",
    values_to = "pct_faltantes"
  ) %>%
  filter(pct_faltantes > 0) %>%
  arrange(desc(pct_faltantes)) %>%
  print()

cat("\n→ ¿Alguna variable supera el 20%? Si es así, ¿por qué podría ser?\n")

# Visualización gráfica de datos faltantes (descomentar para usar)
cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ OPCIONAL: Descomente las siguientes líneas para generar gráficos de NAs.   │
│ vis_miss() muestra un 'mapa de calor' donde cada celda del dataset se     │
│ pinta de negro (presente) o rojo (faltante). Permite ver si los NAs        │
│ están dispersos aleatoriamente o agrupados en ciertas filas/columnas.      │
└──────────────────────────────────────────────────────────────────────────────┘
")
# vis_miss(datos_costeros) +
#   labs(title = "Patrón de datos faltantes — Monitoreo costero CA-RD")
#
# gg_miss_var(datos_costeros) +
#   labs(title = "Cantidad de NAs por variable")


# --- 2.3 Detección de outliers -----------------------------------------------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 2.3 DETECCIÓN DE OUTLIERS (VALORES ATÍPICOS)                               │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ ¿QUÉ ES UN OUTLIER?                                                       │
│ Un valor que se aleja mucho del resto de los datos. Puede ser:             │
│   - Un ERROR (digitación, instrumento mal calibrado, unidades incorrectas) │
│   - Un DATO REAL pero inusual (un huracán categoría 5 es raro pero real)   │
│                                                                            │
│ ¿CÓMO LOS DETECTAMOS?                                                     │
│ Usaremos el método del RANGO INTERCUARTÍLICO (IQR):                        │
│   - Q1 = percentil 25 (25% de los datos están por debajo)                  │
│   - Q3 = percentil 75 (75% de los datos están por debajo)                  │
│   - IQR = Q3 - Q1 (el rango del 50% central de los datos)                 │
│   - Límite inferior = Q1 - 1.5 × IQR                                      │
│   - Límite superior = Q3 + 1.5 × IQR                                      │
│   - Todo lo que caiga fuera de estos límites es un outlier potencial       │
│                                                                            │
│ IMPORTANTE: Este método DETECTA candidatos, pero la DECISIÓN de qué        │
│ hacer con ellos requiere conocimiento del dominio (del tema).              │
│ No se borran outliers automáticamente: se investigan.                      │
└──────────────────────────────────────────────────────────────────────────────┘
")

# Primero creamos una función auxiliar reutilizable.
# En R, usted puede crear sus propias funciones con function().
# Esta función recibe un vector numérico (x) y devuelve TRUE/FALSE
# indicando cuáles valores son outliers según el método IQR.

detectar_outliers_iqr <- function(x, factor_iqr = 1.5) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)   # Primer cuartil
  q3  <- quantile(x, 0.75, na.rm = TRUE)   # Tercer cuartil
  iqr <- q3 - q1                             # Rango intercuartílico
  limite_inf <- q1 - factor_iqr * iqr       # Límite inferior
  limite_sup <- q3 + factor_iqr * iqr       # Límite superior
  return(x < limite_inf | x > limite_sup)    # TRUE si está fuera de límites
}

cat("→ Función detectar_outliers_iqr() creada.\n")
cat("  Apliquémosla a todas las variables cuantitativas...\n\n")

# Definimos cuáles son las variables cuantitativas a revisar
vars_cuanti <- c("nivel_mar_mm", "erosion_m_anio", "salinidad_ppm",
                 "viento_max_kmh", "familias_desplazadas", "perdida_econ_kusd")

cat("--- Conteo de outliers por variable ---\n\n")

# EXPLICACIÓN de este bloque:
# 1. summarise(across(...)) aplica la detección a cada variable cuantitativa
# 2. ~ sum(detectar_outliers_iqr(.), na.rm = TRUE) cuenta cuántos outliers hay
# 3. .names = "{.col}" mantiene los nombres originales de las columnas
# 4. pivot_longer() transforma a formato largo (igual que con los NAs)
# 5. arrange(desc(n_outliers)) ordena de más outliers a menos

outliers_resumen <- datos_costeros %>%
  summarise(across(
    all_of(vars_cuanti),
    ~ sum(detectar_outliers_iqr(.), na.rm = TRUE),
    .names = "{.col}"
  )) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "variable",
    values_to = "n_outliers"
  ) %>%
  arrange(desc(n_outliers))

print(outliers_resumen)

cat("\n→ Ahora inspeccionemos los outliers detectados para decidir qué hacer.\n")

# Inspección detallada de outliers específicos
cat("\n--- Registros con outliers en nivel_mar_mm ---\n")
cat("    (Recordemos: inyectamos un 250 mm en el registro 3)\n\n")

datos_costeros %>%
  filter(detectar_outliers_iqr(nivel_mar_mm)) %>%
  select(id_registro, comunidad, anio, nivel_mar_mm) %>%
  print()

cat("\n--- Registros con valores negativos en familias_desplazadas ---\n")
cat("    (Un conteo negativo es lógicamente IMPOSIBLE)\n\n")

datos_costeros %>%
  filter(familias_desplazadas < 0) %>%
  select(id_registro, comunidad, familias_desplazadas) %>%
  print()


# --- 2.4 Diagnóstico de consistencia cualitativa -----------------------------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 2.4 DIAGNÓSTICO DE CONSISTENCIA EN VARIABLES CUALITATIVAS                  │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ Los datos cualitativos tienen problemas diferentes a los numéricos:        │
│   - Mayúsculas/minúsculas inconsistentes: 'Alto' vs 'alto' vs 'ALTO'      │
│   - Acentos: 'Panamá' vs 'Panama'                                         │
│   - Sinónimos: 'Sin datos' vs 'No disponible' vs 'ND' vs NA               │
│   - Espacios extra: ' Alto ' vs 'Alto'                                    │
│                                                                            │
│ Para R, 'Alto' y 'alto' son DOS categorías diferentes.                     │
│ Esto infla artificialmente el número de categorías y genera errores        │
│ en tablas de frecuencia y gráficos.                                        │
│                                                                            │
│ HERRAMIENTA CLAVE: table() cuenta frecuencias de cada categoría.           │
│ Si vemos categorías que deberían ser iguales, hay que estandarizar.        │
└──────────────────────────────────────────────────────────────────────────────┘
")

cat("\n--- Frecuencias de percepción de riesgo ---\n")
cat("    (Busque categorías duplicadas por mayúsculas/minúsculas)\n\n")

table(datos_costeros$percepcion_riesgo, useNA = "ifany")
# useNA = "ifany" incluye los NA en el conteo. Sin esto, R los oculta.

cat("\n→ ¿Notó que aparecen 'alto', 'Alto', 'MUY ALTO' y 'Muy alto'?\n")
cat("  Son la misma respuesta pero R las cuenta como categorías distintas.\n")

cat("\n--- Valores únicos de comunidad ---\n")
cat("    (Busque la misma comunidad con diferente ortografía)\n\n")

sort(unique(datos_costeros$comunidad))
# unique() extrae valores distintos; sort() los ordena alfabéticamente
# Así es más fácil detectar visualmente duplicados como
# "Cartí Sugdup, Panamá" vs "Carti Sugdup, Panama"

cat("\n→ ¿Notó 'Carti Sugdup, Panama' (sin acentos) junto a la versión correcta?\n")
cat("  En datos reales, esto pasa constantemente con nombres geográficos.\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  PARTE 3: SCRIPT DE LIMPIEZA DE DATOS                                    ║
# ╚════════════════════════════════════════════════════════════════════════════╝

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║  PASO 3: Limpieza sistemática del dataset                                  ║
╠══════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  Ahora que sabemos QUÉ problemas tiene nuestro dataset, vamos a           ║
║  CORREGIRLOS de forma sistemática. La limpieza se hace en cadena usando    ║
║  el pipe (%>%) para que cada paso se aplique sobre el resultado del        ║
║  anterior. Esto crea un flujo REPRODUCIBLE y AUDITABLE.                    ║
║                                                                            ║
║  PRINCIPIOS DE LIMPIEZA:                                                   ║
║  1. Nunca modificar los datos originales → trabajamos sobre una COPIA      ║
║  2. Documentar cada decisión → ¿por qué corregimos así y no de otra forma?║
║  3. Ser conservador → si no estamos seguros, marcar en vez de borrar       ║
║  4. Combinar criterio estadístico con conocimiento del dominio             ║
║                                                                            ║
║  NOTA: Observe que empezamos con datos_costeros y guardamos el resultado   ║
║  en datos_limpios. Los datos originales quedan intactos.                   ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

datos_limpios <- datos_costeros %>%
  
  # ─── PASO 3.1: Estandarización de texto cualitativo ────────────────────────
  
  # mutate() MODIFICA columnas existentes o CREA nuevas.
  # No cambia el número de filas, solo transforma valores.
  mutate(
    
    # str_to_title() convierte texto a formato "Título":
    #   "alto"     → "Alto"
    #   "MUY ALTO" → "Muy Alto"
    #   "Bajo"     → "Bajo" (no cambia si ya está correcto)
    # Así unificamos todas las variantes de capitalización.
    percepcion_riesgo = str_to_title(percepcion_riesgo),
    
    # case_when() funciona como una serie de condiciones SI/ENTONCES:
    #   SI se detecta "carti" o "cartí" (ignorando mayúsculas) → corregir
    #   SI NINGUNA condición se cumple (TRUE ~) → dejar como está
    # str_detect() busca un patrón de texto; (?i) = ignorar mayúsculas
    comunidad = case_when(
      str_detect(comunidad, "(?i)carti|cartí") ~ "Cartí Sugdup, Panamá",
      TRUE ~ comunidad
    )
  ) %>%
  
  # ─── PASO 3.2: Tratamiento de outliers ─────────────────────────────────────
  
  mutate(
    
    # OUTLIER 1: nivel_mar_mm = 250 (registro 3)
    # DECISIÓN: Es claramente un error de digitación (x10).
    # ACCIÓN: Dividir entre 10 los valores mayores a 100 mm.
    # JUSTIFICACIÓN: 100 mm de aumento acumulado en una década excede
    # cualquier escenario IPCC plausible para la región.
    nivel_mar_mm = if_else(nivel_mar_mm > 100, nivel_mar_mm / 10, nivel_mar_mm),
    # if_else(condición, valor_si_verdadero, valor_si_falso)
    
    # OUTLIER 2: viento_max_kmh = 380 (registro 40)
    # DECISIÓN: Físicamente imposible. No hay forma de corregir porque
    # no sabemos el valor real.
    # ACCIÓN: Reemplazar por NA (dato faltante).
    # JUSTIFICACIÓN: Mejor un dato faltante que un dato falso.
    viento_max_kmh = if_else(viento_max_kmh > 320, NA_real_, viento_max_kmh),
    # NA_real_ es el NA específico para números decimales.
    # Usamos 320 km/h como umbral: máximo teórico de huracán cat. 5.
    
    # OUTLIER 3: familias_desplazadas = -12 (registro 50)
    # DECISIÓN: Error lógico evidente. Un conteo no puede ser negativo.
    # ACCIÓN: Reemplazar por NA.
    familias_desplazadas = if_else(
      familias_desplazadas < 0, NA_real_, familias_desplazadas
    ),
    
    # OUTLIER 4: erosion_m_anio = 15.5 (registro 22)
    # DECISIÓN: Ambiguo. Podría ser un error de unidades O un evento real.
    # ACCIÓN: NO corregir, pero marcar con un FLAG (bandera) para que
    # el investigador lo revise manualmente.
    # JUSTIFICACIÓN: Cuando hay duda, preservar el dato y señalarlo.
    flag_erosion_extrema = erosion_m_anio > 10
    # Esto crea una nueva columna TRUE/FALSE.
    # TRUE = "este registro requiere revisión manual"
    
    # NOTA: El outlier de salinidad (5000 ppm, registro 15) se deja para
    # el Ejercicio 2. Los estudiantes deben decidir qué hacer con él.
  ) %>%
  
  # ─── PASO 3.3: Recodificación de variables cualitativas ────────────────────
  
  mutate(
    
    # Convertir percepción de riesgo a FACTOR ORDENADO.
    # ¿Qué es un factor?
    # Un factor es un tipo de datos en R para variables categóricas.
    # A diferencia de texto simple (chr), un factor:
    #   - Tiene niveles (levels) definidos y un orden si lo especificamos
    #   - Permite operaciones como "Alto > Bajo" (imposible con texto)
    #   - Es necesario para muchos análisis estadísticos y gráficos
    #
    # ordered = TRUE crea un factor ORDINAL (con orden lógico):
    # Muy Bajo < Bajo < Moderado < Alto < Muy Alto
    percepcion_riesgo_ord = factor(
      percepcion_riesgo,
      levels  = c("Muy Bajo", "Bajo", "Moderado", "Alto", "Muy Alto",
                  "No Responde"),
      ordered = TRUE
    ),
    
    # Crear variable dicotómica (binaria: Sí/No) del estado del ecosistema.
    # ¿POR QUÉ? A veces necesitamos simplificar una variable con muchas
    # categorías para un análisis rápido o un modelo logístico.
    # "Degradado" y "Críticamente degradado" → "Sí" (en riesgo)
    # "Saludable" y "En recuperación" → "No" (no en riesgo crítico)
    # "Sin datos" → NA (no podemos clasificar sin información)
    ecosistema_en_riesgo = case_when(
      estado_ecosistema %in% c("Degradado", "Críticamente degradado") ~ "Sí",
      estado_ecosistema == "Sin datos" ~ NA_character_,
      TRUE ~ "No"
    ),
    
    # Extraer el nombre del PAÍS desde la variable comunidad.
    # str_extract() con expresión regular: busca texto después de ", "
    # (?<=, ) es un 'lookbehind': busca la posición justo después de ", "
    # .*$ captura todo hasta el final de la cadena
    # "Cartí Sugdup, Panamá" → "Panamá"
    # "Iztapa, Guatemala" → "Guatemala"
    pais = str_extract(comunidad, "(?<=, ).*$")
  )

cat("✓ Limpieza completada exitosamente.\n\n")

# --- Verificación post-limpieza ---

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ VERIFICACIÓN POST-LIMPIEZA                                                 │
│ Siempre verifique que la limpieza funcionó como esperaba.                  │
│ Compare el ANTES y el DESPUÉS para cada corrección que hizo.               │
└──────────────────────────────────────────────────────────────────────────────┘
")

cat("\n--- ANTES (sin limpiar): percepción de riesgo ---\n")
table(datos_costeros$percepcion_riesgo, useNA = "ifany")

cat("\n--- DESPUÉS (limpio): percepción de riesgo ---\n")
table(datos_limpios$percepcion_riesgo, useNA = "ifany")

cat("\n→ Observe: 'alto' y 'MUY ALTO' ya no aparecen como categorías separadas.\n")
cat("  Fueron unificadas a 'Alto' y 'Muy Alto' respectivamente.\n")

cat("\n--- ANTES: comunidades únicas ---\n")
sort(unique(datos_costeros$comunidad))

cat("\n--- DESPUÉS: comunidades únicas ---\n")
sort(unique(datos_limpios$comunidad))

cat("\n→ 'Carti Sugdup, Panama' fue corregida a 'Cartí Sugdup, Panamá'.\n")
cat("  Ahora tenemos 5 comunidades limpias en vez de 6.\n")

cat("\n--- VERIFICACIÓN: Variable 'pais' extraída correctamente ---\n")
table(datos_limpios$pais)

cat("\n→ Se extrajeron exitosamente los 5 países de la variable comunidad.\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  PARTE 4: TRANSFORMACIONES CON pivot_longer() Y pivot_wider()             ║
# ╚════════════════════════════════════════════════════════════════════════════╝

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║  PASO 4: Transformaciones de estructura con tidyr                          ║
╠══════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  ¿QUÉ SON pivot_longer() Y pivot_wider()?                                  ║
║                                                                            ║
║  Son funciones que CAMBIAN LA FORMA de una tabla sin perder información.    ║
║  Piénselas como doblar y desdoblar una hoja de datos:                      ║
║                                                                            ║
║  pivot_longer() = ALARGAR la tabla                                         ║
║    Toma varias columnas y las apila en DOS columnas:                       ║
║    una con el NOMBRE de la variable y otra con su VALOR.                   ║
║    → Más filas, menos columnas                                             ║
║    → Útil para: comparar indicadores, gráficos facetados, análisis grupal  ║
║                                                                            ║
║  pivot_wider() = ENSANCHAR la tabla                                        ║
║    Toma dos columnas (nombre y valor) y las despliega como columnas.       ║
║    → Menos filas, más columnas                                             ║
║    → Útil para: reportes, tablas de resumen, matrices de comparación       ║
║                                                                            ║
║  ¿POR QUÉ IMPORTA ESTO?                                                   ║
║  Porque diferentes análisis requieren diferentes FORMAS de la misma tabla. ║
║  Un gráfico facetado necesita datos largos. Un reporte necesita datos      ║
║  anchos. Saber transformar entre ambos formatos es una habilidad clave.    ║
║                                                                            ║
║     FORMATO ANCHO              →     FORMATO LARGO                         ║
║  ┌──────┬──────┬──────┐           ┌──────┬────────┬───────┐                ║
║  │ com. │ sal. │ eros.│           │ com. │ indic. │ valor │                ║
║  ├──────┼──────┼──────┤           ├──────┼────────┼───────┤                ║
║  │ Tela │  450 │  1.8 │   ────►   │ Tela │ sal.   │  450  │                ║
║  │ Izt. │  380 │  2.1 │           │ Tela │ eros.  │  1.8  │                ║
║  └──────┴──────┴──────┘           │ Izt. │ sal.   │  380  │                ║
║  (2 filas × 3 cols)               │ Izt. │ eros.  │  2.1  │                ║
║                                   └──────┴────────┴───────┘                ║
║                                   (4 filas × 3 cols)                       ║
╚══════════════════════════════════════════════════════════════════════════════╝
")


# --- 4.1 pivot_longer(): De ancho a largo con indicadores cuantitativos ------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 4.1 pivot_longer() CON INDICADORES CUANTITATIVOS                           │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ OBJETIVO: Queremos una tabla donde cada fila sea UNA medición de UN        │
│ indicador en UNA comunidad. Esto nos permitirá:                            │
│   - Comparar todos los indicadores en un mismo gráfico                     │
│   - Agrupar por tipo de problema                                           │
│   - Calcular estadísticas por indicador fácilmente                         │
│                                                                            │
│ ANATOMÍA DE pivot_longer():                                                │
│   pivot_longer(                                                            │
│     cols      = <qué columnas apilar>,                                     │
│     names_to  = <nombre de la nueva columna con los nombres>,              │
│     values_to = <nombre de la nueva columna con los valores>               │
│   )                                                                        │
│                                                                            │
│ Ejemplo mental:                                                            │
│   Si tenemos columnas: nivel_mar_mm, erosion_m_anio, salinidad_ppm         │
│   pivot_longer las convierte en:                                           │
│     indicador = c("nivel_mar_mm", "erosion_m_anio", "salinidad_ppm")       │
│     valor     = c(35.2, 1.8, 450)                                         │
└──────────────────────────────────────────────────────────────────────────────┘
")

datos_largo <- datos_limpios %>%
  
  # PASO 1: select() elige solo las columnas que necesitamos.
  # Incluimos los identificadores (id, comunidad, pais, anio, trimestre)
  # y las 6 variables cuantitativas.
  # ¿POR QUÉ? Porque pivot_longer() actúa sobre TODAS las columnas que
  # le indiquemos. Si dejamos las cualitativas, se apilarían también
  # y mezclaríamos texto con números (eso no tiene sentido).
  select(id_registro, comunidad, pais, anio, trimestre,
         nivel_mar_mm, erosion_m_anio, salinidad_ppm,
         viento_max_kmh, familias_desplazadas, perdida_econ_kusd) %>%
  
  # PASO 2: pivot_longer() transforma las 6 columnas de indicadores
  # en 2 columnas: "indicador" (nombre) y "valor" (número).
  # cols = nivel_mar_mm:perdida_econ_kusd usa el rango de columnas
  # (desde nivel_mar_mm hasta perdida_econ_kusd, inclusive).
  pivot_longer(
    cols      = nivel_mar_mm:perdida_econ_kusd,
    names_to  = "indicador",      # Aquí van los nombres: "nivel_mar_mm", etc.
    values_to = "valor"           # Aquí van los valores numéricos
  ) %>%
  
  # PASO 3: Agregar etiquetas legibles y mapear a problemas.
  # Los nombres de las columnas originales son técnicos ("nivel_mar_mm").
  # Creamos etiquetas humanas ("Nivel del mar (mm)") y vinculamos cada
  # indicador con el problema crítico al que corresponde.
  mutate(
    indicador_etiqueta = case_when(
      indicador == "nivel_mar_mm"         ~ "Nivel del mar (mm)",
      indicador == "erosion_m_anio"       ~ "Erosión costera (m/año)",
      indicador == "salinidad_ppm"        ~ "Salinidad acuífero (ppm)",
      indicador == "viento_max_kmh"       ~ "Viento máximo (km/h)",
      indicador == "familias_desplazadas" ~ "Familias desplazadas (n)",
      indicador == "perdida_econ_kusd"    ~ "Pérdida económica (kUSD)",
      TRUE ~ indicador
    ),
    problema_asociado = case_when(
      indicador == "nivel_mar_mm"         ~ "P1: Aumento nivel del mar",
      indicador == "erosion_m_anio"       ~ "P4: Pérdida de playas",
      indicador == "salinidad_ppm"        ~ "P3: Intrusión salina",
      indicador == "viento_max_kmh"       ~ "P2: Ciclones tropicales",
      indicador == "familias_desplazadas" ~ "P5: Desplazamiento forzado",
      indicador == "perdida_econ_kusd"    ~ "Transversal: Pérdida económica"
    )
  )

cat("→ Transformación completada.\n\n")
cat("  ANTES: ", nrow(datos_limpios), "filas × 6 indicadores en columnas separadas\n")
cat("  DESPUÉS:", nrow(datos_largo), "filas × 1 columna 'indicador' + 1 columna 'valor'\n")
cat("  Cálculo:", nrow(datos_limpios), "× 6 =", nrow(datos_limpios) * 6,
    "(cada fila se multiplicó por 6 indicadores)\n\n")

cat("--- Primeras 12 filas del formato largo ---\n\n")
datos_largo %>%
  slice_head(n = 12) %>%
  print(width = Inf)

cat("\n→ Note cómo los registros 1 y 2 ahora ocupan 6 filas cada uno\n")
cat("  (una por cada indicador). Los identificadores se repiten.\n")


# --- 4.2 pivot_wider(): De largo a ancho para tabla resumen ------------------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 4.2 pivot_wider() — TABLA RESUMEN POR COMUNIDAD Y AÑO                     │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ OBJETIVO: Crear una tabla donde cada fila sea una combinación              │
│ comunidad-año y cada columna sea la MEDIA de un indicador.                 │
│ Esta es la forma típica para un REPORTE o un DASHBOARD.                    │
│                                                                            │
│ ANATOMÍA DE pivot_wider():                                                 │
│   pivot_wider(                                                             │
│     names_from  = <columna cuyos valores se convertirán en nombres>,       │
│     values_from = <columna cuyos valores llenarán las nuevas columnas>     │
│   )                                                                        │
│                                                                            │
│ Flujo: datos_largo → agrupar → calcular medias → pivot_wider              │
│                                                                            │
│ Es la operación INVERSA a pivot_longer():                                  │
│   pivot_longer() : muchas columnas → 2 columnas (nombre + valor)           │
│   pivot_wider()  : 2 columnas (nombre + valor) → muchas columnas           │
└──────────────────────────────────────────────────────────────────────────────┘
")

tabla_resumen <- datos_largo %>%
  
  # PASO 1: Agrupar por comunidad, año e indicador.
  # group_by() define los grupos sobre los que calcularemos estadísticas.
  # Cada combinación única de (comunidad, anio, indicador_etiqueta) es un grupo.
  group_by(comunidad, anio, indicador_etiqueta) %>%
  
  # PASO 2: Calcular la media de cada grupo.
  # summarise() colapsa cada grupo a una sola fila con la estadística pedida.
  # na.rm = TRUE ignora los NAs (si no, cualquier grupo con NA daría NA).
  # .groups = "drop" elimina la agrupación después del cálculo.
  summarise(
    media = round(mean(valor, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  
  # PASO 3: pivot_wider() despliega los indicadores como columnas.
  # Cada valor único de indicador_etiqueta se convierte en una columna nueva.
  # Los valores de "media" llenan esas columnas.
  pivot_wider(
    names_from  = indicador_etiqueta,
    values_from = media
  ) %>%
  
  # PASO 4: Ordenar para facilitar la lectura.
  arrange(comunidad, anio)

cat("→ Tabla resumen creada:", nrow(tabla_resumen), "filas ×",
    ncol(tabla_resumen), "columnas.\n")
cat("  Cada fila = una comunidad en un año.\n")
cat("  Cada columna numérica = la media de un indicador.\n\n")

cat("--- Primeras 10 filas de la tabla resumen ---\n\n")
tabla_resumen %>%
  slice_head(n = 10) %>%
  print(width = Inf)

cat("\n→ Esta tabla ya se podría exportar como CSV para un informe o dashboard.\n")


# --- 4.3 pivot_longer() con datos cualitativos -------------------------------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 4.3 pivot_longer() CON VARIABLES CUALITATIVAS                              │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ pivot_longer() no es solo para números. También funciona con texto.         │
│                                                                            │
│ OBJETIVO: Apilar las 4 variables cualitativas (percepción de riesgo,       │
│ ecosistema protector, estado del ecosistema, estrategia de adaptación)     │
│ en formato largo para poder calcular frecuencias CRUZADAS de forma         │
│ eficiente usando count() y group_by().                                     │
│                                                                            │
│ ¿POR QUÉ ES ÚTIL?                                                         │
│ Si queremos saber cuál es la respuesta más frecuente en CADA dimensión     │
│ cualitativa, es más fácil hacer un solo count() sobre la tabla larga       │
│ que hacer 4 table() separados sobre cada variable.                         │
└──────────────────────────────────────────────────────────────────────────────┘
")

datos_cuali_largo <- datos_limpios %>%
  # Seleccionamos identificadores + las 4 variables cualitativas
  select(id_registro, comunidad, pais,
         percepcion_riesgo, ecosistema_protector,
         estado_ecosistema, estrategia_adaptacion) %>%
  # Apilamos las 4 cualitativas en 2 columnas:
  #   dimension_cualitativa = nombre de la variable original
  #   respuesta = el valor/texto de esa variable
  pivot_longer(
    cols      = percepcion_riesgo:estrategia_adaptacion,
    names_to  = "dimension_cualitativa",
    values_to = "respuesta"
  )

cat("→ Formato largo cualitativo:", nrow(datos_cuali_largo), "filas\n")
cat("  (", nrow(datos_limpios), "registros × 4 dimensiones =",
    nrow(datos_limpios) * 4, "filas)\n\n")

cat("--- Top 3 respuestas más frecuentes por dimensión ---\n\n")

# count() cuenta frecuencias de cada combinación de valores.
# group_by() + slice_head(n = 3) selecciona los 3 más frecuentes por grupo.
datos_cuali_largo %>%
  count(dimension_cualitativa, respuesta, sort = TRUE) %>%
  group_by(dimension_cualitativa) %>%
  slice_head(n = 3) %>%
  print(n = 20)

cat("\n→ Observe qué respuestas dominan en cada dimensión.\n")
cat("  ¿Es coherente que 'Alto' domine en percepción de riesgo?\n")
cat("  ¿Tiene sentido que 'Degradado' domine en estado del ecosistema?\n")


# --- 4.4 pivot_wider(): Matriz de co-ocurrencia cualitativa ------------------

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ 4.4 pivot_wider() — MATRIZ COMUNIDAD × PERCEPCIÓN DE RIESGO               │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ OBJETIVO: Crear una tabla de contingencia (cross-tabulation) donde:         │
│   Filas    = comunidades                                                   │
│   Columnas = niveles de percepción de riesgo                               │
│   Valores  = conteo de cuántas veces aparece cada combinación              │
│                                                                            │
│ Esto responde a: ¿hay diferencias en la percepción de riesgo entre        │
│ comunidades? ¿Cartí Sugdup (que ya fue reubicada) percibe más riesgo?     │
│                                                                            │
│ NOTA TÉCNICA: values_fill = 0 reemplaza los NAs en el conteo con ceros.   │
│ Sin esto, si una comunidad nunca reportó "Muy bajo", esa celda             │
│ aparecería como NA en vez de 0.                                            │
└──────────────────────────────────────────────────────────────────────────────┘
")

matriz_percepcion <- datos_limpios %>%
  # count() crea una tabla de frecuencia: ¿cuántas veces aparece cada
  # combinación de comunidad + percepción?
  count(comunidad, percepcion_riesgo) %>%
  # pivot_wider() despliega los niveles de percepción como columnas
  pivot_wider(
    names_from  = percepcion_riesgo,  # Cada nivel de percepción → columna
    values_from = n,                  # El conteo llena las celdas
    values_fill = 0                   # Donde no hay datos → poner 0
  )

cat("--- Matriz: Percepción de riesgo por comunidad ---\n\n")
print(matriz_percepcion, width = Inf)

cat("\n→ PARA DISCUTIR: ¿Las comunidades de los 3 casos documentados\n")
cat("  (Cartí Sugdup, Iztapa, Tela) muestran mayor percepción de riesgo\n")
cat("  que las otras? ¿Los datos cualitativos coinciden con la evidencia?\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  PARTE 5: EJERCICIOS PARA ESTUDIANTES                                    ║
# ╚════════════════════════════════════════════════════════════════════════════╝

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║  EJERCICIOS PARA PRACTICAR                                                 ║
║  Intente resolverlos antes de ver las soluciones al final del script.      ║
╠══════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║ EJERCICIO 1 — Diagnóstico de completitud:                                  ║
║   Usando datos_costeros (SIN limpiar), cuente cuántos registros tienen     ║
║   al menos UN valor faltante en cualquier variable cuantitativa.           ║
║   Pista: use rowSums(is.na(...)) o rowwise() + c_across()                  ║
║                                                                            ║
║ EJERCICIO 2 — Decisión sustantiva sobre outlier:                           ║
║   La variable 'salinidad_ppm' tiene un outlier de 5000 ppm (registro 15). ║
║   Investigue: ¿es plausible durante una intrusión salina severa?           ║
║   Decida si corregir, marcar con flag, o eliminar.                         ║
║   Escriba su justificación como comentario en el código.                   ║
║   Recuerde: la decisión debe basarse en conocimiento del dominio,          ║
║   no solo en criterios estadísticos.                                       ║
║                                                                            ║
║ EJERCICIO 3 — pivot_longer() selectivo:                                    ║
║   Transforme datos_limpios a formato largo incluyendo SOLO los             ║
║   indicadores de los Problemas 1, 3 y 5:                                   ║
║     - nivel_mar_mm (P1)                                                    ║
║     - salinidad_ppm (P3)                                                   ║
║     - familias_desplazadas (P5)                                            ║
║   Luego calcule la media por país y por problema.                          ║
║                                                                            ║
║ EJERCICIO 4 — pivot_wider() para reporte:                                  ║
║   Construya una tabla donde las FILAS sean los países y las COLUMNAS       ║
║   sean las estrategias de adaptación. Los valores deben ser el CONTEO      ║
║   de cuántas veces se reporta cada estrategia en cada país.                ║
║                                                                            ║
║ EJERCICIO 5 — Integración cuali-cuanti:                                    ║
║   Para cada nivel de percepción de riesgo (Muy Bajo a Muy Alto),           ║
║   calcule la MEDIANA de pérdida económica (perdida_econ_kusd).             ║
║   Use pivot_wider() para presentar el resultado como tabla de una fila.    ║
║   Pregunta analítica: ¿Coincide la percepción subjetiva de riesgo con     ║
║   la magnitud objetiva de la pérdida económica? ¿Por qué sí o no?         ║
║                                                                            ║
║ EJERCICIO 6 — Reflexión cualitativa (sin código):                          ║
║   Lea las narrativas de informantes clave (narrativa_informante).          ║
║   Clasifíquelas manualmente en categorías temáticas:                       ║
║     - Pérdida de medios de vida                                            ║
║     - Degradación ambiental                                                ║
║     - Desplazamiento y migración                                           ║
║     - Infraestructura y contención                                         ║
║     - Inseguridad hídrica                                                  ║
║   ¿Qué patrones emergen? ¿Cómo se conectan con los 5 problemas?           ║
║                                                                            ║
╚══════════════════════════════════════════════════════════════════════════════╝
")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  PARTE 6: SOLUCIONES (solo para el facilitador)                           ║
# ╚════════════════════════════════════════════════════════════════════════════╝

cat("
══════════════════════════════════════════════════════════════════════════════
  SOLUCIONES — Intente resolver los ejercicios antes de ver esto.
══════════════════════════════════════════════════════════════════════════════
")

# --- SOLUCIÓN Ejercicio 1 ---
cat("\n--- [SOLUCIÓN] Ejercicio 1: Registros con al menos 1 NA cuantitativo ---\n\n")

# rowSums(is.na(...)) cuenta cuántos NAs hay en CADA FILA.
# Luego filtramos las filas donde ese conteo es mayor a 0.
registros_con_na <- datos_costeros %>%
  filter(
    rowSums(is.na(select(., all_of(vars_cuanti)))) > 0
    # select(., all_of(vars_cuanti)) selecciona solo las columnas cuantitativas
    # is.na() convierte a TRUE/FALSE
    # rowSums() suma los TRUEs de cada fila
    # > 0 filtra filas con al menos un NA
  ) %>%
  nrow()

cat("Registros con al menos un NA en variables cuantitativas:", registros_con_na, "\n")
cat("Esto representa el", round(registros_con_na / n * 100, 1), "% del dataset.\n")

# --- SOLUCIÓN Ejercicio 4 ---
cat("\n--- [SOLUCIÓN] Ejercicio 4: Estrategia de adaptación por país ---\n\n")

datos_limpios %>%
  count(pais, estrategia_adaptacion) %>%
  pivot_wider(
    names_from  = estrategia_adaptacion,
    values_from = n,
    values_fill = 0
  ) %>%
  print(width = Inf)

# --- SOLUCIÓN Ejercicio 5 ---
cat("\n--- [SOLUCIÓN] Ejercicio 5: Mediana de pérdida × percepción ---\n\n")

datos_limpios %>%
  filter(percepcion_riesgo != "No Responde") %>%
  group_by(percepcion_riesgo) %>%
  summarise(mediana_perdida = median(perdida_econ_kusd, na.rm = TRUE)) %>%
  pivot_wider(
    names_from  = percepcion_riesgo,
    values_from = mediana_perdida
  ) %>%
  print(width = Inf)

cat("\n→ ANÁLISIS: Si la mediana de pérdida económica AUMENTA conforme\n")
cat("  aumenta la percepción de riesgo, hay coherencia cuali-cuanti.\n")
cat("  Si no, puede indicar que la percepción está influida por otros\n")
cat("  factores (memoria de desastres, medios de comunicación, etc.)\n")
cat("  y no solo por la experiencia económica directa.\n")


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║  NOTAS METODOLÓGICAS PARA LA DISCUSIÓN EN CLASE                          ║
# ╚════════════════════════════════════════════════════════════════════════════╝

cat("
┌──────────────────────────────────────────────────────────────────────────────┐
│ NOTAS PARA LA DISCUSIÓN EN CLASE                                           │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                            │
│ 1. DATO FALTANTE ≠ DATO SIN VALOR                                         │
│    En gestión costera, un NA en 'salinidad_ppm' puede significar que       │
│    el instrumento falló, que la estación se inundó, o que la comunidad     │
│    no tuvo acceso al pozo. El MECANISMO de generación del NA importa       │
│    para decidir cómo tratarlo (imputar, eliminar, o dejar como está).      │
│                                                                            │
│ 2. OUTLIERS: DECISIÓN SUSTANTIVA, NO SOLO ESTADÍSTICA                      │
│    Un viento de 380 km/h es físicamente imposible → error claro.           │
│    Pero 5000 ppm de salinidad PUEDE ser real en intrusión severa.          │
│    La limpieza requiere conocimiento del dominio, no solo de R.            │
│                                                                            │
│ 3. INTEGRACIÓN CUALI-CUANTI                                               │
│    Las narrativas de informantes clave COMPLEMENTAN los indicadores:       │
│    una erosión de 2 m/año es un número, pero 'el mar se comió tres        │
│    casas' es la experiencia vivida que da sentido al dato.                 │
│    En investigación mixta, ambas fuentes son igualmente válidas.           │
│                                                                            │
│ 4. pivot_longer() COMO HERRAMIENTA ANALÍTICA                              │
│    No es solo una transformación técnica: pasar de formato ancho a largo   │
│    permite COMPARAR indicadores heterogéneos en un mismo marco visual      │
│    y facilita el análisis FACETADO por problema o por comunidad.           │
│                                                                            │
│ 5. CASOS REALES COMO ANCLA PEDAGÓGICA                                     │
│    Cartí Sugdup (Panamá), Iztapa (Guatemala) y Tela (Honduras) son        │
│    casos documentados. Los datos simulados aquí reflejan ÓRDENES DE        │
│    MAGNITUD plausibles, pero NO son datos oficiales.                       │
│                                                                            │
│ Referencia regional: CEPAL-SICA (2024), Informe sobre cambio climático    │
│ y zonas costeras en Centroamérica y República Dominicana.                  │
└──────────────────────────────────────────────────────────────────────────────┘
")

cat("\n✓ Script del taller finalizado. ¡Buen análisis!\n")

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================