# =============================================================================
# SP-8502 · Métodos Cuanti-Cuali con IA Responsable · GIACT UCR
# =============================================================================
# ARCHIVO : 12_analisis_cualitativo.R
# SPRINT  : 3 — Análisis Profundo y Modelado (Semanas 9–12)
# TEMA    : Codificación Cualitativa · Análisis Temático con R
# AUTOR   : [Nombre del estudiante] · Carné: [XXXXXXX]
# FECHA   : [DD/MM/YYYY]
# =============================================================================
# DESCRIPCIÓN:
#   Implementa en R el proceso de codificación cualitativa para los datos
#   textuales de entrevistas semiestructuradas (Fase II del diseño mixto).
#
#   Estrategia: Análisis Temático según Braun & Clarke (2006, 2019)
#   Pasos:
#     (1) Familiarización: estadísticos textuales básicos
#     (2) Códigos iniciales: diccionario de códigos inductivo-deductivo
#     (3) Construcción de temas: agrupación jerárquica de códigos
#     (4) Revisión de temas: saturación y validación cruzada
#     (5) Definición y nominación: descripción densa de cada tema
#     (6) Producción del informe: tabla de evidencias
#
#   Datos: Transcripciones sintéticas de 20 entrevistas semiestructuradas
#   (simuladas para cumplir la prohibición de datos reales en IA R-469-2025)
#
# NOTA SOBRE IA Y CODIFICACIÓN CUALITATIVA:
#   El Tutor IA puede SUGERIR códigos o patrones de frecuencia, pero
#   la interpretación del significado cultural es EXCLUSIVAMENTE humana.
#   Documentar en la Bitácora qué sugirió la IA vs. qué decidió el estudiante.
# =============================================================================

# ── 0. Cargar entorno ────────────────────────────────────────────────────────
if (!require("tidyverse")) source("../sprint1/00_setup_ambiente.R")

pkgs <- c("tidyverse","tidytext","wordcloud2","ggplot2","patchwork","igraph")
invisible(lapply(pkgs, function(p) {
  if (!require(p, character.only = TRUE, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/", quiet = TRUE)
  library(p, character.only = TRUE)
}))

set.seed(8502)

cat("=============================================================\n")
cat("  SP-8502 · SPRINT 3 · Análisis Cualitativo\n")
cat("  Codificación Temática · Entrevistas Pescadores CR\n")
cat("=============================================================\n\n")

# =============================================================================
# SECCIÓN 1: DATOS — TRANSCRIPCIONES SINTÉTICAS (n=20 entrevistas)
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("SECCIÓN 1: Corpus de Entrevistas (simulado · n=20)\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# Transcripciones sintéticas que representan los principales patrones
# temáticos esperados en comunidades pesqueras del Pacífico Central CR

corpus_raw <- tibble(
  id_entrevista  = paste0("E", str_pad(1:20, 2, pad = "0")),
  comunidad      = c("Quepos","Quepos","Quepos","Quepos",
                     "Tárcoles","Tárcoles","Tárcoles","Tárcoles","Tárcoles",
                     "Punta Morales","Punta Morales","Punta Morales",
                     "Jacó","Jacó","Jacó","Jacó",
                     "Herradura","Herradura","Herradura","Herradura"),
  años_exp       = c(25,12,30,18, 20,8,35,15,22, 28,10,40, 14,22,7,31, 19,26,11,33),
  sas_alto       = c(TRUE,FALSE,TRUE,FALSE, TRUE,FALSE,TRUE,FALSE,TRUE,
                     TRUE,FALSE,TRUE, FALSE,TRUE,FALSE,TRUE, TRUE,FALSE,TRUE,FALSE),
  texto_respuesta = c(
    # === QUEPOS: Acceso a ANP cercano · SAS mixto ===
    "Yo respeto las vedas porque si no lo hacemos nosotros mismos, nadie más lo va a hacer. El parque nos cuida el recurso, y nosotros debemos cuidar el parque. Mis hijos van a vivir de esto también.",
    "A mí no me han enseñado nada de pesca responsable. Lo del INCOPESCA es puro papel. Yo jalo lo que puedo porque si no lo agarro yo, lo agarra el barco industrial de allá afuera.",
    "En la cooperativa aprendimos que con aparejos selectivos mantenemos el stock. Cuesta más al principio pero a la larga sale mejor. Ahora reporto mis capturas porque sé que sirven de algo.",
    "El mar ya no es el mismo de antes. Cuando era niño había mero en cualquier parte. Ahora hay que ir más lejos y pescar más profundo. Pero no sé si es culpa nuestra o del calentamiento.",
    # === TÁRCOLES: Comunidad más alejada · Menor acceso institucional ===
    "Nosotros en Tárcoles estamos solos. No viene nadie a enseñarnos. Lo que sé de pesca responsable lo aprendí solo, mirando cómo se mueve el banco. La naturaleza te enseña si la escuchas.",
    "Mi familia lleva tres generaciones pescando acá. El mar nos ha dado todo. Pero ahora veo menos peces. Los jóvenes ya no quieren pescar porque no alcanza. Eso me preocupa más que las reglas del gobierno.",
    "Participé en un curso del CIMAR hace años. Ahí entendí lo del ciclo de vida del pargo. Desde entonces uso anzuelos más grandes para no agarrar juveniles. Mis compañeros se burlaron, pero ahora ellos hacen lo mismo.",
    "El ANP está lejos de aquí pero igual nos afecta. Dicen que no podemos pescar en ciertas zonas, pero esas zonas son donde siempre hemos pescado. No explican por qué.",
    "Para mí la pesca responsable es un lujo. Si no pesco suficiente esta semana, mis hijos no comen. Eso es la realidad que nadie quiere escuchar en los talleres.",
    # === PUNTA MORALES: Alta movilidad estacional ===
    "En temporada de lluvia me voy a Guanacaste. Aquí no hay suficiente. Pero donde voy sigo mis prácticas: no agarro lo que no debo. El recurso es de todos, no solo de los que viven en la costa.",
    "Nunca he ido a ningún taller. ¿Para qué? Sé pescar desde los ocho años. Lo que necesito es que los barcos extranjeros dejen de saquear fuera de las 12 millas. Eso destruye más que todo lo que yo pueda hacer.",
    "Mi abuelo me decía que hay que dejar algo para mañana. Eso es la pesca responsable para mí: no es un papel del gobierno, es una manera de vivir. Mis nietos van a poder pescar aquí si hacemos bien las cosas.",
    # === JACÓ: Turismo y pesca coexistiendo ===
    "En Jacó ya cambió todo. Los turistas quieren ver delfines y tortugas, no barcos. Eso nos ha obligado a cambiar. Ahora hago pesca deportiva también, con devolución. Se gana más y se cuida el recurso.",
    "Participo en el programa MARES porque me pagan el GPS y el seguro de la lancha. No voy a mentir. Pero cuando empecé a ver los datos de mis capturas, entendí para qué era. Ahora sí creo en el programa.",
    "La cooperativa me dijo que tenía que ir a un taller o me quitaban el beneficio. Fui por obligación. Pero el técnico explicó bien lo del pargo y la biología. Cambié mi manera de trabajar después.",
    "Hay pescadores que hacen turismo de día y pescan de noche. Dicen que así se puede. Pero yo vi cómo destruyeron un arrecife con trasmallo. Eso no está bien y lo dije en la reunión aunque no me gustaron las caras.",
    # === HERRADURA: Comunidad intermedia ===
    "En Herradura somos pocos pero estamos organizados. Tenemos grupo de WhatsApp donde compartimos dónde hay pesca y dónde no. También avisamos si alguien está incumpliendo las vedas.",
    "Yo fui a la Universidad una vez a presentar datos de mis capturas. Me pareció importante. Queremos que los científicos y los pescadores hablemos el mismo idioma. Eso ayuda a tomar mejores decisiones.",
    "Antes yo tiraba el plástico al mar como todos. Un chico del CIMAR nos mostró fotos de tortugas con bolsas en el estómago. Eso me cambió. Ahora recojo lo que puedo. No es suficiente pero es algo.",
    "El problema es que las reglas cambian sin consultarnos. De un año a otro cambia la veda, el tamaño mínimo, las zonas. No podemos planear nada. Eso desmotiva a los que queremos hacer bien las cosas."
  )
)

cat("Corpus cargado:", nrow(corpus_raw), "entrevistas\n")
cat("Comunidades:", paste(unique(corpus_raw$comunidad), collapse = ", "), "\n\n")

# =============================================================================
# SECCIÓN 2: PASO 1 — FAMILIARIZACIÓN (Estadísticos textuales)
# =============================================================================
cat("─────────────────────────────────────────────────────────────\n")
cat("PASO 1 · FAMILIARIZACIÓN — Estadísticos Textuales\n")
cat("─────────────────────────────────────────────────────────────\n\n")

corpus_tokens <- corpus_raw %>%
  mutate(n_palabras = str_count(texto_respuesta, "\\w+"),
         n_oraciones = str_count(texto_respuesta, "[.!?]+"))

cat("Estadísticos del corpus:\n")
cat("  Total palabras:", sum(corpus_tokens$n_palabras), "\n")
cat("  Palabras promedio por entrevista:",
    round(mean(corpus_tokens$n_palabras), 1), "\n")
cat("  Rango:", min(corpus_tokens$n_palabras), "–",
    max(corpus_tokens$n_palabras), "palabras\n\n")

# Tokenización y limpieza
stop_es <- c("que","de","en","el","la","los","las","un","una","unos",
             "y","a","no","me","se","lo","le","mi","su","es","son",
             "más","con","por","para","pero","si","ya","como","hay",
             "también","esto","ese","eso","esa","cuando","donde","ahora",
             "muy","todo","todos","nada","algo","así","bien","qué",
             "aquí","allá","van","tiene","tengo","fue","han","has")

palabras_freq <- corpus_raw %>%
  unnest_tokens(word, texto_respuesta) %>%
  filter(!word %in% stop_es, str_length(word) > 3) %>%
  count(word, sort = TRUE)

cat("TOP 20 palabras más frecuentes (excluyendo stopwords):\n")
print(head(palabras_freq, 20))

# Términos por comunidad
cat("\nTérminos clave por comunidad:\n")
corpus_raw %>%
  unnest_tokens(word, texto_respuesta) %>%
  filter(!word %in% stop_es, str_length(word) > 3) %>%
  count(comunidad, word, sort = TRUE) %>%
  group_by(comunidad) %>%
  slice_head(n = 5) %>%
  print()

# =============================================================================
# SECCIÓN 3: PASO 2–3 — CÓDIGOS INICIALES Y CONSTRUCCIÓN DE TEMAS
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASOS 2–3 · CÓDIGOS INICIALES → TEMAS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

# DICCIONARIO DE CÓDIGOS (inductivo-deductivo)
# Deductivos: basados en el marco teórico (MMR Sprint 1)
# Inductivos: emergidos de la primera lectura del corpus

diccionario_codigos <- tibble(
  codigo = c(
    # Tema 1: Conocimiento y Experiencia
    "CON-01","CON-02","CON-03",
    # Tema 2: Motivación e Identidad
    "MOT-01","MOT-02","MOT-03","MOT-04",
    # Tema 3: Acceso Institucional
    "INS-01","INS-02","INS-03","INS-04",
    # Tema 4: Tensión Económica vs. Sostenibilidad
    "TEN-01","TEN-02","TEN-03",
    # Tema 5: Gobernanza y Confianza
    "GOB-01","GOB-02","GOB-03"
  ),
  descripcion = c(
    "Conocimiento empírico de ciclos biológicos",
    "Aprendizaje intergeneracional (abuelos/padres)",
    "Formación técnica recibida (CIMAR, talleres)",
    "Motivación por herencia/futuros familiares",
    "Identidad cultural pesquera como freno/impulso",
    "Motivación instrumental (incentivos materiales)",
    "Cambio actitudinal post-capacitación",
    "Acceso geográfico a programas institucionales",
    "Desconfianza histórica con INCOPESCA",
    "Participación en redes colaborativas (coop., WA)",
    "Exposición a programas exitosos (MARES, CIMAR)",
    "Dilema: subsistencia inmediata vs. sostenibilidad",
    "Comparación con amenaza externa (barcos industriales)",
    "Adaptación a nuevas fuentes de ingreso (turismo)",
    "Percepción de reglas cambiantes e inconsistentes",
    "Demanda de co-gestión y consulta",
    "Valoración de comunicación ciencia-pesca"
  ),
  origen = c(rep("Deductivo",3),rep("Inductivo",4),
             rep("Deductivo",4),rep("Inductivo",3),rep("Inductivo",3)),
  tema_superior = c(
    rep("T1: Conocimiento y Experiencia", 3),
    rep("T2: Motivación e Identidad",     4),
    rep("T3: Acceso Institucional",        4),
    rep("T4: Tensión Económica",           3),
    rep("T5: Gobernanza y Confianza",      3)
  )
)

cat("Diccionario de códigos generado:\n")
print(diccionario_codigos %>%
        count(tema_superior, origen, name = "n_codigos"))

# MATRIZ DE CODIFICACIÓN (cada entrevista × cada código)
# En investigación real: este proceso es MANUAL + software (Atlas.ti, NVivo)
# Aquí se simula con lógica de palabras clave para cada código

codificar <- function(texto, palabras_clave) {
  any(str_detect(str_to_lower(texto),
                  paste(palabras_clave, collapse = "|")))
}

matriz_codigos <- corpus_raw %>%
  mutate(
    `CON-01` = codificar(texto_respuesta, c("biolog","ciclo","juvenil","banco","stock")),
    `CON-02` = codificar(texto_respuesta, c("abuelo","generaci","tradici","niño","padre")),
    `CON-03` = codificar(texto_respuesta, c("curso","taller","cimar","capacit","aprend")),
    `MOT-01` = codificar(texto_respuesta, c("hijo","nieto","futuro","generat","mañana")),
    `MOT-02` = codificar(texto_respuesta, c("identidad","cultura","manera de vivir","siempre hemos")),
    `MOT-03` = codificar(texto_respuesta, c("seguro","gps","pagan","beneficio","incentiv")),
    `MOT-04` = codificar(texto_respuesta, c("cambié","cambió","ahora sí","entendí","diferente")),
    `INS-01` = codificar(texto_respuesta, c("lejos","solos","no viene","acceso","distancia")),
    `INS-02` = codificar(texto_respuesta, c("incopesca","desconfianza","papel","no sirv")),
    `INS-03` = codificar(texto_respuesta, c("cooperativa","whatsapp","grupo","organizad","juntos")),
    `INS-04` = codificar(texto_respuesta, c("mares","cimar","programa","universid")),
    `TEN-01` = codificar(texto_respuesta, c("comen","subsistencia","alcanza","lujo","necesit")),
    `TEN-02` = codificar(texto_respuesta, c("industrial","barco","extranjero","saqueo","afuera")),
    `TEN-03` = codificar(texto_respuesta, c("turismo","deportiva","devolución","alternativa")),
    `GOB-01` = codificar(texto_respuesta, c("cambian","reglas","veda","consulta","sin preguntar")),
    `GOB-02` = codificar(texto_respuesta, c("co-gestión","decidir","nosotros","particip","voz")),
    `GOB-03` = codificar(texto_respuesta, c("científic","datos","investigad","lenguaje","universid"))
  )

cat("\nMatriz de codificación (primeras 6 entrevistas):\n")
matriz_codigos %>%
  select(id_entrevista, comunidad, starts_with("CON"), starts_with("MOT")) %>%
  head(6) %>%
  print()

# =============================================================================
# SECCIÓN 4: PASO 4 — FRECUENCIA Y SATURACIÓN DE CÓDIGOS
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASO 4 · REVISIÓN DE TEMAS — Frecuencia y Saturación\n")
cat("─────────────────────────────────────────────────────────────\n\n")

freq_codigos <- matriz_codigos %>%
  select(starts_with(c("CON","MOT","INS","TEN","GOB"))) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "codigo", values_to = "n_citas") %>%
  left_join(diccionario_codigos, by = "codigo") %>%
  mutate(
    pct_corpus     = round(n_citas / nrow(corpus_raw) * 100, 1),
    saturacion     = case_when(
      pct_corpus >= 50 ~ "✓ Saturado (≥50%)",
      pct_corpus >= 30 ~ "~ Emergiendo (30–49%)",
      TRUE             ~ "⚠ Raro (<30%)"
    )
  ) %>%
  arrange(tema_superior, desc(n_citas))

cat("Frecuencia de códigos por tema:\n")
print(freq_codigos %>%
        select(codigo, descripcion, n_citas, pct_corpus, saturacion,
               tema_superior))

# Gráfico de frecuencia de temas
freq_temas <- freq_codigos %>%
  group_by(tema_superior) %>%
  summarise(
    n_citas_tema  = sum(n_citas),
    n_codigos     = n(),
    .groups = "drop"
  ) %>%
  mutate(tema_superior = fct_reorder(tema_superior, n_citas_tema))

p_temas <- ggplot(freq_temas,
                   aes(x = n_citas_tema, y = tema_superior)) +
  geom_col(fill = "#2c7fb8", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(n_citas_tema, " citas")),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Frecuencia de Temas Cualitativos",
    subtitle = "Análisis temático · 20 entrevistas · Pacífico Central CR",
    x        = "Número de referencias codificadas",
    y        = NULL,
    caption  = "SP-8502 · Sprint 3 · 2026"
  )

ggsave("outputs/figuras/S3_frecuencia_temas.png",
       plot = p_temas, width = 9, height = 5, dpi = 300)

# =============================================================================
# SECCIÓN 5: PASO 5–6 — DEFINICIÓN DE TEMAS Y CITAS ILUSTRATIVAS
# =============================================================================
cat("\n─────────────────────────────────────────────────────────────\n")
cat("PASOS 5–6 · DEFINICIÓN DENSA Y CITAS ILUSTRATIVAS\n")
cat("─────────────────────────────────────────────────────────────\n\n")

temas_definidos <- tibble(
  Tema = c("T1","T2","T3","T4","T5"),
  Nombre = c(
    "Conocimiento situado y capital experiencial",
    "Identidad pesquera y motivación intergeneracional",
    "Mediación institucional: acceso desigual y confianza",
    "Tensión entre subsistencia y sostenibilidad",
    "Gobernanza legítima: co-gestión y diálogo ciencia-pesca"
  ),
  Descripcion_densa = c(
    "Los pescadores artesanales poseen un corpus de conocimiento ecológico
     local (CEL) construido generacionalmente. Este CEL coexiste con la
     formación técnica recibida y, cuando ambas se articulan, produce
     cambios de comportamiento duraderos. El CEL no es 'inferior' al
     conocimiento científico: es su complemento indispensable.",
    "La identidad como pescador artesanal actúa como freno o impulso
     según el contexto. La preocupación por las generaciones futuras
     ('mis hijos/nietos') emerge como motivador prosocial más potente
     que los incentivos materiales. El cambio actitudinal se produce
     cuando la formación técnica conecta con esta narrativa.",
    "El acceso a programas de extensión está geográficamente estratificado.
     Comunidades como Tárcoles y Punta Morales reportan aislamiento
     institucional. La desconfianza histórica con INCOPESCA constituye
     una barrera subjetiva significativa, independiente del acceso físico.",
    "La racionalidad de subsistencia inmediata domina bajo condiciones de
     precariedad económica. La sostenibilidad es percibida como 'lujo'
     cuando la seguridad alimentaria está comprometida. La amenaza de la
     pesca industrial actúa como narrativa desresponsabilizante que
     requiere ser abordada en los programas de extensión.",
    "Los pescadores demandan ser consultados en los cambios regulatorios.
     El modelo de co-gestión adaptativa, donde científicos y pescadores
     comparten datos y decisiones, genera legitimidad y adherencia.
     La comunicación ciencia-pesca en lenguaje accesible es valorada
     como transformadora cuando se experimenta."
  ),
  Cita_ilustrativa = c(
    '"Un chico del CIMAR nos mostró fotos... Eso me cambió." (E18, Herradura)',
    '"Mi abuelo me decía que hay que dejar algo para mañana. Eso es la pesca responsable para mí." (E12, Punta Morales)',
    '"En Tárcoles estamos solos. No viene nadie a enseñarnos." (E05, Tárcoles)',
    '"Para mí la pesca responsable es un lujo. Si no pesco suficiente esta semana, mis hijos no comen." (E09, Tárcoles)',
    '"Queremos que los científicos y los pescadores hablemos el mismo idioma." (E18, Herradura)'
  ),
  Conexion_cuantitativa = c(
    "T1 mediado en M3 por 'años_experiencia' (β* = ?)",
    "T2 no capturado cuantitativamente → gap analítico para tesis",
    "T3 operacionalizado en 'extension_pesquera' y 'distancia_anp_km'",
    "T4 operacionalizado en 'ingreso_mensual_usd' y su no-linealidad",
    "T5 sugiere variable omitida de gobernanza para modelo M3 ampliado"
  )
)

cat("TEMAS DEFINIDOS (Braun & Clarke 2006):\n\n")
for (i in 1:nrow(temas_definidos)) {
  cat(temas_definidos$Tema[i], ":", temas_definidos$Nombre[i], "\n")
  cat("  →", substr(temas_definidos$Descripcion_densa[i], 1, 80), "...\n")
  cat("  Cita:", temas_definidos$Cita_ilustrativa[i], "\n\n")
}

# Guardar resultados cualitativos
write_csv(diccionario_codigos, "datos/procesados/diccionario_codigos_s3.csv")
write_csv(freq_codigos,        "datos/procesados/frecuencia_codigos_s3.csv")
write_csv(temas_definidos,     "datos/procesados/temas_cualitativos_s3.csv")
write_csv(matriz_codigos,      "datos/procesados/matriz_codificacion_s3.csv")

cat("✓ Archivos cualitativos guardados en datos/procesados/\n")
cat("✓ Figura: outputs/figuras/S3_frecuencia_temas.png\n")
cat("  Siguiente: 13_bootstrap_montecarlo.R\n")
cat("=============================================================\n")
