############################################################
# SIMULACIÓN + ANÁLISIS EN R DE 2 GRUPOS FOCALES (CUALI)
# Caso: erosión en Tela/Miami y por qué falló barrera de sacos
# Autor: (tu nombre)
# Nota IA: si lo usas en curso, regístralo en Bitácora IA
############################################################

# install.packages(c("tidyverse", "stringr", "tidyr", "purrr", "janitor"))
library(tidyverse)
library(stringr)
library(tidyr)
library(purrr)
library(janitor)

set.seed(8502)

############################################################
# 1) Definir actores y grupos focales (2 grupos)
############################################################
grupos <- tibble(
  grupo_id = c("GF1_comunidad", "GF2_gobernanza"),
  descripcion = c("Comunidad + turismo + pesca", "Municipalidad + técnicos + ambiente + ONG")
)

participantes <- tibble(
  participante_id = paste0("P", str_pad(1:12, 2, pad = "0")),
  actor = c(
    # GF1 (6)
    "Residente", "Residente", "Turismo", "Turismo", "Pesca", "Pesca",
    # GF2 (6)
    "Municipalidad", "Municipalidad", "Ambiente", "Ambiente", "ONG", "Area_Protegida"
  ),
  grupo_id = rep(c("GF1_comunidad", "GF2_gobernanza"), each = 6)
)

############################################################
# 2) Banco de “respuestas plausibles” por (actor x tema)
#    Incluye etiqueta CMO: Contexto / Mecanismo / Resultado
############################################################

# Familias de causas (lo que quieres mapear)
familias <- c("biofisica", "tecnica", "institucional", "social")

# Plantillas por familia (se mezclarán para simular diversidad)
plantillas <- list(
  biofisica = list(
    Contexto  = c("Las marejadas y el oleaje han aumentado en intensidad.",
                  "La corriente litoral se lleva la arena hacia el este.",
                  "Las tormentas recientes cambiaron el perfil de la playa."),
    Mecanismo = c("Los sacos se socavaron por debajo y el agua abrió canales.",
                  "La energía del oleaje rebotó y aceleró la pérdida de arena cerca.",
                  "El sistema no tenía aporte de sedimento suficiente para recuperarse."),
    Resultado = c("La playa quedó más angosta en semanas.",
                  "La línea de costa retrocedió pese a los sacos.",
                  "Se perdió arena y quedó expuesta infraestructura.")
  ),
  tecnica = list(
    Contexto  = c("La colocación no siguió un diseño ingenieril claro.",
                  "El material de los sacos se degradó rápido con sol y sal.",
                  "La barrera quedó discontinua y con puntos débiles."),
    Mecanismo = c("Los sacos se rompieron y se dispersó el relleno.",
                  "La barrera cambió el flujo y creó erosión a los lados.",
                  "Sin mantenimiento, cualquier obra temporal falla."),
    Resultado = c("Se volvió más costoso reponer sacos cada mes.",
                  "Hubo residuos y contaminación por geotextil/plástico.",
                  "La intervención no aguantó el primer temporal fuerte.")
  ),
  institucional = list(
    Contexto  = c("No había claridad de quién era responsable del mantenimiento.",
                  "La coordinación entre instituciones fue lenta.",
                  "El financiamiento fue reactivo, no planificado."),
    Mecanismo = c("Se actuó en emergencia sin monitoreo ni evaluación.",
                  "No se integró ordenamiento territorial con la intervención.",
                  "Los permisos y la gestión de obras se retrasaron."),
    Resultado = c("Se repitieron acciones de corto plazo sin cambiar la tendencia.",
                  "La gente perdió confianza en las soluciones propuestas.",
                  "No se consolidó un plan de adaptación a 5–10 años.")
  ),
  social = list(
    Contexto  = c("Hay presión por proteger negocios y casas frente al mar.",
                  "El turismo exige playa amplia, pero hay poco margen.",
                  "No todos los grupos se sienten incluidos en las decisiones."),
    Mecanismo = c("Algunas personas retiraron o movieron sacos para proteger su tramo.",
                  "Hubo conflictos por dónde colocar barreras y quién se beneficia.",
                  "La comunicación del riesgo no fue consistente."),
    Resultado = c("Se generaron tensiones entre vecinos y autoridades.",
                  "Aumentó la percepción de inseguridad económica.",
                  "Se priorizó lo inmediato sobre cambios estructurales.")
  )
)

# Ajustes “por actor”: pequeñas frases típicas (para simular estilos)
frases_actor <- list(
  Residente = c("Yo lo vi desde mi casa.", "Antes la playa era más ancha.", "Aquí la marea llega más arriba."),
  Turismo = c("Si la playa se reduce, baja la ocupación.", "La imagen del destino se afecta.", "Necesitamos soluciones visibles."),
  Pesca = c("Cambió el fondo y los sitios de pesca.", "El oleaje nos impide salir.", "La arena se mueve raro."),
  Municipalidad = c("Hay limitaciones presupuestarias.", "Se requiere priorizar obras.", "Necesitamos evidencias para justificar inversión."),
  Ambiente = c("Hay que integrar soluciones basadas en naturaleza.", "Se debe evaluar impactos ambientales.", "Se requiere enfoque de adaptación."),
  ONG = c("La participación comunitaria es clave.", "Se necesita transparencia.", "Hay que evitar soluciones que dañen ecosistemas."),
  Area_Protegida = c("Manglares y arrecifes amortiguan oleaje.", "El ecosistema está bajo presión.", "Hay que restaurar barreras naturales.")
)

############################################################
# 3) Generar “transcripciones” sintéticas (2 grupos)
############################################################
# Parámetros
n_intervenciones_por_participante <- 10  # total aprox: 12*10=120 intervenciones

simular_intervencion <- function(actor) {
  fam <- sample(familias, 1)
  cmo <- sample(c("Contexto", "Mecanismo", "Resultado"), 1)
  frase_base <- sample(plantillas[[fam]][[cmo]], 1)
  sello_actor <- sample(frases_actor[[actor]], 1)
  texto <- paste(sello_actor, frase_base)
  tibble(familia = fam, cmo = cmo, texto = texto)
}

datos_sim <- participantes %>%
  slice(rep(1:n(), each = n_intervenciones_por_participante)) %>%
  mutate(turno = rep(1:n_intervenciones_por_participante, times = nrow(participantes))) %>%
  rowwise() %>%
  mutate(tmp = list(simular_intervencion(actor))) %>%
  unnest(tmp) %>%
  ungroup() %>%
  left_join(grupos, by = "grupo_id") %>%
  mutate(doc_id = paste(grupo_id, participante_id,
                        paste0("T", str_pad(turno, width = 2, side = "left", pad = "0")),
                        sep = "_"))
# Vista rápida
datos_sim %>% select(doc_id, grupo_id, actor, cmo, familia, texto) %>% head(8)

############################################################
# 4) "Codificación asistida" por diccionario (simulada)
#    En la práctica, tú ajustarías estas palabras clave con tu codebook.
############################################################
diccionario <- list(
  # Familia biofísica
  biofisica = c("oleaje","marejada","marea","corriente","tormenta","perfil","sedimento","arena"),
  # Familia técnica
  tecnica = c("diseño","ingenier","geotext","plástico","romp","discontinua","mantenimiento","barrera"),
  # Familia institucional
  institucional = c("coordin","instituc","permiso","financia","plan","monitore","responsable","evaluación"),
  # Familia social
  social = c("conflict","vecin","particip","turismo","inclus","beneficia","tension","comunicación")
)

# Función para detectar familia por keywords (puede devolver múltiples)
detectar_codigos <- function(texto, dicc) {
  map_lgl(dicc, ~ str_detect(str_to_lower(texto), str_c(.x, collapse="|")))
}

codigos_long <- datos_sim %>%
  mutate(hit = map(texto, detectar_codigos, dicc = diccionario)) %>%
  mutate(hit = map(hit, ~ tibble(codigo = names(.x), presente = as.logical(.x)))) %>%
  unnest(hit) %>%
  filter(presente) %>%
  select(doc_id, grupo_id, actor, cmo, texto, codigo)

############################################################
# 5) Resultados: ¿qué “aprendes” con 2 grupos?
#    A) Frecuencia de códigos por grupo y por actor
############################################################
freq_grupo <- codigos_long %>%
  count(grupo_id, codigo, sort = TRUE) %>%
  group_by(grupo_id) %>%
  mutate(pct = n/sum(n)) %>%
  ungroup()

freq_actor <- codigos_long %>%
  count(actor, codigo, sort = TRUE)

freq_grupo
freq_actor

############################################################
# 6) Matriz CMO por grupo (Contexto–Mecanismo–Resultado)
############################################################
matriz_cmo <- codigos_long %>%
  count(grupo_id, cmo, codigo) %>%
  pivot_wider(names_from = cmo, values_from = n, values_fill = 0) %>%
  arrange(grupo_id, desc(Mecanismo + Resultado + Contexto))

matriz_cmo

############################################################
# 7) Indicadores rápidos para "¿sirve la metodología?"
#    A) Diversidad temática por grupo (Shannon simple)
############################################################
shannon <- function(p) { -sum(p * log(p), na.rm = TRUE) }

diversidad <- freq_grupo %>%
  group_by(grupo_id) %>%
  summarise(
    shannon = shannon(pct),
    n_total_codigos = sum(n),
    n_temas = n_distinct(codigo),
    .groups = "drop"
  )

diversidad

############################################################
# 8) Saturación (códigos nuevos conforme avanzas)
#    Aproximación: recorremos intervenciones y vemos aparición de códigos nuevos
############################################################
saturacion <- codigos_long %>%
  arrange(grupo_id, doc_id) %>%
  group_by(grupo_id) %>%
  mutate(idx = row_number()) %>%
  summarise(
    curva = list({
      cods <- codigo
      vistos <- character(0)
      nuevos <- integer(length(cods))
      acumulados <- integer(length(cods))
      for(i in seq_along(cods)) {
        if(!(cods[i] %in% vistos)) {
          nuevos[i] <- 1
          vistos <- c(vistos, cods[i])
        } else {
          nuevos[i] <- 0
        }
        acumulados[i] <- length(vistos)
      }
      tibble(paso=i <- seq_along(cods), nuevos=nuevos, acumulados=acumulados)
    }),
    .groups = "drop"
  ) %>%
  unnest(curva)

saturacion %>% group_by(grupo_id) %>% summarise(max_acumulados = max(acumulados))

############################################################
# 9) Visualizaciones (opcionales)
############################################################
# A) barras por grupo
ggplot(freq_grupo, aes(x = reorder(codigo, n), y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ grupo_id, scales = "free_y") +
  labs(x = "Código (familia)", y = "Frecuencia", title = "Temas detectados por grupo (diccionario)")

# B) curva de saturación
ggplot(saturacion, aes(x = paso, y = acumulados)) +
  geom_line() +
  facet_wrap(~ grupo_id) +
  labs(x = "Paso (avance de segmentos codificados)", y = "Códigos únicos acumulados",
       title = "Curva simple de saturación (simulada)")

############################################################
# 10) Exportar para reportar
############################################################
write_csv(datos_sim, "GF_simulados_transcripcion.csv")
write_csv(codigos_long, "GF_simulados_codigos_long.csv")
write_csv(matriz_cmo, "GF_simulados_matriz_CMO.csv")
write_csv(diversidad, "GF_simulados_diversidad.csv")

sessionInfo()
