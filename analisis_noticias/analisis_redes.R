# =============================================================================
# Análisis de red de TEMAS — Co-ocurrencia por párrafo
# =============================================================================
# Metodología: nodo = tema (string normalizado), edge = co-ocurrencia en párrafo,
# grafo no dirigido ponderado. Centralidades: strength (principal), betweenness
# en G_thr (umbral por percentil). Outputs: nodes_topics.csv, edges_topics.csv,
# global_metrics.json, PNG/PDF.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(igraph)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(writexl)
  library(jsonlite)
})

# Fuente principal: PostgreSQL noticias_chile (tabla noticias, 2018-2026). Ver db_noticias.R.
source("db_noticias.R")
CONFIG_PATH <- "config_search.json"
OUTPUT_DIR <- file.path(getwd(), "resultados_redes")
# Fallback si no se usa DB: corpus en Excel/CSV
CORPUS_DIR <- file.path(getwd(), "corpus_filtrado")
CORPUS_CSV_DIR <- file.path(getwd(), "corpus_noticias")
CORPUS_CSV_PATTERN <- "^(noticias_chile|noticias_datamedios).*\\.csv$"
CORPUS_PATTERN <- "^corpus_filtrado.*\\.xlsx$"

# Parámetros base
N_MIN_ACTOR <- 5L        # Mínimo de menciones para incluir actor
N_MIN_TEMA <- 5L         # Mínimo de apariciones para incluir tema
TOP_N <- 20L             # Top-N para rankings
PESO_MIN_ARISTA <- 12L   # Peso mínimo para G_full (opción B: grafo más claro)
MAX_NODOS_GRAFICO <- 25L # Límite de nodos en el gráfico (opción B: menos nodos, más legible)

# Umbralización para betweenness (percentil sobre pesos: conservar top edges)
EDGE_WEIGHT_PERCENTILE <- 0.93   # Top 7% de aristas por peso (opción B)
EDGE_WEIGHT_PERCENTILE_MIN <- 0.85  # Mínimo si G_thr queda muy fragmentado
GIANT_COMPONENT_RATIO_MIN <- 0.3   # Si componente gigante < 30% nodos, bajar percentil

# =============================================================================
# 1. LIMPIEZA DE TEXTO (CRÍTICO para eliminar HTML)
# =============================================================================

limpiar_html <- function(texto) {
  if (is.na(texto) || nchar(texto) < 5) return("")
  
  # Eliminar etiquetas HTML completas
  texto <- gsub("<[^>]+>", " ", texto, perl = TRUE)
  
  # Eliminar entidades HTML
  texto <- gsub("&[a-z]+;", " ", texto, ignore.case = TRUE)
  texto <- gsub("&#[0-9]+;", " ", texto)
  
  # Eliminar fragmentos HTML residuales
  texto <- gsub("</?(div|p|a|span|strong|em|script|style|figure|audio|video)[^>]*>?", " ", texto, ignore.case = TRUE, perl = TRUE)
  
  # Eliminar patrones específicos problemáticos
  texto <- gsub("(endif|controlslist|nodownload|también|lee también)", " ", texto, ignore.case = TRUE)
  
  # Normalizar espacios
  texto <- gsub("\\s+", " ", texto)
  texto <- trimws(texto)
  
  texto
}

# Normalización de temas (lowercase, strip, sin dobles espacios)
normalizar_tema <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\\s+", " ", x)
  x[x == ""] <- NA_character_
  x
}

# Umbralización: conservar edges con weight >= percentil (top edges por peso)
threshold_graph_by_weight_percentile <- function(g_full, p = EDGE_WEIGHT_PERCENTILE) {
  if (vcount(g_full) == 0 || ecount(g_full) == 0) return(g_full)
  w <- E(g_full)$weight
  if (is.null(w)) return(g_full)
  thresh <- quantile(w, probs = p, na.rm = TRUE)
  g_thr <- delete_edges(g_full, E(g_full)[weight < thresh])
  # Eliminar nodos aislados
  g_thr <- induced_subgraph(g_thr, degree(g_thr) > 0)
  g_thr
}

# =============================================================================
# 2. Preparación del corpus CON LIMPIEZA
# =============================================================================

cargar_corpus <- function(ruta = NULL, n_muestra = NULL, desde = "2018-01-01", hasta = "2026-12-31", preferir_corpus_filtrado = TRUE) {
  # Si no se pasa ruta: usar corpus filtrado reciente (p. ej. del pipeline) o cargar desde DB
  if (is.null(ruta)) {
    ruta_filtrado <- getOption("noticias_ultimo_corpus_filtrado")
    if (preferir_corpus_filtrado && !is.null(ruta_filtrado) && file.exists(ruta_filtrado)) {
      cat("Usando corpus filtrado (pipeline):", basename(ruta_filtrado), "\n")
      ruta <- ruta_filtrado
    } else if (preferir_corpus_filtrado && dir.exists(CORPUS_DIR)) {
      archivos <- list.files(CORPUS_DIR, pattern = CORPUS_PATTERN, full.names = TRUE)
      if (length(archivos) > 0L) {
        ruta <- archivos[order(file.info(archivos)$mtime, decreasing = TRUE)[1L]]
        cat("Usando corpus filtrado más reciente:", basename(ruta), "\n")
      }
    }
    if (is.null(ruta)) {
      cat("Cargando corpus desde PostgreSQL (noticias_chile, ", desde, " a ", hasta, ")...\n", sep = "")
      df <- cargar_noticias_db(conn = NULL, desde = desde, hasta = hasta)
      if (nrow(df) == 0L) stop("No hay noticias en la base en ese rango. Revisa la conexión y fechas.")
    }
  }
  if (!is.null(ruta)) {
    from_csv <- grepl("\\.csv$", ruta, ignore.case = TRUE)
    cat("[1/5] Leyendo archivo (puede tardar en Excel muy grande):", basename(ruta), "\n")
    if (from_csv) {
      df <- read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
      df <- as.data.frame(df)
      nombres_esperados <- c("titulo", "fecha", "medio", "autor", "resumen", "contenido", "contenido_limpio", "url", "url_imagen", "temas", "ID")
      for (n in names(df)) {
        if (!n %in% nombres_esperados && tolower(n) %in% c("titulo", "fecha", "medio", "autor", "resumen", "contenido", "url", "temas", "id"))
          names(df)[names(df) == n] <- tolower(n)
      }
    } else {
      # Lectura más rápida: tipo texto evita guessing costoso en archivos grandes
      df <- read_excel(ruta, sheet = 1, col_types = "text")
      df <- as.data.frame(df)
      nombre_a_interno <- c(
        "Título" = "titulo", "Fecha" = "fecha", "Medio" = "medio", "Autor" = "autor",
        "Resumen / Bajada" = "resumen", "Contenido" = "contenido",
        "Contenido limpio" = "contenido_limpio", "URL" = "url", "URL imagen" = "url_imagen",
        "Temas" = "temas", "ID" = "ID"
      )
      for (i in seq_len(ncol(df))) {
        if (names(df)[i] %in% names(nombre_a_interno))
          names(df)[i] <- nombre_a_interno[names(df)[i]]
      }
    }
    if (!"id_noticia" %in% names(df)) {
      if ("ID" %in% names(df)) {
        df$id_noticia <- paste0("n_", df$ID)
      } else {
        df$id_noticia <- paste0("n_", seq_len(nrow(df)))
      }
    }
  }
  
  # Limpieza de HTML (puede tardar si hay muchas filas)
  n_filas <- nrow(df)
  cat("[2/5] Limpiando HTML (", n_filas, " noticias, 3 columnas)...\n", sep = "")
  for (col in c("contenido", "titulo", "resumen")) {
    if (col %in% names(df))
      df[[col]] <- vapply(df[[col]], limpiar_html, character(1), USE.NAMES = FALSE)
  }
  cat("[3/5] Construyendo texto completo y párrafos...\n")
  cols_texto <- intersect(c("titulo", "resumen", "contenido"), names(df))
  df$texto_completo <- apply(df[, cols_texto, drop = FALSE], 1L, function(x) {
    paste(na.omit(as.character(x)), collapse = " ")
  })
  df$parrafos <- lapply(df$texto_completo, function(t) {
    p <- str_split(t, "\\n+|(?<=[.!?])\\s+")[[1]]
    p <- trimws(p[nchar(p) > 20])
    if (length(p) == 0) trimws(t) else p
  })
  cat("[4/5] Eliminando duplicados por texto...\n")
  df <- df[!duplicated(df$texto_completo), ]
  
  if (!is.null(n_muestra) && n_muestra > 0 && nrow(df) > n_muestra) {
    df <- df[seq_len(n_muestra), , drop = FALSE]
    cat("[5/5] Corpus (MUESTRA TEST):", nrow(df), "noticias\n")
  } else {
    cat("[5/5] Corpus listo:", nrow(df), "noticias únicas (tras limpieza)\n")
  }
  invisible(df)
}

# =============================================================================
# 3. FILTRADO ULTRA-AGRESIVO DE ACTORES
# =============================================================================

# Lista negra EXPANDIDA de ruido
RUIDO_COMPLETO <- c(
  # HTML y metadata
  "Div", "Script", "Audio", "Video", "Strong", "Endif", "Figure", "Controlslist", "Nodownload",
  # Días de la semana
  "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo",
  # Meses
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto",
  "Septiembre", "Octubre", "Noviembre", "Diciembre",
  # Lugares genéricos
  "Chile", "Santiago", "Valparaíso", "Viña", "Mar", "Región", "Comuna", "Barrio",
  "Población", "Villa", "Sector", "Norte", "Sur", "Este", "Oeste", "Centro",
  # Términos temporales
  "Año", "Día", "Hoy", "Ayer", "Semana", "Mes", "País", "Ciudad", "Zona",
  # Verbos y conectores que podrían ser capitalizados
  "Lee", "También", "Ver", "Más", "Información", "Aquí", "Ahora", "Antes",
  # Términos de navegación web
  "Compartir", "Comentar", "Imprimir", "Enviar", "Guardar", "Descargar"
)

es_ruido_html <- function(texto) {
  # Detectar fragmentos HTML residuales
  grepl("[<>/]|&[a-z]+;|&#[0-9]+;", texto, perl = TRUE)
}

es_ruido_temporal <- function(texto) {
  # Detectar días, meses, años
  grepl(paste(RUIDO_COMPLETO, collapse = "|"), texto, ignore.case = TRUE)
}

es_muy_corto_o_muy_largo <- function(texto) {
  nchar(texto) < 3 | nchar(texto) > 50
}

tiene_numeros_dominantes <- function(texto) {
  # Si más del 30% son números, probablemente es basura
  nums <- str_count(texto, "[0-9]")
  chars <- nchar(texto)
  if (chars == 0) return(TRUE)
  (nums / chars) > 0.3
}

extraer_actores_udpipe_limpio <- function(textos, doc_ids, model_path = NULL) {
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("Instala udpipe: install.packages('udpipe')")
  }
  
  modelo <- NULL
  if (is.null(model_path)) {
    modelos_disponibles <- list.files(getwd(), pattern = "spanish.*\\.udpipe$", full.names = TRUE)
    if (length(modelos_disponibles) > 0) {
      model_path <- modelos_disponibles[1]
      cat("Usando modelo existente:", basename(model_path), "\n")
    } else {
      cat("Descargando modelo español de udpipe...\n")
      dl <- udpipe::udpipe_download_model(language = "spanish")
      model_path <- dl$file_model
    }
  }
  
  modelo <- udpipe::udpipe_load_model(model_path)
  cat("Anotando texto para NER (limpio)...\n")
  
  textos <- as.character(textos)
  textos[is.na(textos) | nchar(textos) < 5] <- "."
  ids <- as.character(doc_ids)
  
  # Procesar en lotes
  batch_size <- 350L
  n <- length(textos)
  lotes <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  anot_list <- list()
  for (i in seq_along(lotes)) {
    idx <- lotes[[i]]
    anot_list[[i]] <- as.data.frame(udpipe::udpipe_annotate(modelo, x = textos[idx], doc_id = ids[idx]))
    if (length(lotes) > 1) cat("  Lote", i, "/", length(lotes), "\n")
  }
  anot <- bind_rows(anot_list)
  
  # Extraer secuencias de PROPN
  anot$upos[is.na(anot$upos)] <- ""
  propn <- anot %>% filter(upos == "PROPN")
  propn$sentence_id <- as.numeric(as.character(propn$sentence_id))
  propn$token_id <- as.numeric(as.character(gsub("-.*", "", propn$token_id)))
  propn <- propn %>% filter(!is.na(sentence_id), !is.na(token_id)) %>% arrange(doc_id, sentence_id, token_id)
  
  actores <- propn %>%
    group_by(doc_id) %>%
    mutate(grp = cumsum(c(1, diff(sentence_id) != 0 | diff(token_id) > 2))) %>%
    group_by(doc_id, grp) %>%
    summarise(actor = paste(token, collapse = " "), .groups = "drop") %>%
    mutate(actor = trimws(actor), actor = str_to_title(actor)) %>%
    filter(nchar(actor) > 1)
  
  cat("Actores brutos extraídos:", nrow(actores), "\n")
  
  # FILTRADO ULTRA-AGRESIVO
  actores <- actores %>%
    filter(
      !es_ruido_html(actor),
      !es_ruido_temporal(actor),
      !es_muy_corto_o_muy_largo(actor),
      !tiene_numeros_dominantes(actor)
    )
  
  cat("Actores tras filtrado de ruido:", nrow(actores), "\n")
  
  # Filtrado semántico adicional
  instituciones <- c("Gobierno", "Ministerio", "Armada", "Carabineros", "PDI",
                     "Fiscalía", "Municipalidad", "Senado", "Cámara", "Corte",
                     "Congreso", "Contraloría", "Defensoría", "Intendencia",
                     "Seremi", "Superintendencia", "Tribunal", "Juzgado")
  
  actores <- actores %>%
    mutate(
      es_institucion = grepl(paste(instituciones, collapse = "|"), actor, ignore.case = TRUE),
      # Persona: Nombre + Apellido (sin artículos al inicio)
      es_persona = grepl("^[A-ZÁÉÍÓÚÑ][a-záéíóúñ]+ [A-ZÁÉÍÓÚÑ][a-záéíóúñ]+", actor) & 
        !grepl("^(El|La|Los|Las|Un|Una|De|Del|Para|Por|Con|Sin|Sobre|Entre) ", actor, ignore.case = TRUE)
    ) %>%
    filter(es_institucion | es_persona) %>%
    select(doc_id, actor)
  
  cat("Actores finales (instituciones + personas):", nrow(actores), "\n")
  actores
}

normalizar_actor <- function(x) {
  x <- str_to_title(trimws(x))
  canonico <- c(
    "Gabriel Boric" = "Boric", "Presidente Boric" = "Boric",
    "Michelle Bachelet" = "Bachelet", "Presidenta Bachelet" = "Bachelet",
    "Sebastián Piñera" = "Piñera", "Presidente Piñera" = "Piñera",
    "Carabineros De Chile" = "Carabineros", "Carabineros" = "Carabineros",
    "Policía De Investigaciones" = "PDI", "PDI" = "PDI",
    "Gobierno De Chile" = "Gobierno", "Gobierno" = "Gobierno",
    "Armada De Chile" = "Armada", "Marina De Chile" = "Armada",
    "Fuerzas Armadas" = "FFAA", "Fuerzas Armadas De Chile" = "FFAA"
  )
  for (k in names(canonico)) {
    x[grepl(k, x, fixed = TRUE)] <- canonico[k]
  }
  x
}

construir_red_actores <- function(df, usar_udpipe = TRUE) {
  cat("\n--- Red de ACTORES (LIMPIA) ---\n")
  
  if (usar_udpipe && requireNamespace("udpipe", quietly = TRUE)) {
    actores <- tryCatch(
      extraer_actores_udpipe_limpio(df$texto_completo, df$id_noticia),
      error = function(e) {
        cat("udpipe falló:", conditionMessage(e), "\n")
        cat("Intenta instalar udpipe o usa usar_udpipe = FALSE\n")
        return(data.frame(doc_id = character(), actor = character()))
      }
    )
  } else {
    cat("ADVERTENCIA: Usar udpipe es ALTAMENTE recomendado para este corpus.\n")
    cat("Sin udpipe, el ruido será mayor.\n")
    return(list(g = make_empty_graph(), actores_df = data.frame()))
  }
  
  if (nrow(actores) == 0) {
    warning("No se extrajeron actores. Verifica el corpus.")
    return(list(g = make_empty_graph(), actores_df = actores))
  }
  
  actores$actor <- normalizar_actor(actores$actor)
  actores <- actores %>% filter(nchar(actor) > 2)
  
  freq <- actores %>% count(actor, name = "menciones") %>% filter(menciones >= N_MIN_ACTOR)
  actores_validos <- freq$actor
  actores <- actores %>% filter(actor %in% actores_validos)
  
  cat("Actores únicos válidos:", length(actores_validos), "\n")
  
  # Co-ocurrencia en noticia
  pares_list <- lapply(unique(actores$doc_id), function(doc) {
    acts <- actores$actor[actores$doc_id == doc]
    if (length(acts) < 2) return(NULL)
    pr <- combn(acts, 2)
    data.frame(a1 = pr[1, ], a2 = pr[2, ], stringsAsFactors = FALSE)
  })
  pares <- bind_rows(pares_list)
  
  if (is.null(pares) || nrow(pares) == 0) {
    cat("No hay pares de actores co-ocurriendo.\n")
    return(list(g = make_empty_graph(), actores_df = actores))
  }
  
  pesos <- pares %>% count(a1, a2, name = "peso") %>% filter(peso >= PESO_MIN_ARISTA)
  pesos <- pesos %>% filter(a1 %in% actores_validos, a2 %in% actores_validos)
  
  if (nrow(pesos) == 0) {
    cat("No hay aristas con peso suficiente.\n")
    return(list(g = make_empty_graph(), actores_df = actores))
  }
  
  g <- graph_from_data_frame(pesos, directed = FALSE, vertices = freq)
  E(g)$weight <- E(g)$peso
  g <- simplify(g, edge.attr.comb = "sum")
  if ("weight" %in% edge_attr_names(g)) E(g)$peso <- E(g)$weight
  cat("Nodos:", vcount(g), "Aristas:", ecount(g), "\n")
  list(g = g, actores_df = actores)
}

# =============================================================================
# 4. Red de TEMAS (con variantes)
# =============================================================================

cargar_temas_con_variantes <- function(config_path = CONFIG_PATH) {
  if (!file.exists(config_path)) {
    warning("No se encontró ", config_path, ". Usando temas básicos.")
    return(list())
  }
  
  config <- fromJSON(config_path, simplifyVector = FALSE)
  temas_base <- config$search_terms
  # Excluir POLITICA (si existe) del análisis de redes
  temas_base <- temas_base[!names(temas_base) %in% c("POLITICA", "Politica", "politica")]
  
  temas_expandidos <- list()
  
  for (grupo in names(temas_base)) {
    terminos <- temas_base[[grupo]]
    for (t in terminos) {
      variantes <- c(tolower(t))
      
      if (grepl("campamento", t, ignore.case = TRUE)) {
        variantes <- c(variantes, "campamento", "toma de terreno", "tomas de terreno", 
                       "asentamiento informal", "asentamientos informales")
      }
      if (grepl("Armada", t, ignore.case = TRUE)) {
        variantes <- c(variantes, "armada", "marina", "naval", "fuerzas navales", "escuadra")
      }
      if (grepl("pobreza", t, ignore.case = TRUE)) {
        variantes <- c(variantes, "pobreza", "vulnerabilidad", "precariedad", "marginalidad")
      }
      if (grepl("narcotráfico", t, ignore.case = TRUE)) {
        variantes <- c(variantes, "narcotráfico", "narcotraficante", "tráfico de drogas", 
                       "microtráfico", "drogas")
      }
      if (grepl("estigma", t, ignore.case = TRUE)) {
        variantes <- c(variantes, "estigma", "estigmatización", "estigmatizar", "estigmatizado")
      }
      if (grepl("personas en situación de calle", t, ignore.case = TRUE)) {
        variantes <- c(variantes, "situación de calle", "en situación de calle")
      }
      
      temas_expandidos[[t]] <- unique(tolower(variantes))
    }
  }
  
  # Consolidar conceptos redundantes: términos que son variantes de otro no crean nodo propio
  consolidar <- list(
    "tomas de terreno" = "campamentos",
    "asentamientos informales" = "campamentos"
  )
  for (alias in names(consolidar)) {
    canonico <- consolidar[[alias]]
    if (alias %in% names(temas_expandidos) && canonico %in% names(temas_expandidos)) {
      temas_expandidos[[canonico]] <- unique(c(temas_expandidos[[canonico]], temas_expandidos[[alias]]))
      temas_expandidos[[alias]] <- NULL
    }
  }
  
  temas_expandidos
}

construir_red_temas <- function(df, config_path = CONFIG_PATH) {
  cat("\n--- Red de TEMAS ---\n")
  
  temas_config <- cargar_temas_con_variantes(config_path)
  if (length(temas_config) == 0) {
    warning("No se encontraron temas.")
    return(list(g = make_empty_graph(), temas_df = data.frame()))
  }
  
  cat("Temas canónicos:", length(temas_config), "\n")
  
  tdf_list <- list()
  for (i in seq_len(nrow(df))) {
    parrafos <- df$parrafos[[i]]
    if (length(parrafos) == 0) next
    
    for (j in seq_along(parrafos)) {
      p_lower <- tolower(parrafos[j])
      
      temas_p <- c()
      for (tema_canonico in names(temas_config)) {
        variantes <- temas_config[[tema_canonico]]
        if (any(sapply(variantes, function(v) grepl(v, p_lower, fixed = TRUE)))) {
          temas_p <- c(temas_p, tema_canonico)
        }
      }
      
      temas_p <- unique(temas_p)
      if (length(temas_p) >= 2) {
        temas_p_norm <- normalizar_tema(temas_p)
        temas_p_norm <- temas_p_norm[!is.na(temas_p_norm)]
        if (length(temas_p_norm) >= 2) {
          tdf_list[[length(tdf_list) + 1]] <- data.frame(
            doc_id = df$id_noticia[i], parrafo_id = j, tema = temas_p_norm,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (length(tdf_list) == 0) {
    warning("No se detectaron temas.")
    return(list(g = make_empty_graph(), temas_df = data.frame()))
  }
  
  tdf <- bind_rows(tdf_list)
  tdf$clave <- paste(tdf$doc_id, tdf$parrafo_id, sep = "_")
  tdf_uniq <- tdf %>% distinct(clave, tema)
  
  claves_ok <- tdf_uniq %>% group_by(clave) %>% filter(n() >= 2) %>% pull(clave) %>% unique()
  pares_list <- list()
  for (c in claves_ok) {
    temas_c <- tdf_uniq$tema[tdf_uniq$clave == c]
    if (length(temas_c) >= 2) {
      pr <- combn(temas_c, 2)
      pares_list[[length(pares_list) + 1]] <- data.frame(
        t1 = pr[1, ], t2 = pr[2, ], stringsAsFactors = FALSE
      )
    }
  }
  pares <- bind_rows(pares_list)
  
  pesos <- pares %>% count(t1, t2, name = "peso") %>% filter(peso >= PESO_MIN_ARISTA)
  freq <- tdf %>% count(tema, name = "menciones") %>% filter(menciones >= N_MIN_TEMA)
  temas_validos <- freq$tema
  pesos <- pesos %>% filter(t1 %in% temas_validos, t2 %in% temas_validos)
  
  if (nrow(pesos) == 0) {
    return(list(g = make_empty_graph(), temas_df = tdf))
  }
  
  g <- graph_from_data_frame(pesos, directed = FALSE, vertices = freq)
  E(g)$weight <- E(g)$peso
  g <- simplify(g, edge.attr.comb = "sum")
  if ("weight" %in% edge_attr_names(g)) E(g)$peso <- E(g)$weight
  cat("Nodos:", vcount(g), "Aristas:", ecount(g), "\n")
  list(g = g, temas_df = tdf)
}

# =============================================================================
# 5. Métricas metodológicas (strength, betweenness en G_thr, components, communities)
# =============================================================================

# Strength = grado ponderado (suma de pesos de aristas incidentes)
# Betweenness en G_thr con distance = 1/weight; comunidades Louvain en G_full
calcular_metricas_nodos <- function(g_full, g_thr) {
  if (vcount(g_full) == 0) return(data.frame())
  w_full <- E(g_full)$weight
  if (is.null(w_full)) w_full <- rep(1, ecount(g_full))
  # Strength en G_full (métrica principal de centralidad)
  str_val <- strength(g_full, weights = w_full)
  # Betweenness en G_thr (evitar densidad excesiva); distance = 1/weight
  if (vcount(g_thr) > 0 && ecount(g_thr) > 0) {
    w_thr <- E(g_thr)$weight
    if (is.null(w_thr)) w_thr <- rep(1, ecount(g_thr))
    dist_thr <- 1 / (w_thr + 1)  # peso -> distancia (mayor peso = menor distancia)
    bt <- betweenness(g_thr, weights = dist_thr)
    bt_df <- data.frame(topic = V(g_thr)$name, betweenness = bt, stringsAsFactors = FALSE)
  } else {
    bt_df <- data.frame(topic = character(), betweenness = numeric())
  }
  # Louvain en G_full (determinista)
  set.seed(42)
  comm <- cluster_louvain(g_full)
  comm_id <- as.character(membership(comm))
  # Componentes
  comps <- components(g_full)
  comp_id <- comps$membership
  gsize <- comps$csize
  giant_id <- which.max(gsize)
  is_giant <- comp_id == giant_id
  # Construir tabla de nodos
  nodes_df <- data.frame(
    topic = V(g_full)$name,
    strength = str_val,
    betweenness = NA_real_,
    community_id = comm_id,
    component_id = as.integer(comp_id),
    is_giant_component = is_giant,
    stringsAsFactors = FALSE
  )
  if (nrow(bt_df) > 0) {
    for (i in seq_len(nrow(bt_df))) {
      idx <- which(nodes_df$topic == bt_df$topic[i])
      if (length(idx) > 0) nodes_df$betweenness[idx] <- bt_df$betweenness[i]
    }
  }
  nodes_df$betweenness[is.na(nodes_df$betweenness)] <- 0
  nodes_df
}

# Métricas globales (macro)
calcular_metricas_globales <- function(g_full, g_thr) {
  n_nodes <- vcount(g_full)
  n_edges <- ecount(g_full)
  density <- if (n_nodes > 1) edge_density(g_full) else 0
  comps <- components(g_full)
  n_components <- comps$no
  gsize <- comps$csize
  giant_size <- if (n_components > 0) max(gsize) else 0
  g_giant <- NULL
  if (giant_size > 1) {
    giant_ids <- which(comps$membership == which.max(gsize))
    g_giant <- induced_subgraph(g_full, giant_ids)
  }
  avg_path_length <- NA_real_
  diameter_val <- NA_real_
  if (!is.null(g_giant) && vcount(g_giant) > 1 && ecount(g_giant) > 0) {
    avg_path_length <- mean_distance(g_giant)
    diameter_val <- diameter(g_giant)
  }
  list(
    n_nodes = n_nodes,
    n_edges = n_edges,
    density = round(density, 6),
    n_components = n_components,
    giant_component_size = giant_size,
    avg_path_length = round(avg_path_length, 4),
    diameter = diameter_val
  )
}

# Wrapper: obtiene G_thr con manejo de fragmentación (bajar percentil si giant muy pequeño)
obtener_g_thr_robusto <- function(g_full) {
  p <- EDGE_WEIGHT_PERCENTILE
  g_thr <- threshold_graph_by_weight_percentile(g_full, p)
  comps_thr <- components(g_thr)
  n_thr <- vcount(g_thr)
  giant_thr <- if (comps_thr$no > 0) max(comps_thr$csize) else 0
  ratio <- if (n_thr > 0) giant_thr / n_thr else 1
  while (ratio < GIANT_COMPONENT_RATIO_MIN && p > EDGE_WEIGHT_PERCENTILE_MIN) {
    p <- max(EDGE_WEIGHT_PERCENTILE_MIN, p - 0.05)
    g_thr <- threshold_graph_by_weight_percentile(g_full, p)
    comps_thr <- components(g_thr)
    n_thr <- vcount(g_thr)
    giant_thr <- if (comps_thr$no > 0) max(comps_thr$csize) else 0
    ratio <- if (n_thr > 0) giant_thr / n_thr else 1
    cat("  G_thr fragmentado, percentil ajustado a", p, "\n")
  }
  g_thr
}

# Compatibilidad (usa G_full para strength/betweenness vía pipeline actual)
calcular_metricas <- function(g, g_thr = NULL) {
  if (is.null(g_thr)) g_thr <- g
  calcular_metricas_nodos(g, g_thr)
}

# =============================================================================
# Visualización ACADÉMICA del grafo de temas (estilo paper)
# =============================================================================
visualizar_grafo_temas <- function(g, titulo, out_path) {
  if (vcount(g) < 2) {
    cat("Grafo con menos de 2 nodos, no se visualiza.\n")
    return(invisible(NULL))
  }
  w <- if ("weight" %in% edge_attr_names(g)) E(g)$weight else rep(1, ecount(g))
  str_val <- strength(g, weights = w)
  V(g)$strength <- str_val
  
  if (vcount(g) > MAX_NODOS_GRAFICO) {
    top_ids <- order(str_val, decreasing = TRUE)[seq_len(MAX_NODOS_GRAFICO)]
    g <- induced_subgraph(g, top_ids)
    w <- if ("weight" %in% edge_attr_names(g)) E(g)$weight else rep(1, ecount(g))
    str_val <- strength(g, weights = w)
    cat("  (Visualización limitada a", vcount(g), "nodos principales)\n")
  }
  
  comm <- cluster_louvain(g)
  V(g)$community <- as.character(membership(comm))
  n_comm <- length(unique(V(g)$community))
  # Paleta Okabe-Ito (accesible, académica)
  pal_okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  pal <- rep(pal_okabe, length.out = n_comm)[seq_len(n_comm)]
  V(g)$color <- pal[as.numeric(factor(V(g)$community))]
  
  # Tamaño nodo = centralidad (escala suave)
  rng_str <- range(str_val, na.rm = TRUE)
  if (diff(rng_str) == 0) rng_str[2] <- rng_str[1] + 1
  V(g)$size <- 6 + 16 * (str_val - rng_str[1]) / diff(rng_str)
  V(g)$size <- pmax(6, pmin(24, V(g)$size))
  
  # Grosor arista = peso
  peso_arista <- if ("peso" %in% edge_attr_names(g)) E(g)$peso else w
  rng_p <- range(peso_arista, na.rm = TRUE)
  if (diff(rng_p) == 0) rng_p[2] <- rng_p[1] + 1
  E(g)$width <- 0.5 + 3.5 * (peso_arista - rng_p[1]) / diff(rng_p)
  E(g)$width <- pmax(0.5, pmin(4, E(g)$width))
  E(g)$color <- "black"   # Líneas rectas negras (estilo académico)
  
  set.seed(42)
  l <- layout_with_fr(g, niter = 2000)
  l <- norm_coords(l, -1.05, 1.05, -1.05, 1.05)
  
  # Etiquetas académicas: Cluster A, Cluster B, ...
  nombres_cluster <- if (n_comm <= 26) {
    paste0("Cluster ", LETTERS[seq_len(n_comm)])
  } else {
    paste0("Cluster ", seq_len(n_comm))
  }
  
  # PNG alta resolución (300 dpi para publicación)
  png(out_path, width = 2400, height = 2400, res = 300, bg = "white")
  par(mar = c(2.2, 1.2, 2, 1.2), mgp = c(2, 0.5, 0), bg = "white", family = "serif")
  plot(g, layout = l,
       vertex.label = V(g)$name,
       vertex.label.cex = 0.52,
       vertex.label.color = "#1a1a1a",
       vertex.label.family = "serif",
       vertex.label.font = 1,
       vertex.frame.color = V(g)$color,
       vertex.frame.width = 1,
       vertex.color = adjustcolor(V(g)$color, alpha.f = 0.78),
       edge.curved = 0,
       edge.color = E(g)$color,
       margin = c(0, 0, 0, 0))
  title(main = titulo, cex.main = 1.15, font.main = 2, line = 0.5)
  mtext(sprintf("N = %d nodos, E = %d aristas. Tamaño del nodo: centralidad; grosor del lazo: frecuencia de co-ocurrencia.",
                vcount(g), ecount(g)), side = 1, cex = 0.68, col = "#333333", line = 0.8)
  # Leyenda de clusters (tamaño aumentado, proporción equilibrada)
  legend("topleft", legend = nombres_cluster, bty = "o", cex = 0.72, pt.cex = 1.9,
         pch = 21, pt.bg = pal[seq_len(n_comm)], col = pal[seq_len(n_comm)], pt.lwd = 1,
         inset = c(0.02, 0.02), box.col = "#e0e0e0", bg = "white", border = "#b0b0b0",
         title = "Comunidades", title.col = "#1a1a1a", title.adj = 0, title.cex = 0.85)
  dev.off()
  cat("Gráfico guardado:", out_path, "\n")
  
  # PDF para publicación (vector, escalable)
  out_pdf <- sub("\\.png$", ".pdf", out_path)
  pdf(out_pdf, width = 8, height = 8, bg = "white")
  par(mar = c(2.2, 1.2, 2, 1.2), mgp = c(2, 0.5, 0), bg = "white", family = "serif")
  plot(g, layout = l,
       vertex.label = V(g)$name, vertex.label.cex = 0.52, vertex.label.color = "#1a1a1a",
       vertex.label.family = "serif", vertex.label.font = 1,
       vertex.frame.color = V(g)$color, vertex.frame.width = 1,
       vertex.color = adjustcolor(V(g)$color, alpha.f = 0.78),
       edge.curved = 0, edge.color = E(g)$color, margin = c(0, 0, 0, 0))
  title(main = titulo, cex.main = 1.15, font.main = 2, line = 0.5)
  mtext(sprintf("N = %d nodos, E = %d aristas. Tamaño: centralidad; grosor: co-ocurrencia.",
                vcount(g), ecount(g)), side = 1, cex = 0.68, col = "#333333", line = 0.8)
  legend("topleft", legend = nombres_cluster, bty = "o", cex = 0.72, pt.cex = 1.9,
         pch = 21, pt.bg = pal[seq_len(n_comm)], col = pal[seq_len(n_comm)], pt.lwd = 1,
         inset = c(0.02, 0.02), box.col = "#e0e0e0", bg = "white", border = "#b0b0b0",
         title = "Comunidades", title.col = "#1a1a1a", title.adj = 0, title.cex = 0.85)
  dev.off()
  cat("Gráfico PDF guardado:", out_pdf, "\n")
}

# Visualizar grafo por métrica (strength o betweenness) para tamaño de nodos
# Solo se visualiza la componente gigante (evita nodos aislados al rededor)
visualizar_grafo_por_metrica <- function(g, met_temas, metric = c("strength", "betweenness"), titulo, out_base) {
  if (vcount(g) < 2) {
    cat("Grafo con menos de 2 nodos, no se visualiza.\n")
    return(invisible(NULL))
  }
  # Restringir a componente gigante
  comps <- components(g)
  giant_ids <- which(comps$membership == which.max(comps$csize))
  g <- induced_subgraph(g, giant_ids)
  if (vcount(g) < 2) {
    cat("Componente gigante con menos de 2 nodos, no se visualiza.\n")
    return(invisible(NULL))
  }
  cat("  Visualizando componente gigante:", vcount(g), "nodos,", ecount(g), "aristas\n")
  metric <- match.arg(metric)
  w <- if ("weight" %in% edge_attr_names(g)) E(g)$weight else rep(1, ecount(g))
  # Obtener valores de la métrica por nodo
  nodos_df <- data.frame(name = V(g)$name, stringsAsFactors = FALSE)
  nodos_df <- nodos_df %>% left_join(met_temas %>% select(topic, strength, betweenness), by = c("name" = "topic"))
  met_val <- nodos_df[[metric]]
  met_val[is.na(met_val)] <- 0
  V(g)$met_val <- met_val
  if (vcount(g) > MAX_NODOS_GRAFICO) {
    top_ids <- order(met_val, decreasing = TRUE)[seq_len(MAX_NODOS_GRAFICO)]
    g <- induced_subgraph(g, top_ids)
    w <- if ("weight" %in% edge_attr_names(g)) E(g)$weight else rep(1, ecount(g))
    met_val <- V(g)$met_val
    cat("  (Visualización limitada a", vcount(g), "nodos principales)\n")
  }
  comm <- cluster_louvain(g)
  V(g)$community <- as.character(membership(comm))
  n_comm <- length(unique(V(g)$community))
  pal_okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  pal <- rep(pal_okabe, length.out = n_comm)[seq_len(n_comm)]
  V(g)$color <- pal[as.numeric(factor(V(g)$community))]
  rng_m <- range(met_val, na.rm = TRUE)
  if (diff(rng_m) == 0) rng_m[2] <- rng_m[1] + 1
  V(g)$size <- 6 + 16 * (met_val - rng_m[1]) / diff(rng_m)
  V(g)$size <- pmax(6, pmin(24, V(g)$size))
  peso_arista <- if ("peso" %in% edge_attr_names(g)) E(g)$peso else w
  rng_p <- range(peso_arista, na.rm = TRUE)
  if (diff(rng_p) == 0) rng_p[2] <- rng_p[1] + 1
  E(g)$width <- 0.5 + 3.5 * (peso_arista - rng_p[1]) / diff(rng_p)
  E(g)$width <- pmax(0.5, pmin(4, E(g)$width))
  E(g)$color <- "black"
  set.seed(42)
  l <- layout_with_fr(g, niter = 2000)
  l <- norm_coords(l, -1.05, 1.05, -1.05, 1.05)
  nombres_cluster <- if (n_comm <= 26) {
    paste0("Cluster ", LETTERS[seq_len(n_comm)])
  } else {
    paste0("Cluster ", seq_len(n_comm))
  }
  etiq_met <- if (metric == "strength") "grado ponderado (strength)" else "betweenness"
  png(paste0(out_base, ".png"), width = 2400, height = 2400, res = 300, bg = "white")
  par(mar = c(2.2, 1.2, 2, 1.2), mgp = c(2, 0.5, 0), bg = "white", family = "serif")
  plot(g, layout = l, vertex.label = V(g)$name, vertex.label.cex = 0.52, vertex.label.color = "#1a1a1a",
       vertex.label.family = "serif", vertex.label.font = 1, vertex.frame.color = V(g)$color,
       vertex.frame.width = 1, vertex.color = adjustcolor(V(g)$color, alpha.f = 0.78),
       edge.curved = 0, edge.color = E(g)$color, margin = c(0, 0, 0, 0))
  title(main = titulo, cex.main = 1.15, font.main = 2, line = 0.5)
  mtext(sprintf("N = %d nodos, E = %d aristas. Tamaño del nodo: %s.", vcount(g), ecount(g), etiq_met),
        side = 1, cex = 0.68, col = "#333333", line = 0.8)
  legend("topleft", legend = nombres_cluster, bty = "o", cex = 0.72, pt.cex = 1.9,
         pch = 21, pt.bg = pal[seq_len(n_comm)], col = pal[seq_len(n_comm)], pt.lwd = 1,
         inset = c(0.02, 0.02), box.col = "#e0e0e0", bg = "white", border = "#b0b0b0",
         title = "Comunidades", title.col = "#1a1a1a", title.adj = 0, title.cex = 0.85)
  dev.off()
  cat("Gráfico guardado:", out_base, ".png\n", sep = "")
  out_pdf <- paste0(out_base, ".pdf")
  pdf(out_pdf, width = 8, height = 8, bg = "white")
  par(mar = c(2.2, 1.2, 2, 1.2), mgp = c(2, 0.5, 0), bg = "white", family = "serif")
  plot(g, layout = l, vertex.label = V(g)$name, vertex.label.cex = 0.52, vertex.label.color = "#1a1a1a",
       vertex.label.family = "serif", vertex.label.font = 1, vertex.frame.color = V(g)$color,
       vertex.frame.width = 1, vertex.color = adjustcolor(V(g)$color, alpha.f = 0.78),
       edge.curved = 0, edge.color = E(g)$color, margin = c(0, 0, 0, 0))
  title(main = titulo, cex.main = 1.15, font.main = 2, line = 0.5)
  mtext(sprintf("N = %d nodos, E = %d aristas. Tamaño del nodo: %s.", vcount(g), ecount(g), etiq_met),
        side = 1, cex = 0.68, col = "#333333", line = 0.8)
  legend("topleft", legend = nombres_cluster, bty = "o", cex = 0.72, pt.cex = 1.9,
         pch = 21, pt.bg = pal[seq_len(n_comm)], col = pal[seq_len(n_comm)], pt.lwd = 1,
         inset = c(0.02, 0.02), box.col = "#e0e0e0", bg = "white", border = "#b0b0b0",
         title = "Comunidades", title.col = "#1a1a1a", title.adj = 0, title.cex = 0.85)
  dev.off()
  cat("Gráfico PDF guardado:", out_pdf, "\n")
}

visualizar_grafo <- visualizar_grafo_temas

# =============================================================================
# 6. Main y exportación
# =============================================================================

ejecutar_analisis <- function(ruta_corpus = NULL, n_muestra = NULL, ejecutar_validacion = FALSE) {
  set.seed(42)  # Determinismo
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  
  df <- cargar_corpus(ruta_corpus, n_muestra = n_muestra)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M")
  if (!is.null(n_muestra)) stamp <- paste0(stamp, "_TEST")
  
  res_temas <- construir_red_temas(df)
  g_full <- res_temas$g
  
  if (vcount(g_full) > 0) {
    # G_thr para betweenness (umbralización robusta)
    g_thr <- obtener_g_thr_robusto(g_full)
    cat("G_thr:", vcount(g_thr), "nodos,", ecount(g_thr), "aristas\n")
    
    # Métricas de nodos
    met_temas <- calcular_metricas_nodos(g_full, g_thr)
    
    # Métricas globales
    global_met <- calcular_metricas_globales(g_full, g_thr)
    
    # Edgelist para Excel (ordenado por peso descendente)
    edgelist <- igraph::as_data_frame(g_full, what = "edges")
    if (nrow(edgelist) > 0) {
      names(edgelist)[names(edgelist) == "from"] <- "source"
      names(edgelist)[names(edgelist) == "to"] <- "target"
      if (!"weight" %in% names(edgelist)) edgelist$weight <- 1
      edgelist <- edgelist %>% arrange(desc(weight))
    } else {
      edgelist <- data.frame(source = character(), target = character(), weight = numeric())
    }
    
    # Nodos ordenados por strength y por betweenness
    nodes_out <- met_temas %>% select(topic, strength, betweenness, community_id, component_id, is_giant_component)
    nodes_por_strength <- nodes_out %>% arrange(desc(strength), desc(betweenness))
    nodes_por_betweenness <- nodes_out %>% arrange(desc(betweenness), desc(strength))
    
    # Excel: documento para grafo por grado (strength)
    xlsx_grado <- list(
      "Nodos (orden por strength)" = as.data.frame(nodes_por_strength),
      "Aristas (orden por peso)" = as.data.frame(edgelist)
    )
    write_xlsx(xlsx_grado, file.path(OUTPUT_DIR, paste0("red_grado_", stamp, ".xlsx")))
    cat("Guardado: red_grado_", stamp, ".xlsx\n", sep = "")
    
    # Excel: documento para grafo por betweenness
    xlsx_betweenness <- list(
      "Nodos (orden por betweenness)" = as.data.frame(nodes_por_betweenness),
      "Aristas (orden por peso)" = as.data.frame(edgelist)
    )
    write_xlsx(xlsx_betweenness, file.path(OUTPUT_DIR, paste0("red_betweenness_", stamp, ".xlsx")))
    cat("Guardado: red_betweenness_", stamp, ".xlsx\n", sep = "")
    
    # Excel resumen (todos los nodos ordenados por strength)
    write_xlsx(list(
      "Nodos" = as.data.frame(nodes_por_strength),
      "Aristas" = as.data.frame(edgelist)
    ), file.path(OUTPUT_DIR, paste0("red_temas_metricas_", stamp, ".xlsx")))
    
    # Export global_metrics.json
    write(toJSON(global_met, pretty = TRUE, auto_unbox = TRUE),
          file.path(OUTPUT_DIR, paste0("global_metrics_", stamp, ".json")))
    cat("Guardado: global_metrics_", stamp, ".json\n", sep = "")
    
    # Gráfico 1: por grado (strength)
    visualizar_grafo_por_metrica(g_full, met_temas, "strength",
      "Red de temas por grado (strength)",
      file.path(OUTPUT_DIR, paste0("red_grado_", stamp)))
    
    # Gráfico 2: por betweenness
    visualizar_grafo_por_metrica(g_full, met_temas, "betweenness",
      "Red de temas por betweenness",
      file.path(OUTPUT_DIR, paste0("red_betweenness_", stamp)))
    
    # Top 20 por strength y por betweenness (log/print)
    cat("\n--- TOP 20 POR STRENGTH ---\n")
    top_str <- met_temas %>% arrange(desc(strength)) %>% head(TOP_N) %>% select(topic, strength)
    print(top_str)
    cat("\n--- TOP 20 POR BETWEENNESS ---\n")
    top_bt <- met_temas %>% arrange(desc(betweenness)) %>% head(TOP_N) %>% select(topic, betweenness)
    print(top_bt)
    
    # Log métricas globales
    cat("\n--- MÉTRICAS GLOBALES ---\n")
    cat("n_nodes:", global_met$n_nodes, "| n_edges:", global_met$n_edges, "\n")
    cat("density:", global_met$density, "| n_components:", global_met$n_components, "\n")
    cat("giant_component_size:", global_met$giant_component_size, "\n")
    cat("avg_path_length:", global_met$avg_path_length, "| diameter:", global_met$diameter, "\n")
  }
  
  cat("\n╔════════════════════════════════════════╗\n")
  cat("║   ANÁLISIS COMPLETADO                   ║\n")
  cat("╚════════════════════════════════════════╝\n")
  cat("\nResultados en:", normalizePath(OUTPUT_DIR, winslash = "/"), "\n")
  
  out <- list(temas = res_temas, g_full = g_full)
  if (vcount(g_full) > 0) out$met_temas <- met_temas
  invisible(out)
}

# Este archivo define las funciones. Para ejecutar:
#   - Test (muestra): source("test_analisis_redes.R")
#   - Análisis completo: source("ejecutar_analisis.R")