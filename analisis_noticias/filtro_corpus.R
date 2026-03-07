# =============================================================================
# Filtro del corpus de noticias — Capas de pertinencia
# =============================================================================
# Fuente por defecto: PostgreSQL noticias_chile (tabla noticias, 2018-2026).
# Principio rector: Una noticia es válida si permite reconstruir evaluaciones
# sociales, categorizaciones o marcos simbólicos en torno a los grupos analizados.
#
# Uso: source("filtro_corpus.R") o Rscript filtro_corpus.R [ruta_csv_opcional] [ruta_salida]
# Requiere: install.packages(c("jsonlite", "stringi", "writexl", "DBI", "RPostgres"))
# =============================================================================

library(jsonlite)
library(stringi)
library(writexl)
source("db_noticias.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Rutas (salida Excel)
CONFIG_PATH <- file.path(getwd(), "config_search.json")
OUTPUT_DIR <- file.path(getwd(), "corpus_filtrado")
# Rango de fechas por defecto (base noticias_chile)
FILTRO_FECHA_DESDE <- "2018-01-01"
FILTRO_FECHA_HASTA <- "2026-12-31"

# Umbrales configurables (modo más conservador = retiene más información)
SCORE_MINIMO_CAPA3 <- 5L      # 3-4: más bajo = más noticias pasan
SIMILARIDAD_TITULO_CAPA4 <- 0.75  # más alto = menos consideradas "duplicadas"

# =============================================================================
# Listas de términos (aparte de config_search.json)
# =============================================================================

CONTROVERSIA_EVALUACION <- c(
  "controversia", "debate", "critica", "denuncia", "cuestionamiento",
  "protesta", "disturbio", "conflicto", "tension", "enfrentamiento",
  "acusacion", "reclamo", "demanda", "rechazo", "respaldo", "defensa",
  "polemica", "discusion", "enfrentado", "enfrentaron", "enfrentan",
  "carabineros", "pdi", "manifestacion", "manifestaciones", "saqueo", "saqueos",
  "violencia", "delincuencia", "orden publico", "derechos humanos",
  "vecinos", "comunidad", "autoridades", "habitantes"
)

VERBOS_EVALUATIVOS <- c(
  "cuestiono", "cuestionaron", "denuncio", "denunciaron", "defendio",
  "critico", "criticaron", "justifico", "respaldo", "acuso", "acusaron",
  "rechazo", "rechazaron", "advirtio", "advirtieron", "sostuvo", "sostuvieron",
  "califico", "calificaron", "tildo", "tildaron", "catalogo", "catalogaron",
  "cuestionar", "denunciar", "criticar", "defender", "acusar", "rechazar",
  "explico", "manifesto", "preciso", "aseguro", "anadio", "indicó", "comento",
  "reclamo", "exigio", "solicito", "pidio", "informo", "reporto", "destaco"
)

SUSTANTIVOS_EVALUATIVOS <- c(
  "abuso", "abusos", "exceso", "excesos", "ilegitimo", "ilegitima",
  "privilegios", "privilegio", "estigma", "estigmatizacion",
  "legitimidad", "ilegitimidad", "impunidad", "criminalizacion",
  "discriminacion", "discriminador", "prejuicio", "prejuicios",
  "trato desigual", "desigualdad", "abusos de poder", "abuso de poder"
)

INDICADORES_CITA <- c(
  "segun", "senaló", "senalaron", "indico", "indicaron", "manifesto",
  "manifestaron", "declaro", "declararon", "afirmo", "afirmaron",
  "sostuvo", "sostuvieron", "aseguro", "aseguraron", "dijo", "dijeron",
  "agrego", "anadio", "preciso", "explico", "advirtio", "advirtieron",
  "segun vecinos", "segun autoridades", "segun la comunidad",
  "en declaraciones", "en entrevista", "en conversacion",
  "al respecto", "en ese sentido", "por su parte", "a su vez",
  "el ministro", "la autoridad", "los afectados", "el dirigente"
)

MARCO_VISIBILIDAD <- c(
  "orden publico", "derechos humanos", "marginalidad", "conflicto social",
  "moralidad publica", "debate publico", "opinion publica", "espacio publico",
  "controversia", "representacion", "estigmatizacion", "criminalizacion",
  "vulnerabilidad", "exclusion", "desigualdad", "privilegios"
)

# =============================================================================
# Funciones auxiliares
# =============================================================================

normalizar_texto <- function(s) {
  if (is.null(s) || (length(s) > 0 && is.na(s)) || !is.character(s)) return("")
  s <- trimws(tolower(as.character(s)))
  if (length(s) == 0 || nchar(s) == 0) return("")
  stri_trans_general(s, "Latin-ASCII")
}

texto_completo <- function(df, i) {
  partes <- character(0)
  for (col in c("titulo", "resumen", "contenido")) {
    if (col %in% names(df)) {
      v <- df[i, col]
      if (!is.na(v) && nchar(trimws(as.character(v))) > 0) {
        partes <- c(partes, as.character(v))
      }
    }
  }
  paste(partes, collapse = " ")
}

# Versión vectorizada: construye _texto para todo el df de una vez (más rápido en corpus grandes)
texto_completo_vectorizado <- function(df) {
  cols <- intersect(c("titulo", "resumen", "contenido"), names(df))
  if (length(cols) == 0L) return(character(nrow(df)))
  m <- as.matrix(df[, cols, drop = FALSE])
  m[is.na(m)] <- ""
  apply(m, 1L, function(x) paste(trimws(as.character(x))[nchar(trimws(as.character(x))) > 0], collapse = " "))
}

crear_regex_terminos <- function(terminos) {
  if (length(terminos) == 0) return("$^")
  parts <- character(length(terminos))
  for (j in seq_along(terminos)) {
    tn <- normalizar_texto(terminos[j])
    w <- strsplit(tn, "\\s+")[[1]]
    if (length(w) == 1) {
      parts[j] <- paste0("\\b", gsub("([.*+?^${}()|[\\]\\\\])", "\\\\\\1", tn, perl = TRUE), "\\b")
    } else {
      parts[j] <- gsub("([.*+?^${}()|[\\]\\\\])", "\\\\\\1", tn, perl = TRUE)
    }
  }
  paste(parts, collapse = "|")
}

contar_terminos <- function(texto, terminos) {
  t <- normalizar_texto(texto)
  if (nchar(t) == 0) return(0L)
  count <- 0L
  for (term in terminos) {
    tn <- normalizar_texto(term)
    w <- strsplit(tn, "\\s+")[[1]]
    if (length(w) == 1) {
      pat <- paste0("\\b", gsub("([.*+?^${}()|[\\]\\\\])", "\\\\\\1", tn, perl = TRUE), "\\b")
      if (grepl(pat, t, perl = TRUE, ignore.case = TRUE)) count <- count + 1L
    } else {
      if (grepl(gsub("([.*+?^${}()|[\\]\\\\])", "\\\\\\1", tn, perl = TRUE), t, perl = TRUE, ignore.case = TRUE))
        count <- count + 1L
    }
  }
  count
}

jaccard_palabras <- function(a, b) {
  wa <- strsplit(normalizar_texto(a), "\\s+")[[1]]
  wb <- strsplit(normalizar_texto(b), "\\s+")[[1]]
  wa <- wa[nchar(wa) > 0]
  wb <- wb[nchar(wb) > 0]
  if (length(wa) == 0 || length(wb) == 0) return(0)
  i <- length(intersect(wa, wb))
  u <- length(union(wa, wb))
  if (u == 0) return(0)
  i / u
}

# =============================================================================
# Capas de filtrado
# =============================================================================

aplicar_capa0 <- function(df, config) {
  naval <- config$search_terms$NAVAL_FFAA %||% character(0)
  vulner <- config$search_terms$VULNERABILIDAD %||% character(0)
  simbol <- config$search_terms$PROCESOS_SIMBOLICOS %||% character(0)
  seg_orden <- config$search_terms$SEGURIDAD_ORDEN %||% character(0)
  grupo_terms <- c(naval, vulner)
  secundario <- c(simbol, CONTROVERSIA_EVALUACION, seg_orden)

  txt_norm <- vapply(seq_len(nrow(df)), function(i) normalizar_texto(df$`_texto`[i]), character(1))
  r_grupo <- crear_regex_terminos(grupo_terms)
  r_sec <- crear_regex_terminos(secundario)
  mask <- grepl(r_grupo, txt_norm, perl = TRUE, ignore.case = TRUE) &
    grepl(r_sec, txt_norm, perl = TRUE, ignore.case = TRUE)
  df[mask, , drop = FALSE]
}

aplicar_capa1 <- function(df) {
  evaluativos <- c(VERBOS_EVALUATIVOS, SUSTANTIVOS_EVALUATIVOS, INDICADORES_CITA)
  txt <- vapply(seq_len(nrow(df)), function(i) normalizar_texto(df$`_texto`[i]), character(1))
  r <- crear_regex_terminos(evaluativos)
  mask <- grepl(r, txt, perl = TRUE, ignore.case = TRUE)
  df[mask, , drop = FALSE]
}

aplicar_capa2 <- function(df, config) {
  naval <- config$search_terms$NAVAL_FFAA %||% character(0)
  vulner <- config$search_terms$VULNERABILIDAD %||% character(0)
  seg_orden <- config$search_terms$SEGURIDAD_ORDEN %||% character(0)
  grupo_terms <- c(naval, vulner)
  marco <- c(MARCO_VISIBILIDAD, seg_orden, "orden publico", "derechos humanos",
             "conflicto", "carabineros", "pdi", "protesta", "manifestacion",
             "vecinos", "comunidad", "sector", "barrio", "poblacion")

  txt <- vapply(seq_len(nrow(df)), function(i) normalizar_texto(df$`_texto`[i]), character(1))
  titulo <- vapply(seq_len(nrow(df)), function(i) normalizar_texto(df$titulo[i]), character(1))
  r_grupo <- crear_regex_terminos(grupo_terms)
  r_marco <- crear_regex_terminos(marco)
  grupo_en_titular <- grepl(r_grupo, titulo, perl = TRUE, ignore.case = TRUE)
  grupo_y_marco <- grepl(r_grupo, txt, perl = TRUE, ignore.case = TRUE) &
    grepl(r_marco, txt, perl = TRUE, ignore.case = TRUE)
  mask <- grupo_en_titular | grupo_y_marco
  df[mask, , drop = FALSE]
}

aplicar_capa3 <- function(df, config) {
  naval <- config$search_terms$NAVAL_FFAA %||% character(0)
  vulner <- config$search_terms$VULNERABILIDAD %||% character(0)
  simbol <- config$search_terms$PROCESOS_SIMBOLICOS %||% character(0)
  grupo_terms <- c(naval, vulner)

  txt <- vapply(seq_len(nrow(df)), function(i) normalizar_texto(df$`_texto`[i]), character(1))
  r_grupo <- crear_regex_terminos(grupo_terms)
  r_simbol <- crear_regex_terminos(simbol)
  r_eval <- crear_regex_terminos(c(VERBOS_EVALUATIVOS, SUSTANTIVOS_EVALUATIVOS))
  r_controv <- crear_regex_terminos(CONTROVERSIA_EVALUACION)
  r_cita <- crear_regex_terminos(INDICADORES_CITA)

  score <- as.integer(grepl(r_grupo, txt, perl = TRUE, ignore.case = TRUE)) * 2L +
    as.integer(grepl(r_simbol, txt, perl = TRUE, ignore.case = TRUE)) * 2L +
    as.integer(grepl(r_eval, txt, perl = TRUE, ignore.case = TRUE)) * 2L +
    as.integer(grepl(r_controv, txt, perl = TRUE, ignore.case = TRUE)) * 1L +
    as.integer(grepl(r_cita, txt, perl = TRUE, ignore.case = TRUE)) * 1L

  df[score >= SCORE_MINIMO_CAPA3, , drop = FALSE]
}

aplicar_capa4 <- function(df) {
  if (nrow(df) < 2) return(df)

  evaluativos <- c(VERBOS_EVALUATIVOS, SUSTANTIVOS_EVALUATIVOS, INDICADORES_CITA)
  df$`_puntaje` <- vapply(seq_len(nrow(df)), function(i) {
    txt <- df$`_texto`[i]
    nchar(txt) + 10L * contar_terminos(txt, evaluativos)
  }, integer(1))
  df$`_fecha` <- as.character(ifelse(is.na(df$fecha), "", df$fecha))
  df$`_medio` <- as.character(ifelse(is.na(df$medio), "", df$medio))

  gr <- paste(df$`_fecha`, df$`_medio`, sep = "\t")
  uniq_gr <- unique(gr)
  idx_keep <- integer(0)

  for (ug in uniq_gr) {
    sel <- which(gr == ug)
    sub <- df[sel, , drop = FALSE]
    ord <- order(sub$`_puntaje`, decreasing = TRUE)
    sub <- sub[ord, , drop = FALSE]
    orig_idx <- sel[ord]
    kept <- integer(0)
    for (k in seq_len(nrow(sub))) {
      idx <- orig_idx[k]
      tit_k <- as.character(df$titulo[idx] %||% "")
      dup <- FALSE
      for (j in kept) {
        tit_j <- as.character(df$titulo[j] %||% "")
        if (jaccard_palabras(tit_k, tit_j) >= SIMILARIDAD_TITULO_CAPA4) {
          dup <- TRUE
          break
        }
      }
      if (!dup) kept <- c(kept, idx)
    }
    idx_keep <- c(idx_keep, kept)
  }

  df <- df[, !names(df) %in% c("_puntaje", "_fecha", "_medio"), drop = FALSE]
  df[sort(idx_keep), , drop = FALSE]
}

# =============================================================================
# Main
# =============================================================================

ejecutar_filtro <- function(csv_path = NULL, out_path = NULL, desde = FILTRO_FECHA_DESDE, hasta = FILTRO_FECHA_HASTA) {
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

  if (is.null(csv_path)) {
    cat("Cargando desde PostgreSQL (noticias_chile, ", desde, " a ", hasta, ")...\n", sep = "")
    df <- cargar_noticias_db(conn = NULL, desde = desde, hasta = hasta)
    origen_entrada <- paste0("PostgreSQL noticias_chile (", desde, " a ", hasta, ")")
    log_stub <- paste0("db_", format(Sys.time(), "%Y%m%d_%H%M"))
    if (is.null(out_path))
      out_path <- file.path(OUTPUT_DIR, paste0("corpus_filtrado_", log_stub, ".xlsx"))
  } else {
    if (!file.exists(csv_path)) stop("No existe: ", csv_path)
    cat("Leyendo CSV:", csv_path, "\n")
    df <- read.csv(csv_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    origen_entrada <- csv_path
    log_stub <- sub("\\.csv$", "", basename(csv_path))
    if (is.null(out_path))
      out_path <- file.path(OUTPUT_DIR, paste0("corpus_filtrado_", log_stub, ".xlsx"))
  }
  if (!grepl("\\.xlsx$", out_path, ignore.case = TRUE))
    out_path <- sub("\\.[^.]+$", ".xlsx", out_path)

  cat("Directorio de trabajo:", getwd(), "\n")
  cat("Archivo de salida:", normalizePath(out_path, winslash = "/", mustWork = FALSE), "\n")
  n_inicial <- nrow(df)
  cat("Noticias iniciales:", n_inicial, "\n")

  cat("Preparando texto para análisis...\n")
  df$`_texto` <- texto_completo_vectorizado(df)

  config <- fromJSON(CONFIG_PATH, simplifyVector = TRUE)

  df0 <- aplicar_capa0(df, config)
  cat("CAPA 0 (pertinencia mínima):", nrow(df0), "(", n_inicial - nrow(df0), "eliminadas)\n")

  df1 <- aplicar_capa1(df0)
  cat("CAPA 1 (contenido evaluativo):", nrow(df1), "(", nrow(df0) - nrow(df1), "eliminadas)\n")

  df2 <- aplicar_capa2(df1, config)
  cat("CAPA 2 (visibilidad social):", nrow(df2), "(", nrow(df1) - nrow(df2), "eliminadas)\n")

  df3 <- aplicar_capa3(df2, config)
  cat("CAPA 3 (densidad simbólica, score>=", SCORE_MINIMO_CAPA3, "):", nrow(df3), "(", nrow(df2) - nrow(df3), "eliminadas)\n")

  df4 <- aplicar_capa4(df3)
  cat("CAPA 4 (redundancia semántica):", nrow(df4), "(", nrow(df3) - nrow(df4), "eliminadas)\n")

  df4 <- df4[, names(df4) != "_texto", drop = FALSE]

  # Ordenar columnas para que la información quede clara en Excel
  cols_orden <- c("titulo", "fecha", "medio", "autor", "resumen", "contenido",
                  "contenido_limpio", "url", "url_imagen", "temas",
                  "query_label", "search_query_original", "search_query", "ID")
  cols_presentes <- intersect(cols_orden, names(df4))
  cols_resto <- setdiff(names(df4), cols_presentes)
  df4 <- df4[, c(cols_presentes, cols_resto), drop = FALSE]

  # Renombrar encabezados para mayor claridad
  nombres_claros <- c(
    titulo = "Título",
    fecha = "Fecha",
    medio = "Medio",
    autor = "Autor",
    resumen = "Resumen / Bajada",
    contenido = "Contenido",
    contenido_limpio = "Contenido limpio",
    url = "URL",
    url_imagen = "URL imagen",
    temas = "Temas",
    query_label = "Búsqueda que lo encontró",
    search_query_original = "Término de búsqueda",
    search_query = "Consulta original",
    ID = "ID"
  )
  for (n in names(nombres_claros)) {
    if (n %in% names(df4)) names(df4)[names(df4) == n] <- as.character(nombres_claros[n])
  }

  # Excel limita celdas a 32.767 caracteres; truncar texto largo
  EXCEL_MAX_CHARS <- 32000L
  for (j in seq_len(ncol(df4))) {
    if (is.character(df4[[j]])) {
      df4[[j]] <- ifelse(nchar(df4[[j]]) > EXCEL_MAX_CHARS,
                         paste0(substr(df4[[j]], 1L, EXCEL_MAX_CHARS), " [truncado...]"),
                         df4[[j]])
    }
  }

  write_xlsx(list("Noticias" = df4), out_path)
  out_abs <- normalizePath(out_path, winslash = "/", mustWork = FALSE)
  if (file.exists(out_path)) {
    cat("\nCorpus filtrado guardado (Excel):\n  ", out_abs, "\n")
  } else {
    warning("El archivo podría no haberse creado. Ruta intentada: ", out_abs)
  }
  cat("Total final:", nrow(df4), "noticias (", round(100 * nrow(df4) / n_inicial, 1), "% del original)\n")

  log_path <- file.path(OUTPUT_DIR, paste0("log_filtrado_", log_stub, ".txt"))
  writeLines(c(
    paste("Entrada:", origen_entrada),
    paste("Salida:", out_path),
    paste("Inicial:", n_inicial),
    paste("Después CAPA 0:", nrow(df0)),
    paste("Después CAPA 1:", nrow(df1)),
    paste("Después CAPA 2:", nrow(df2)),
    paste("Después CAPA 3:", nrow(df3)),
    paste("Final (CAPA 4):", nrow(df4))
  ), log_path)
  cat("Log:", log_path, "\n")

  # Para que el análisis de redes use este corpus filtrado (p. ej. en el pipeline)
  options(noticias_ultimo_corpus_filtrado = out_path)
  invisible(list(df = df4, out_path = out_path))
}

# Ejecutar al cargar (source) o al correr con Rscript
if (interactive()) {
  # En RStudio: source("filtro_corpus.R") ejecuta el filtro automáticamente
  tryCatch({
    ejecutar_filtro()
  }, error = function(e) {
    message("Error al ejecutar el filtro:")
    stop(e)
  })
} else {
  args <- commandArgs(trailingOnly = TRUE)
  csv_in <- if (length(args) >= 1) args[1] else NULL
  csv_out <- if (length(args) >= 2) args[2] else NULL
  ejecutar_filtro(csv_in, csv_out)
}
