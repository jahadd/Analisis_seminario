# =============================================================================
# Serie temporal de menciones por grupo de términos (publicación científica)
# =============================================================================
#
# Descripción:
#   Serie temporal: dos líneas (Vulnerabilidad socioeconómica vs Fuerzas armadas)
#   por año, según conteo de términos del diccionario en el corpus de noticias.
#
# Requisitos:
#   DBI, RPostgres (db_noticias.R), dplyr, stringr, jsonlite, writexl
#
# Uso:
#   source("graficos_serie_y_palabras.R")
#   # O sin ejecución automática:
#   source("graficos_serie_y_palabras.R", echo = FALSE, local = new.env())
#   ejecutar_graficos()
#
# Salidas (preparadas para revista):
#   - PNG (300 DPI) y PDF (vectorial): serie de líneas e histograma
#   - Excel con datos tabulares y palabra más frecuente por año
#
# Versión: 2.0 | Formato: Publicación científica
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(jsonlite)
  library(writexl)
})

utils::globalVariables(c(
  "fecha", "texto_completo", "anio", "n_vulnerabilidad", "n_fuerzas_armadas",
  "vulnerabilidad", "fuerzas_armadas", "palabra_mas_repetida_vulnerabilidad",
  "palabra_mas_repetida_fuerzas_armadas"
))

# =============================================================================
# CONFIGURACIÓN (editar según requisitos de la revista)
# Fuente por defecto: PostgreSQL noticias_chile (2018-2026). Ver db_noticias.R.
# =============================================================================

source("db_noticias.R")
OUTPUT_DIR     <- file.path(getwd(), "resultados_graficos")
CONFIG_PATH    <- file.path(getwd(), "config_search.json")
GRAFICOS_FECHA_DESDE <- "2018-01-01"
GRAFICOS_FECHA_HASTA <- "2026-12-31"

# Listados específicos para el gráfico de serie temporal (lenguaje mediático).
# No usan config_search.json; "derechos humanos" excluido de fuerzas armadas.
TERMINOS_VULNERABILIDAD <- c(
  "campamentos", "tomas de terreno", "asentamientos informales",
  "familias vulnerables", "vivienda social", "déficit habitacional",
  "hacinamiento", "periferia", "barrios periféricos", "poblaciones",
  "poblaciones emblemáticas", "olla común", "inseguridad alimentaria",
  "situación de pobreza", "personas en situación de calle",
  "marginalidad", "exclusión social"
)
TERMINOS_FUERZAS_ARMADAS <- c(
  "Armada de Chile", "Marina", "Escuela Naval", "Infantería de Marina",
  "estado de excepción", "despliegue militar", "uso de la fuerza",
  "control del orden público", "fuerzas armadas", "Carabineros",
  "operativos de seguridad", "toque de queda"
)

# Dimensiones y resolución (estándar publicación)
ANCHO_CM   <- 17.8   # ≈ 7 in, aprox. columna única típica
ALTO_CM    <- 12     # ≈ 4.7 in
RES_DPI    <- 300    # Mínimo para impresión
EXPORTAR_PDF <- TRUE # PDF preferido en revistas (vectorial)
EXPORTAR_PNG <- TRUE

# Ventana temporal
ANIO_MIN <- 2000L
ANIO_MAX <- 2030L

# Paleta colorblind-friendly (Tol / Paul Tol)
COL_VULN  <- "#4477AA"   # Azul
COL_FFAA  <- "#EE6677"   # Rojo-rosa
COL_BG    <- "#FFFFFF"
COL_PANEL <- "#FFFFFF"
COL_GRID  <- "#CCCCCC"
COL_AXIS  <- "#333333"
COL_TICK  <- "#555555"
COL_LINE0 <- "#999999"
COL_BORDER<- "#666666"

# Tipografía (serif habitual en ciencias)
BASE_FAMILY <- "serif"

# Ejecutar automáticamente al hacer source()
EJECUTAR_AL_SOURCE <- TRUE

# =============================================================================
# LIMPIEZA DE TEXTO (alineada con analisis_redes.R)
# =============================================================================

limpiar_html <- function(texto) {
  if (is.na(texto) || nchar(as.character(texto)) < 2) return("")
  t <- as.character(texto)
  t <- gsub("<[^>]+>", " ", t, perl = TRUE)
  t <- gsub("&[a-z]+;|&#[0-9]+;", " ", t, ignore.case = TRUE)
  t <- gsub("</?(div|p|a|span|strong|em|script|style|figure|audio|video)[^>]*>?",
            " ", t, ignore.case = TRUE, perl = TRUE)
  t <- gsub("\\s+", " ", t)
  trimws(t)
}

# =============================================================================
# 1. CARGA DE DATOS (desde PostgreSQL noticias_chile)
# =============================================================================

cargar_corpus <- function(desde = GRAFICOS_FECHA_DESDE, hasta = GRAFICOS_FECHA_HASTA, preferir_corpus_filtrado = TRUE) {
  ruta_filtrado <- getOption("noticias_ultimo_corpus_filtrado")
  if (preferir_corpus_filtrado && !is.null(ruta_filtrado) && file.exists(ruta_filtrado)) {
    message("Corpus: usando filtrado (pipeline) ", basename(ruta_filtrado))
    df <- read_excel(ruta_filtrado, sheet = 1L, col_types = "text")
    df <- as.data.frame(df, stringsAsFactors = FALSE)
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
  } else {
    message("Corpus: PostgreSQL noticias_chile (", desde, " a ", hasta, ")")
    df <- cargar_noticias_db(conn = NULL, desde = desde, hasta = hasta)
    if (nrow(df) == 0L) stop("No hay noticias en la base en ese rango.")
  }
  for (col in c("contenido", "titulo", "resumen")) {
    if (col %in% names(df)) df[[col]] <- vapply(df[[col]], limpiar_html, character(1L), USE.NAMES = FALSE)
  }
  # Normalizar fecha a Date (Excel puede guardar como texto "YYYY-MM-DD" o número serial)
  if ("fecha" %in% names(df)) {
    f <- df$fecha
    if (!inherits(f, "Date")) {
      f <- as.character(f)
      out <- as.Date(f, optional = TRUE)
      idx_na <- which(is.na(out) & nzchar(trimws(f)))
      if (length(idx_na) > 0L) {
        num <- suppressWarnings(as.numeric(f[idx_na]))
        ok <- is.finite(num)
        if (any(ok)) out[idx_na[ok]] <- as.Date(num[ok], origin = "1899-12-30")
      }
      df$fecha <- out
    }
  }
  cols_texto <- intersect(c("titulo", "resumen", "contenido"), names(df))
  df$texto_completo <- apply(df[, cols_texto, drop = FALSE], 1L, function(x) {
    paste(na.omit(as.character(x)), collapse = " ")
  })
  df
}

# Devuelve los grupos de términos para la serie temporal (listado específico del script).
cargar_grupos <- function() {
  list(
    vulnerabilidad  = TERMINOS_VULNERABILIDAD,
    fuerzas_armadas = TERMINOS_FUERZAS_ARMADAS
  )
}

# =============================================================================
# 2. CONTEO POR AÑO Y PALABRA MÁS FRECUENTE
# =============================================================================

contar_uso_por_anio <- function(df, grupos) {
  df <- df %>% filter(!is.na(fecha), nchar(as.character(texto_completo)) > 0)
  df$anio <- NA_integer_
  if (inherits(df$fecha, "Date")) {
    df$anio <- as.integer(format(df$fecha, "%Y"))
  } else {
    fch <- as.character(df$fecha)
    df$anio <- as.integer(str_extract(fch, "\\d{4}"))
  }
  df <- df %>% filter(!is.na(anio), anio >= ANIO_MIN, anio <= ANIO_MAX)

  texto <- tolower(df$texto_completo)
  term_v <- tolower(grupos$vulnerabilidad)
  term_f <- tolower(grupos$fuerzas_armadas)

  n_vuln <- vapply(seq_len(nrow(df)), function(i) {
    sum(vapply(term_v, function(t) str_count(texto[i], fixed(t)), integer(1L)))
  }, integer(1L))
  n_ffaa <- vapply(seq_len(nrow(df)), function(i) {
    sum(vapply(term_f, function(t) str_count(texto[i], fixed(t)), integer(1L)))
  }, integer(1L))
  df$n_vulnerabilidad  <- n_vuln
  df$n_fuerzas_armadas <- n_ffaa

  por_anio <- df %>%
    group_by(anio) %>%
    summarise(
      vulnerabilidad    = sum(n_vulnerabilidad, na.rm = TRUE),
      fuerzas_armadas   = sum(n_fuerzas_armadas, na.rm = TRUE),
      .groups = "drop"
    )

  pal_mas_v <- character(nrow(por_anio))
  pal_mas_f <- character(nrow(por_anio))
  for (k in seq_len(nrow(por_anio))) {
    a <- por_anio$anio[k]
    idx <- which(df$anio == a)
    if (length(idx) == 0L) next
    txt_anio <- paste(texto[idx], collapse = " ")
    counts_v <- vapply(term_v, function(t) sum(str_count(txt_anio, fixed(t))), integer(1L))
    pal_mas_v[k] <- if (max(counts_v) > 0L) term_v[which.max(counts_v)] else ""
    counts_f <- vapply(term_f, function(t) sum(str_count(txt_anio, fixed(t))), integer(1L))
    pal_mas_f[k] <- if (max(counts_f) > 0L) term_f[which.max(counts_f)] else ""
  }
  por_anio$palabra_mas_repetida_vulnerabilidad  <- pal_mas_v
  por_anio$palabra_mas_repetida_fuerzas_armadas <- pal_mas_f

  list(por_anio = por_anio, n_noticias = nrow(df))
}

# =============================================================================
# UTILIDADES GRÁFICAS
# =============================================================================

.pretty_y_ticks <- function(ymax) {
  ticks <- pretty(c(0, ymax), n = 6L)
  ticks[ticks >= 0]
}

.guardar_dispositivo <- function(ruta_base, fun_plot) {
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  ruta_base <- sub("\\.(pdf|png|tiff?)$", "", ruta_base, ignore.case = TRUE)
  par_grafico <- list(mar = c(4.2, 4.5, 3, 2), family = BASE_FAMILY, bg = COL_BG, las = 1L, xaxs = "i", yaxs = "i")
  if (EXPORTAR_PDF) {
    ruta_pdf <- paste0(ruta_base, ".pdf")
    pdf(ruta_pdf, width = ANCHO_CM / 2.54, height = ALTO_CM / 2.54, onefile = TRUE, colormodel = "srgb")
    op <- do.call(par, par_grafico)
    on.exit({ par(op); dev.off() }, add = FALSE)
    fun_plot()
    message("  PDF: ", basename(ruta_pdf))
  }
  if (EXPORTAR_PNG) {
    ruta_png <- paste0(ruta_base, ".png")
    ancho_px <- round(ANCHO_CM / 2.54 * RES_DPI)
    alto_px  <- round(ALTO_CM / 2.54 * RES_DPI)
    png(ruta_png, width = ancho_px, height = alto_px, res = RES_DPI, bg = COL_BG)
    op <- do.call(par, par_grafico)
    on.exit({ par(op); dev.off() }, add = FALSE)
    fun_plot()
    message("  PNG: ", basename(ruta_png))
  }
}

# =============================================================================
# 3. GRÁFICO DE SERIE TEMPORAL (LÍNEAS)
# =============================================================================

grafico_serie_tiempo <- function(por_anio, n_noticias, base_name) {
  if (nrow(por_anio) == 0L) {
    message("  Omitiendo gráfico de líneas: no hay datos por año.")
    return(invisible(NULL))
  }
  x <- por_anio$anio
  y1 <- por_anio$vulnerabilidad
  y2 <- por_anio$fuerzas_armadas
  x_ok <- is.finite(x)
  ymax <- max(c(y1[x_ok], y2[x_ok]), na.rm = TRUE)
  if (!is.finite(ymax) || ymax < 0) ymax <- 1
  ymax <- ymax * 1.12
  xmin <- min(x[x_ok], na.rm = TRUE)
  xmax <- max(x[x_ok], na.rm = TRUE)
  if (!is.finite(xmin) || !is.finite(xmax)) {
    message("  Omitiendo gráfico de líneas: años no válidos.")
    return(invisible(NULL))
  }
  y_ticks <- .pretty_y_ticks(ymax)
  ejes_x <- seq(xmin, xmax, by = max(1L, (xmax - xmin) %/% 8))

  plot_interno <- function() {
    plot(x, y1, type = "n",
         xlab = "Año", ylab = "Número de menciones",
         ylim = c(0, ymax), xlim = c(xmin, xmax),
         main = NA, axes = FALSE, cex.lab = 1.1)
    abline(h = y_ticks, col = COL_GRID, lwd = 0.8)
    abline(v = ejes_x, col = COL_GRID, lwd = 0.8)
    abline(h = 0, col = COL_LINE0, lwd = 1)
    lines(x, y1, col = COL_VULN, lwd = 2.5, lend = "round")
    lines(x, y2, col = COL_FFAA, lwd = 2.5, lend = "round")
    points(x, y1, pch = 21, bg = COL_VULN, col = "white", cex = 1.1, lwd = 1)
    points(x, y2, pch = 21, bg = COL_FFAA, col = "white", cex = 1.1, lwd = 1)
    axis(1L, at = ejes_x, labels = ejes_x, tck = -0.02, col = COL_BORDER, col.axis = COL_AXIS, cex.axis = 0.9)
    axis(2L, at = y_ticks,
         labels = format(y_ticks, big.mark = ".", scientific = FALSE),
         tck = -0.02, col = COL_BORDER, col.axis = COL_AXIS, cex.axis = 0.9)
    box(col = COL_BORDER, lwd = 1)
    legend("topleft",
           legend = c("Vulnerabilidad socioeconómica", "Fuerzas armadas"),
           col = c(COL_VULN, COL_FFAA), lwd = 2.5, pch = 21, pt.bg = c(COL_VULN, COL_FFAA),
           pt.cex = 1, cex = 0.9, bty = "n", x.intersp = 0.8, y.intersp = 1)
    mtext(sprintf("N = %s noticias", format(n_noticias, big.mark = ".")),
          side = 3L, line = -1.5, cex = 0.85, col = COL_TICK, adj = 1)
  }

  .guardar_dispositivo(base_name, plot_interno)
}

# =============================================================================
# 4. GRÁFICO DE BARRAS AGRUPADAS (HISTOGRAMA TEMPORAL)
# =============================================================================

grafico_histograma_tiempo <- function(por_anio, n_noticias, base_name) {
  if (nrow(por_anio) == 0L) {
    message("  Omitiendo gráfico de barras: no hay datos por año.")
    return(invisible(NULL))
  }
  x <- por_anio$anio
  mat <- as.matrix(por_anio[, c("vulnerabilidad", "fuerzas_armadas")])
  colnames(mat) <- c("Vulnerabilidad", "Fuerzas armadas")
  rownames(mat) <- x
  ymax <- max(mat, na.rm = TRUE)
  if (!is.finite(ymax) || ymax < 0) ymax <- 1
  ymax <- ymax * 1.15
  y_ticks <- .pretty_y_ticks(ymax)

  plot_interno <- function() {
    barplot(t(mat), beside = TRUE, col = c(COL_VULN, COL_FFAA),
                  border = COL_BORDER, space = c(0.2, 0.8),
                  xlab = "Año", ylab = "Número de menciones",
                  ylim = c(0, ymax), names.arg = x, cex.names = 0.9,
                  axes = FALSE, main = NA)
    abline(h = y_ticks, col = COL_GRID, lwd = 0.8)
    abline(h = 0, col = COL_LINE0, lwd = 1)
    axis(2L, at = y_ticks,
         labels = format(y_ticks, big.mark = ".", scientific = FALSE),
         tck = -0.02, col = COL_BORDER, col.axis = COL_AXIS, cex.axis = 0.9)
    box(col = COL_BORDER, lwd = 1)
    legend("topleft",
           legend = c("Vulnerabilidad socioeconómica", "Fuerzas armadas"),
           fill = c(COL_VULN, COL_FFAA), border = COL_BORDER,
           cex = 0.9, bty = "n", x.intersp = 0.8, y.intersp = 1)
    mtext(sprintf("N = %s noticias", format(n_noticias, big.mark = ".")),
          side = 3L, line = -1.5, cex = 0.85, col = COL_TICK, adj = 1)
  }

  .guardar_dispositivo(base_name, plot_interno)
}

# =============================================================================
# 5. EJECUCIÓN PRINCIPAL
# =============================================================================

ejecutar_graficos <- function() {
  grupos <- cargar_grupos()
  df <- cargar_corpus()
  stamp <- format(Sys.time(), "%Y%m%d_%H%M")
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

  res_serie <- contar_uso_por_anio(df, grupos)
  por_anio <- res_serie$por_anio
  n_noticias <- res_serie$n_noticias

  if (nrow(por_anio) == 0L) {
    message("No hay noticias con año válido en el rango ", ANIO_MIN, "-", ANIO_MAX, ". No se generan gráficos.")
    message("Comprueba que la columna 'Fecha' en el corpus tenga fechas válidas.")
    return(invisible(list(por_anio = por_anio, n_noticias = n_noticias)))
  }

  base_lineas <- file.path(OUTPUT_DIR, paste0("serie_tiempo_lineas_", stamp))
  base_histo  <- file.path(OUTPUT_DIR, paste0("serie_tiempo_histograma_", stamp))

  message("Generando gráficos para publicación...")
  grafico_serie_tiempo(por_anio, n_noticias, base_lineas)
  grafico_histograma_tiempo(por_anio, n_noticias, base_histo)

  excel_df <- por_anio %>%
    arrange(anio) %>%
    rename(
      Año = anio,
      `Menciones vulnerabilidad` = vulnerabilidad,
      `Palabra más repetida (vulnerabilidad)` = palabra_mas_repetida_vulnerabilidad,
      `Menciones fuerzas armadas` = fuerzas_armadas,
      `Palabra más repetida (fuerzas armadas)` = palabra_mas_repetida_fuerzas_armadas
    )
  excel_path <- file.path(OUTPUT_DIR, paste0("serie_tiempo_grupos_", stamp, ".xlsx"))
  write_xlsx(list("Serie temporal por año" = as.data.frame(excel_df)), excel_path)
  message("Excel: ", basename(excel_path))

  message("\nResultados en: ", normalizePath(OUTPUT_DIR, winslash = "/"))
  invisible(list(por_anio = por_anio, n_noticias = n_noticias))
}

# -----------------------------------------------------------------------------
if (isTRUE(EJECUTAR_AL_SOURCE)) {
  ejecutar_graficos()
}
