# =============================================================================
# Conexión y carga desde PostgreSQL — noticias_chile
# =============================================================================
# Base: noticias_chile (usuario noticias). Tabla principal: noticias.
# Filtro temporal por defecto: 2018-01-01 a 2026-12-31.
#
# Configuración: definir la clave en el entorno para no hardcodearla.
#   En R: Sys.setenv(NOTICIAS_DB_PASSWORD = "tu_clave")
#   O en archivo .Renviron en la raíz del proyecto o en el home:
#     NOTICIAS_DB_HOST=localhost
#     NOTICIAS_DB_PORT=5432
#     NOTICIAS_DB_NAME=noticias_chile
#     NOTICIAS_DB_USER=noticias
#     NOTICIAS_DB_PASSWORD=tu_clave
#
# Requiere: install.packages(c("DBI", "RPostgres"))
# =============================================================================

if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RPostgres", quietly = TRUE)) {
  stop("Instala paquetes: install.packages(c('DBI', 'RPostgres'))")
}

# Parámetros de conexión (por defecto; pueden sobreescribirse con variables de entorno)
NOTICIAS_DB_HOST     <- Sys.getenv("NOTICIAS_DB_HOST",     "localhost")
NOTICIAS_DB_PORT     <- as.integer(Sys.getenv("NOTICIAS_DB_PORT", "5432"))
NOTICIAS_DB_NAME    <- Sys.getenv("NOTICIAS_DB_NAME",    "noticias_chile")
NOTICIAS_DB_USER    <- Sys.getenv("NOTICIAS_DB_USER",    "noticias")
NOTICIAS_DB_PASSWORD <- Sys.getenv("NOTICIAS_DB_PASSWORD", "")

#' Conectar a la base noticias_chile
#' @param host, port, dbname, user, password Si no se pasan, se usan las variables de entorno.
conectar_noticias_chile <- function(host = NOTICIAS_DB_HOST,
                                    port = NOTICIAS_DB_PORT,
                                    dbname = NOTICIAS_DB_NAME,
                                    user = NOTICIAS_DB_USER,
                                    password = NOTICIAS_DB_PASSWORD) {
  if (is.null(password) || (is.character(password) && nchar(password) == 0L)) {
    stop("Define la clave de la base: Sys.setenv(NOTICIAS_DB_PASSWORD = 'tu_clave') o crea .Renviron con NOTICIAS_DB_PASSWORD=...")
  }
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = host,
    port     = port,
    dbname   = dbname,
    user     = user,
    password = as.character(password)
  )
}

#' Cargar noticias desde PostgreSQL (tabla noticias) en el rango de fechas dado.
#' Devuelve un data.frame con nombres estándar para filtro y análisis.
#' @param conn Conexión DBI; si es NULL, se abre y se cierra dentro de la función.
#' @param desde Fecha mínima (inclusive), carácter "YYYY-MM-DD".
#' @param hasta Fecha máxima (inclusive), carácter "YYYY-MM-DD".
cargar_noticias_db <- function(conn = NULL,
                               desde = "2018-01-01",
                               hasta = "2026-12-31") {
  abrir_y_cerrar <- is.null(conn)
  if (abrir_y_cerrar) {
    conn <- conectar_noticias_chile()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  q <- "SELECT id, titulo, bajada, cuerpo, cuerpo_limpio, fecha, url, fuente
        FROM noticias
        WHERE fecha >= $1 AND fecha <= $2
        ORDER BY fecha DESC"
  df <- DBI::dbGetQuery(conn, q, params = list(desde, hasta))
  if (nrow(df) == 0L) {
    warning("No hay filas en noticias entre ", desde, " y ", hasta)
    df <- data.frame(
      id = character(), titulo = character(), resumen = character(),
      contenido = character(), contenido_limpio = character(), fecha = as.Date(character()),
      url = character(), medio = character(), ID = character(), id_noticia = character(),
      autor = character(), url_imagen = character(), temas = character(),
      stringsAsFactors = FALSE
    )
    return(df)
  }
  # Mapear columnas del schema a nombres usados por filtro y análisis
  names(df)[names(df) == "bajada"]       <- "resumen"
  names(df)[names(df) == "cuerpo"]       <- "contenido"
  names(df)[names(df) == "cuerpo_limpio"] <- "contenido_limpio"
  names(df)[names(df) == "fuente"]       <- "medio"
  df$ID <- df$id
  df$id_noticia <- paste0("n_", df$id)
  # Columnas opcionales que no existen en el schema (compatibilidad con scripts que las usan)
  if (!"autor" %in% names(df))       df$autor       <- NA_character_
  if (!"url_imagen" %in% names(df)) df$url_imagen  <- NA_character_
  if (!"temas" %in% names(df))      df$temas       <- NA_character_
  df
}
