# =============================================================================
# Pipeline completo: filtro → análisis de redes → gráficos
# =============================================================================
# Orden:
#   1. db_noticias.R — funciones de conexión a PostgreSQL
#   2. filtro_corpus.R — filtra noticias desde DB (2018-2026), escribe Excel y
#      deja la ruta en options(noticias_ultimo_corpus_filtrado)
#   3. ejecutar_analisis.R — análisis de redes; usa ESE corpus filtrado (no DB)
#   4. graficos_serie_y_palabras.R — serie temporal; también usa corpus filtrado
#
# Requisitos: NOTICIAS_DB_PASSWORD en .Renviron o Sys.setenv()
# Uso: source("ejecutar_pipeline.R")   o   Rscript ejecutar_pipeline.R
# =============================================================================

inicio <- Sys.time()
cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  PIPELINE: Filtro → Análisis de redes → Gráficos            ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# Ejecutar desde la raíz del proyecto (donde están db_noticias.R, filtro_corpus.R, etc.)
if (!file.exists("db_noticias.R")) {
  stop("Ejecuta este script desde la raíz del proyecto: setwd('ruta/a/Scrapping_noticias')")
}

# -----------------------------------------------------------------------------
# 1. db_noticias.R — funciones de conexión y carga
# -----------------------------------------------------------------------------
cat("--- 1. Cargando db_noticias.R ---\n")
tryCatch({
  source("db_noticias.R")
  cat("  OK: Conexión y cargar_noticias_db disponibles.\n\n")
}, error = function(e) {
  stop("Error al cargar db_noticias.R: ", conditionMessage(e))
})

# -----------------------------------------------------------------------------
# 2. filtro_corpus.R — filtro por capas, salida Excel
# -----------------------------------------------------------------------------
cat("--- 2. Ejecutando filtro_corpus.R ---\n")
tryCatch({
  source("filtro_corpus.R")
  cat("  OK: Filtro completado.\n\n")
}, error = function(e) {
  stop("Error en filtro_corpus.R: ", conditionMessage(e))
})

# -----------------------------------------------------------------------------
# 3. ejecutar_analisis.R (incluye analisis_redes.R) — redes de temas
# -----------------------------------------------------------------------------
cat("--- 3. Ejecutando análisis de redes (ejecutar_analisis.R) ---\n")
tryCatch({
  source("ejecutar_analisis.R")
  cat("  OK: Análisis de redes completado.\n\n")
}, error = function(e) {
  stop("Error en análisis de redes: ", conditionMessage(e))
})

# -----------------------------------------------------------------------------
# 4. graficos_serie_y_palabras.R — serie temporal y gráficos
# -----------------------------------------------------------------------------
cat("--- 4. Ejecutando graficos_serie_y_palabras.R ---\n")
tryCatch({
  source("graficos_serie_y_palabras.R")
  cat("  OK: Gráficos generados.\n\n")
}, error = function(e) {
  stop("Error en graficos_serie_y_palabras.R: ", conditionMessage(e))
})

# -----------------------------------------------------------------------------
# Resumen
# -----------------------------------------------------------------------------
elapsed <- round(difftime(Sys.time(), inicio, units = "mins"), 1)
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  PIPELINE COMPLETADO                                        ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
cat("  Tiempo total:", elapsed, "min\n")
cat("  Salidas:\n")
cat("    - corpus_filtrado/     (Excel filtrado)\n")
cat("    - resultados_redes/    (grafos, métricas, JSON)\n")
cat("    - resultados_graficos/ (serie temporal, PNG/PDF, Excel)\n\n")
