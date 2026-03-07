# =============================================================================
# Ejecutar análisis de red de temas — Corpus completo
# =============================================================================
# Uso: source("ejecutar_analisis.R")
# =============================================================================

source("analisis_redes.R")

cat("Iniciando análisis de redes (grafo de temas, corpus completo)...\n")
tryCatch({
  ejecutar_analisis()
}, error = function(e) {
  message("ERROR: ", conditionMessage(e))
  traceback()
})
