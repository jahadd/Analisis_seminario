# Scrapping noticias — Pipeline de análisis

Proyecto para filtrar noticias desde una base PostgreSQL (`noticias_chile`), analizar redes de temas (co-ocurrencia) y generar gráficos de serie temporal. La fuente de datos es la tabla `noticias` (schema en `schema.sql`).

---

## Requisitos

- **R** (4.x recomendado)
- **PostgreSQL** con la base `noticias_chile` y usuario `noticias`
- Paquetes R: `DBI`, `RPostgres`, `readxl`, `igraph`, `dplyr`, `stringr`, `tidyr`, `writexl`, `jsonlite`, `stringi`  
  Opcional para análisis de redes (actores): `udpipe`

```r
install.packages(c("DBI", "RPostgres", "readxl", "igraph", "dplyr", "stringr", "tidyr", "writexl", "jsonlite", "stringi"))
```

---

## Configuración inicial

1. **Clave de la base de datos**  
   Crear `.Renviron` en la raíz del proyecto (o copiar desde `.Renviron.example`) y definir:

   ```
   NOTICIAS_DB_HOST=localhost
   NOTICIAS_DB_PORT=5432
   NOTICIAS_DB_NAME=noticias_chile
   NOTICIAS_DB_USER=noticias
   NOTICIAS_DB_PASSWORD=tu_clave
   ```

   O en R antes de ejecutar: `Sys.setenv(NOTICIAS_DB_PASSWORD = "tu_clave")`.

2. **Directorio de trabajo**  
   Ejecutar siempre desde la raíz del proyecto (donde están `db_noticias.R`, `filtro_corpus.R`, etc.):

   ```r
   setwd("ruta/completa/a/Scrapping_noticias")
   ```

---

## Cómo ejecutar

| Qué hacer | Comando |
|-----------|---------|
| **Pipeline completo** (filtro → redes → gráficos) | `source("ejecutar_pipeline.R")` o `Rscript ejecutar_pipeline.R` |
| Solo filtro | `source("filtro_corpus.R")` |
| Solo análisis de redes | `source("ejecutar_analisis.R")` |
| Solo gráficos de serie temporal | `source("graficos_serie_y_palabras.R")` |

**Salidas:**

- `corpus_filtrado/` — Excel filtrado y log del filtro
- `resultados_redes/` — Grafos (PNG/PDF), Excel de nodos/aristas, `global_metrics_*.json`
- `resultados_graficos/` — Serie temporal (líneas, histograma), Excel por año

---

## Estructura del pipeline

1. **db_noticias.R** — Conexión a PostgreSQL y función `cargar_noticias_db(desde, hasta)`.
2. **filtro_corpus.R** — Cinco capas de filtrado; escribe Excel y deja la ruta en `options(noticias_ultimo_corpus_filtrado)`.
3. **ejecutar_analisis.R** → **analisis_redes.R** — Carga el corpus filtrado (o DB), construye red de temas, métricas y grafos.
4. **graficos_serie_y_palabras.R** — Serie temporal de menciones por año (vulnerabilidad vs fuerzas armadas).

El análisis de redes y los gráficos usan por defecto el **corpus filtrado** generado en el paso 2 (no vuelven a leer toda la base).

---

# Opciones configurables

Todas las opciones siguientes se cambian editando el archivo indicado (variables al inicio del script o del bloque correspondiente).

---

## 1. Base de datos (`db_noticias.R`)

| Variable / función | Valor por defecto | Descripción |
|--------------------|-------------------|-------------|
| **NOTICIAS_DB_HOST** | `"localhost"` | Host de PostgreSQL (o env `NOTICIAS_DB_HOST`) |
| **NOTICIAS_DB_PORT** | `5432` | Puerto (env `NOTICIAS_DB_PORT`) |
| **NOTICIAS_DB_NAME** | `"noticias_chile"` | Nombre de la base (env `NOTICIAS_DB_NAME`) |
| **NOTICIAS_DB_USER** | `"noticias"` | Usuario (env `NOTICIAS_DB_USER`) |
| **NOTICIAS_DB_PASSWORD** | (vacío, obligatorio por env) | Clave; debe definirse en `.Renviron` o `Sys.setenv()` |
| **cargar_noticias_db(desde, hasta)** | `"2018-01-01"`, `"2026-12-31"` | Rango de fechas (inclusive) para cargar de la tabla `noticias` |

---

## 2. Filtro de corpus (`filtro_corpus.R`)

### Rutas y fechas

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| **CONFIG_PATH** | `config_search.json` | Archivo con `search_terms` (NAVAL_FFAA, VULNERABILIDAD, etc.) |
| **OUTPUT_DIR** | `corpus_filtrado` | Carpeta donde se escribe el Excel y el log |
| **FILTRO_FECHA_DESDE** | `"2018-01-01"` | Fecha mínima al cargar desde DB |
| **FILTRO_FECHA_HASTA** | `"2026-12-31"` | Fecha máxima al cargar desde DB |

### Umbrales de las capas

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| **SCORE_MINIMO_CAPA3** | `5L` | Puntaje mínimo en Capa 3 (densidad simbólica). Valores más bajos (ej. 3–4) dejan pasar más noticias. |
| **SIMILARIDAD_TITULO_CAPA4** | `0.75` | Umbral Jaccard para considerar títulos “duplicados” en Capa 4. Más alto → más noticias consideradas duplicadas y eliminadas. |

### Listas de términos (en el script)

Además de `config_search.json`, el filtro usa listas fijas en el código:

- **CONTROVERSIA_EVALUACION** — Términos de controversia / conflicto
- **VERBOS_EVALUATIVOS** — Verbos de valoración (denunciar, criticar, etc.)
- **SUSTANTIVOS_EVALUATIVOS** — Sustantivos de valoración (abuso, estigma, etc.)
- **INDICADORES_CITA** — Indicadores de cita (según, manifestó, etc.)
- **MARCO_VISIBILIDAD** — Términos de marco de visibilidad

Editar el script para cambiar estos vectores.

### Función principal

- **ejecutar_filtro(csv_path, out_path, desde, hasta)**  
  - `csv_path = NULL`: entrada desde DB (con `desde`/`hasta`).  
  - Si se pasa `csv_path`, lee ese CSV y opcionalmente `out_path` para el Excel.  
  - `desde` / `hasta` por defecto: `FILTRO_FECHA_DESDE` y `FILTRO_FECHA_HASTA`.

---

## 3. Configuración global de búsqueda (`config_search.json`)

Usado por el **filtro** (capas 0–3). No modifica los gráficos de serie temporal, que usan listas propias en `graficos_serie_y_palabras.R`.

| Clave | Descripción |
|-------|-------------|
| **start_date** / **end_date** | Fechas de referencia (no aplican filtro por fecha en R; el filtro usa FILTRO_FECHA_*). |
| **target_regions** | Lista de regiones; actualmente **no** se usa en el filtro (no hay filtro por región). |
| **search_terms** | Diccionario de listas de términos: **NAVAL_FFAA**, **VULNERABILIDAD**, **PROCESOS_SIMBOLICOS**, **SEGURIDAD_ORDEN**. |
| **query_strategy** | Estrategia de consultas; usado si hay scripts de scraping que lean este JSON. |

Editar el JSON para cambiar los términos que usa el filtro (pertinencia, visibilidad, densidad).

---

## 4. Análisis de redes (`analisis_redes.R`)

### Rutas

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| **CONFIG_PATH** | `config_search.json` | Términos para construir la red de temas |
| **OUTPUT_DIR** | `resultados_redes` | Carpeta de salida (grafos, Excel, JSON) |
| **CORPUS_DIR** | `corpus_filtrado` | Carpeta del Excel filtrado (fallback si no hay opción) |
| **CORPUS_PATTERN** | `^corpus_filtrado.*\\.xlsx$` | Patrón del nombre del archivo Excel |

### Parámetros del grafo y la visualización

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| **N_MIN_ACTOR** | `5L` | Mínimo de menciones para incluir un actor (red de actores con udpipe) |
| **N_MIN_TEMA** | `5L` | Mínimo de apariciones para incluir un tema en la red |
| **TOP_N** | `20L` | Cantidad de nodos en rankings (Top N por strength/betweenness) |
| **PESO_MIN_ARISTA** | `12L` | Peso mínimo de una arista para entrar al grafo (más alto → menos aristas, grafo más claro) |
| **MAX_NODOS_GRAFICO** | `25L` | Máximo de nodos mostrados en cada gráfico (por centralidad) |
| **EDGE_WEIGHT_PERCENTILE** | `0.93` | Percentil de peso para el grafo umbralizado G_thr (betweenness). Solo se mantienen aristas en el top ~7%. |
| **EDGE_WEIGHT_PERCENTILE_MIN** | `0.85` | Percentil mínimo si G_thr queda muy fragmentado (se baja el percentil hasta este valor) |
| **GIANT_COMPONENT_RATIO_MIN** | `0.3` | Si la componente gigante tiene &lt; 30% de los nodos, se baja el percentil |

### Carga del corpus

- **cargar_corpus(ruta, n_muestra, desde, hasta, preferir_corpus_filtrado)**  
  - `ruta = NULL`: usa corpus filtrado reciente (opción `noticias_ultimo_corpus_filtrado`) o el Excel más reciente en `corpus_filtrado`, o carga desde DB con `desde`/`hasta`.  
  - `desde` / `hasta`: por defecto `"2018-01-01"` y `"2026-12-31"`.  
  - `preferir_corpus_filtrado = TRUE`: prioriza Excel filtrado sobre DB.  
  - `n_muestra`: si &gt; 0, limita a esa cantidad de noticias (pruebas).

---

## 5. Gráficos de serie temporal (`graficos_serie_y_palabras.R`)

### Fuente de datos y fechas

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| **OUTPUT_DIR** | `resultados_graficos` | Carpeta de salida (PNG, PDF, Excel) |
| **GRAFICOS_FECHA_DESDE** | `"2018-01-01"` | Fecha mínima si se carga desde DB |
| **GRAFICOS_FECHA_HASTA** | `"2026-12-31"` | Fecha máxima si se carga desde DB |

Si existe la opción `noticias_ultimo_corpus_filtrado` (p. ej. tras el pipeline), se usa ese Excel y no la DB.

### Términos por grupo (solo este script)

Definidos en el script; **no** leen `config_search.json`:

| Variable | Descripción |
|----------|-------------|
| **TERMINOS_VULNERABILIDAD** | Vector de términos para “vulnerabilidad socioeconómica” (campamentos, vivienda social, pobreza, etc.). |
| **TERMINOS_FUERZAS_ARMADAS** | Vector de términos para “fuerzas armadas” (Armada, Marina, estado de excepción, etc.). “Derechos humanos” está excluido de este grupo. |

Editar estos vectores al inicio del script para cambiar qué se cuenta en la serie temporal.

### Ventana temporal y formato

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| **ANIO_MIN** | `2000L` | Año mínimo para agregar datos por año |
| **ANIO_MAX** | `2030L` | Año máximo para agregar datos por año |
| **ANCHO_CM** | `17.8` | Ancho de la figura (cm) |
| **ALTO_CM** | `12` | Alto de la figura (cm) |
| **RES_DPI** | `300` | Resolución PNG (puntos por pulgada) |
| **EXPORTAR_PDF** | `TRUE` | Exportar versión PDF |
| **EXPORTAR_PNG** | `TRUE` | Exportar versión PNG |

### Colores y tipografía

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| **COL_VULN** | `"#4477AA"` | Color línea/barras vulnerabilidad |
| **COL_FFAA** | `"#EE6677"` | Color línea/barras fuerzas armadas |
| **COL_BG**, **COL_PANEL**, **COL_GRID**, **COL_AXIS**, **COL_TICK**, **COL_LINE0**, **COL_BORDER** | (ver script) | Fondo, ejes, grid, bordes |
| **BASE_FAMILY** | `"serif"` | Familia de fuente base |

### Ejecución al cargar

| Variable | Valor por defecto | Descripción |
|----------|-------------------|-------------|
| **EJECUTAR_AL_SOURCE** | `TRUE` | Si es `TRUE`, al hacer `source("graficos_serie_y_palabras.R")` se ejecuta `ejecutar_graficos()`. |

### Función de carga

- **cargar_corpus(desde, hasta, preferir_corpus_filtrado)**  
  - Si hay opción `noticias_ultimo_corpus_filtrado` y el archivo existe, usa ese Excel.  
  - Si no, carga desde DB con `desde`/`hasta`.  
  - `preferir_corpus_filtrado = TRUE` por defecto.

---

## Resumen de archivos clave

| Archivo | Rol |
|---------|-----|
| **schema.sql** | Definición de tablas de PostgreSQL (`noticias`, `titulos_terminos_diarios`, etc.). |
| **config_search.json** | Términos de búsqueda para el filtro (y para scraping si se usa). |
| **.Renviron** / **.Renviron.example** | Variables de entorno para conexión a la base (no versionar `.Renviron` si contiene clave). |

---

## Notas

- El **filtro** no aplica filtro por región; `target_regions` en el JSON no se usa en los scripts R.
- Los **gráficos de serie temporal** usan únicamente **TERMINOS_VULNERABILIDAD** y **TERMINOS_FUERZAS_ARMADAS** definidos en `graficos_serie_y_palabras.R`, no los términos de `config_search.json`.
- Para regenerar solo redes o solo gráficos sin volver a filtrar, ejecutar `ejecutar_analisis.R` o `graficos_serie_y_palabras.R` después de haber corrido el pipeline al menos una vez (así existirá un corpus filtrado reciente).
