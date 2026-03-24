# Análisis — Seminario de Título

Repositorio de análisis y código del seminario de título:

**Análisis experimental del esfuerzo prosocial en grupos socialmente diferenciados desde una contextualización semántica**

- **Autor:** Matías Carmach (Magíster en Psicología Social, UAI)  
- **Supervisores:** Teresa Ropert, Sebastián Contreras Huerta
- **Agradecimientos:** José Javier Bórquez y Diego Garrido 

---

## Resumen del estudio

Se examinó si el esfuerzo prosocial varía entre grupos socialmente diferenciados en Chile —miembros de la Academia Politécnica Naval (APOLINAV), una cohorte en situación de vulnerabilidad socioeconómica y un grupo de control— mediante una tarea experimental que manipuló el nivel de recompensa y el beneficiario (uno mismo vs. otro). Se empleó un diseño cuasi-experimental con 45 participantes, analizados con ANOVAs de medidas repetidas (grupo, recompensa, esfuerzo, beneficiario). Como segunda capa interpretativa se realizó una *media scoping review* de noticias (2010–2025) para caracterizar el contexto simbólico de estos colectivos.

**Palabras clave:** esfuerzo prosocial; desigualdad social; diferenciación grupal; discurso mediático; categorías sociales.

---

## Estructura del repositorio

| Carpeta | Contenido |
|--------|------------|
| **`Prosocial_marinos/`** | Análisis del componente experimental: datos (marinos, vulnerables, control), ANOVAs (JASP), scripts R para gráficos de esfuerzo prosocial y DASS-21. |
| **`analisis_noticias/`** | *Revisión Sistemática de Prensa Digital*: scraping de prensa chilena, filtro de corpus por capas de pertinencia, análisis de redes temáticas y gráficos de serie temporal. |

---

## Prosocial_marinos

- **`Conducta_Prosocial_Marinos.xlsx`** — Datos agregados por grupo (marinos APOLINAV, vulnerables, control).
- **`Anova_seminario.jasp`** — Análisis ANOVA (JASP).
- **`Script Limpieza.R`** — Script R de limpieza y preparación de los datos.
- **`Análisis Gráficos Marinos.R`** — Script R para generar gráficos de esfuerzo (self/other, recompensa, esfuerzo) y estrés (DASS-21) por grupo.
- **`Informe procesamiento de los datos.docx`** — Informe descriptivo del procesamiento de datos.
- Gráficos PNG de resultados (overall, reward, effort, DASS-21 por grupo).

Requisitos: R, paquetes usados en el script (p. ej. `ggplot2`, `readxl` según el código).

---

## analisis_noticias

Pipeline de noticias: **filtro de corpus → análisis de redes → gráficos**.

- **Requisitos:** R, PostgreSQL (base `noticias_chile`), variables en `.Renviron` (no versionado).  
- **Uso:** desde la carpeta `analisis_noticias/` ejecutar en R: `source("ejecutar_pipeline.R")`, o en terminal: `Rscript ejecutar_pipeline.R`.

Incluye:

- **`db_noticias.R`** — Conexión y carga desde PostgreSQL.  
- **`filtro_corpus.R`** — Filtro por capas de pertinencia (términos naval/vulnerabilidad, evaluativos, marco de visibilidad, redundancia).  
- **`ejecutar_analisis.R`** / **`analisis_redes.R`** — Análisis de redes temáticas.  
- **`graficos_serie_y_palabras.R`** — Serie temporal y gráficos.  
- **`scraping/`** — Scripts por medio (EMOL, La Tercera, Cooperativa, Biobío, etc.) y `prensa_scraping.R`.  
- **`config_search.json`** — Términos de búsqueda por dimensión (NAVAL_FFAA, VULNERABILIDAD, etc.).
- **`SEMINARIO_DE_TITULO_FINAL-3.pdf`** — Documento final del seminario de título.



