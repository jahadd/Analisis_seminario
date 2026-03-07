-- Schema completo de la base de datos noticias_chile.
-- Permite replicar la base desde cero. Ejecutar en una base vacía como usuario noticias.
-- Uso (desde raíz del proyecto): /Library/PostgreSQL/18/bin/psql -U noticias -d noticias_chile -h localhost -f noticias/schema.sql
-- O: cd noticias && /Library/PostgreSQL/18/bin/psql -U noticias -d noticias_chile -h localhost -f schema.sql
--
-- Orden: 1) Crear BD y usuario (fuera de este script), 2) Ejecutar este schema, 3) Scraping y análisis.

-- ---------------------------------------------------------------------------
-- 1. Tabla principal: noticias (alineada a prensa_datos.parquet)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS noticias (
    id              TEXT PRIMARY KEY,
    titulo          TEXT NOT NULL,
    bajada          TEXT,
    cuerpo          TEXT,
    cuerpo_limpio   TEXT,
    fecha           DATE NOT NULL,
    fecha_scraping  DATE,
    url             TEXT NOT NULL,
    fuente          VARCHAR(100) NOT NULL,
    año             INTEGER,
    created_at      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(url, fuente)
);

CREATE INDEX IF NOT EXISTS idx_noticias_fecha ON noticias(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_noticias_fecha_id ON noticias(fecha, id);
CREATE INDEX IF NOT EXISTS idx_noticias_url ON noticias(url);
CREATE INDEX IF NOT EXISTS idx_noticias_fuente ON noticias(fuente);

COMMENT ON TABLE noticias IS 'Noticias de prensa; estructura desde prensa_datos.parquet; id es hash único, UNIQUE(url, fuente).';

-- ---------------------------------------------------------------------------
-- 2. Tabla: términos por fecha (desde titulares; script run_analisis_titulos.R)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_terminos_diarios (
    fecha      DATE NOT NULL,
    termino    VARCHAR(150) NOT NULL,
    frecuencia INTEGER NOT NULL,
    CONSTRAINT pk_titulos_terminos_diarios PRIMARY KEY (fecha, termino)
);

CREATE INDEX IF NOT EXISTS idx_titulos_terminos_fecha ON titulos_terminos_diarios(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_titulos_terminos_termino ON titulos_terminos_diarios(termino);
CREATE INDEX IF NOT EXISTS idx_titulos_terminos_fecha_termino ON titulos_terminos_diarios(fecha, termino);

COMMENT ON TABLE titulos_terminos_diarios IS 'Frecuencia de cada término por día; origen: columna titulo de noticias.';

-- ---------------------------------------------------------------------------
-- 3. Tabla: métricas diarias globales
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS metricas_titulos_diarias (
    fecha          DATE NOT NULL PRIMARY KEY,
    total_noticias INTEGER NOT NULL,
    terminos_unicos INTEGER NOT NULL,
    actualizado_en TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_metricas_titulos_fecha ON metricas_titulos_diarias(fecha DESC);

COMMENT ON TABLE metricas_titulos_diarias IS 'Una fila por día: total noticias procesadas y términos únicos ese día.';

-- ---------------------------------------------------------------------------
-- 4. Tabla: términos por fecha y por medio (para pestaña Medios del dashboard)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS titulos_terminos_por_medio (
    fecha      DATE NOT NULL,
    fuente     VARCHAR(100) NOT NULL,
    termino    VARCHAR(150) NOT NULL,
    frecuencia INTEGER NOT NULL,
    CONSTRAINT pk_titulos_terminos_por_medio PRIMARY KEY (fecha, fuente, termino)
);

CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fecha ON titulos_terminos_por_medio(fecha DESC);
CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fuente ON titulos_terminos_por_medio(fuente);
CREATE INDEX IF NOT EXISTS idx_titulos_terminos_por_medio_fecha_fuente ON titulos_terminos_por_medio(fecha, fuente);

COMMENT ON TABLE titulos_terminos_por_medio IS 'Frecuencia de cada término por día y por medio (fuente); origen: run_analisis_titulos.R para el gráfico «Términos más repetidos por medio».';
