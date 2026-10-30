# 04_narrativa.R
# Genera Dashboard/narrativa.html: dashboard por capas que parte del precio de Zonaprop y suma
# ingreso familiar (ITF), barrios y oferta. Ejecutar desde la raiz del proyecto.
#
# Capa de barrios: se completa sola si existe "Bases limpias/zonaprop_barrio.csv" con columnas
#   fecha (primer dia del mes), barrio, precio_pesos (pesos corrientes)
# Uso opcional: Rscript scripts/04_narrativa.R [ruta_csv_barrios] [ruta_html_salida]

library(dplyr)
library(readr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
ruta_barrio <- if (length(args) >= 1) args[1] else "Bases limpias/zonaprop_barrio.csv"
ruta_salida <- if (length(args) >= 2) args[2] else "Dashboard/narrativa.html"
dir.create(dirname(ruta_salida), showWarnings = FALSE, recursive = TRUE)

dir_limpias <- "Bases limpias"
leer <- function(f) read_csv(file.path(dir_limpias, f), show_col_types = FALSE)
desde <- as.Date("2015-01-01")

# ---- mensual: precio Zonaprop, IPCBA y avisos ---------------------------
monthly <- leer("ipcba.csv") |> select(fecha, ipc = ipcba_general) |>
  left_join(leer("alquiler_index_caba.csv") |>
              select(fecha, alq_idx = indice_general, alq_est = indice_estrenar, alq_usa = indice_usado),
            by = "fecha") |>
  left_join(leer("oferta_zonaprop_caba.csv") |> select(fecha, zp_cant = alquiler_cantidad), by = "fecha") |>
  filter(fecha >= desde) |>
  mutate(d = format(fecha, "%Y-%m"), .before = 1) |>
  select(-fecha)

# ---- trimestral: precio Zonaprop (promedio) e ITF ------------------------
panel <- leer("panel_trimestral.csv")
quarterly <- panel |>
  filter(fecha_trim >= as.Date("2014-07-01")) |>
  transmute(anio, trim = trimestre, ipc = ipcba_general_prom, alq = alq_indice_general_prom,
            itf_total, itf_norte, itf_centro, itf_sur)
ref_ipc <- panel$ipcba_general_prom[max(which(!is.na(panel$ipcba_general_prom)))]

# ---- barrios (opcional) --------------------------------------------------
barrios <- tibble(d = character(), b = character(), p = numeric())
if (file.exists(ruta_barrio)) {
  b <- read_csv(ruta_barrio, show_col_types = FALSE)
  faltan <- setdiff(c("fecha", "barrio", "precio_pesos"), names(b))
  if (length(faltan)) stop("Faltan columnas en ", ruta_barrio, ": ", paste(faltan, collapse = ", "))
  barrios <- b |>
    filter(fecha >= desde, !is.na(precio_pesos)) |>
    transmute(d = format(as.Date(fecha), "%Y-%m"), b = barrio, p = precio_pesos)
  cat(sprintf("Barrios: %d filas, %d barrios\n", nrow(barrios), n_distinct(barrios$b)))
} else {
  cat("Sin base de barrios (", ruta_barrio, "): la capa 2 queda como pendiente.\n", sep = "")
}

datos <- list(monthly = monthly, quarterly = quarterly, barrios = barrios, ref_ipc = ref_ipc)
json <- toJSON(datos, dataframe = "rows", na = "null", auto_unbox = TRUE, digits = 8)

html <- paste(readLines("scripts/narrativa_template.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
stopifnot(grepl("__DATA__", html, fixed = TRUE))
html <- sub("__DATA__", json, html, fixed = TRUE)
con <- file(ruta_salida, open = "wb"); writeBin(charToRaw(enc2utf8(html)), con); close(con)
cat(sprintf("%s (%.0f KB) | %d meses, %d trimestres\n", ruta_salida, file.size(ruta_salida) / 1024,
            nrow(monthly), nrow(quarterly)))
