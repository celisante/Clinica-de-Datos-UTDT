# leer_hitos.R
# Lee "Bases limpias/hitos_alquileres_caba_2015_2026.xlsx" y devuelve un data frame con:
#   d   fecha del hito (YYYY-MM-DD)
#   id  letra que identifica al hito (A, B, ...)
#   txt descripcion
# La hoja tiene filas de titulo y un encabezado; los hitos son las filas con una fecha de Excel en la 1ra columna.
# Uso:  source("scripts/leer_hitos.R"); hitos <- leer_hitos()

leer_hitos <- function(ruta = "Bases limpias/hitos_alquileres_caba_2015_2026.xlsx") {
  x <- suppressMessages(readxl::read_excel(ruta, sheet = 1, col_names = FALSE, col_types = "text"))
  fecha <- suppressWarnings(as.numeric(x[[1]]))
  ok <- !is.na(fecha) & !is.na(x[[3]])
  h <- data.frame(d = format(as.Date(fecha[ok], origin = "1899-12-30")), id = trimws(x[[3]][ok]), txt = trimws(x[[2]][ok]),
                  stringsAsFactors = FALSE)
  stopifnot(nrow(h) > 0, !anyDuplicated(h$id))
  h[order(h$d), ]
}
