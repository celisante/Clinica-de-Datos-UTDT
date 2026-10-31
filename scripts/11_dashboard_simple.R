# 11_dashboard_simple.R
# Genera Dashboard/simple.html: dos graficos.
#  1) Precio de alquiler de un 2 amb (Zonaprop, 50 m2, promedio general) en pesos constantes del 3T 2026, con los periodos entre hitos sombreados.
#  2) Ese mismo precio junto al ingreso individual medio de los perceptores y al ingreso total familiar (ITF), en pesos constantes.
# Constantes = deflactado con el IPCBA general, base promedio del ultimo trimestre completo (3T 2026).
# Ejecutar desde la raiz del proyecto:  Rscript scripts/11_dashboard_simple.R

library(dplyr)
library(readr)
library(jsonlite)

leer <- function(f) read_csv(file.path("Bases limpias", f), show_col_types = FALSE)
trim_de <- function(f) as.Date(sprintf("%s-%02d-01", format(f, "%Y"), (as.integer(format(f, "%m")) - 1L) %/% 3L * 3L + 1L))

# ---- mensual: precio Zonaprop e IPCBA ------------------------------------
mensual <- leer("alquiler_index_caba.csv") |> select(fecha, nom = indice_general) |>
  inner_join(leer("ipcba.csv") |> select(fecha, ipc = ipcba_general), by = "fecha") |>
  filter(!is.na(nom)) |> arrange(fecha)

ipc_trim <- leer("ipcba.csv") |> mutate(trim = trim_de(fecha)) |>
  group_by(trim) |> summarise(n = n(), ipc = mean(ipcba_general), .groups = "drop") |> filter(n == 3)
ult_trim <- max(ipc_trim$trim)
ref <- ipc_trim$ipc[ipc_trim$trim == ult_trim]
cat(sprintf("Base de pesos constantes: promedio IPCBA del trimestre que empieza %s = %.4f\n", ult_trim, ref))

mensual <- mensual |> mutate(real = nom * ref / ipc)

# ---- trimestral: precio (trimestres completos) + ingresos ------------------
precio_q <- mensual |> mutate(trim = trim_de(fecha)) |> group_by(trim) |>
  summarise(n = n(), precio = mean(nom), .groups = "drop") |> filter(n == 3) |> select(-n)
panel <- leer("panel_trimestral.csv") |> transmute(trim = fecha_trim, itf = itf_total, ing_ind = ing_perceptores_prom)

trimestral_nom <- precio_q |> full_join(panel, by = "trim") |> inner_join(ipc_trim |> select(trim, ipc), by = "trim") |>
  filter(trim >= as.Date("2015-10-01")) |> arrange(trim)   # valores corrientes + IPCBA del trimestre (se conservan para el Excel)
trimestral <- trimestral_nom |>
  mutate(across(c(precio, itf, ing_ind), ~ .x * ref / ipc, .names = "{.col}")) |>
  transmute(d = format(trim, "%Y-%m"), precio, itf, ing_ind)

source("scripts/leer_hitos.R")
hitos <- leer_hitos()

# ---- alquiler como % del ITF, por zona ---------------------------------------
# Zona de cada barrio: "Bases limpias/barrio_zona.csv" (BARRIO, COMUNA, ZONA). El precio de cada zona es el promedio de sus barrios
# ponderado por los m2 ofertados de 2 amb (IDECBA), igual que el precio por comuna; queda vacio si los barrios con precio cubren
# menos del 50 % de los m2 de la zona. Se promedia por trimestre completo (3 meses) y se divide por el ITF medio de la misma zona.
MIN_COBERTURA <- 0.5
alias <- c("la paternal" = "paternal", "montserrat" = "monserrat")   # nombres distintos entre Zonaprop y barrio_zona.csv
clave <- function(s) { k <- stringr::str_to_lower(iconv(s, to = "ASCII//TRANSLIT")) |> stringr::str_remove_all("[^a-z ]") |> stringr::str_squish()
  dplyr::if_else(k %in% names(alias), unname(alias[k]), k) }
bz <- leer("barrio_zona.csv") |> transmute(key = clave(BARRIO), zona = tolower(ZONA))
pb <- leer("zonaprop_barrio.csv") |> filter(!is.na(precio_pesos)) |> mutate(key = clave(barrio)) |> select(key, fecha, precio_pesos)
peso <- leer("superficie_alquiler_2amb_barrio.csv") |> mutate(key = clave(barrio)) |> select(key, fecha, m2 = superficie_m2)
sin_zona <- setdiff(unique(pb$key), bz$key)
if (length(sin_zona)) stop("Barrios de Zonaprop sin zona en barrio_zona.csv: ", paste(sin_zona, collapse = ", "))
peso_tot <- peso |> inner_join(bz, by = "key") |> group_by(zona, fecha) |> summarise(m2_total = sum(m2, na.rm = TRUE), .groups = "drop")
precio_zona <- pb |> left_join(peso, by = c("key", "fecha")) |> filter(!is.na(m2), m2 > 0) |> inner_join(bz, by = "key") |>
  group_by(zona, fecha) |> summarise(precio = weighted.mean(precio_pesos, m2), m2_con = sum(m2), .groups = "drop") |>
  left_join(peso_tot, by = c("zona", "fecha")) |> filter(m2_con / m2_total >= MIN_COBERTURA) |>
  mutate(trim = trim_de(fecha)) |> group_by(zona, trim) |> filter(n() == 3) |> summarise(precio = mean(precio), .groups = "drop")
itf_q <- leer("itf_zona.csv") |> transmute(trim = as.Date(sprintf("%d-%02d-01", anio, (trimestre - 1L) * 3L + 1L)), zona, itf)
alq_zona <- precio_q |> rename(p_caba = precio) |>
  left_join(precio_zona |> tidyr::pivot_wider(names_from = zona, values_from = precio, names_prefix = "p_"), by = "trim") |>
  left_join(itf_q |> tidyr::pivot_wider(names_from = zona, values_from = itf, names_prefix = "itf_"), by = "trim") |>
  filter(!is.na(itf_total), trim >= as.Date("2016-01-01"))
alq_zona_comp <- alq_zona   # con precios e ITF corrientes por zona (se conserva para el Excel)
alq_zona <- alq_zona |>
  transmute(d = format(trim, "%Y-%m"),
            norte = p_norte / itf_norte * 100, centro = p_centro / itf_centro * 100, sur = p_sur / itf_sur * 100, caba = p_caba / itf_total * 100,
            caba_norte = p_caba / itf_norte * 100, caba_centro = p_caba / itf_centro * 100, caba_sur = p_caba / itf_sur * 100)
cat(sprintf("Alquiler/ITF por zona: %d trimestres, ultimo %s\n", nrow(alq_zona), tail(alq_zona$d, 1)))

# ---- mapa de calor: avisos estimados por barrio y mes (oferta_barrio.csv, ver scripts/10_oferta_barrio.R) ---------
# Solo 5 barrios. Los 6 tramos de color son cuantiles de los avisos positivos de estos 5 barrios (con la escala de los 48 barrios de
# narrativa.html, Palermo y Caballito quedaban todo el periodo en el tramo mas alto y no se veia su variacion).
BARRIOS_MAPA <- c("Palermo", "Caballito", "Puerto Madero", "San Telmo", "Agronomia")
o <- leer("oferta_barrio.csv")
stopifnot(all(BARRIOS_MAPA %in% o$barrio))
pos <- sort(o$avisos_estimados[o$barrio %in% BARRIOS_MAPA & !is.na(o$avisos_estimados) & o$avisos_estimados > 0])
meses <- sort(unique(o$fecha))
serie <- function(b, col) { d <- o[o$barrio == b, ]; unname(d[[col]][match(meses, d$fecha)]) }
vals <- lapply(setNames(BARRIOS_MAPA, BARRIOS_MAPA), serie, col = "avisos_estimados")
orden <- names(sort(sapply(vals, mean, na.rm = TRUE), decreasing = TRUE))   # de mayor a menor oferta promedio
calor <- list(months = format(meses, "%Y-%m"), proy_desde = format(min(o$fecha[o$proyectado]), "%Y-%m"), orden = orden,
              vals = vals[orden], obs = lapply(setNames(orden, orden), serie, col = "unidades_idecba"),
              bins = c(0, unname(quantile(pos, (1:5) / 6)), max(pos) + 1e-9),
              ramp = substr(viridisLite::viridis(6, begin = .02, end = .9, direction = -1), 1, 7))   # viridis invertida: violeta = mas avisos
cat(sprintf("Mapa de calor: %s | %d meses, proyectado desde %s\n", paste(orden, collapse = ", "), length(meses), calor$proy_desde))

datos <- list(
  calor = calor,
  alq_zona = alq_zona,
  mensual = mensual |> transmute(d = format(fecha, "%Y-%m"), v = real),
  trimestral = trimestral,
  hitos = hitos,
  ref_trim = sprintf("%dT %s", (as.integer(format(ult_trim, "%m")) - 1L) %/% 3L + 1L, format(ult_trim, "%Y"))
)
json <- toJSON(datos, dataframe = "rows", na = "null", auto_unbox = TRUE, digits = 8)

html <- paste(readLines("scripts/simple_template.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
stopifnot(grepl("__DATA__", html, fixed = TRUE))
html <- sub("__DATA__", json, html, fixed = TRUE)
ruta <- "Dashboard/simple.html"
dir.create(dirname(ruta), showWarnings = FALSE, recursive = TRUE)
con <- file(ruta, open = "wb"); writeBin(charToRaw(enc2utf8(html)), con); close(con)
cat(sprintf("%s (%.0f KB) | %d meses, %d trimestres\n", ruta, file.size(ruta) / 1024, nrow(mensual), nrow(trimestral)))

# ---- Excel con los datos de los graficos de linea ----------------------------------
# Una pestana por grafico (G1 precio mensual, G2 alquiler vs ingresos, G3 alquiler como % del ITF) + hitos/periodos y notas.
# El mapa de calor no es un grafico de lineas y no se incluye.
library(openxlsx)
etq_trim <- function(f) sprintf("%dT %s", (as.integer(format(f, "%m")) - 1L) %/% 3L + 1L, format(f, "%Y"))
g1 <- mensual |> mutate(var_mensual = (real / lag(real) - 1) * 100) |>
  transmute(Fecha = fecha, `Alquiler 2 amb, pesos corrientes (Zonaprop, indice general)` = nom, `IPCBA general` = ipc,
            `Alquiler 2 amb, pesos constantes` = real, `Variacion mensual real (%)` = var_mensual)
h2 <- hitos |> mutate(d = as.Date(d)) |> arrange(d)
mes1 <- function(x) as.Date(format(x, "%Y-%m-01"))
per <- tibble::tibble(Periodo = c("Tras la salida del cepo", "DNU 320: congelamiento", "Ley de Alquileres (27.551)", "Desregulacion (DNU 70/2023)"),
                      desde = mes1(c(h2$d[h2$id == "B"], h2$d[h2$id == "C"], h2$d[h2$id == "D"], h2$d[h2$id == "F"])),
                      hasta = c(mes1(h2$d[h2$id == "C"]), mes1(h2$d[h2$id == "D"]), mes1(h2$d[h2$id == "F"]), max(mensual$fecha)),
                      Hitos = c("B a C", "C a D", "D a F", "F a hoy")) |>
  mutate(`Alquiler al inicio (pesos constantes)` = mensual$real[match(desde, mensual$fecha)],
         `Alquiler al final (pesos constantes)` = mensual$real[match(hasta, mensual$fecha)],
         `Variacion real (%)` = (`Alquiler al final (pesos constantes)` / `Alquiler al inicio (pesos constantes)` - 1) * 100) |>
  rename(Desde = desde, Hasta = hasta)
g2 <- trimestral_nom |> transmute(Trimestre = etq_trim(trim), `Inicio del trimestre` = trim, `IPCBA general (promedio del trimestre)` = ipc,
  `Alquiler 2 amb, corriente` = precio, `Ingreso individual medio de perceptores, corriente` = ing_ind, `ITF medio total, corriente` = itf,
  `Alquiler 2 amb, constante` = precio * ref / ipc, `Ingreso individual medio, constante` = ing_ind * ref / ipc, `ITF medio total, constante` = itf * ref / ipc)
g3 <- alq_zona_comp |> transmute(Trimestre = etq_trim(trim), `Inicio del trimestre` = trim,
  `Precio Norte (promedio de barrios)` = p_norte, `ITF Norte` = itf_norte, `Alquiler / ITF Norte (%)` = p_norte / itf_norte * 100,
  `Precio Centro (promedio de barrios)` = p_centro, `ITF Centro` = itf_centro, `Alquiler / ITF Centro (%)` = p_centro / itf_centro * 100,
  `Precio Sur (promedio de barrios)` = p_sur, `ITF Sur` = itf_sur, `Alquiler / ITF Sur (%)` = p_sur / itf_sur * 100,
  `Precio total CABA` = p_caba, `Precio CABA / ITF Norte (%)` = p_caba / itf_norte * 100, `Precio CABA / ITF Centro (%)` = p_caba / itf_centro * 100,
  `Precio CABA / ITF Sur (%)` = p_caba / itf_sur * 100)
notas <- tibble::tibble(Nota = c(
  sprintf("Pesos constantes: pesos del %s, deflactados con el IPCBA general (base = promedio de ese trimestre = %.2f).", etq_trim(ult_trim), ref),
  "G1: precio mensual de alquiler de un 2 ambientes de 50 m2, promedio general de CABA (Zonaprop, Bases limpias/alquiler_index_caba.csv). Desde dic 2015.",
  "G2: promedio trimestral (solo trimestres con 3 meses). Ingreso individual = ingreso medio de perceptores de ingreso; ITF = ingreso total familiar medio de CABA (IDECBA). Los ingresos llegan hasta el 2T 2026.",
  "G3: todo en pesos corrientes del mismo trimestre (el cociente no depende de la base). Precio de cada zona = promedio de sus barrios ponderado por m2 ofertados de 2 amb (IDECBA), solo si los barrios con precio cubren al menos el 50 % de los m2.",
  "G3: zona de cada barrio segun Bases limpias/barrio_zona.csv. Las columnas 'Precio CABA / ITF zona' usan el precio total de CABA contra el ITF de cada zona (version aproximada).",
  "Periodos: fechas de los hitos de Bases limpias/hitos_alquileres_caba_2015_2026.xlsx. El mapa de calor de avisos no se incluye (no es un grafico de lineas).",
  "Generado por scripts/11_dashboard_simple.R junto con Dashboard/simple.html."))
wb <- createWorkbook()
hdr <- createStyle(textDecoration = "bold", fgFill = "#E8E7E1", wrapText = TRUE, valign = "top", border = "bottom")
escribir <- function(nombre, df, formatos = list(), anchos = NULL, extra = NULL) {
  addWorksheet(wb, nombre); writeData(wb, nombre, df, headerStyle = hdr)
  for (f in formatos) addStyle(wb, nombre, createStyle(numFmt = f$fmt), rows = 2:(nrow(df) + 1), cols = f$cols, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, nombre, cols = seq_len(ncol(df)), widths = if (is.null(anchos)) 20 else anchos)
  setRowHeights(wb, nombre, rows = 1, heights = 62); freezePane(wb, nombre, firstRow = TRUE, firstCol = TRUE)
}
fecha_fmt <- "mmm yyyy"; peso_fmt <- "#,##0"; pct_fmt <- "0.0"
escribir("G1 Alquiler mensual", g1, list(list(fmt = fecha_fmt, cols = 1), list(fmt = "#,##0", cols = 2), list(fmt = "0.00", cols = 3), list(fmt = peso_fmt, cols = 4), list(fmt = "0.0", cols = 5)))
escribir("G1 Periodos", per, list(list(fmt = fecha_fmt, cols = 2:3), list(fmt = peso_fmt, cols = 5:6), list(fmt = pct_fmt, cols = 7)), c(30, 12, 12, 10, 22, 22, 16))
escribir("Hitos", h2 |> transmute(Fecha = d, Hito = id, Descripcion = txt), list(list(fmt = "dd/mm/yyyy", cols = 1)), c(14, 8, 120))
escribir("G2 Alquiler vs ingresos", g2, list(list(fmt = "mmm yyyy", cols = 2), list(fmt = "0.00", cols = 3), list(fmt = peso_fmt, cols = 4:9)), c(11, 13, 16, rep(20, 6)))
escribir("G3 Alquiler % ITF", g3, list(list(fmt = "mmm yyyy", cols = 2), list(fmt = peso_fmt, cols = c(3, 4, 6, 7, 9, 10, 12)), list(fmt = pct_fmt, cols = c(5, 8, 11, 13, 14, 15))), c(11, 13, rep(17, 13)))
escribir("Notas", notas, anchos = 150)
ruta_xlsx <- "Dashboard/datos_graficos_simple.xlsx"
saveWorkbook(wb, ruta_xlsx, overwrite = TRUE)
cat(sprintf("%s: %s
", ruta_xlsx, paste(names(wb), collapse = " | ")))
