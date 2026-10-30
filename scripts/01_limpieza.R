# 01_limpieza.R
# Lee las bases en "Bases crudas/" y deja tablas ordenadas (tidy) en "Bases limpias/".
# Ejecutar desde la raiz del proyecto (abrir Clinica-de-Datos-UTDT.Rproj).
# No modifica ninguna base cruda.

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)

dir_crudas <- "Bases crudas"
dir_limpias <- "Bases limpias"
dir.create(dir_limpias, showWarnings = FALSE)

# ---- helpers -----------------------------------------------------------

meses <- c(ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6,
           jul = 7, ago = 8, sep = 9, set = 9, oct = 10, nov = 11, dic = 12)

# "dic", "ago.", "Septiembre*", "Marzo" -> 12, 8, 9, 3
mes_num <- function(x) {
  m <- unname(meses[str_sub(str_to_lower(str_remove_all(x, "[^A-Za-z]")), 1, 3)])
  if (anyNA(m)) stop("Mes no reconocido: ", paste(unique(x[is.na(m)]), collapse = ", "))
  m
}

fecha_ym <- function(anio, mes) as.Date(sprintf("%04d-%02d-01", as.integer(anio), as.integer(mes)))

num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  mal <- !is.na(x) & is.na(out)
  if (any(mal)) warning("Valores no numericos pasados a NA: ", paste(unique(x[mal]), collapse = ", "))
  out
}

# Verifica que la serie mensual no tenga meses faltantes ni repetidos
chequear_mensual <- function(df, nombre, por = character()) {
  chk <- df |>
    group_by(across(all_of(por))) |>
    summarise(n = n(), esperado = length(seq(min(fecha), max(fecha), by = "month")),
              dup = anyDuplicated(fecha) > 0, .groups = "drop")
  if (any(chk$n != chk$esperado) || any(chk$dup)) warning(nombre, ": serie mensual con huecos o duplicados")
  cat(sprintf("%-38s %5d filas | %s a %s\n", nombre, nrow(df), min(df$fecha), max(df$fecha)))
}

# "1er.trimestre " -> 1
trim_num <- function(x) as.integer(str_extract(x, "^[1-4]"))

# Celdas tipo "70.310a": numero con marca de calidad (a = valor indicativo, CV 10-20%)
parse_valor_flag <- function(x) {
  flag <- if_else(str_detect(x, "[A-Za-z]$"), str_extract(x, "[A-Za-z]$"), NA_character_)
  txt <- str_remove(x, "[A-Za-z]+$")
  # en las celdas con marca, el punto es separador de miles ("70.310a" = 70310)
  txt <- if_else(!is.na(flag) & str_detect(txt, "^\\d{1,3}\\.\\d{3}$"), str_remove(txt, "\\."), txt)
  tibble(valor = num(txt), flag_cv = flag)
}

guardar <- function(df, archivo) write_excel_csv(df, file.path(dir_limpias, archivo), na = "")

# ---- 1. Indice de alquiler CABA (mensual) ------------------------------

alq_idx <- read_csv(file.path(dir_crudas, "INDEX_CABA_ALQUILER_INDEX.csv"),
                    na = c("", "NA", "null"), show_col_types = FALSE) |>
  transmute(fecha = fecha_ym(2000 + as.integer(str_extract(fecha, "\\d+$")), mes_num(fecha)),
            indice_estrenar = `ESTRENAR`,
            indice_general = `INDEX CABA`,
            indice_usado = `USADO`)
chequear_mensual(alq_idx, "alquiler_index_caba")
guardar(alq_idx, "alquiler_index_caba.csv")

# ---- 2. Oferta Zonaprop CABA (mensual) ---------------------------------

zonaprop <- read_excel(file.path(dir_crudas, "indice_oferta_CABA_zonaprop.xlsx"),
                       sheet = 1, col_types = "text") |>
  filter(!is.na(year)) |>
  # la columna `fecha` mezcla fechas y texto ("Ago-25"): se reconstruye desde year y month
  transmute(fecha = fecha_ym(year, month),
            venta_indice = num(venta_indice),
            venta_cantidad = num(venta_cantidad),
            alquiler_indice = num(alquiler_indice),
            alquiler_cantidad = num(alquiler_cantidad))
chequear_mensual(zonaprop, "oferta_zonaprop_caba")
guardar(zonaprop, "oferta_zonaprop_caba.csv")

# ---- 3. IPCBA (mensual, base 2021 = 100) -------------------------------

ipcba <- read_excel(file.path(dir_crudas, "IPCBA_consolidada.xlsx"),
                    sheet = 1, skip = 3, col_types = "text") |>
  filter(!is.na(suppressWarnings(as.numeric(Mes)))) |>   # descarta notas al pie
  transmute(fecha = as.Date(as.numeric(Mes), origin = "1899-12-30"),
            ipcba_general = num(`Nivel General`),
            ipcba_viv = num(`IPCBA Viv`),
            ipcba_alq = num(`IPCBA Alq`))
chequear_mensual(ipcba, "ipcba")
guardar(ipcba, "ipcba.csv")

# ---- 4. SMVM (mensual) -------------------------------------------------

smvm <- read_csv(file.path(dir_crudas, "RegistroSMVM - RegistroSMVM.csv"),
                 show_col_types = FALSE) |>
  transmute(fecha = as.Date(indice_tiempo), smvm = salario_minimo_vital_movil_mensual)
chequear_mensual(smvm, "smvm")
guardar(smvm, "smvm.csv")

# ---- 5. Precio promedio de alquiler por ambientes (mensual, IDECBA) ----
# Pesos corrientes; unidad base de 30 m2 (1 amb), 43 m2 (2 amb) y 70 m2 (3 amb).

pv_raw <- read_excel(file.path(dir_crudas, "Valorpromedio_123ambientes_IDECBA.xlsx"),
                     sheet = 1, col_names = FALSE, col_types = "text")
precio_amb <- pv_raw |>
  select(anio = 1, mes = 2, `1` = 3, `2` = 4, `3` = 5) |>
  slice(-(1:3)) |>
  filter(!is.na(mes)) |>
  mutate(anio = as.integer(anio)) |>
  fill(anio) |>
  mutate(provisorio = str_detect(mes, "\\*"),
         fecha = fecha_ym(anio, mes_num(mes))) |>
  pivot_longer(c(`1`, `2`, `3`), names_to = "ambientes", values_to = "precio_pesos") |>
  transmute(fecha, ambientes = as.integer(ambientes), precio_pesos = num(precio_pesos), provisorio) |>
  arrange(fecha, ambientes)
chequear_mensual(precio_amb, "precio_alquiler_ambientes", por = "ambientes")
guardar(precio_amb, "precio_alquiler_ambientes.csv")

# ---- 6. Superficie en alquiler de 2 ambientes por barrio (mensual) -----
# Unidad: m2 (suma de superficie cubierta de los deptos de 2 amb ofertados en el mes).

sup_raw <- read_excel(file.path(dir_crudas, "SuperficieTotalxBarrio_IDECBA.xlsx"),
                      sheet = 1, col_names = FALSE, col_types = "text", .name_repair = "minimal")
anios <- as.integer(unlist(sup_raw[2, -1]))
for (i in seq_along(anios)[-1]) if (is.na(anios[i])) anios[i] <- anios[i - 1]
mes_txt <- unlist(sup_raw[3, -1])
cols_fecha <- tibble(col = seq_along(mes_txt) + 1L,
                     fecha = fecha_ym(anios, mes_num(mes_txt)),
                     provisorio = str_detect(mes_txt, "\\*"))

fila_fin <- which(str_detect(sup_raw[[1]], "^\\* Dato provisorio"))[1] - 1
# las columnas no tienen nombre: se nombran con su indice para poder unir con las fechas
sup_larga <- sup_raw[4:fila_fin, ] |>
  set_names(c("barrio", as.character(cols_fecha$col))) |>
  mutate(barrio = str_squish(barrio)) |>
  pivot_longer(-barrio, names_to = "col", values_to = "superficie_m2") |>
  mutate(col = as.integer(col), superficie_m2 = num(superficie_m2)) |>
  left_join(cols_fecha, by = "col") |>
  select(barrio, fecha, superficie_m2, provisorio) |>
  arrange(barrio, fecha)

sup_total <- sup_larga |> filter(barrio == "Total") |> select(-barrio)
sup_barrio <- sup_larga |> filter(barrio != "Total")

chequear_mensual(sup_total, "superficie_alquiler_2amb_total_caba")
chequear_mensual(sup_barrio, "superficie_alquiler_2amb_barrio", por = "barrio")
dif <- sup_barrio |> group_by(fecha) |> summarise(suma = sum(superficie_m2, na.rm = TRUE)) |>
  inner_join(sup_total, by = "fecha") |> mutate(dif = suma - superficie_m2)
cat(sprintf("  control: Total vs suma de %d barrios -> diferencia max = %s m2\n",
            n_distinct(sup_barrio$barrio), max(abs(dif$dif))))
guardar(sup_total, "superficie_alquiler_2amb_total_caba.csv")
guardar(sup_barrio, "superficie_alquiler_2amb_barrio.csv")

# ---- 7. Ingreso total familiar medio por zona (trimestral) -------------

itf_raw <- read_excel(file.path(dir_crudas, "ITF_CABA.xlsx"), sheet = 1,
                      col_names = FALSE, col_types = "text", .name_repair = "minimal")
zonas <- c("total", "norte", "centro", "sur")
anio_act <- NA_integer_
itf <- list()
for (i in 5:nrow(itf_raw)) {
  l <- str_squish(itf_raw[[1]][i])
  if (is.na(l)) next
  if (str_detect(l, "^\\d{4}$")) { anio_act <- as.integer(l); next }
  if (!str_detect(l, "^[1-4](er|do|to)\\. trimestre")) next
  v <- unlist(itf_raw[i, 2:5])
  itf[[length(itf) + 1]] <- bind_cols(tibble(anio = anio_act, trimestre = trim_num(l), zona = zonas),
                                      parse_valor_flag(v))
}
itf <- bind_rows(itf) |> rename(itf = valor)
cat(sprintf("%-38s %5d filas | %dT%d a %dT%d\n", "itf_zona", nrow(itf),
            min(itf$trimestre[itf$anio == min(itf$anio)]), min(itf$anio),
            max(itf$trimestre[itf$anio == max(itf$anio)]), max(itf$anio)))
guardar(itf, "itf_zona.csv")

# ---- 8. Ingresos individuales (trimestral) -----------------------------

ing_raw <- read_excel(file.path(dir_crudas, "Ingreso_individual_CABA.xlsx"), sheet = 1,
                      col_names = FALSE, col_types = "text", .name_repair = "minimal")
# la hoja declara miles de columnas vacias: se recorta a las que tienen encabezado
ult <- max(which(!is.na(unlist(ing_raw[3, ]))))
ing_raw <- ing_raw[, seq_len(ult + 1)]

anio_fila <- as.integer(unlist(ing_raw[2, ]))
for (j in seq_along(anio_fila)[-1]) if (is.na(anio_fila[j])) anio_fila[j] <- anio_fila[j - 1]
cols_val <- which(!is.na(unlist(ing_raw[3, ])) & seq_len(ncol(ing_raw)) > 1)
periodos <- tibble(col = cols_val, anio = anio_fila[cols_val],
                   trimestre = trim_num(unlist(ing_raw[3, cols_val])))

fila_ini <- 4
fila_fin <- which(str_detect(ing_raw[[1]], "^1 [A-Z]"))[1] - 1   # primera nota al pie
etiq <- ing_raw[[1]][fila_ini:fila_fin]
etiq_limpia <- etiq |> str_squish() |>
  # quita los superindices de notas al pie pegados a la etiqueta ("laborales1", "($)3", "(TMC) 4")
  (\(x) if_else(nchar(x) > 4, str_remove(x, "(?<=[\\)A-Za-z])\\s?\\d$"), x))()

vals <- ing_raw[fila_ini:fila_fin, cols_val]
es_seccion <- apply(vals, 1, function(r) all(is.na(r)))

seccion_act <- seccion_padre <- NA_character_
secc <- variable <- rep(NA_character_, length(etiq))
for (i in seq_along(etiq)) {
  l <- etiq_limpia[i]
  if (es_seccion[i]) { seccion_act <- seccion_padre <- l; next }
  # las filas que encabezan un desglose pertenecen a la seccion madre, no al desglose que abren
  secc[i] <- if (str_detect(l, "^Población perceptora de jubilación y/o pensión por")) seccion_padre else seccion_act
  variable[i] <- if (str_detect(l, "^(Q[1-5]|Norte|Centro|Sur)$")) {
    paste0("Distribución de población perceptora (%) - ", l)
  } else l
  # estas dos filas traen dato y a la vez encabezan un desglose (quintiles / zona)
  if (str_detect(l, "^Población perceptora de jubilación y/o pensión por")) seccion_act <- l
}
meta <- tibble(fila = seq_along(etiq), seccion = secc, variable = variable) |> filter(!is.na(variable))
stopifnot(!anyDuplicated(meta[c("seccion", "variable")]))

ing <- map_dfr(seq_len(nrow(periodos)), function(k) {
  j <- periodos$col[k]
  bind_cols(meta,
            tibble(anio = periodos$anio[k], trimestre = periodos$trimestre[k],
                   valor = num(ing_raw[[j]][fila_ini:fila_fin][meta$fila]),
                   flag_cv = ing_raw[[j + 1]][fila_ini:fila_fin][meta$fila]))
}) |>
  mutate(flag_cv = if_else(flag_cv == "a", "a", NA_character_)) |>
  select(anio, trimestre, seccion, variable, valor, flag_cv) |>
  arrange(seccion, variable, anio, trimestre)
cat(sprintf("%-38s %5d filas | %d variables | %dT%d a %dT%d\n", "ingreso_individual", nrow(ing),
            nrow(meta), min(periodos$trimestre[periodos$anio == min(periodos$anio)]), min(periodos$anio),
            max(periodos$trimestre[periodos$anio == max(periodos$anio)]), max(periodos$anio)))
guardar(ing, "ingreso_individual.csv")

cat("\nListo. Tablas en:", normalizePath(dir_limpias), "\n")
