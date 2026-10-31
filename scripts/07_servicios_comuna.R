# 07_servicios_comuna.R
# Arma tablas de servicios (subte, espacios verdes) y de precio por comuna, a partir de:
#   Bases crudas/geo/{barrios.geojson, subte_estaciones.geojson, espacios_verdes_publicos.geojson, poblacion_por_barrio.csv}
#   Bases limpias/{zonaprop_barrio.csv, superficie_alquiler_2amb_barrio.csv, ipcba.csv}
# Salidas en Bases limpias/:  servicios_barrio.csv, servicios_comuna.csv, precio_comuna.csv
# Ejecutar desde la raiz del proyecto.

library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

CRS_M <- 32721                 # UTM 21S (metros), para areas y distancias
MIN_COBERTURA <- 0.5           # precio de comuna solo si los barrios con dato suman >= 50 % de los m2 ofertados

sin_acento <- function(s) str_to_lower(iconv(s, to = "ASCII//TRANSLIT")) |> str_remove_all("[^a-z ]") |> str_squish()
alias <- c("boca" = "la boca", "la paternal" = "paternal", "montserrat" = "monserrat")
clave <- function(s) { k <- sin_acento(s); if_else(k %in% names(alias), unname(alias[k]), k) }

# ---- capas ---------------------------------------------------------------
barrios <- st_read("Bases crudas/geo/barrios.geojson", quiet = TRUE) |> st_make_valid() |>
  transmute(key = clave(nombre), barrio = nombre, comuna = as.integer(comuna)) |> st_transform(CRS_M)
barrios$area_km2 <- as.numeric(st_area(barrios)) / 1e6

subte <- st_read("Bases crudas/geo/subte_estaciones.geojson", quiet = TRUE) |> st_transform(CRS_M)
verdes <- st_read("Bases crudas/geo/espacios_verdes_publicos.geojson", quiet = TRUE) |> st_make_valid() |>
  st_transform(CRS_M) |> select(nombre, clasificac)

pob <- read_csv("Bases crudas/geo/poblacion_por_barrio.csv", show_col_types = FALSE) |>
  transmute(key = clave(BARRIO), poblacion = POBLACION)
stopifnot(setequal(pob$key, barrios$key))

# ---- servicios por barrio ---------------------------------------------------
# subte: estaciones dentro del barrio y distancia del punto representativo del barrio a la estacion mas cercana
en_barrio <- st_join(subte, barrios |> select(key), join = st_within) |> st_drop_geometry() |> count(key, name = "estaciones_subte")
punto <- suppressWarnings(st_point_on_surface(barrios))
dist_subte <- apply(st_distance(punto, subte), 1, min)

# espacios verdes: area que cae dentro de cada barrio (interseccion; los poligonos no se superponen entre si por barrio)
verde_u <- st_union(verdes)                                      # une solapamientos para no contar dos veces
verde_barrio <- st_intersection(barrios |> select(key), verde_u) |>
  mutate(verde_m2 = as.numeric(st_area(geometry))) |> st_drop_geometry() |> group_by(key) |> summarise(verde_m2 = sum(verde_m2))

servicios_barrio <- barrios |> st_drop_geometry() |>
  mutate(dist_subte_m = dist_subte) |>
  left_join(pob, by = "key") |>
  left_join(en_barrio, by = "key") |>
  left_join(verde_barrio, by = "key") |>
  mutate(estaciones_subte = coalesce(estaciones_subte, 0L), verde_m2 = coalesce(verde_m2, 0),
         estaciones_por_km2 = estaciones_subte / area_km2,
         verde_pct_area = verde_m2 / (area_km2 * 1e6) * 100,
         verde_m2_por_hab = verde_m2 / poblacion) |>
  select(barrio, comuna, poblacion, area_km2, estaciones_subte, estaciones_por_km2, dist_subte_m,
         verde_m2, verde_pct_area, verde_m2_por_hab, key)

# ---- servicios por comuna -----------------------------------------------------
servicios_comuna <- servicios_barrio |> group_by(comuna) |>
  summarise(dist_subte_m = weighted.mean(dist_subte_m, poblacion),   # ponderada por poblacion (antes de sumar poblacion)
            barrios = n(), poblacion = sum(poblacion), area_km2 = sum(area_km2),
            estaciones_subte = sum(estaciones_subte), verde_m2 = sum(verde_m2), .groups = "drop") |>
  mutate(estaciones_por_km2 = estaciones_subte / area_km2, verde_pct_area = verde_m2 / (area_km2 * 1e6) * 100,
         verde_m2_por_hab = verde_m2 / poblacion)

# ---- precio por comuna (promedio de barrios ponderado por m2 ofertados de 2 amb) ----
ipc <- read_csv("Bases limpias/ipcba.csv", show_col_types = FALSE) |> select(fecha, ipc = ipcba_general)
ref_ipc <- mean(tail(ipc$ipc, 3))
com <- servicios_barrio |> select(key, comuna)
peso <- read_csv("Bases limpias/superficie_alquiler_2amb_barrio.csv", show_col_types = FALSE) |>
  mutate(key = clave(barrio)) |> select(key, fecha, m2 = superficie_m2)
pb <- read_csv("Bases limpias/zonaprop_barrio.csv", show_col_types = FALSE) |>
  mutate(key = clave(barrio)) |> select(key, fecha, precio_pesos) |>
  left_join(peso, by = c("key", "fecha"))
# peso total de la comuna por mes (todos los barrios, tengan o no precio): base para medir cobertura
peso_tot <- peso |> left_join(com, by = "key") |> group_by(comuna, fecha) |> summarise(m2_total = sum(m2, na.rm = TRUE), .groups = "drop")
precio_comuna <- pb |> filter(!is.na(precio_pesos), !is.na(m2), m2 > 0) |> left_join(com, by = "key") |>
  group_by(comuna, fecha) |>
  summarise(precio_pesos = weighted.mean(precio_pesos, m2), m2_con_precio = sum(m2), n_barrios = n(), .groups = "drop") |>
  left_join(peso_tot, by = c("comuna", "fecha")) |>
  mutate(cobertura = m2_con_precio / m2_total,
         precio_pesos = if_else(cobertura >= MIN_COBERTURA, precio_pesos, NA_real_)) |>
  left_join(ipc, by = "fecha") |>
  mutate(precio_real = precio_pesos * ref_ipc / ipc) |>
  select(fecha, comuna, precio_pesos, precio_real, cobertura, n_barrios) |>
  arrange(comuna, fecha)

# ---- controles ------------------------------------------------------------------
cat(sprintf("Subte: %d estaciones; %d caen dentro de algun barrio.\n", nrow(subte), sum(servicios_barrio$estaciones_subte)))
cat(sprintf("Poblacion total: %s | verde total: %.1f km2 (%.1f %% de la ciudad)\n",
            format(sum(servicios_barrio$poblacion), big.mark = "."), sum(servicios_barrio$verde_m2) / 1e6,
            sum(servicios_barrio$verde_m2) / (sum(servicios_barrio$area_km2) * 1e6) * 100))
cat(sprintf("Cobertura de precio_comuna (>= %.0f %%): %d de %d comuna-mes con precio\n", MIN_COBERTURA * 100,
            sum(!is.na(precio_comuna$precio_pesos)), nrow(precio_comuna)))
print(servicios_comuna |> mutate(across(c(area_km2, estaciones_por_km2, verde_pct_area, verde_m2_por_hab), ~ round(.x, 2)),
                                 dist_subte_m = round(dist_subte_m), verde_m2 = round(verde_m2)), n = 15)
last <- max(precio_comuna$fecha)
cat("\nPrecio por comuna, ultimo mes (", format(last), "):\n", sep = "")
print(precio_comuna |> filter(fecha == last) |> mutate(precio_real = round(precio_real), cobertura = round(cobertura, 2)))

write_excel_csv(servicios_barrio |> select(-key), "Bases limpias/servicios_barrio.csv", na = "")
write_excel_csv(servicios_comuna, "Bases limpias/servicios_comuna.csv", na = "")
write_excel_csv(precio_comuna, "Bases limpias/precio_comuna.csv", na = "")
