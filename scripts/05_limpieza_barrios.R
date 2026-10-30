# 05_limpieza_barrios.R
# Limpia "Bases crudas/alquileres_caba_2amb_50m2.xlsx" (precio de alquiler de un 2 amb de 50 m2 por barrio,
# Zonaprop, pesos corrientes por mes) y deja "Bases limpias/zonaprop_barrio.csv".
# Ejecutar desde la raiz del proyecto. No modifica la base cruda.
#
# Problemas de la base cruda y como se tratan:
#  1. Imputacion por arrastre: cuando falta un mes, la base repite el ultimo valor observado en pesos
#     nominales (columnas *_imputado y *_mes_origen). Con inflacion alta, un valor arrastrado muchos meses
#     queda absurdamente bajo (ej. Villa Soldati: $15.493 de 2021 a 2026, 1,7 % del promedio CABA).
#     Regla: se conservan los valores observados y los imputados con hasta MAX_EDAD_IMPUTACION meses
#     de antiguedad; el resto pasa a NA.
#  2. dic 2015 esta en otra escala: la mediana de los barrios es 3,2 veces el indice CABA de ese mes y
#     en ene 2016 vuelve a ~0,93. Se descarta dic 2015 y todo valor arrastrado desde ese mes.
#  3. Nombres de barrio: se pasan a los nombres de superficie_alquiler_2amb_barrio.csv (IDECBA).

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

MAX_EDAD_IMPUTACION <- 3     # meses
MES_DESCARTADO <- as.Date("2015-12-01")

crudo <- "Bases crudas/alquileres_caba_2amb_50m2.xlsx"
x <- read_excel(crudo, col_types = "text")

serial <- function(v) as.Date(suppressWarnings(as.numeric(v)), origin = "1899-12-30")
meses_entre <- function(a, b) (as.integer(format(a, "%Y")) - as.integer(format(b, "%Y"))) * 12L +
  as.integer(format(a, "%m")) - as.integer(format(b, "%m"))

# ---- nombres de barrio ----------------------------------------------------
idecba <- read_csv("Bases limpias/superficie_alquiler_2amb_barrio.csv", show_col_types = FALSE) |>
  distinct(barrio) |> pull()
sin_acento <- function(s) str_to_lower(iconv(s, to = "ASCII//TRANSLIT")) |> str_remove_all("[^a-z ]") |> str_squish()
manual <- c("La Boca" = "Boca", "Lugano" = "Villa Lugano", "Monserrat" = "Montserrat",
            "Paternal" = "La Paternal", "Santa Rita" = "Villa Santa Rita", "Villa Gral Mitre" = "Villa Gral. Mitre")
nombre_idecba <- function(b) {
  b <- if_else(b %in% names(manual), unname(manual[b]), b)
  idecba[match(sin_acento(b), sin_acento(idecba))]
}
mapa <- tibble(barrio_crudo = unique(x$barrio)) |> mutate(barrio = nombre_idecba(barrio_crudo))
stopifnot(!anyNA(mapa$barrio), !anyDuplicated(mapa$barrio), setequal(mapa$barrio, idecba))

# ---- una columna por tipo, con la regla de validez -------------------------
tipo <- function(x, col, flag, origen, nombre) {
  tibble(periodo = x$periodo, barrio_crudo = x$barrio, valor = suppressWarnings(as.numeric(x[[col]])),
         imputado = x[[flag]] == "TRUE", origen = serial(x[[origen]]),
         fecha = serial(x$periodo)) |>
    mutate(edad = meses_entre(fecha, origen),
           motivo = case_when(
             is.na(valor) ~ "sin dato",
             fecha == MES_DESCARTADO | (!is.na(origen) & origen == MES_DESCARTADO) ~ "dic 2015",
             imputado & edad > MAX_EDAD_IMPUTACION ~ "arrastre viejo",
             TRUE ~ "ok"),
           tipo = nombre)
}
t_all <- bind_rows(
  tipo(x, "index_ars_mes", "index_imputado", "index_mes_origen", "general"),
  tipo(x, "estrenar_ars_mes", "estrenar_imputado", "estrenar_mes_origen", "estrenar"),
  tipo(x, "usado_ars_mes", "usado_imputado", "usado_mes_origen", "usado"))

cat("Valores por tipo y motivo (filas de la base cruda):\n")
print(t_all |> count(tipo, motivo) |> pivot_wider(names_from = motivo, values_from = n, values_fill = 0))

limpia <- t_all |>
  mutate(valor = if_else(motivo == "ok", valor, NA_real_)) |>
  select(fecha, barrio_crudo, tipo, valor) |>
  pivot_wider(names_from = tipo, values_from = valor) |>
  left_join(t_all |> filter(tipo == "general") |>
              transmute(fecha, barrio_crudo, imputado = imputado & motivo == "ok",
                        meses_desde_dato = if_else(motivo == "ok", edad, NA_integer_)),
            by = c("fecha", "barrio_crudo")) |>
  left_join(mapa, by = "barrio_crudo") |>
  transmute(fecha, barrio, precio_pesos = general, precio_estrenar = estrenar, precio_usado = usado,
            imputado, meses_desde_dato) |>
  filter(!(is.na(precio_pesos) & is.na(precio_estrenar) & is.na(precio_usado)), fecha != MES_DESCARTADO) |>
  arrange(barrio, fecha)

# ---- controles -------------------------------------------------------------
caba <- read_csv("Bases limpias/alquiler_index_caba.csv", show_col_types = FALSE) |> select(fecha, caba = indice_general)
chk <- limpia |> filter(!is.na(precio_pesos)) |> left_join(caba, by = "fecha") |> mutate(rel = precio_pesos / caba)
cat(sprintf("\nControl vs promedio CABA (precio barrio / INDEX CABA): min %.2f | p1 %.2f | mediana %.2f | max %.2f\n",
            min(chk$rel), quantile(chk$rel, .01), median(chk$rel), max(chk$rel)))
dup <- limpia |> count(barrio, fecha) |> filter(n > 1)
stopifnot(nrow(dup) == 0)
cat(sprintf("zonaprop_barrio: %d filas | %d barrios | %s a %s | %d con precio_pesos\n",
            nrow(limpia), n_distinct(limpia$barrio), min(limpia$fecha), max(limpia$fecha), sum(!is.na(limpia$precio_pesos))))
cat("\nBarrios con menos cobertura de precio_pesos (meses):\n")
print(limpia |> group_by(barrio) |> summarise(meses = sum(!is.na(precio_pesos))) |> arrange(meses) |> head(8))

write_excel_csv(limpia, "Bases limpias/zonaprop_barrio.csv", na = "")
