# 02_panel_trimestral.R
# Construye el panel trimestral CABA a partir de "Bases limpias/".
# Una fila por trimestre; una columna por variable. Ejecutar desde la raiz del proyecto.
#
# Reglas de agregacion de las series mensuales (solo trimestres con los 3 meses; si no, NA):
#   - indices / precios (alquiler, Zonaprop, IPCBA, precio por ambientes): promedio del trimestre
#     (_prom) y valor del ultimo mes (_cierre, solo en las series de precios e indices principales)
#   - SMVM: valor del mes intermedio del trimestre (mismo criterio que el IDECBA)
#   - superficie en oferta: promedio mensual del trimestre (m2 por mes)
# Series en pesos: se agregan versiones _real en pesos constantes del ultimo trimestre con IPCBA
# completo, deflactando con el promedio trimestral del IPCBA nivel general.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

dir_limpias <- "Bases limpias"
leer <- function(f) read_csv(file.path(dir_limpias, f), show_col_types = FALSE)

# ---- helpers -----------------------------------------------------------

con_trim <- function(df) {
  df |> mutate(anio = as.integer(format(fecha, "%Y")),
               trimestre = (as.integer(format(fecha, "%m")) - 1L) %/% 3L + 1L)
}

# Agrega una serie mensual a trimestral. `vars` = columnas; `cierre` = columnas de las que
# ademas se guarda el valor del ultimo mes. Devuelve NA si el trimestre no tiene 3 meses.
agregar <- function(df, vars, cierre = character(), prefijo = "", intermedio = FALSE, suma = FALSE) {
  df <- con_trim(df) |> arrange(fecha)
  out <- df |>
    group_by(anio, trimestre) |>
    summarise(n = n(),
              across(all_of(vars),
                     list(prom = ~ if (intermedio) .x[2] else if (suma) sum(.x) else mean(.x)),
                     .names = "{.col}__{.fn}"),
              across(all_of(cierre), ~ last(.x), .names = "{.col}__cierre"),
              .groups = "drop") |>
    mutate(across(-c(anio, trimestre, n), ~ if_else(n == 3, .x, NA_real_))) |>
    select(-n)
  names(out) <- str_replace(names(out), "__prom$", if (intermedio) "" else "_prom") |>
    str_replace("__cierre$", "_cierre")
  names(out)[-(1:2)] <- paste0(prefijo, names(out)[-(1:2)])
  out
}

# ---- series mensuales -> trimestrales ----------------------------------

alq <- agregar(leer("alquiler_index_caba.csv"),
               vars = c("indice_estrenar", "indice_general", "indice_usado"),
               cierre = "indice_general", prefijo = "alq_")

zp <- agregar(leer("oferta_zonaprop_caba.csv"),
              vars = c("venta_indice", "venta_cantidad", "alquiler_indice", "alquiler_cantidad"),
              prefijo = "zonaprop_")

ipc <- agregar(leer("ipcba.csv"),
               vars = c("ipcba_general", "ipcba_viv", "ipcba_alq"),
               cierre = "ipcba_general")

smvm <- agregar(leer("smvm.csv"), vars = "smvm", intermedio = TRUE)

amb <- leer("precio_alquiler_ambientes.csv") |>
  mutate(var = paste0("precio_alq_", ambientes, "amb")) |>
  select(fecha, var, precio_pesos) |>
  pivot_wider(names_from = var, values_from = precio_pesos)
amb <- agregar(amb, vars = c("precio_alq_1amb", "precio_alq_2amb", "precio_alq_3amb"))

sup <- agregar(leer("superficie_alquiler_2amb_total_caba.csv") |> select(fecha, superficie_m2),
               vars = "superficie_m2", prefijo = "oferta_2amb_")
names(sup) <- str_replace(names(sup), "oferta_2amb_superficie_m2_prom", "oferta_2amb_m2_mensual_prom")

# ---- series trimestrales -----------------------------------------------

itf <- leer("itf_zona.csv") |>
  mutate(flag_cv = !is.na(flag_cv)) |>
  pivot_wider(names_from = zona, values_from = c(itf, flag_cv), names_glue = "{.value}_{zona}") |>
  rename_with(~ str_replace(.x, "^flag_cv_", "itf_indicativo_"), starts_with("flag_cv_"))

# Variables de ingreso individual que se llevan al panel (el resto queda en ingreso_individual.csv)
ing_vars <- c(
  ing_poblacion_perceptora   = "Población perceptora",
  ing_perceptores_prom       = "Ingreso de perceptores de ingreso (promedio en $)",
  ing_laboral_prom           = "Ingreso laboral de perceptores (promedio en $)",
  ing_laboral_asalariados    = "Ingreso laboral de asalariados (promedio en $)",
  ing_laboral_cuenta_propia  = "Ingreso laboral de trabajador por cuenta propia",
  ing_no_laboral_prom        = "Ingreso no laboral perceptores (promedio en $)"
)
ing <- leer("ingreso_individual.csv") |>
  filter(variable %in% ing_vars,
         !(variable == "Población perceptora" & seccion != "Ingresos individuales")) |>
  mutate(var = names(ing_vars)[match(variable, ing_vars)]) |>
  select(anio, trimestre, var, valor) |>
  pivot_wider(names_from = var, values_from = valor)

# ---- panel -------------------------------------------------------------

trim_ini <- c(2010L, 1L)
trim_fin <- c(2026L, 3L)
grilla <- expand_grid(anio = trim_ini[1]:trim_fin[1], trimestre = 1:4) |>
  filter(!(anio == trim_fin[1] & trimestre > trim_fin[2]))

panel <- reduce(list(alq, zp, ipc, smvm, amb, sup, itf, ing),
                \(a, b) left_join(a, b, by = c("anio", "trimestre")),
                .init = grilla) |>
  mutate(fecha_trim = as.Date(sprintf("%04d-%02d-01", anio, (trimestre - 1L) * 3L + 1L)), .after = trimestre)

# Pesos constantes del ultimo trimestre con IPCBA disponible
ref <- panel |> filter(!is.na(ipcba_general_prom)) |> slice_tail(n = 1)
cat(sprintf("Base de deflactacion: pesos constantes de %dT%d (IPCBA general prom = %.2f)\n",
            ref$trimestre, ref$anio, ref$ipcba_general_prom))
deflactor <- ref$ipcba_general_prom / panel$ipcba_general_prom

en_pesos <- c("smvm", "precio_alq_1amb_prom", "precio_alq_2amb_prom", "precio_alq_3amb_prom",
              "itf_total", "itf_norte", "itf_centro", "itf_sur",
              "ing_perceptores_prom", "ing_laboral_prom", "ing_laboral_asalariados",
              "ing_laboral_cuenta_propia", "ing_no_laboral_prom")
for (v in en_pesos) panel[[paste0(v, "_real")]] <- panel[[v]] * deflactor

# Razones utiles para el cruce (sin necesidad de deflactar)
panel <- panel |>
  mutate(alq_2amb_sobre_itf    = precio_alq_2amb_prom / itf_total,
         alq_2amb_sobre_smvm   = precio_alq_2amb_prom / smvm,
         alq_2amb_sobre_ing_perceptores = precio_alq_2amb_prom / ing_perceptores_prom)

# Solo se conservan los trimestres con algun dato
panel <- panel |> filter(if_any(-c(anio, trimestre, fecha_trim), ~ !is.na(.x)))

cat(sprintf("panel_trimestral: %d trimestres x %d columnas | %dT%d a %dT%d\n",
            nrow(panel), ncol(panel), first(panel$trimestre), first(panel$anio),
            last(panel$trimestre), last(panel$anio)))
cat("Trimestres con datos en TODAS las fuentes principales:\n")
completos <- panel |> filter(!is.na(alq_indice_general_prom), !is.na(zonaprop_venta_indice_prom),
                             !is.na(ipcba_general_prom), !is.na(smvm), !is.na(precio_alq_2amb_prom),
                             !is.na(oferta_2amb_m2_mensual_prom), !is.na(itf_total), !is.na(ing_perceptores_prom))
cat(sprintf("  %d trimestres: %dT%d a %dT%d\n", nrow(completos), first(completos$trimestre),
            first(completos$anio), last(completos$trimestre), last(completos$anio)))

write_excel_csv(panel, file.path(dir_limpias, "panel_trimestral.csv"), na = "")
