# 03_dashboard.R
# Genera Dashboard/dashboard.html (un solo archivo, sin dependencias) a partir de "Bases limpias/".
# Ejecutar desde la raiz del proyecto.

library(dplyr)
library(readr)
library(jsonlite)

dir_limpias <- "Bases limpias"
dir_out <- "Dashboard"
dir.create(dir_out, showWarnings = FALSE)
leer <- function(f) read_csv(file.path(dir_limpias, f), show_col_types = FALSE)

desde <- as.Date("2015-01-01")

# ---- mensual ------------------------------------------------------------
amb <- leer("precio_alquiler_ambientes.csv") |>
  mutate(v = paste0("p", ambientes)) |>
  select(fecha, v, precio_pesos) |>
  tidyr::pivot_wider(names_from = v, values_from = precio_pesos)

monthly <- leer("ipcba.csv") |> select(fecha, ipc = ipcba_general) |>
  left_join(amb, by = "fecha") |>
  left_join(leer("alquiler_index_caba.csv") |>
              select(fecha, alq_idx = indice_general, alq_est = indice_estrenar, alq_usa = indice_usado), by = "fecha") |>
  left_join(leer("oferta_zonaprop_caba.csv") |>
              select(fecha, zp_alq = alquiler_indice, zp_cant = alquiler_cantidad), by = "fecha") |>
  left_join(leer("superficie_alquiler_2amb_total_caba.csv") |> select(fecha, m2 = superficie_m2), by = "fecha") |>
  filter(fecha >= desde) |>
  mutate(d = format(fecha, "%Y-%m"), .before = 1) |>
  select(-fecha)

# ---- trimestral (del panel) --------------------------------------------
panel <- leer("panel_trimestral.csv")
quarterly <- panel |>
  filter(fecha_trim >= as.Date("2014-07-01")) |>
  transmute(anio, trim = trimestre, ipc = ipcba_general_prom,
            p1 = precio_alq_1amb_prom, p2 = precio_alq_2amb_prom, p3 = precio_alq_3amb_prom,
            ing = ing_perceptores_prom,
            itf_total, itf_norte, itf_centro, itf_sur)

ref_ipc <- panel$ipcba_general_prom[max(which(!is.na(panel$ipcba_general_prom)))]

# ---- estadisticas citadas en las notas (calculadas, no escritas a mano) --
m <- monthly |> mutate(avisos = m2 / 43, ratio = alq_idx / (p2 * 50 / 43) * 100)
ok_av <- m |> filter(!is.na(zp_cant), !is.na(m2))
ok_pr <- m |> filter(!is.na(alq_idx), !is.na(p2))
var_log <- function(x) diff(log(x))
mes_txt <- function(d) paste(c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")[as.integer(substr(d, 6, 7))], substr(d, 1, 4))
primero <- ok_pr[1, ]; ultimo <- ok_pr[nrow(ok_pr), ]
stats <- list(
  cor_avisos = cor(ok_av$zp_cant, ok_av$avisos),
  ratio_ini = primero$ratio, ratio_ini_mes = mes_txt(primero$d),
  ratio_fin = ultimo$ratio, ratio_fin_mes = mes_txt(ultimo$d),
  cor_var = cor(var_log(ok_pr$alq_idx), var_log(ok_pr$p2))
)
print(unlist(stats))

datos <- list(monthly = monthly, quarterly = quarterly, ref_ipc = ref_ipc, stats = stats)
json <- toJSON(datos, dataframe = "rows", na = "null", auto_unbox = TRUE, digits = 8)

html <- paste(readLines("scripts/dashboard_template.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
stopifnot(grepl("__DATA__", html, fixed = TRUE))
html <- sub("__DATA__", json, html, fixed = TRUE)
con <- file(file.path(dir_out, "dashboard.html"), open = "wb")
writeBin(charToRaw(enc2utf8(html)), con)
close(con)
cat(sprintf("Dashboard/dashboard.html (%.0f KB) | %d meses, %d trimestres\n",
            file.size(file.path(dir_out, "dashboard.html")) / 1024, nrow(monthly), nrow(quarterly)))
