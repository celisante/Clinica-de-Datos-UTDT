# 10_oferta_barrio.R
# Cantidad de avisos de alquiler de 2 ambientes por barrio y mes: combina IDECBA (como se reparte la oferta entre barrios)
# con Zonaprop (cuantos avisos hay en total).
#
# Metodo:
#  1. IDECBA publica m2 ofertados de 2 amb por barrio. Dividido por el estandar metodologico (43 m2) da departamentos ofertados
#     (la muestra por barrio es chica: de ahi el paso 2).
#  2. Participacion de cada barrio = suma de m2 del barrio en los ultimos 3 meses / suma de m2 de todos los barrios en esos mismos
#     3 meses (ventana movil; en los 2 primeros meses de la serie usa los meses disponibles). Suavizar evita que un cero aislado
#     borre la oferta del barrio y evita dividir por cero cuando un mes tiene total 0.
#  3. Avisos estimados del barrio = participacion x total de avisos de Zonaprop (alquiler_cantidad) del mes. Las participaciones
#     suman 1, asi que los barrios suman exactamente el total de Zonaprop.
#  4. Zonaprop termina en ago 2025: despues se proyecta el total con la variacion del total IDECBA (EXTENSION = "idecba") o repitiendo
#     el ultimo valor (EXTENSION = "ultimo"). Esos meses quedan marcados como proyectados.
# Salida: Bases limpias/oferta_barrio.csv.  Ejecutar desde la raiz del proyecto.

library(dplyr)
library(tidyr)
library(readr)
library(zoo)

M2_POR_DEPTO <- 43          # estandar metodologico de IDECBA para 2 ambientes
VENTANA <- 3                # meses de la ventana movil
EXTENSION <- "idecba"       # "idecba" o "ultimo"
DESDE <- as.Date("2015-01-01")

sup <- read_csv("Bases limpias/superficie_alquiler_2amb_barrio.csv", show_col_types = FALSE)
stopifnot(!anyNA(sup$superficie_m2))
zp <- read_csv("Bases limpias/oferta_zonaprop_caba.csv", show_col_types = FALSE) |> select(fecha, zp_total = alquiler_cantidad)

meses <- sort(unique(sup$fecha))
ult_zp <- max(zp$fecha)

# ---- participacion suavizada ------------------------------------------------
total_m2 <- sup |> group_by(fecha) |> summarise(m2_total = sum(superficie_m2), .groups = "drop") |> arrange(fecha)
den <- total_m2 |> mutate(den = rollapplyr(m2_total, VENTANA, sum, partial = TRUE),
                          ventana_meses = pmin(row_number(), VENTANA)) |> select(fecha, den, ventana_meses)

part <- sup |> arrange(barrio, fecha) |> group_by(barrio) |>
  mutate(num = rollapplyr(superficie_m2, VENTANA, sum, partial = TRUE)) |> ungroup() |>
  left_join(den, by = "fecha") |>
  mutate(participacion = if_else(den > 0, num / den, NA_real_))

# ---- total de Zonaprop, con proyeccion despues de su ultimo mes ---------------
total <- tibble(fecha = meses) |> left_join(zp, by = "fecha") |> left_join(den |> select(fecha, den), by = "fecha")
ref <- total |> filter(fecha == ult_zp)
total <- total |> mutate(
  proyectado = fecha > ult_zp,
  zp_total = case_when(
    !proyectado ~ zp_total,
    EXTENSION == "idecba" & ref$den > 0 ~ ref$zp_total * den / ref$den,
    TRUE ~ ref$zp_total))
stopifnot(!anyNA(total$zp_total[total$fecha >= DESDE]))

out <- part |>
  left_join(total |> select(fecha, zp_total, proyectado), by = "fecha") |>
  transmute(fecha, barrio, m2_idecba = superficie_m2, unidades_idecba = round(superficie_m2 / M2_POR_DEPTO, 1),
            participacion = round(participacion, 5), avisos_estimados = round(participacion * zp_total, 1),
            total_zonaprop = round(zp_total, 1), proyectado, ventana_meses, sin_dato = is.na(participacion)) |>
  filter(fecha >= DESDE) |> arrange(barrio, fecha)

# ---- controles -----------------------------------------------------------------
chk <- out |> group_by(fecha) |> summarise(suma = sum(avisos_estimados, na.rm = TRUE), total = first(total_zonaprop), .groups = "drop")
cat(sprintf("Control: los barrios suman el total de Zonaprop (diferencia maxima %.2f avisos por mes, por redondeo)\n", max(abs(chk$suma - chk$total))))
cat(sprintf("Filas: %d | barrios: %d | %s a %s | proyectados: %d meses (desde %s)\n", nrow(out), n_distinct(out$barrio),
            min(out$fecha), max(out$fecha), n_distinct(out$fecha[out$proyectado]), format(min(out$fecha[out$proyectado]))))
cat(sprintf("m2 IDECBA = 0 en el mes: %d de %d barrio-mes (%.0f %%)\n", sum(out$m2_idecba == 0), nrow(out), mean(out$m2_idecba == 0) * 100))
cat(sprintf("Avisos estimados = 0 tras suavizar: %d (%.1f %%) | sin dato: %d\n", sum(out$avisos_estimados == 0, na.rm = TRUE),
            mean(out$avisos_estimados == 0, na.rm = TRUE) * 100, sum(out$sin_dato)))
cat("Total Zonaprop (observado ultimo mes -> proyectado ultimo mes):\n")
print(out |> distinct(fecha, total_zonaprop, proyectado) |> filter(fecha >= ult_zp - 62) |> mutate(fecha = format(fecha)), n = 20)
cat("\nBarrios con mayor y menor promedio de avisos estimados:\n")
print(out |> group_by(barrio) |> summarise(media = round(mean(avisos_estimados, na.rm = TRUE), 1), ceros = sum(avisos_estimados == 0, na.rm = TRUE)) |> arrange(desc(media)) |> slice(c(1:4, 45:48)))

write_excel_csv(out, "Bases limpias/oferta_barrio.csv", na = "")
