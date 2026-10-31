# 08_mapa_servicios.R
# Figura que cruza el precio de alquiler con servicios (subte, espacios verdes):
#   fila 1: mapas por comuna (precio, subte, verde); fila 2: dispersion por barrio (precio vs servicios).
# Salida: Mapas/mapa_comunas_servicios.png.  Requiere haber corrido scripts/05 y 07.
# Ejecutar desde la raiz del proyecto.

library(sf)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(grid)

dir.create("Mapas", showWarnings = FALSE)
sin_acento <- function(s) str_to_lower(iconv(s, to = "ASCII//TRANSLIT")) |> str_remove_all("[^a-z ]") |> str_squish()
alias <- c("boca" = "la boca", "la paternal" = "paternal", "montserrat" = "monserrat")
clave <- function(s) { k <- sin_acento(s); if_else(k %in% names(alias), unname(alias[k]), k) }

# ---- datos -----------------------------------------------------------------
g <- st_read("Bases crudas/geo/barrios.geojson", quiet = TRUE) |> st_make_valid() |> mutate(comuna = as.integer(comuna))
comunas <- g |> group_by(comuna) |> summarise(geometry = st_union(geometry), .groups = "drop")
subte <- st_read("Bases crudas/geo/subte_estaciones.geojson", quiet = TRUE)

sv_com <- read_csv("Bases limpias/servicios_comuna.csv", show_col_types = FALSE)
sv_bar <- read_csv("Bases limpias/servicios_barrio.csv", show_col_types = FALSE) |> mutate(key = clave(barrio))
pc <- read_csv("Bases limpias/precio_comuna.csv", show_col_types = FALSE)
ult <- max(pc$fecha[!is.na(pc$precio_real)])
lbl_mes <- function(d) paste(c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")[as.integer(format(d, "%m"))], format(d, "%Y"))

ipc <- read_csv("Bases limpias/ipcba.csv", show_col_types = FALSE) |> select(fecha, ipc = ipcba_general)
ref_ipc <- mean(tail(ipc$ipc, 3))
pb <- read_csv("Bases limpias/zonaprop_barrio.csv", show_col_types = FALSE) |>
  filter(fecha == ult, !is.na(precio_pesos)) |> left_join(ipc, by = "fecha") |>
  mutate(key = clave(barrio), precio_real = precio_pesos * ref_ipc / ipc) |>
  inner_join(sv_bar |> select(key, comuna, dist_subte_m, verde_m2_por_hab, estaciones_por_km2), by = "key")

mapa_com <- comunas |> left_join(sv_com, by = "comuna") |> left_join(pc |> filter(fecha == ult) |> select(comuna, precio_real), by = "comuna")

# ---- mapas por comuna ---------------------------------------------------------------
tema_mapa <- theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12.5), plot.subtitle = element_text(color = "grey35", size = 9, margin = margin(b = 4)),
        legend.position = "bottom", legend.key.width = unit(1.1, "cm"), legend.key.height = unit(.28, "cm"),
        legend.title = element_text(size = 8.5), legend.text = element_text(size = 8), plot.margin = margin(6, 8, 4, 8),
        plot.background = element_rect(fill = "#fcfcfb", color = NA))
mk <- function(var, titulo, sub, cols, nombre, fmt) {
  d <- mapa_com
  ggplot(d) + geom_sf(aes(fill = .data[[var]]), color = "#fcfcfb", linewidth = .5) +
    geom_sf_label(aes(label = comuna), size = 3, fontface = "bold", fill = alpha("#fcfcfb", .88), label.size = 0, label.padding = unit(1.6, "pt")) +
    scale_fill_gradientn(colours = cols, labels = fmt, name = nombre, guide = guide_colorbar(title.position = "top")) +
    labs(title = titulo, subtitle = sub) + tema_mapa
}
# escala viridis invertida en todas las variables (apta para daltonismo): violeta oscuro = valor alto, amarillo = valor bajo
azul <- viridisLite::viridis(5, begin = .02, end = .9, direction = -1)                      # precio: viridis
naranja <- viridisLite::viridis(5, begin = .02, end = .9, direction = -1)   # subte: viridis
verde <- viridisLite::viridis(5, begin = .02, end = .9, direction = -1)    # verde: viridis
m1 <- mk("precio_real", "Precio de alquiler", paste0("2 amb de 50 m², ", lbl_mes(ult), ", pesos constantes del 3T 2026"), azul,
         "Miles de pesos por mes", function(v) round(v / 1e3))
m2 <- mk("estaciones_por_km2", "Subte", "Estaciones por km² (90 estaciones; un combinado cuenta una vez por línea)", naranja,
         "Estaciones por km²", function(v) format(round(v, 1), decimal.mark = ","))
m3 <- mk("verde_m2_por_hab", "Espacios verdes públicos", "m² de espacio verde por habitante (población del censo)", verde,
         "m² por habitante", function(v) format(round(v, 1), decimal.mark = ","))

# ---- dispersion por barrio ------------------------------------------------------------
cor_s <- function(x, y) cor(x, y, method = "spearman", use = "complete.obs")
tema_sc <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12), plot.subtitle = element_text(color = "grey35", size = 9),
        panel.grid.minor = element_blank(), plot.background = element_rect(fill = "#fcfcfb", color = NA), plot.margin = margin(6, 12, 4, 8))
sc <- function(x, xlab, titulo, color, log = FALSE) {
  r <- cor_s(pb[[x]], pb$precio_real)
  p <- ggplot(pb, aes(.data[[x]], precio_real)) +
    geom_point(size = 2.6, color = color, alpha = .9) +
    geom_text(aes(label = barrio), size = 2.5, color = "grey30", check_overlap = TRUE, nudge_y = 18000, hjust = .5) +
    scale_y_continuous(labels = function(v) paste0("$ ", round(v / 1e3), " mil")) +
    labs(title = titulo, subtitle = sprintf("%d barrios con precio en %s · correlación de Spearman = %s", nrow(pb), lbl_mes(ult), format(round(r, 2), nsmall = 2, decimal.mark = ",")),
         x = xlab, y = "Precio de alquiler (pesos constantes)") + coord_cartesian(clip = "off") + tema_sc
  if (log) p + scale_x_log10(labels = function(v) format(v, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE), expand = expansion(mult = .12)) else p
}
s1 <- sc("dist_subte_m", "Distancia al subte más cercano (m, escala log.)", "Precio vs distancia al subte", "#31688e", log = TRUE)
s2 <- sc("verde_m2_por_hab", "m² de espacio verde por habitante (escala log.)", "Precio vs espacio verde", "#31688e", log = TRUE)

# ---- armado ------------------------------------------------------------------------------------
png("Mapas/mapa_comunas_servicios.png", width = 15, height = 9.6, units = "in", res = 140, bg = "#fcfcfb")
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 3, heights = unit(c(.55, 4.6, .1, 3.9), "null"))))
vp <- function(r, c, ...) viewport(layout.pos.row = r, layout.pos.col = c, ...)
grid.text("Precio de alquiler y servicios por comuna y barrio", x = .02, y = .62, just = "left", gp = gpar(fontface = "bold", fontsize = 19), vp = vp(1, 1:3))
grid.text("Zonaprop (2 amb, 50 m²), BA Data (subte, espacios verdes, polígonos), IDECBA (IPCBA) y censo (población). Los valores de comuna ponderan los barrios por m² ofertados.",
          x = .02, y = .18, just = "left", gp = gpar(col = "grey35", fontsize = 10.5), vp = vp(1, 1:3))
print(m1, vp = vp(2, 1)); print(m2, vp = vp(2, 2)); print(m3, vp = vp(2, 3))
print(s1, vp = vp(4, 1:2 |> head(1))); print(s2, vp = vp(4, 2))
grid.text("Lectura: las correlaciones son descriptivas, con 47 barrios y un solo mes;\nno implican que el subte o el verde causen el precio (el ingreso\nde cada zona influye en ambos).\n\nPuerto Madero tiene ~410 m² de verde por habitante (Reserva\nEcológica y muy poca población) y eleva el promedio de la comuna 1.\nLa población es la del censo, por barrio.",
          x = .06, y = .5, just = "left", gp = gpar(col = "grey30", fontsize = 10.5), vp = vp(4, 3))
dev.off()

cat("Mapas/mapa_comunas_servicios.png\n")
cat(sprintf("Spearman precio vs distancia al subte: %.2f | vs verde por hab: %.2f | vs estaciones/km2: %.2f (n = %d barrios, %s)\n",
            cor_s(pb$dist_subte_m, pb$precio_real), cor_s(pb$verde_m2_por_hab, pb$precio_real), cor_s(pb$estaciones_por_km2, pb$precio_real),
            nrow(pb), format(ult)))
