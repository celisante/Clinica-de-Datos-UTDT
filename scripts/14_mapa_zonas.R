# 14_mapa_zonas.R
# Mapa simple de CABA con las zonas Norte, Centro y Sur delimitadas (la agrupacion que usa el dashboard para comparar el alquiler con el ITF).
# La zona de cada barrio sale de "Bases limpias/barrio_zona.csv". Los colores son los de las lineas del grafico del alquiler como % del ITF.
# Salida:   Mapas/mapa_zonas.png
# Requiere: Bases crudas/geo/barrios.geojson (BA Data). Ejecutar desde la raiz del proyecto.

library(sf)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

dir.create("Mapas", showWarnings = FALSE)

sin_acento <- function(s) str_to_lower(iconv(s, to = "ASCII//TRANSLIT")) |> str_remove_all("[^a-z ]") |> str_squish()
alias <- c("la boca" = "boca", "la paternal" = "paternal", "montserrat" = "monserrat")   # nombres distintos entre el geojson y barrio_zona.csv
clave <- function(s) { k <- sin_acento(s); if_else(k %in% names(alias), unname(alias[k]), k) }

bz <- read_csv("Bases limpias/barrio_zona.csv", show_col_types = FALSE) |>
  transmute(key = clave(BARRIO), comuna = COMUNA, zona = str_to_title(ZONA))
g <- st_read("Bases crudas/geo/barrios.geojson", quiet = TRUE) |> st_make_valid() |> mutate(key = clave(nombre)) |>
  left_join(bz |> select(key, zona), by = "key")
sin_zona <- g$nombre[is.na(g$zona)]
if (length(sin_zona)) stop("Barrios del geojson sin zona en barrio_zona.csv: ", paste(sin_zona, collapse = ", "))
g$zona <- factor(g$zona, levels = c("Norte", "Centro", "Sur"))

zonas <- g |> group_by(zona) |> summarise(geometry = st_union(geometry), .groups = "drop")
comunas <- bz |> mutate(zona = factor(zona, levels = c("Norte", "Centro", "Sur"))) |> group_by(zona) |> summarise(c = paste(sort(unique(comuna)), collapse = ", "), .groups = "drop")
txt <- paste(sprintf("%s: comunas %s", comunas$zona, sub(", (\\d+)$", " y \\1", comunas$c)), collapse = "   ·   ")
colores <- c(Norte = "#0072b2", Centro = "#e69f00", Sur = "#009e73")   # Okabe-Ito, igual que el grafico

etiquetas <- zonas |> st_transform(32721) |> mutate(geometry = st_centroid(geometry), lbl = toupper(as.character(zona))) |> st_transform(4326)   # centroide: queda mas adentro de cada zona que el punto interior
# Sur es una zona concava: su centroide cae sobre el limite con Centro, asi que la etiqueta va en un punto fijo dentro de la zona (Villa Lugano / Soldati)
pt_sur <- st_sfc(st_point(c(-58.475, -34.683)), crs = 4326)
stopifnot(st_within(pt_sur, zonas$geometry[zonas$zona == "Sur"], sparse = FALSE)[1, 1])
etiquetas$geometry[etiquetas$zona == "Sur"] <- pt_sur
p <- ggplot() +
  geom_sf(data = g, aes(fill = zona), color = "#fcfcfb", linewidth = .25, alpha = .55) +
  geom_sf(data = zonas, fill = NA, color = "#0b0b0b", linewidth = .9) +
  geom_sf_label(data = etiquetas, aes(label = lbl), fontface = "bold", size = 5, fill = alpha("#fcfcfb", .9), label.size = 0) +
  scale_fill_manual(values = colores, guide = "none") +
  labs(title = "Zonas Norte, Centro y Sur de CABA", subtitle = txt,
       caption = "Los barrios se agrupan según Bases limpias/barrio_zona.csv. Fuente: BA Data (polígonos de barrios). Elaboración propia.") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 17), plot.subtitle = element_text(color = "grey30", size = 10.5, margin = margin(b = 8)),
        plot.caption = element_text(color = "grey40", size = 8.5, margin = margin(t = 6)),
        plot.margin = margin(14, 14, 10, 14), plot.background = element_rect(fill = "#fcfcfb", color = NA))
ggsave("Mapas/mapa_zonas.png", p, width = 8.5, height = 8.2, dpi = 150)
cat(sprintf("Mapas/mapa_zonas.png | barrios por zona: %s\n", paste(names(table(g$zona)), table(g$zona), collapse = ", ")))
cat(txt, "\n")
