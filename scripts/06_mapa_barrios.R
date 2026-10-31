# 06_mapa_barrios.R
# Mapa de calor del precio de alquiler por barrio (Zonaprop, 2 amb de 50 m2), en pesos constantes.
# Salidas:  Mapas/mapa_precios_barrio.html  (interactivo, con deslizador de meses)
#           Mapas/mapa_precios_barrio.png   (estatico, 5 fechas con la misma escala)
# Requiere: Bases limpias/zonaprop_barrio.csv (scripts/05) y Bases crudas/geo/barrios.geojson (BA Data).
# Ejecutar desde la raiz del proyecto.

library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(jsonlite)

dir.create("Mapas", showWarnings = FALSE)

# ---- datos ---------------------------------------------------------------
sin_acento <- function(s) str_to_lower(iconv(s, to = "ASCII//TRANSLIT")) |> str_remove_all("[^a-z ]") |> str_squish()
alias <- c("boca" = "la boca", "la paternal" = "paternal", "montserrat" = "monserrat")
clave <- function(s) { k <- sin_acento(s); if_else(k %in% names(alias), unname(alias[k]), k) }

g <- st_read("Bases crudas/geo/barrios.geojson", quiet = TRUE) |>
  st_make_valid() |>
  mutate(key = clave(nombre))

ipc <- read_csv("Bases limpias/ipcba.csv", show_col_types = FALSE) |> select(fecha, ipc = ipcba_general)
ref_ipc <- mean(tail(ipc$ipc, 3))            # promedio del 3T 2026 = pesos constantes del 3T 2026

precios <- read_csv("Bases limpias/zonaprop_barrio.csv", show_col_types = FALSE) |>
  left_join(ipc, by = "fecha") |>
  mutate(key = clave(barrio), real = precio_pesos * ref_ipc / ipc) |>
  filter(!is.na(real))
stopifnot(setequal(precios$key, g$key))

meses <- sort(unique(precios$fecha))
lbl_mes <- function(d) paste(c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")[as.integer(format(d, "%m"))], format(d, "%Y"))

# ---- escala de color: 7 tramos por cuantiles de todo el periodo -----------
# viridis invertida: escala apta para daltonismo (varia en luminosidad: violeta oscuro = valor alto, amarillo = valor bajo);
# "sin dato" se marca con rayado, no con un gris. Los valores van de bajo a alto, asi que el primer color es el amarillo.
azules <- substr(viridisLite::viridis(7, begin = .02, end = .9, direction = -1), 1, 7)
q <- quantile(precios$real, seq(0, 1, length.out = 8))
cortes <- round(q / 1e4) * 1e4
cortes[1] <- floor(min(precios$real) / 1e4) * 1e4
cortes[8] <- ceiling(max(precios$real) / 1e4) * 1e4
stopifnot(!anyDuplicated(cortes))
fmt_p <- function(v) ifelse(v >= 1e6, paste0("$ ", format(round(v / 1e6, 2), decimal.mark = ","), " M"), paste0("$ ", round(v / 1e3), " mil"))
etiquetas <- paste(fmt_p(cortes[-8]), "a", fmt_p(cortes[-1]))

# ---- 1) mapa estatico ------------------------------------------------------
fechas_png <- c(as.Date(c("2016-12-01", "2019-12-01", "2022-12-01", "2025-12-01")), max(meses))
est <- precios |> filter(fecha %in% fechas_png) |> select(key, fecha, real)
mapa_est <- tidyr::crossing(g |> select(key, nombre), fecha = fechas_png) |>
  st_as_sf() |>
  left_join(est, by = c("key", "fecha")) |>
  mutate(tramo = as.character(cut(real, breaks = cortes, labels = etiquetas, include.lowest = TRUE)),
         tramo = factor(coalesce(tramo, "Sin dato"), levels = c(etiquetas, "Sin dato")),
         panel = factor(lbl_mes(fecha), levels = lbl_mes(fechas_png)))
# rayado diagonal sobre los barrios sin dato (lineas paralelas a 45 grados recortadas con cada poligono)
rayado <- function(poligonos, paso = 700) {
  u <- st_transform(poligonos, 32721); bb <- st_bbox(u); h <- bb["ymax"] - bb["ymin"]; w <- bb["xmax"] - bb["xmin"]
  ks <- seq(-h, w, by = paso)
  lineas <- st_sfc(lapply(ks, function(k) st_linestring(rbind(c(bb["xmin"] + k, bb["ymin"]), c(bb["xmin"] + k + h, bb["ymax"])))), crs = 32721) |> st_union()
  st_intersection(st_union(st_geometry(u)), lineas) |> st_transform(4326)
}
sin_dato <- mapa_est |> filter(tramo == "Sin dato")
hatch <- bind_rows(lapply(split(sin_dato, sin_dato$panel), function(p) st_sf(panel = p$panel[1], geometry = rayado(p)))) |>
  mutate(panel = factor(panel, levels = levels(mapa_est$panel)))
p <- ggplot(mapa_est) +
  geom_sf(aes(fill = tramo), color = "#fcfcfb", linewidth = .25) +
  geom_sf(data = sin_dato, fill = NA, color = "#8a8a8a", linewidth = .3) +
  geom_sf(data = hatch, color = "#555555", linewidth = .3) +
  scale_fill_manual(values = setNames(c(azules, "#ffffff"), c(etiquetas, "Sin dato")), name = "Precio (pesos constantes del 3T 2026)", drop = FALSE,
                    guide = guide_legend(nrow = 1, title.position = "top", override.aes = list(color = c(rep(NA, 7), "#555555")))) +
  facet_wrap(~panel, nrow = 2) +
  labs(title = "Precio de alquiler de un 2 ambientes por barrio",
       subtitle = "Zonaprop, 2 amb de 50 m². Rayado = sin dato confiable ese mes (ver Bases limpias/README.md).",
       caption = "Fuente: Zonaprop; IDECBA (IPCBA); BA Data (polígonos de barrios). Elaboración propia.") +
  theme_void(base_size = 11) +
  theme(legend.position = "bottom", legend.key.size = unit(.4, "cm"), legend.text = element_text(size = 7.5),
        plot.title = element_text(face = "bold", size = 15), plot.subtitle = element_text(color = "grey30", margin = margin(b = 10)),
        plot.caption = element_text(color = "grey40", size = 8), strip.text = element_text(face = "bold", size = 11, margin = margin(b = 4)),
        plot.margin = margin(12, 12, 8, 12), plot.background = element_rect(fill = "#fcfcfb", color = NA))
ggsave("Mapas/mapa_precios_barrio.png", p, width = 12, height = 8.2, dpi = 150)

# ---- 2) mapa interactivo ---------------------------------------------------
g_s <- g |> st_transform(32721) |> st_simplify(dTolerance = 15, preserveTopology = TRUE) |> st_transform(4326)
vals <- lapply(g$key, function(k) {
  v <- precios |> filter(key == k) |> select(fecha, real)
  unname(round(v$real[match(meses, v$fecha)]))
})
names(vals) <- g$key
tmp_geo <- tempfile(fileext = ".geojson")
st_write(g_s |> select(key, nombre, comuna), tmp_geo, quiet = TRUE, delete_dsn = TRUE)
geo <- fromJSON(paste(readLines(tmp_geo, warn = FALSE), collapse = ""), simplifyVector = FALSE)

datos_js <- list(geo = geo, months = lbl_mes(meses), vals = vals, bins = as.list(unname(cortes)), cols = as.list(azules),
                 ref = lbl_mes(max(meses)))

js <- "
function(el, x, d) {
  var map = this;
  function color(v) { if (v == null) return 'url(#sinDato)'; var i = d.bins.length - 2; while (i > 0 && v < d.bins[i]) i--; return d.cols[Math.min(i, d.cols.length - 1)]; }
  var idx = d.months.length - 1;
  function fmtP(v) { return v == null ? 'sin dato' : '$ ' + Math.round(v).toLocaleString('es-AR'); }
  var layer = L.geoJSON(d.geo, {
    style: function(f) { return {fillColor: color(d.vals[f.properties.key][idx]), fillOpacity: 1, color: '#fcfcfb', weight: 1.2}; },
    onEachFeature: function(f, l) {
      l.bindTooltip(function() {
        var v = d.vals[f.properties.key], now = v[idx], prev = idx >= 12 ? v[idx - 12] : null;
        var box = document.createElement('div'), t = document.createElement('b'); t.textContent = f.properties.nombre; box.appendChild(t);
        var c = document.createElement('div'); c.textContent = 'Comuna ' + f.properties.comuna; box.appendChild(c);
        var p = document.createElement('div'); p.textContent = d.months[idx] + ': ' + fmtP(now); box.appendChild(p);
        if (now != null && prev != null) { var y = document.createElement('div'); var ch = (now / prev - 1) * 100;
          y.textContent = 'Var. interanual real: ' + (ch > 0 ? '+' : '') + ch.toFixed(1).replace('.', ',') + ' %'; box.appendChild(y); }
        return box; }, {sticky: true});
      l.on('mouseover', function() { l.setStyle({weight: 2.5, color: '#0b0b0b'}); l.bringToFront(); });
      l.on('mouseout', function() { layer.resetStyle(l); });
    }
  }).addTo(map);
  // rayado para los barrios sin dato (no depende de un color, apto para daltonismo)
  (function() {
    var svg = map.getPanes().overlayPane.querySelector('svg'), ns = 'http://www.w3.org/2000/svg';
    var defs = document.createElementNS(ns, 'defs'), p = document.createElementNS(ns, 'pattern');
    p.setAttribute('id', 'sinDato'); p.setAttribute('patternUnits', 'userSpaceOnUse'); p.setAttribute('width', 6); p.setAttribute('height', 6); p.setAttribute('patternTransform', 'rotate(45)');
    var r = document.createElementNS(ns, 'rect'); r.setAttribute('width', 6); r.setAttribute('height', 6); r.setAttribute('fill', '#ffffff');
    var l = document.createElementNS(ns, 'line'); l.setAttribute('x1', 0); l.setAttribute('y1', 0); l.setAttribute('x2', 0); l.setAttribute('y2', 6); l.setAttribute('stroke', '#555555'); l.setAttribute('stroke-width', 2);
    p.appendChild(r); p.appendChild(l); defs.appendChild(p); svg.insertBefore(defs, svg.firstChild);
  })();
  var cajaCiudad = layer.getBounds();
  function ajustar() {
    map.invalidateSize();
    if (!isFinite(map.getZoom())) map.setView(cajaCiudad.getCenter(), 11, {animate: false});   // sin vista o zoom NaN: se reinicia
    var s = map.getSize(); if (!s.x || !s.y) return;                                           // contenedor aun sin tamano: espera al ResizeObserver
    map.fitBounds(cajaCiudad, {padding: [20, 20], animate: false});
  }
  ajustar(); setTimeout(ajustar, 100); setTimeout(ajustar, 600);
  var rt = null; new ResizeObserver(function() { clearTimeout(rt); rt = setTimeout(ajustar, 100); }).observe(map.getContainer());
  function repaint() { layer.setStyle(function(f) { return {fillColor: color(d.vals[f.properties.key][idx]), fillOpacity: 1, color: '#fcfcfb', weight: 1.2}; }); }

  var ctl = L.control({position: 'bottomleft'});
  ctl.onAdd = function() {
    var div = L.DomUtil.create('div', 'leaflet-bar'); div.style.cssText = 'background:#fcfcfb;padding:8px 12px;font:13px system-ui,sans-serif;min-width:260px;color:#0b0b0b';
    var lab = document.createElement('div'); lab.style.cssText = 'font-weight:600;margin-bottom:4px'; div.appendChild(lab);
    var row = document.createElement('div'); row.style.cssText = 'display:flex;gap:8px;align-items:center'; div.appendChild(row);
    var btn = document.createElement('button'); btn.textContent = '▶'; btn.setAttribute('aria-label', 'Reproducir'); btn.style.cssText = 'cursor:pointer;border:1px solid #c3c2b7;background:#fcfcfb;border-radius:6px;width:30px;height:26px'; row.appendChild(btn);
    var sl = document.createElement('input'); sl.type = 'range'; sl.min = 0; sl.max = d.months.length - 1; sl.value = idx; sl.style.flex = '1'; sl.setAttribute('aria-label', 'Mes'); row.appendChild(sl);
    function show() { lab.textContent = d.months[idx]; sl.value = idx; repaint(); }
    sl.addEventListener('input', function() { idx = +sl.value; show(); });
    var timer = null;
    btn.addEventListener('click', function() {
      if (timer) { clearInterval(timer); timer = null; btn.textContent = '▶'; return; }
      if (idx >= d.months.length - 1) idx = 0;
      btn.textContent = '❚❚';
      timer = setInterval(function() { if (idx >= d.months.length - 1) { clearInterval(timer); timer = null; btn.textContent = '▶'; return; } idx++; show(); }, 120);
    });
    L.DomEvent.disableClickPropagation(div); show(); return div; };
  ctl.addTo(map);

  var leg = L.control({position: 'bottomright'});
  leg.onAdd = function() {
    var div = L.DomUtil.create('div', 'leaflet-bar'); div.style.cssText = 'background:#fcfcfb;padding:8px 12px;font:12px system-ui,sans-serif;color:#0b0b0b';
    var h = document.createElement('div'); h.style.cssText = 'font-weight:600;margin-bottom:4px'; h.textContent = 'Pesos constantes del 3T 2026'; div.appendChild(h);
    function k(v) { return v >= 1e6 ? '$ ' + (v / 1e6).toFixed(2).replace('.', ',') + ' M' : '$ ' + Math.round(v / 1e3) + ' mil'; }
    for (var i = d.cols.length - 1; i >= 0; i--) { var r = document.createElement('div'); r.style.cssText = 'display:flex;align-items:center;gap:6px;margin:1px 0';
      var s = document.createElement('span'); s.style.cssText = 'display:inline-block;width:18px;height:12px;background:' + d.cols[i]; r.appendChild(s);
      r.appendChild(document.createTextNode(k(d.bins[i]) + ' a ' + k(d.bins[i + 1]))); div.appendChild(r); }
    var n = document.createElement('div'); n.style.cssText = 'display:flex;align-items:center;gap:6px;margin-top:3px';
    var s2 = document.createElement('span'); s2.style.cssText = 'display:inline-block;width:18px;height:12px;border:1px solid #999;background:repeating-linear-gradient(45deg,#fff 0,#fff 3px,#555 3px,#555 5px)'; n.appendChild(s2); n.appendChild(document.createTextNode('Sin dato confiable')); div.appendChild(n);
    return div; };
  leg.addTo(map);
}"

# sin mapa base de teselas: los proveedores gratuitos cambian condiciones (CARTO ya pide API key) y asi funciona sin internet
m <- leaflet(options = leafletOptions(zoomSnap = .25, attributionControl = FALSE)) |>
  onRender(js, data = datos_js) |>
  prependContent(tags$style("html,body{margin:0;height:100%} .leaflet-container{background:#f4f4f1}"),
                 tags$div(style = "position:absolute;z-index:1000;top:10px;left:56px;background:#fcfcfb;padding:8px 14px;border-radius:8px;box-shadow:0 1px 6px rgba(0,0,0,.2);font-family:system-ui,sans-serif",
                          tags$div(style = "font-weight:700;font-size:15px", "Alquiler de un 2 ambientes por barrio"),
                          tags$div(style = "font-size:12px;color:#52514e", "Zonaprop, 50 m² · pesos constantes del 3T 2026 · mové el deslizador para ver otros meses")))
m$height <- "100vh"; m$width <- "100%"
saveWidget(m, file.path("Mapas", "mapa_precios_barrio.html"), selfcontained = TRUE, title = "Alquiler por barrio")
unlink(file.path("Mapas", "mapa_precios_barrio_files"), recursive = TRUE)   # carpeta temporal de saveWidget

cat(sprintf("Mapas/mapa_precios_barrio.html (%.0f KB) y .png\n", file.size("Mapas/mapa_precios_barrio.html") / 1024))
cat("Tramos:", paste(etiquetas, collapse = " | "), "\n")
cat(sprintf("Barrios sin dato en %s: %d\n", lbl_mes(max(meses)), n_distinct(g$key) - n_distinct(precios$key[precios$fecha == max(meses)])))
