# 13_mapa_variacion.R
# Mapa de la variacion porcentual real del precio de alquiler por barrio entre 2016 y 2026
# (Zonaprop, 2 amb de 50 m2, pesos constantes del 3T 2026).
# Variacion = promedio de los meses de 2026 (ene-sep) / promedio de los meses de 2016 - 1. Se usan promedios y no dos meses
# sueltos para que no pese el ruido de un mes; hace falta un minimo de MIN_MESES meses con dato confiable en cada ventana.
# Salida:   Mapas/mapa_variacion_precio.html  (interactivo)
# Requiere: Bases limpias/zonaprop_barrio.csv (scripts/05) y Bases crudas/geo/barrios.geojson (BA Data).
# Ejecutar desde la raiz del proyecto.

library(sf)
library(dplyr)
library(readr)
library(stringr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(jsonlite)

dir.create("Mapas", showWarnings = FALSE)
ANIO_INI <- 2016; ANIO_FIN <- 2026; MIN_MESES <- 6

sin_acento <- function(s) str_to_lower(iconv(s, to = "ASCII//TRANSLIT")) |> str_remove_all("[^a-z ]") |> str_squish()
alias <- c("boca" = "la boca", "la paternal" = "paternal", "montserrat" = "monserrat")
clave <- function(s) { k <- sin_acento(s); if_else(k %in% names(alias), unname(alias[k]), k) }

g <- st_read("Bases crudas/geo/barrios.geojson", quiet = TRUE) |> st_make_valid() |> mutate(key = clave(nombre))

ipc <- read_csv("Bases limpias/ipcba.csv", show_col_types = FALSE) |> select(fecha, ipc = ipcba_general)
ref_ipc <- mean(tail(ipc$ipc, 3))            # promedio del 3T 2026 = pesos constantes del 3T 2026
precios <- read_csv("Bases limpias/zonaprop_barrio.csv", show_col_types = FALSE) |>
  left_join(ipc, by = "fecha") |>
  mutate(key = clave(barrio), real = precio_pesos * ref_ipc / ipc, anio = as.integer(format(fecha, "%Y"))) |>
  filter(!is.na(real), anio %in% c(ANIO_INI, ANIO_FIN))
stopifnot(all(precios$key %in% g$key))
ult_mes <- max(precios$fecha)

ventana <- function(a) precios |> filter(anio == a) |> group_by(key) |>
  summarise(n = n(), p = mean(real), .groups = "drop") |> filter(n >= MIN_MESES) |> select(key, p)
v <- inner_join(ventana(ANIO_INI), ventana(ANIO_FIN), by = "key", suffix = c("_ini", "_fin")) |>
  mutate(var = (p_fin / p_ini - 1) * 100)
cat(sprintf("Barrios comparables: %d de %d | variacion: min %.1f %%, mediana %.1f %%, max %.1f %%\n", nrow(v), nrow(g), min(v$var), median(v$var), max(v$var)))
cat("Sin dato:", paste(g$nombre[!g$key %in% v$key], collapse = ", "), "\n")

# ---- escala: viridis invertida (violeta oscuro = mayor variacion, amarillo = mayor caida), 7 tramos con cortes fijos; el 0 es un corte ----
cols <- substr(viridisLite::viridis(7, begin = .02, end = .9, direction = -1), 1, 7)
cortes <- c(floor(min(v$var)), -15, -10, -5, 0, 5, 10, ceiling(max(v$var)))
stopifnot(!is.unsorted(cortes, strictly = TRUE))
print(table(cut(v$var, cortes, include.lowest = TRUE)))

g_s <- g |> st_transform(32721) |> st_simplify(dTolerance = 15, preserveTopology = TRUE) |> st_transform(4326)
tmp_geo <- tempfile(fileext = ".geojson")
st_write(g_s |> select(key, nombre, comuna), tmp_geo, quiet = TRUE, delete_dsn = TRUE)
geo <- fromJSON(paste(readLines(tmp_geo, warn = FALSE), collapse = ""), simplifyVector = FALSE)
datos <- v |> mutate(across(c(p_ini, p_fin), round), var = round(var, 1))
datos_js <- list(geo = geo, d = setNames(lapply(seq_len(nrow(datos)), function(i) as.list(datos[i, c("p_ini", "p_fin", "var")])), datos$key),
                 bins = as.list(cortes), cols = as.list(cols), ini = ANIO_INI, fin = ANIO_FIN,
                 fin_txt = sprintf("ene-%s %d", c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")[as.integer(format(ult_mes, "%m"))], ANIO_FIN))

js <- "
function(el, x, d) {
  var map = this;
  map.setView([-34.61, -58.44], 11, {animate: false});   // vista inicial: sin ella no existe el panel SVG donde va el rayado
  function color(v) { if (v == null) return 'url(#sinDato)'; var i = d.bins.length - 2; while (i > 0 && v < d.bins[i]) i--; return d.cols[Math.min(i, d.cols.length - 1)]; }
  function fmtP(v) { return '$ ' + Math.round(v).toLocaleString('es-AR'); }
  function fmtV(v) { return (v > 0 ? '+' : v < 0 ? '\\u2212' : '') + Math.abs(v).toFixed(1).replace('.', ',') + ' %'; }
  function val(f) { var r = d.d[f.properties.key]; return r ? r['var'] : null; }
  var layer = L.geoJSON(d.geo, {
    style: function(f) { return {fillColor: color(val(f)), fillOpacity: 1, color: '#fcfcfb', weight: 1.2}; },
    onEachFeature: function(f, l) {
      l.bindTooltip(function() {
        var r = d.d[f.properties.key], box = document.createElement('div'), t = document.createElement('b'); t.textContent = f.properties.nombre; box.appendChild(t);
        function line(s) { var e = document.createElement('div'); e.textContent = s; box.appendChild(e); }
        line('Comuna ' + f.properties.comuna);
        if (r) { line('Promedio ' + d.ini + ': ' + fmtP(r.p_ini)); line('Promedio ' + d.fin_txt + ': ' + fmtP(r.p_fin)); line('Variaci\\u00f3n real: ' + fmtV(r['var'])); }
        else line('Sin dato confiable para comparar');
        return box; }, {sticky: true});
      l.on('mouseover', function() { l.setStyle({weight: 2.5, color: '#0b0b0b'}); l.bringToFront(); });
      l.on('mouseout', function() { layer.resetStyle(l); });
    }
  }).addTo(map);
  (function() {   // rayado para los barrios sin dato (no depende del color)
    var svg = map.getPanes().overlayPane.querySelector('svg'), ns = 'http://www.w3.org/2000/svg';
    var defs = document.createElementNS(ns, 'defs'), p = document.createElementNS(ns, 'pattern');
    p.setAttribute('id', 'sinDato'); p.setAttribute('patternUnits', 'userSpaceOnUse'); p.setAttribute('width', 6); p.setAttribute('height', 6); p.setAttribute('patternTransform', 'rotate(45)');
    var r = document.createElementNS(ns, 'rect'); r.setAttribute('width', 6); r.setAttribute('height', 6); r.setAttribute('fill', '#ffffff');
    var l = document.createElementNS(ns, 'line'); l.setAttribute('x1', 0); l.setAttribute('y1', 0); l.setAttribute('x2', 0); l.setAttribute('y2', 6); l.setAttribute('stroke', '#555555'); l.setAttribute('stroke-width', 2);
    p.appendChild(r); p.appendChild(l); defs.appendChild(p); svg.insertBefore(defs, svg.firstChild);
  })();
  var caja = layer.getBounds();
  function ajustar() {
    map.invalidateSize();
    if (!isFinite(map.getZoom())) map.setView(caja.getCenter(), 11, {animate: false});
    var s = map.getSize(); if (!s.x || !s.y) return;
    map.fitBounds(caja, {padding: [20, 20], animate: false});
  }
  ajustar(); setTimeout(ajustar, 100); setTimeout(ajustar, 600);
  var rt = null; new ResizeObserver(function() { clearTimeout(rt); rt = setTimeout(ajustar, 100); }).observe(map.getContainer());

  var leg = L.control({position: 'bottomright'});
  leg.onAdd = function() {
    var div = L.DomUtil.create('div', 'leaflet-bar'); div.style.cssText = 'background:#fcfcfb;padding:8px 12px;font:12px system-ui,sans-serif;color:#0b0b0b';
    var h = document.createElement('div'); h.style.cssText = 'font-weight:600;margin-bottom:4px'; h.textContent = 'Variaci\\u00f3n real del precio'; div.appendChild(h);
    function k(v) { return (v > 0 ? '+' : v < 0 ? '\\u2212' : '') + Math.abs(v) + ' %'; }
    for (var i = d.cols.length - 1; i >= 0; i--) { var r = document.createElement('div'); r.style.cssText = 'display:flex;align-items:center;gap:6px;margin:1px 0';
      var s = document.createElement('span'); s.style.cssText = 'display:inline-block;width:18px;height:12px;background:' + d.cols[i]; r.appendChild(s);
      r.appendChild(document.createTextNode(k(d.bins[i]) + ' a ' + k(d.bins[i + 1]))); div.appendChild(r); }
    var n = document.createElement('div'); n.style.cssText = 'display:flex;align-items:center;gap:6px;margin-top:3px';
    var s2 = document.createElement('span'); s2.style.cssText = 'display:inline-block;width:18px;height:12px;border:1px solid #999;background:repeating-linear-gradient(45deg,#fff 0,#fff 3px,#555 3px,#555 5px)'; n.appendChild(s2); n.appendChild(document.createTextNode('Sin dato confiable')); div.appendChild(n);
    return div; };
  leg.addTo(map);
}"

# sin mapa base de teselas (funciona sin internet)
m <- leaflet(options = leafletOptions(zoomSnap = .25, attributionControl = FALSE)) |>
  onRender(js, data = datos_js) |>
  prependContent(tags$style("html,body{margin:0;height:100%} .leaflet-container{background:#f4f4f1}"),
                 tags$div(style = "position:absolute;z-index:1000;top:10px;left:56px;background:#fcfcfb;padding:8px 14px;border-radius:8px;box-shadow:0 1px 6px rgba(0,0,0,.2);font-family:system-ui,sans-serif",
                          tags$div(style = "font-weight:700;font-size:15px", sprintf("Variación del precio de alquiler por barrio, %d a %d", ANIO_INI, ANIO_FIN)),
                          tags$div(style = "font-size:12px;color:#52514e", "Zonaprop, 2 amb de 50 m² · pesos constantes del 3T 2026 · promedio de cada año")))
m$height <- "100vh"; m$width <- "100%"
saveWidget(m, file.path("Mapas", "mapa_variacion_precio.html"), selfcontained = TRUE, title = "Variacion del alquiler por barrio")
unlink(file.path("Mapas", "mapa_variacion_precio_files"), recursive = TRUE)
cat(sprintf("Mapas/mapa_variacion_precio.html (%.0f KB)\nTramos: %s\n", file.size("Mapas/mapa_variacion_precio.html") / 1024, paste(cortes, collapse = " | ")))
