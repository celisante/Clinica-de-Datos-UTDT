# 09_mapa_interactivo.R
# Mapa interactivo (un solo HTML) del precio de alquiler y los servicios, por comuna o por barrio.
#   - Variable: precio (con deslizador de meses), subte (estaciones por km2), verde (m2 por hab), distancia al subte.
#   - Capas opcionales: estaciones de subte y espacios verdes de 1 ha o mas.
# Salida: Mapas/mapa_interactivo.html. Requiere haber corrido scripts/05 y 07.
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
sin_acento <- function(s) str_to_lower(iconv(s, to = "ASCII//TRANSLIT")) |> str_remove_all("[^a-z ]") |> str_squish()
alias <- c("boca" = "la boca", "la paternal" = "paternal", "montserrat" = "monserrat")
clave <- function(s) { k <- sin_acento(s); if_else(k %in% names(alias), unname(alias[k]), k) }
lbl_mes <- function(d) paste(c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")[as.integer(format(d, "%m"))], format(d, "%Y"))

a_geojson <- function(x) {   # sf (WGS84) -> lista R lista para serializar, con 5 decimales
  tmp <- tempfile(fileext = ".geojson")
  st_write(x, tmp, quiet = TRUE, delete_dsn = TRUE, layer_options = "COORDINATE_PRECISION=5")
  fromJSON(paste(readLines(tmp, warn = FALSE), collapse = ""), simplifyVector = FALSE)
}
simplificar <- function(x, tol) x |> st_transform(32721) |> st_simplify(dTolerance = tol, preserveTopology = TRUE) |> st_transform(4326)

# ---- datos -----------------------------------------------------------------
ipc <- read_csv("Bases limpias/ipcba.csv", show_col_types = FALSE) |> select(fecha, ipc = ipcba_general)
ref_ipc <- mean(tail(ipc$ipc, 3))

zb <- read_csv("Bases limpias/zonaprop_barrio.csv", show_col_types = FALSE) |>
  left_join(ipc, by = "fecha") |> mutate(key = clave(barrio), real = precio_pesos * ref_ipc / ipc) |> filter(!is.na(real))
pc <- read_csv("Bases limpias/precio_comuna.csv", show_col_types = FALSE) |> filter(!is.na(precio_real))
meses <- sort(unique(c(zb$fecha, pc$fecha)))

sv_b <- read_csv("Bases limpias/servicios_barrio.csv", show_col_types = FALSE) |> mutate(key = clave(barrio))
sv_c <- read_csv("Bases limpias/servicios_comuna.csv", show_col_types = FALSE)

g <- st_read("Bases crudas/geo/barrios.geojson", quiet = TRUE) |> st_make_valid() |>
  transmute(key = clave(nombre), nombre, comuna = as.integer(comuna))
comunas <- g |> group_by(comuna) |> summarise(geometry = st_union(geometry), .groups = "drop")

serie <- function(df, id_col, id, val) { v <- df |> filter(.data[[id_col]] == id); unname(round(v[[val]][match(meses, v$fecha)])) }

# barrio
gb <- g |> simplificar(12) |> transmute(id = key, name = nombre, comuna)
lev_barrio <- list(
  geo = a_geojson(gb),
  price = setNames(lapply(g$key, \(k) serie(zb, "key", k, "real")), g$key),
  svc = setNames(lapply(g$key, \(k) { s <- sv_b |> filter(key == k)
    list(subte = round(s$estaciones_por_km2, 2), verde = round(s$verde_m2_por_hab, 2), dist = round(s$dist_subte_m), pob = s$poblacion) }), g$key))
# comuna
gc <- comunas |> simplificar(12) |> transmute(id = as.character(comuna), name = paste("Comuna", comuna), comuna)
lev_comuna <- list(
  geo = a_geojson(gc),
  price = setNames(lapply(comunas$comuna, \(k) serie(pc, "comuna", k, "precio_real")), as.character(comunas$comuna)),
  svc = setNames(lapply(comunas$comuna, \(k) { s <- sv_c |> filter(comuna == k)
    list(subte = round(s$estaciones_por_km2, 2), verde = round(s$verde_m2_por_hab, 2), dist = round(s$dist_subte_m), pob = s$poblacion) }), as.character(comunas$comuna)))

# estaciones y parques (>= 1 ha)
estaciones <- st_read("Bases crudas/geo/subte_estaciones.geojson", quiet = TRUE) |> select(estacion, linea)
parques <- st_read("Bases crudas/geo/espacios_verdes_publicos.geojson", quiet = TRUE) |> st_make_valid() |>
  st_transform(32721) |> mutate(area_m2 = as.numeric(st_area(geometry))) |> filter(area_m2 >= 1e4) |>
  st_simplify(dTolerance = 15, preserveTopology = TRUE) |> st_transform(4326) |>
  transmute(nombre = coalesce(nombre, nom_mapa, "Espacio verde"), tipo = clasificac, ha = round(area_m2 / 1e4, 1))

datos <- list(
  months = lbl_mes(meses), ref = lbl_mes(max(meses)),
  levels = list(comuna = lev_comuna, barrio = lev_barrio),
  stations = a_geojson(estaciones), parks = a_geojson(parques),
  # escala viridis invertida en todas las variables (apta para daltonismo): violeta oscuro = valor alto, amarillo = valor bajo
  ramps = list(azul = substr(viridisLite::viridis(7, begin = .02, end = .9, direction = -1), 1, 7),                       # precio: viridis
               naranja = substr(viridisLite::viridis(5, begin = .02, end = .9, direction = -1), 1, 7),    # subte: viridis
               verde = substr(viridisLite::viridis(5, begin = .02, end = .9, direction = -1), 1, 7),     # verde: viridis
               violeta = substr(viridisLite::viridis(5, begin = .02, end = .9, direction = -1), 1, 7)))   # distancia: viridis
cat(sprintf("Estaciones: %d | parques >= 1 ha: %d | meses: %d\n", nrow(estaciones), nrow(parques), length(meses)))

js <- "
function(el, x, d) {
  var map = this;
  var S = {level: 'comuna', metric: 'precio', idx: d.months.length - 1, stations: false, parks: false};
  var NOMBRES = {precio: 'Precio de alquiler', subte: 'Estaciones de subte por km²', verde: 'm² de espacio verde por habitante', dist: 'Distancia al subte más cercano'};
  var RAMP = {precio: d.ramps.azul, subte: d.ramps.naranja, verde: d.ramps.verde, dist: d.ramps.violeta};
  var NODATA = 'url(#sinDato)', INK = '#0b0b0b', SURF = '#fcfcfb';   // sin dato: rayado (no depende del color)
  function patron() {
    var svg = map.getPanes().overlayPane.querySelector('svg'); if (!svg || svg.querySelector('#sinDato')) return;
    var ns = 'http://www.w3.org/2000/svg', defs = document.createElementNS(ns, 'defs'), p = document.createElementNS(ns, 'pattern');
    p.setAttribute('id', 'sinDato'); p.setAttribute('patternUnits', 'userSpaceOnUse'); p.setAttribute('width', 6); p.setAttribute('height', 6); p.setAttribute('patternTransform', 'rotate(45)');
    var r = document.createElementNS(ns, 'rect'); r.setAttribute('width', 6); r.setAttribute('height', 6); r.setAttribute('fill', '#ffffff');
    var l = document.createElementNS(ns, 'line'); l.setAttribute('x1', 0); l.setAttribute('y1', 0); l.setAttribute('x2', 0); l.setAttribute('y2', 6); l.setAttribute('stroke', '#555555'); l.setAttribute('stroke-width', 2);
    p.appendChild(r); p.appendChild(l); defs.appendChild(p); svg.insertBefore(defs, svg.firstChild);
  }
  var NODATA_CSS = 'border:1px solid #999;background:repeating-linear-gradient(45deg,#fff 0,#fff 3px,#555 3px,#555 5px)';
  var nf0 = new Intl.NumberFormat('es-AR', {maximumFractionDigits: 0}), nf1 = new Intl.NumberFormat('es-AR', {maximumFractionDigits: 1}), nf2 = new Intl.NumberFormat('es-AR', {maximumFractionDigits: 2});

  function value(L, id) { return S.metric === 'precio' ? L.price[id][S.idx] : L.svc[id][S.metric]; }
  function quantile(sorted, p) { var i = (sorted.length - 1) * p, lo = Math.floor(i), hi = Math.ceil(i); return sorted[lo] + (sorted[hi] - sorted[lo]) * (i - lo); }
  var binCache = {};
  function bins() {
    var key = S.level + '|' + S.metric; if (binCache[key]) return binCache[key];
    var L = d.levels[S.level], vals = [], k = RAMP[S.metric].length;
    Object.keys(L.svc).forEach(function(id) {
      if (S.metric === 'precio') L.price[id].forEach(function(v) { if (v != null) vals.push(v); }); else vals.push(L.svc[id][S.metric]);
    });
    vals.sort(function(a, b) { return a - b; });
    var b = [vals[0]]; for (var i = 1; i < k; i++) { var q = quantile(vals, i / k); if (q > b[b.length - 1]) b.push(q); }
    b.push(vals[vals.length - 1] + 1e-9);
    binCache[key] = b; return b;
  }
  function color(v) {
    if (v == null || !isFinite(v)) return NODATA;
    var b = bins(), r = RAMP[S.metric], i = b.length - 2; while (i > 0 && v < b[i]) i--;
    return r[Math.min(i, r.length - 1)];
  }
  function fmtPesos(v) { return v == null ? 'sin dato' : '$ ' + nf0.format(v); }
  function fmtLeg(v) {
    if (S.metric === 'precio') return v >= 1e6 ? '$ ' + nf2.format(v / 1e6) + ' M' : '$ ' + nf0.format(v / 1e3) + ' mil';
    if (S.metric === 'dist') return nf0.format(v) + ' m';
    return nf1.format(v);
  }

  var layer = null;
  function style(f) { return {fillColor: color(value(d.levels[S.level], f.properties.id)), fillOpacity: 1, color: SURF, weight: S.level === 'barrio' ? 1 : 1.6}; }
  function tooltip(f) {
    var L = d.levels[S.level], id = f.properties.id, sv = L.svc[id], box = document.createElement('div');
    function line(txt, bold) { var e = document.createElement(bold ? 'b' : 'div'); e.textContent = txt; box.appendChild(e); if (bold) box.appendChild(document.createElement('br')); }
    line(f.properties.name, true);
    if (S.level === 'barrio') line('Comuna ' + f.properties.comuna);
    var p = L.price[id][S.idx], prev = S.idx >= 12 ? L.price[id][S.idx - 12] : null;
    line(d.months[S.idx] + ': ' + fmtPesos(p));
    if (p != null && prev != null) { var ch = (p / prev - 1) * 100; line('Var. interanual real: ' + (ch > 0 ? '+' : '') + nf1.format(ch) + ' %'); }
    line('Subte: ' + nf1.format(sv.subte) + ' estaciones/km²');
    line('Distancia al subte: ' + nf0.format(sv.dist) + ' m');
    line('Espacio verde: ' + nf1.format(sv.verde) + ' m²/hab');
    line('Población (censo): ' + nf0.format(sv.pob));
    return box;
  }
  function build() {
    if (layer) map.removeLayer(layer);
    layer = L.geoJSON(d.levels[S.level].geo, {
      style: style,
      onEachFeature: function(f, l) {
        l.bindTooltip(function() { return tooltip(f); }, {sticky: true});
        l.on('mouseover', function() { l.setStyle({weight: 3, color: INK}); l.bringToFront(); });
        l.on('mouseout', function() { layer.resetStyle(l); });
      }
    }).addTo(map);
    patron();
    layer.bringToBack();
    if (stLayer && S.stations) stLayer.eachLayer(function(m) { m.bringToFront(); });
  }
  function repaint() { layer.setStyle(style); legend(); sliderState(); }

  // capas opcionales
  var parksLayer = L.geoJSON(d.parks, {style: {fill: false, color: INK, weight: 1, opacity: .85, dashArray: '3 3'}, interactive: false});
  var stLayer = L.layerGroup(d.stations.features.map(function(f) {
    var m = L.circleMarker([f.geometry.coordinates[1], f.geometry.coordinates[0]], {radius: 4.5, fillColor: '#ffffff', fillOpacity: 1, color: INK, weight: 2});
    var t = document.createElement('div'); var b = document.createElement('b'); b.textContent = f.properties.estacion; t.appendChild(b);
    t.appendChild(document.createTextNode(' (línea ' + f.properties.linea + ')')); m.bindTooltip(t); return m; }));
  function overlays() {
    if (S.parks) { parksLayer.addTo(map); } else { map.removeLayer(parksLayer); }
    if (S.stations) { stLayer.addTo(map); stLayer.eachLayer(function(m) { m.bringToFront(); }); } else { map.removeLayer(stLayer); }
  }

  var cajaCiudad = L.geoJSON(d.levels.barrio.geo).getBounds();
  function ajustar() {
    map.invalidateSize();
    if (!isFinite(map.getZoom())) map.setView(cajaCiudad.getCenter(), 11, {animate: false});   // sin vista o zoom NaN: se reinicia
    var s = map.getSize(); if (!s.x || !s.y) return;                                           // contenedor aun sin tamano: espera al ResizeObserver
    map.fitBounds(cajaCiudad, {padding: [20, 20], animate: false});
  }
  build(); ajustar(); setTimeout(ajustar, 100); setTimeout(ajustar, 600);
  var rt = null; new ResizeObserver(function() { clearTimeout(rt); rt = setTimeout(ajustar, 100); }).observe(map.getContainer());

  // ---- panel de control (arriba a la derecha)
  function el(tag, css, txt) { var e = document.createElement(tag); if (css) e.style.cssText = css; if (txt != null) e.textContent = txt; return e; }
  function seg(opts, current, onpick) {
    var wrap = el('div', 'display:flex;border:1px solid #c3c2b7;border-radius:8px;overflow:hidden;margin:2px 0 8px'), btns = [];
    opts.forEach(function(o) {
      var b = el('button', 'flex:1;border:0;background:transparent;padding:5px 9px;font:inherit;cursor:pointer;color:#52514e;white-space:nowrap', o[1]);
      b.setAttribute('aria-pressed', o[0] === current); b.addEventListener('click', function() { onpick(o[0]); btns.forEach(function(x, i) { var on = opts[i][0] === o[0]; x.setAttribute('aria-pressed', on); paint(x, on); }); });
      function paint(x, on) { x.style.background = on ? 'rgba(11,11,11,.07)' : 'transparent'; x.style.color = on ? INK : '#52514e'; x.style.fontWeight = on ? '600' : '400'; x.style.boxShadow = on ? 'inset 0 -2px 0 ' + INK : 'none'; }
      paint(b, o[0] === current); btns.push(b); wrap.appendChild(b); });
    return wrap;
  }
  var panel = L.control({position: 'topright'});
  panel.onAdd = function() {
    var div = L.DomUtil.create('div', 'leaflet-bar'); div.style.cssText = 'background:' + SURF + ';padding:10px 12px;font:13px system-ui,sans-serif;color:' + INK + ';width:290px';
    div.appendChild(el('div', 'font-weight:600;font-size:12px;color:#898781;text-transform:uppercase;letter-spacing:.05em', 'Nivel'));
    div.appendChild(seg([['comuna', 'Comuna'], ['barrio', 'Barrio']], S.level, function(v) { S.level = v; build(); repaint(); }));
    div.appendChild(el('div', 'font-weight:600;font-size:12px;color:#898781;text-transform:uppercase;letter-spacing:.05em', 'Mapa de'));
    div.appendChild(seg([['precio', 'Precio'], ['subte', 'Subte'], ['verde', 'Verde'], ['dist', 'Dist.']], S.metric, function(v) { S.metric = v; repaint(); }));
    div.appendChild(el('div', 'font-weight:600;font-size:12px;color:#898781;text-transform:uppercase;letter-spacing:.05em;margin-bottom:3px', 'Capas'));
    [['stations', 'Estaciones de subte'], ['parks', 'Espacios verdes de 1 ha o más']].forEach(function(o) {
      var lab = el('label', 'display:flex;align-items:center;gap:6px;margin:2px 0;cursor:pointer'), cb = document.createElement('input'); cb.type = 'checkbox';
      cb.addEventListener('change', function() { S[o[0]] = cb.checked; overlays(); }); lab.appendChild(cb); lab.appendChild(document.createTextNode(o[1])); div.appendChild(lab); });
    L.DomEvent.disableClickPropagation(div); return div; };
  panel.addTo(map);

  // ---- deslizador de meses (abajo a la izquierda)
  var slider = null, lab = null, btn = null, timer = null;
  var ctl = L.control({position: 'bottomleft'});
  ctl.onAdd = function() {
    var div = L.DomUtil.create('div', 'leaflet-bar'); div.style.cssText = 'background:' + SURF + ';padding:8px 12px;font:13px system-ui,sans-serif;min-width:280px;color:' + INK;
    lab = el('div', 'font-weight:600;margin-bottom:4px'); div.appendChild(lab);
    var row = el('div', 'display:flex;gap:8px;align-items:center'); div.appendChild(row);
    btn = el('button', 'cursor:pointer;border:1px solid #c3c2b7;background:' + SURF + ';border-radius:6px;width:30px;height:26px', '▶'); btn.setAttribute('aria-label', 'Reproducir'); row.appendChild(btn);
    slider = document.createElement('input'); slider.type = 'range'; slider.min = 0; slider.max = d.months.length - 1; slider.value = S.idx; slider.style.flex = '1'; slider.setAttribute('aria-label', 'Mes'); row.appendChild(slider);
    slider.addEventListener('input', function() { S.idx = +slider.value; repaint(); });
    btn.addEventListener('click', function() {
      if (timer) { clearInterval(timer); timer = null; btn.textContent = '▶'; return; }
      if (S.idx >= d.months.length - 1) S.idx = 0; btn.textContent = '❚❚';
      timer = setInterval(function() { if (S.idx >= d.months.length - 1) { clearInterval(timer); timer = null; btn.textContent = '▶'; return; } S.idx++; repaint(); }, 120); });
    L.DomEvent.disableClickPropagation(div); return div; };
  ctl.addTo(map);
  function sliderState() {
    var on = S.metric === 'precio'; slider.disabled = !on; btn.disabled = !on; slider.value = S.idx;
    lab.textContent = on ? d.months[S.idx] : 'Mes (solo para el precio): ' + d.months[S.idx]; lab.style.opacity = on ? 1 : .55;
  }

  // ---- leyenda (abajo a la derecha)
  var legDiv = null, leg = L.control({position: 'bottomright'});
  leg.onAdd = function() { legDiv = L.DomUtil.create('div', 'leaflet-bar'); legDiv.style.cssText = 'background:' + SURF + ';padding:8px 12px;font:12px system-ui,sans-serif;color:' + INK; return legDiv; };
  leg.addTo(map);
  function legend() {
    legDiv.textContent = '';
    var h = el('div', 'font-weight:600;margin-bottom:4px;max-width:200px', NOMBRES[S.metric] + (S.metric === 'precio' ? ' (pesos constantes del 3T 2026)' : '')); legDiv.appendChild(h);
    var b = bins(), r = RAMP[S.metric];
    for (var i = b.length - 2; i >= 0; i--) {
      var row = el('div', 'display:flex;align-items:center;gap:6px;margin:1px 0'); row.appendChild(el('span', 'display:inline-block;width:18px;height:12px;background:' + r[Math.min(i, r.length - 1)]));
      row.appendChild(document.createTextNode(fmtLeg(b[i]) + ' a ' + fmtLeg(b[i + 1]))); legDiv.appendChild(row); }
    var n = el('div', 'display:flex;align-items:center;gap:6px;margin-top:3px'); n.appendChild(el('span', 'display:inline-block;width:18px;height:12px;' + NODATA_CSS));
    n.appendChild(document.createTextNode('Sin dato confiable')); legDiv.appendChild(n);
  }
  repaint();
}"

m <- leaflet(options = leafletOptions(zoomSnap = .25, attributionControl = FALSE)) |>
  onRender(js, data = datos) |>
  prependContent(tags$style("html,body{margin:0;height:100%} .leaflet-container{background:#f4f4f1} .leaflet-tooltip{font:12.5px system-ui,sans-serif}"),
                 tags$div(style = "position:absolute;z-index:1000;top:10px;left:56px;background:#fcfcfb;padding:8px 14px;border-radius:8px;box-shadow:0 1px 6px rgba(0,0,0,.2);font-family:system-ui,sans-serif;max-width:430px",
                          tags$div(style = "font-weight:700;font-size:15px", "Alquiler y servicios en CABA"),
                          tags$div(style = "font-size:12px;color:#52514e", "Zonaprop, 2 amb de 50 m² · subte y espacios verdes de BA Data · elegí nivel, variable y capas a la derecha")))
m$height <- "100vh"; m$width <- "100%"
saveWidget(m, file.path("Mapas", "mapa_interactivo.html"), selfcontained = TRUE, title = "Alquiler y servicios en CABA")
unlink(file.path("Mapas", "mapa_interactivo_files"), recursive = TRUE)
cat(sprintf("Mapas/mapa_interactivo.html (%.0f KB)\n", file.size("Mapas/mapa_interactivo.html") / 1024))
