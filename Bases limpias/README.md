# Bases limpias

Generadas por `scripts/01_limpieza.R` a partir de `Bases crudas/`. Todas son CSV UTF-8 (con BOM), punto decimal, `NA` = celda vacía. Todo es Ciudad de Buenos Aires.

Claves: las tablas mensuales se cruzan por `fecha` (primer día del mes); las trimestrales por `anio` + `trimestre`.

## Mensuales

| Archivo | Columnas | Período | Notas |
|---|---|---|---|
| `alquiler_index_caba.csv` | `fecha, indice_estrenar, indice_general, indice_usado` | dic 2015 – sep 2026 | Índice de alquiler. |
| `oferta_zonaprop_caba.csv` | `fecha, venta_indice, venta_cantidad, alquiler_indice, alquiler_cantidad` | jun 2012 – ago 2025 | Índices base jun 2012 = 100. Las cantidades salen de fórmulas del Excel original sobre el índice (p. ej. alquiler = índice × 77,96), no son datos observados. |
| `ipcba.csv` | `fecha, ipcba_general, ipcba_viv, ipcba_alq` | jul 2012 – sep 2026 | **Nivel** del índice, base 2021 = 100 (no variación mensual). |
| `smvm.csv` | `fecha, smvm` | ene 2019 – sep 2026 | Pesos corrientes. |
| `precio_alquiler_ambientes.csv` | `fecha, ambientes, precio_pesos, provisorio` | mar 2010 – sep 2026 | Formato largo (`ambientes` = 1, 2, 3). Pesos corrientes, por unidad base de 30 m² (1 amb), 43 m² (2 amb) y 70 m² (3 amb). Fuente IDECBA. |
| `superficie_alquiler_2amb_barrio.csv` | `barrio, fecha, superficie_m2, provisorio` | jul 2013 – sep 2026 | 48 barrios. Suma de m² de deptos de 2 ambientes ofertados en alquiler en el mes. |
| `superficie_alquiler_2amb_total_caba.csv` | `fecha, superficie_m2, provisorio` | jul 2013 – sep 2026 | Fila "Total" de la fuente (separada para no duplicar al sumar barrios). Coincide con la suma de los 48 barrios. |

| `zonaprop_barrio.csv` | `fecha, barrio, precio_pesos, precio_estrenar, precio_usado, imputado, meses_desde_dato` | ene 2016 – sep 2026 | Precio de alquiler de un 2 amb de 50 m² por barrio (Zonaprop, pesos corrientes). `precio_pesos` es el general (a estrenar + usado). 48 barrios, con los nombres de `superficie_alquiler_2amb_barrio.csv`. Generado por `scripts/05_limpieza_barrios.R` desde `alquileres_caba_2amb_50m2.xlsx`. Ver "Barrios" abajo. |

`provisorio = TRUE` marca los meses con asterisco en la fuente.

### Barrios: qué se hizo con la base cruda

- La base imputa los meses faltantes **repitiendo el último valor observado en pesos corrientes**. Con inflación alta, un valor repetido por muchos meses queda muy por debajo del mercado (p. ej. Villa Soldati: $15.493 de 2021 a 2026, 1,7 % del promedio CABA).
- Regla: se conservan los valores observados y los imputados con hasta 3 meses de antigüedad (`MAX_EDAD_IMPUTACION` en el script). El resto queda vacío. `meses_desde_dato` = 0 si es observado.
- **dic 2015 se descarta**: en ese mes la mediana de los barrios es 3,2 veces el índice CABA, y en ene 2016 vuelve a ~0,9 (otra escala). También se descarta todo valor arrastrado desde ese mes.
- Control: el `precio_pesos` de los barrios queda entre 0,47 y 2,39 veces el promedio CABA (mediana 0,94). El máximo es Puerto Madero, observado, no imputado.
- Cobertura baja: Villa Soldati (31 meses), Villa Riachuelo (40), Villa Gral. Mitre (66, empieza en abr 2021), Villa Real (67), Versalles (78).
- La base no trae cantidad de avisos por barrio.

## Trimestrales

| Archivo | Columnas | Período | Notas |
|---|---|---|---|
| `itf_zona.csv` | `anio, trimestre, zona, itf, flag_cv` | 3T 2014 – 2T 2026 | Ingreso total familiar medio, pesos corrientes. `zona` = total, norte, centro, sur. |
| `ingreso_individual.csv` | `anio, trimestre, seccion, variable, valor, flag_cv` | 1T 2015 – 2T 2026 | 39 variables en formato largo. Las unidades están en el nombre de la variable (`$`, `%`, personas). |

`flag_cv = "a"`: valor indicativo (coeficiente de variación estimado entre 10 % y 20 %).

## Panel trimestral

`panel_trimestral.csv` (generado por `scripts/02_panel_trimestral.R`): una fila por trimestre (2T 2010 – 3T 2026, 66 filas) y 50 columnas. Une todas las tablas de arriba, salvo superficie por barrio e ingreso individual completo (solo 6 variables de ingreso).

Reglas de agregación de las series mensuales (si al trimestre le falta algún mes, queda vacío):

| Prefijo / columna | Regla |
|---|---|
| `alq_*`, `zonaprop_*`, `ipcba_*`, `precio_alq_*` (`_prom`) | Promedio de los 3 meses. |
| `alq_indice_general_cierre`, `ipcba_general_cierre` | Valor del último mes del trimestre. |
| `smvm` | Valor del mes intermedio (criterio del IDECBA). |
| `oferta_2amb_m2_mensual_prom` | Promedio mensual de m² ofertados (2 amb, total CABA). |
| `itf_*`, `ing_*` | Datos trimestrales tal cual. `itf_indicativo_*` = TRUE si el valor es indicativo (CV 10–20 %). |
| `*_real` | Pesos constantes de 3T 2026: valor × (IPCBA general prom. 3T 2026 / IPCBA general prom. del trimestre). |
| `alq_2amb_sobre_*` | Precio de alquiler de 2 amb dividido por ITF, SMVM o ingreso medio de perceptores (mismo trimestre, pesos corrientes). |

Trimestres con datos en todas las fuentes principales: 1T 2019 – 2T 2025 (26 trimestres).

## Cosas a tener en cuenta

- Todos los montos están en **pesos corrientes**; para comparar en el tiempo hay que deflactar (p. ej. con `ipcba_general`).
- `ingreso_individual`: el 3T 2019 de "Ingreso laboral de patrón o empleador" viene como `---` en la fuente y queda `NA`. Además, el IDECBA advierte que los promedios del 2T y 3T 2019 están influidos por valores extremos de ingreso.
- `ingreso_individual`: el nombre de `variable` no es único; es único dentro de cada `seccion`.
- Las series de superficie estuvieron discontinuadas entre sep 2011 y jun 2013 (cambio de proveedor), y el proveedor de datos cambió en jul 2015 (Adinco → Argenprop).
- Cobertura común de todas las series: ene 2019 – ago 2025 en mensual (limitada por SMVM y Zonaprop).
