# Homicidios y Detenciones en Quito — Dashboard (POC)

A modular multi-page Shiny application (bslib `page_navbar()`) that brings
together individual-level homicide and arrest/detention records for the
Distrito Metropolitano de Quito (sourced from Ecuador's Ministerio del
Interior open-data portal) into one exploratory tool. It lets analysts see
*where*, *when*, and under what circumstances these events happen, and
compare raw case counts against population-adjusted incidence rates across
parishes and years — via independent reactive pipelines per page, four
Leaflet map modes (case locations, heatmap, clustered markers, incidence
choropleth), trend visualizations, summary tables, and a downloadable
raw-data view. The full narrative (purpose, data sources, methodology,
usage guide) lives in the app's own **About** page
(`R/mod_about.R`) — this README stays focused on the codebase.

## Project structure

```
crime-dashboard/
├── app.R                       # thin composition root: loads packages, data, wires UI/server
├── R/
│   ├── utils_data.R            # load_app_data(): reads the 5 RDS files, cleans names/types
│   ├── utils_helpers.R         # pure helper functions (filtering, incidence, time fields)
│   ├── mod_map.R                # shared Leaflet map module (4 modes)
│   ├── mod_trends.R             # shared 2x2 trends module (temporal/sex/age/hour-weekday)
│   ├── mod_summary_table.R      # shared frequency-table module
│   ├── mod_raw_table.R          # shared searchable/sortable/downloadable table module
│   ├── mod_homicides.R          # Homicides page: sidebar + reactive pipeline + wiring
│   ├── mod_arrests.R            # Arrests page: sidebar + reactive pipeline + wiring
│   └── mod_about.R              # About page (static content: purpose, sources, usage guide)
├── data/                        # the 5 RDS files the deployed app actually reads (tracked in git)
├── R_prep/                      # offline pipeline that builds data/*.rds from raw sources (not run by the app)
│   ├── 00_Auxiliary_Functions.R # e.g. spatial QC of event coordinates against parish polygons
│   └── 01_Data_Preparation.R    # reads data_geoportal_raw/ + data_crimen_raw/ + population/, writes data/*.rds
├── data_geoportal_raw/          # raw parish/outline shapefiles (gitignored, confidential/large)
├── data_crimen_raw/             # raw homicide & detention source files (gitignored, confidential)
├── data_processed/              # intermediate outputs of R_prep (gitignored)
├── population/                  # raw population projection inputs consumed by R_prep
├── scripts/
│   ├── generate_sample_data.R   # generates synthetic RDS files for smoke-testing without real data
│   └── simplify_polygons.R      # one-off: simplifies data/01 & 02 *.rds in place for map performance (see below)
├── manifest.json                # rsconnect deployment manifest (package snapshot for Posit Cloud/Connect)
└── README.md
```

## Data expected in `data/`

| File                        | Loaded as         |
|-----------------------------|--------------------|
| `01_quito_outline.rds`      | `quito_outline`    |
| `02_quito_parroquias.rds`   | `quito_parroquias` |
| `03_homicidios.rds`         | `homicidios`       |
| `04_detenciones.rds`        | `detenciones`      |
| `05_pob_parroquias.rds`     | `pob_parroquias`   |

`load_app_data()` (in `R/utils_data.R`) reads these once at app start,
strips the numeric prefixes, reprojects spatial layers to WGS84, and
builds a normalized `parroquia_key` join key across all four tabular/
spatial layers (upper-cased, trimmed) so parish names match reliably
between the crime data, the population table, and the parish polygons.

These 5 files are themselves generated offline from confidential/large raw
sources by `R_prep/01_Data_Preparation.R` (shapefiles in
`data_geoportal_raw/`, crime records in `data_crimen_raw/`, population
projections in `population/`). The app never touches `R_prep/` or the raw
folders at runtime — only the finished `data/*.rds` files matter for
deployment.

## Running the app

1. Install dependencies (R ≥ 4.1 recommended):

   ```r
   install.packages(c(
     "shiny", "bslib", "leaflet", "leaflet.extras", "sf",
     "ggplot2", "plotly", "DT"
   ))
   ```

2. Put the 5 real RDS files in `data/`, **or** generate synthetic
   placeholder data to try the app immediately:

   ```r
   # from the project root
   source("scripts/generate_sample_data.R")
   ```

3. Run the app:

   ```r
   shiny::runApp(".")
   ```

## Design notes

- **Independent pipelines per page.** `mod_homicides.R` and
  `mod_arrests.R` each own their filters, their `reactive()` filtering
  pipeline, and their own calls into the shared map/trends/table modules.
  No shared "generic crime record" abstraction was forced across the two
  different datasets — only genuinely reusable, dataset-agnostic pieces
  (map rendering, chart rendering, table rendering, filter helpers) live
  in shared modules.
- **"All" sentinel filters.** Multi-selects that default to "All" use
  `expand_all_selection()` to translate "All" (or an empty selection) into
  "no filter", instead of special-casing it in every reactive.
- **Year range validity.** `sanitize_year_range()` swaps `From`/`To` if
  the user picks them in the wrong order, and the corrected values are
  pushed back into the UI so the widgets never show an invalid combination.
- **Incidence vs. counts.** `compute_incidence()` /
  `compute_incidence_by_parish()` centralize the population-normalization
  math (events per 100,000) used by both the Trends bar chart and the
  choropleth map, keyed on the shared `parroquia_key`.
- **Dynamic grouping.** `split_col()` (defined per page) returns
  `"nombre_parroquia"` when one or more parishes are selected, `"area"`
  when Urban/Rural is filtered (parish takes priority per spec), or `NULL`
  otherwise; `mod_trends.R` uses this single reactive to decide whether to
  facet/color the temporal and sex charts, while age distribution and the
  hour x weekday heatmap always stay aggregated as specified.
- **Maps.** `mod_map.R` uses `leafletProxy()` so only the changed layer is
  redrawn on filter changes rather than rebuilding the whole map, keeping
  interactions responsive.
- **Raw data.** `mod_raw_table.R` wraps `DT::datatable()` with
  column-level filters plus a `downloadHandler()` emitting the currently
  filtered rows as CSV.

## Performance

Early deployments to Posit Cloud were slow to render, mainly because of two
compounding issues: the parish/outline polygons carried far more detail than
a web map needs, and `detenciones` (~77k rows) could get plotted as tens of
thousands of individual unclustered markers. Fixes so far:

- **Simplified boundary polygons.** `quito_outline` and `quito_parroquias`
  are redrawn on *every* page load regardless of any filter, so their vertex
  count is pure fixed overhead. `scripts/simplify_polygons.R` runs
  topology-preserving simplification (`rmapshaper::ms_simplify`, keeping
  parish borders seamless — no slivers/gaps) and overwrites `data/01_*.rds`
  / `data/02_*.rds` in place:
  - `01_quito_outline.rds`: 157k → 9.4k vertices (1.95 MB → 0.13 MB)
  - `02_quito_parroquias.rds`: 575k → 37.6k vertices (7.46 MB → 0.49 MB)

  This is a one-off, re-runnable script (`Rscript scripts/simplify_polygons.R`),
  not part of the app's startup path. If the source shapefiles are ever
  regenerated via `R_prep/01_Data_Preparation.R`, re-run it afterwards.
- **Canvas rendering.** `mod_map.R` builds the Leaflet map with
  `leafletOptions(preferCanvas = TRUE)`, which is substantially faster than
  the SVG default once marker counts get into the thousands.
- **Vectorized popups.** `build_popup()` used to loop row-by-row with
  `apply()`; it's now vectorized per-column, which matters once the
  filtered set reaches tens of thousands of rows.
- **Auto-clustering on large point sets.** In "Casos" (raw marker) mode,
  plotting every individual point is what actually freezes the browser at
  scale. Past `AUTO_CLUSTER_THRESHOLD` (3,000 filtered points, defined in
  `mod_map.R`), the map silently switches to marker clustering — same as
  "Casos agrupados" already does — and shows a small on-map note explaining
  why.

If the underlying datasets keep growing, the next lever to pull is
downsampling/pre-aggregating points server-side before they ever reach
Leaflet, rather than relying on client-side clustering alone.

## Known simplifications in this POC

- The synthetic data generator (`scripts/generate_sample_data.R`) produces
  plausible but fake values purely to validate that the app runs
  end-to-end; it is not a substitute for the real RDS inputs.
- `detenciones` has no explicit Urban/Rural column in the specification;
  the Arrests page looks for `area` or falls back to `area_hecho` if
  present, otherwise the Area filter has no effect (documented in code).
- Popups on point layers show a small fixed set of fields per dataset;
  extend `popup_fields` in `mod_homicides.R` / `mod_arrests.R` to add more.

## Credits

Project author: Mauricio Moreno, PhD. Implementation ideas and code
scaffolding assistance: ChatGPT and Claude (AI assistants), as reflected in the About
page of the running application.
