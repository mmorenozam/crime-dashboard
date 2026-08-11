# Homicidios y Detenciones en Quito — Dashboard (POC)

A modular multi-page Shiny application (bslib `page_navbar()`) exploring
homicide and arrest records for Quito, with independent reactive pipelines
per page, Leaflet maps, trend visualizations, summary tables, and a
downloadable raw-data view.

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
│   └── mod_about.R              # About page (static content)
├── data/                        # place the 5 input RDS files here (see below)
├── scripts/
│   └── generate_sample_data.R   # generates synthetic RDS files for smoke-testing
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

Project author: you (the dashboard owner). Implementation ideas and code
scaffolding assistance: ChatGPT and Claude (AI assistants), as reflected in the About
page of the running application.
