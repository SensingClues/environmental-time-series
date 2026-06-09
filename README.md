# Environmental Time Series Analysis

<!-- TODO: replace the "#" in the Data API badge below with the deployed Data API URL once it is live -->
[![Shiny App – Production](https://img.shields.io/badge/Shiny_App-Production-2E7D32?logo=r&logoColor=white)](https://analytical.sensingclues.org/environmentaltimeseries/)
[![Shiny App – Test](https://img.shields.io/badge/Shiny_App-Test-F57C00?logo=r&logoColor=white)](https://analytical.sensingclues.org/test/environmentaltimeseries/)
[![Data API – coming soon](https://img.shields.io/badge/Data_API-coming_soon-9E9E9E?logo=fastapi&logoColor=white)](#)

A [Shiny](https://shiny.posit.co/) web application by [Sensing Clues](https://www.sensingclues.org) for exploring and visualizing environmental remote sensing time series data, including NDVI (vegetation), Burned Area, and land cover analyses across multiple conservation project areas. The app pairs interactive explorers with an AI Assistant that can answer questions and generate charts, tables, and maps on demand.

## Features

### NDVI Explorer

-   **Time Series** — Two views: a **Monthly view** showing the seasonal NDVI pattern with a historical range band, a vegetation-health summary card, and statistical trend cards (Wilcoxon signed-rank and Seasonal Mann–Kendall); and an **Annual view** showing the year-by-year long-term trend.
-   **Land Cover Explorer** — NDVI trends broken down by land cover type (crops, rangeland, water, trees, flooded vegetation, built area, bare ground), alongside an interactive land use/land cover map.
-   **Delta Map** — A **Monthly view** showing NDVI change against the same month in previous years, and an **Annual change view** comparing the average vegetation health between two selected years across the landscape.

### Burned Area Explorer

-   **Time Series** — A **Seasonal Overview** of monthly burned area against a historical range band, and a **Daily Activity** view showing day-level fire detections across the fire season for one or more years.
-   **Map Explorer** — A **Monthly view** (where fires occurred, with an interactive Leaflet map and GeoJSON export), an **Annual view** (cumulative burn across a full year), and a **Fire Return Period** view (how often each area tends to burn).

### Scenario Explorer

-   **Land Cover Productivity** — Compares productivity and year-to-year stability across land cover classes.
-   **Agricultural Monitoring** — Tracks NDVI for crops and rangeland.
-   **Anomaly Resilience** — Examines how vegetation responded during an anomalous year.

### AI Assistant

-   Ask questions in natural language about vegetation trends, fire patterns, and land cover changes.
-   Multi-provider LLM backend (Anthropic Claude or OpenAI GPT), selectable in the settings panel.
-   A tool-calling agent that queries the Data API and renders results inline as **Plotly charts, tables, interactive Leaflet maps, and side-by-side comparison images**.
-   Responses render Markdown and LaTeX (via MathJax).
-   Uses a server-configured API key when available, or a user-provided key entered in the UI (kept for the session only, never stored).

### General

-   Interactive map showing the selected Area of Interest (AoI).
-   Multilingual UI (English, Dutch, French).
-   Figures are cached after first generation to speed up repeated requests.
-   Multiple satellite data sources and spatial resolutions supported.

## Supported Project Areas

| Area           | Country  | Available in                                  |
|----------------|----------|-----------------------------------------------|
| Mponda         | Zambia   | NDVI Explorer, Scenario Explorer, Burned Area |
| West Lunga     | Zambia   | NDVI Explorer, Scenario Explorer, Burned Area |
| Ancares Courel | Spain    | NDVI Explorer, Scenario Explorer              |
| Stara Planina  | Bulgaria | NDVI Explorer, Scenario Explorer              |
| Kasigau        | Kenya    | NDVI Explorer, Scenario Explorer              |

## Data Sources & Resolutions

| Source         | Resolutions          |
|----------------|----------------------|
| ESA Sentinel-2 | 100 m, 1000 m        |
| Terra MODIS    | 250 m, 500 m, 1000 m |

Data is available from 2015 onwards.

## Data Access (hot/warm/cold)

The app resolves data through a three-tier fallback so it stays responsive even if a tier is unavailable:

1.  **Hot path — Data API.** A companion FastAPI service serves on-demand time series, grids, and geometry. Configured via the `NDVI_API_URL` environment variable (defaults to `http://localhost:8000`) with a hard 2-second timeout.
2.  **Warm path — Parquet.** Pre-computed summaries read from local Parquet files.
3.  **Cold path — Raster.** Raw raster (TIF) files read and processed directly.

If the Data API is unreachable, the app silently falls back to the warm and then cold paths, so all explorers continue to work against local data. The AI Assistant requires the Data API (hot path).

## Project Structure

```
app/
├── app.R                     # Entry point
├── global.R                  # Library loading, global variables, and API/data config
├── server.R                  # Shiny server logic
├── ui.R                      # Top-level UI definition
├── ui/
│   ├── mod_header_ui.R       # Header module
│   ├── mod_sidebar_ui.R      # Sidebar controls module
│   ├── mod_body_ui.R         # Main body/tab content module
│   └── mod_busy_spinner_ui.R # Busy-spinner module
├── src/
│   ├── utilities.R           # Helper functions (incl. API/geometry helpers)
│   ├── visualization.R       # Visualization utilities and hot-path builders
│   ├── generate_plots.R      # Plot generation functions
│   ├── scenario_analysis.R   # Scenario Explorer analyses
│   └── agent_renderers.R     # Renders AI Assistant charts/tables/maps/images
├── translations.json         # UI translation strings (EN/NL/FR)
├── DESCRIPTION               # R package dependencies (deprecated)
└── www/
    ├── figures/              # Cached generated figures
    └── style.css             # Custom app styling
```

## Installation & Running

### Prerequisites

-   R (\>= 4.0 recommended)
-   The following R packages (as loaded in `global.R`):

``` r
install.packages(c(
  "dplyr", "future", "ggplot2", "htmltools", "ipc", "jsonlite",
  "leaflet", "leaflet.extras2", "lubridate", "promises", "raster",
  "RColorBrewer", "shiny", "shinybusy", "shiny.i18n", "shinyjs",
  "shinyTree", "shinyWidgets", "sf", "terra", "tidyr", "trend",
  "plotly", "htmlwidgets", "arrow", "httr", "commonmark", "devtools"
))
```

-   The `sensingcluesr` package (installed from GitHub):

``` r
devtools::install_github("sensingclues/sensingcluesr@v1.0.3")
```

### Run the App

``` r
# From the app/ directory
shiny::runApp("app")
```

Or open `app.R` in RStudio and click **Run App**.

### Configure the Data API and AI Assistant

The hot path and the AI Assistant talk to the companion Data API. Point the app at a running instance via an environment variable (otherwise it defaults to `http://localhost:8000` and silently falls back to local data):

``` r
# In R, before launching the app, or in .Renviron
Sys.setenv(NDVI_API_URL = "https://your-data-api-host")
```

The AI Assistant needs an LLM API key. Either configure a server-side key as an environment variable on the Data API host:

```
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_API_KEY=sk-...
```

…or leave them unset and let each user paste their own key into the Assistant settings panel (used for that session only, never stored).

### Data Setup

The app reads input data from a folder with the following structure:

```
<data_dir>/
├── AoI/
│   └── AoI_<project_area>.geojson  # Area of Interest boundaries
├── BurnedArea/
│   └── <project_area>/
│       └── <resolution>/           # Burned area raster data files
├── LandUse/
│   └── <project_area>/
│       └── S2_10m_LULC_2023/       # Land use/land cover GeoJSONs
└── NDVI/
    └── <project_area>/
        └── <resolution>/           # NDVI raster data files
```

**When running locally**, you can place this data folder anywhere on your machine. Update the `data_dir` variable in `global.R` to point to it:

``` r
# global.R
data_dir <- file.path("path/to/your/local/data/folder")
```

A commented-out example is already provided in `global.R` for reference:

``` r
# Set test folder structure (uncomment when working locally with a different folder structure)
# test_dir <- file.path("www/data")
# data_dir <- test_dir
```

Generated figures are stored automatically in `app/www/figures/`.

## Contact

For data issues or support, contact [helpdesk\@sensingclues.org](mailto:helpdesk@sensingclues.org).

More information: [sensingclues.org/environmental-time-series-analysis](https://www.sensingclues.org/environmental-time-series-analysis)
