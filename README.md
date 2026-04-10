# Environmental Time Series Analysis

A [Shiny](https://shiny.posit.co/) web application by [Sensing Clues](https://www.sensingclues.org) for exploring and visualizing environmental remote sensing time series data, including NDVI (vegetation) and Burned Area analyses across multiple conservation project areas.

## Features

### NDVI Explorer

-   **Time Series** — Plot multi-year NDVI trends for a selected project area and spatial resolution.
-   **Land Cover Explorer** — Visualize NDVI trends broken down by land cover type (crops, rangeland, water, trees, flooded vegetation, built area, bare ground), alongside an interactive land use/land cover map.
-   **Delta Map** — Generate static and interactive maps showing NDVI change (delta) for a selected month and year.

### Burned Area Explorer

-   **Time Series** — Plot burned area trends over time for a selected project area.
-   **Map Explorer** — Generate static burned area maps and interactive maps for a specific month and year, with GeoJSON export functionality.

### General

-   Interactive map showing the selected Area of Interest (AoI).
-   Figures are cached after first generation to speed up repeated requests.
-   Multiple satellite data sources and spatial resolutions supported.

## Supported Project Areas

| Area           | Country  | Available in         |
|----------------|----------|----------------------|
| Mponda         | Zambia   | NDVI Explorer        |
| Ancares Courel | Spain    | NDVI Explorer        |
| Stara Planina  | Bulgaria | NDVI Explorer        |
| Kasigau        | Kenya    | NDVI Explorer        |
| West Lunga     | Zambia   | Burned Area Explorer |

## Data Sources & Resolutions

| Source         | Resolutions          |
|----------------|----------------------|
| ESA Sentinel-2 | 100 m, 1000 m        |
| Terra MODIS    | 250 m, 500 m, 1000 m |

Data is available from 2015 onwards.

## Project Structure

```         
app/
├── app.R                  # Entry point
├── global.R               # Library loading and global variables
├── server.R               # Shiny server logic
├── ui.R                   # Top-level UI definition
├── ui/
│   ├── mod_header_ui.R    # Header module
│   ├── mod_sidebar_ui.R   # Sidebar controls module
│   └── mod_body_ui.R      # Main body/tab content module
├── src/
│   ├── utilities.R        # Helper functions
│   ├── visualization.R    # Visualization utilities
│   └── generate_plots.R   # Plot generation functions
├── translations.json      # UI translation strings (EN/NL/FR)
├── DESCRIPTION            # R package dependencies (deprecated)
└── www/
    ├── figures/           # Cached generated figures
    └── style.css          # Custom app styling
```

## Installation & Running

### Prerequisites

-   R (\>= 4.0 recommended)
-   The following R packages (as loaded in `global.R`):

``` r
install.packages(c(
  "dplyr", "future", "ggplot2", "ipc", "jsonlite",
  "leaflet", "leaflet.extras", "lubridate", "promises",
  "raster", "shiny", "shinybusy", "shiny.i18n", "shinyjs",
  "shinyTree", "shinyWidgets", "sf", "terra", "tidyr",
  "trend", "plotly", "htmlwidgets", "devtools"
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
