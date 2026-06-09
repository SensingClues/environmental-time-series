# -----------------------------------------------------------------------------
# AGENT RENDERERS (Phase 2B) — turn agent chart/table refs into Plotly + tables.
#
# Mode A: ref has $endpoint + $params (no $data) -> fetch via try_api_call(),
#         reshape, hand to an existing plotting function.
# Mode B: ref has $data (no $endpoint) -> render directly from inline data.
# Leaflet maps + static images are handled in Step 3 (return empty plot here).
# -----------------------------------------------------------------------------

# ---- Chart dispatcher --------------------------------------------------------

render_agent_chart <- function(chart) {
  if (is.null(chart)) return(plotly::plotly_empty())

  # Mode B: inline data
  if (!is.null(chart$data) && is.null(chart$endpoint)) {
    return(render_mode_b_chart(chart))
  }

  # Mode A: fetch + existing plotting function
  switch(chart$type %||% "",
    "timeseries_monthly"  = render_chart_timeseries_monthly(chart),
    "timeseries_annual"   = render_chart_timeseries_annual(chart),
    "landcover"           = render_chart_landcover(chart),
    "burned_area_monthly" = render_chart_burned_area_monthly(chart),
    "burned_area_daily"   = render_chart_burned_area_daily(chart),
    "anomaly"             = render_chart_anomaly(chart),
    "phenology"           = render_chart_phenology(chart),
    plotly::layout(plotly::plotly_empty(),
                   title = paste("Unknown chart type:", chart$type %||% "NA"))
  )
}

# ---- Mode B generic chart ----------------------------------------------------

render_mode_b_chart <- function(chart) {
  df <- tryCatch(
    as.data.frame(do.call(rbind, lapply(chart$data, as.data.frame))),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0 || ncol(df) < 2) return(plotly::plotly_empty())

  x_key <- chart$x_key %||% names(df)[1]
  y_key <- chart$y_key %||% names(df)[2]

  # Reorder x-axis chronologically if values are month names
  month_levels <- c("January","February","March","April","May","June",
                    "July","August","September","October","November","December")
  if (all(df[[x_key]] %in% month_levels)) {
    df[[x_key]] <- factor(df[[x_key]], levels = month_levels)
  }

  # Convert year-like integers to character to prevent decimal axis
  # A value is year-like if it's a 4-digit integer between 1990 and 2100
  if (is.numeric(df[[x_key]]) &&
      all(df[[x_key]] == floor(df[[x_key]]), na.rm = TRUE) &&
      all(df[[x_key]] >= 1990 & df[[x_key]] <= 2100, na.rm = TRUE)) {
    df[[x_key]] <- as.character(df[[x_key]])
  }

  p <- if (identical(chart$type, "simple_line")) {
    plotly::plot_ly(df, x = ~get(x_key), y = ~get(y_key),
                    type = "scatter", mode = "lines+markers",
                    line = list(color = "#2d6a4f"))
  } else {
    plotly::plot_ly(df, x = ~get(x_key), y = ~get(y_key),
                    type = "bar", marker = list(color = "#2d6a4f"))
  }

  p <- plotly::layout(p,
    title  = chart$title %||% "",
    xaxis  = list(title = x_key),
    yaxis  = list(title = y_key),
    margin = list(t = 40)
  )
  plotly::config(p, displayModeBar = FALSE)
}

# ---- Mode A chart renderers --------------------------------------------------

render_chart_timeseries_annual <- function(chart) {
  df <- try_api_call(chart$endpoint, chart$params)
  if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())
  plot_ndvi_annual(df)
}

render_chart_timeseries_monthly <- function(chart) {
  df <- try_api_call(chart$endpoint, chart$params)
  if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())

  # Highlight the agent-specified year, else the most recent COMPLETE year
  # (the max year is usually still in progress). Single-year data -> use it.
  highlight_year <- as.integer(chart$params$year %||%
                                 max(df$year[df$year < max(df$year)], na.rm = TRUE))
  if (length(unique(df$year)) == 1) highlight_year <- unique(df$year)

  # Reshape to the columns plot_ndvi_anomaly() expects (YearMonth, NDVI, Year, Month).
  df$YearMonth <- as.Date(paste(df$year, sprintf("%02d", df$month), "01", sep = "-"))
  df$NDVI      <- df$mean_ndvi
  df$Year      <- as.character(df$year)
  df$Month     <- sprintf("%02d", df$month)
  keep  <- c("YearMonth", "NDVI", "Year", "Month")
  train <- df[df$year != highlight_year, keep, drop = FALSE]
  test  <- df[df$year == highlight_year, keep, drop = FALSE]
  if (nrow(test) == 0) return(plotly::plotly_empty())

  plot_ndvi_anomaly(train_ndvi_df = train, test_ndvi_df = test) %>%
    plotly::layout(
      legend = list(orientation = "h", x = 0.5, xanchor = "center",
                    y = -0.32, yanchor = "top", font = list(size = 9)),
      margin = list(t = 30, b = 130)
    )
}

render_chart_landcover <- function(chart) {
  df <- try_api_call(chart$endpoint, chart$params)
  if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())
  year_param <- as.integer(chart$params$year %||% max(df$year, na.rm = TRUE))
  df_year <- df[df$year == year_param, ]
  summary_df <- df_year %>%
    dplyr::group_by(land_cover) %>%
    dplyr::summarise(mean_ndvi = mean(mean_ndvi, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(mean_ndvi))
  p <- plotly::plot_ly(summary_df, x = ~land_cover, y = ~mean_ndvi,
                       type = "bar", marker = list(color = "#2d6a4f"))
  plotly::layout(p,
    title  = paste("NDVI by land cover class,", year_param),
    xaxis  = list(title = ""),
    yaxis  = list(title = "Mean NDVI"),
    margin = list(t = 40)
  )
}

render_chart_burned_area_monthly <- function(chart) {
  df <- try_api_call(chart$endpoint, chart$params)
  if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())

  highlight_year <- as.integer(chart$params$year %||%
                                 max(df$year[df$year < max(df$year)], na.rm = TRUE))
  if (length(unique(df$year)) == 1) highlight_year <- unique(df$year)

  # Reshape to plot_ba_timeseries_plotly()'s contract (Month, mean_val, lower_ci, upper_ci).
  # The baseline (ba_mean/CI) is per-calendar-month and year-independent.
  tr <- unique(df[, c("month", "ba_mean", "ba_lower_ci", "ba_upper_ci")])
  train_data <- data.frame(
    Month    = sprintf("%02d", tr$month),
    mean_val = tr$ba_mean,
    lower_ci = pmax(tr$ba_lower_ci, 0),
    upper_ci = tr$ba_upper_ci,
    stringsAsFactors = FALSE
  )
  te <- df[df$year == highlight_year, ]
  if (nrow(te) == 0) return(plotly::plotly_empty())
  test_data <- data.frame(
    Month    = sprintf("%02d", te$month),
    mean_val = te$burned_km2,
    lower_ci = te$burned_km2,
    upper_ci = te$burned_km2,
    stringsAsFactors = FALSE
  )
  plot_ba_timeseries_plotly(train_data = train_data, test_data = test_data,
                            test_year = highlight_year)
}

render_chart_burned_area_daily <- function(chart) {
  df <- try_api_call(chart$endpoint, chart$params)
  if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())
  daily <- data.frame(
    date = as.Date(df$date),
    km2  = df$burned_km2,
    year = as.character(df$year),
    stringsAsFactors = FALSE
  )
  plot_ba_daily_activity(daily_data = daily,
                         selected_years = unique(daily$year))
}

# /ndvi/by-landcover requires a `year`; the scenario plots need every year. Fetch
# year-by-year over the sensor's range and bind. NULL/empty -> NULL.
.agent_lc_fetch_all <- function(params) {
  sensor     <- params$sensor %||% "modis"
  start_year <- if (identical(sensor, "modis")) 2000L else 2019L
  end_year   <- as.integer(format(Sys.Date(), "%Y"))
  parts <- lapply(start_year:end_year, function(y) {
    py <- params; py$year <- y
    try_api_call("/api/v1/ndvi/by-landcover", py)
  })
  out <- do.call(rbind, Filter(Negate(is.null), parts))
  if (is.null(out) || nrow(out) == 0) NULL else out
}

render_chart_anomaly <- function(chart) {
  params_lc <- modifyList(chart$params, list(format = "table"))
  params_lc$year <- NULL
  df <- try_api_call("/api/v1/ndvi/by-landcover", params_lc)

  if (is.null(df)) {
    sensor <- chart$params$sensor %||% "sentinel2"
    years  <- if (sensor == "modis") 2000:2025 else 2019:2025
    df_list <- lapply(years, function(y) {
      try_api_call("/api/v1/ndvi/by-landcover",
                   modifyList(chart$params, list(year = y, format = "table")))
    })
    df <- do.call(rbind, Filter(Negate(is.null), df_list))
  }

  if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())

  # Ensure required columns exist with correct names and types
  required <- c("year", "month", "land_cover", "mean_ndvi")
  if (!all(required %in% names(df))) return(plotly::plotly_empty())

  df$year       <- as.integer(df$year)
  df$month      <- as.integer(df$month)
  df$land_cover <- as.character(df$land_cover)
  df$mean_ndvi  <- as.numeric(df$mean_ndvi)

  df <- df[complete.cases(df[, required]), ]
  if (nrow(df) == 0) return(plotly::plotly_empty())

  anomaly_year <- as.integer(chart$params$year %||% max(df$year, na.rm = TRUE))
  if (!anomaly_year %in% df$year) anomaly_year <- max(df$year, na.rm = TRUE)

  result <- tryCatch(
    plot_anomaly_resilience(df = df, anomaly_year = anomaly_year),
    error = function(e) NULL
  )
  if (is.null(result)) return(plotly::plotly_empty())
  result$heatmap
}

render_chart_phenology <- function(chart) {
  df <- .agent_lc_fetch_all(chart$params)
  if (is.null(df)) return(plotly::plotly_empty())
  selected_class <- chart$params$land_cover %||% "Crops"
  res <- tryCatch(plot_agricultural_monitoring(df = df, selected_class = selected_class),
                  error = function(e) NULL)
  if (is.null(res) || is.null(res$plot)) return(plotly::plotly_empty())
  res$plot
}

# ---- Table dispatcher --------------------------------------------------------

render_agent_table <- function(tbl) {
  if (is.null(tbl)) return(data.frame())

  # Mode B: inline data
  if (!is.null(tbl$data) && is.null(tbl$endpoint)) {
    df <- tryCatch(
      as.data.frame(do.call(rbind, lapply(tbl$data, as.data.frame))),
      error = function(e) data.frame()
    )
    if (!is.null(tbl$columns) && length(tbl$columns) > 0) {
      cols <- intersect(unlist(tbl$columns), names(df))
      if (length(cols) > 0) df <- df[, cols, drop = FALSE]
    }
    return(df)
  }

  # Mode A: fetch + per-type tidy
  df <- try_api_call(tbl$endpoint, tbl$params)
  if (is.null(df) || nrow(df) == 0) return(data.frame())

  df <- switch(tbl$type %||% "",
    "ndvi_annual" = {
      df$mean_ndvi <- round(df$mean_ndvi, 4)
      df[, c("year", "mean_ndvi"), drop = FALSE]
    },
    "ndvi_by_class" = {
      year_param <- as.integer(tbl$params$year %||% max(df$year, na.rm = TRUE))
      df %>%
        dplyr::filter(year == year_param) %>%
        dplyr::group_by(land_cover) %>%
        dplyr::summarise(mean_ndvi = round(mean(mean_ndvi, na.rm = TRUE), 4),
                         .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(mean_ndvi))
    },
    "burned_area" = {
      df$burned_km2 <- round(df$burned_km2, 2)
      keep <- intersect(c("year", "month", "burned_km2", "ba_mean"), names(df))
      df[, keep, drop = FALSE]
    },
    "anomaly" = {
      df$anomaly_value <- round(df$anomaly_value, 4)
      df %>%
        dplyr::arrange(anomaly_value) %>%
        dplyr::select(year, month, land_cover, anomaly_value)
    },
    "phenology" = {
      df %>%
        dplyr::select(year, land_cover, green_up_month, peak_month,
                      peak_ndvi, senescence_month) %>%
        dplyr::mutate(peak_ndvi = round(peak_ndvi, 4))
    },
    df
  )

  if (!is.null(tbl$columns) && length(tbl$columns) > 0) {
    cols <- intersect(unlist(tbl$columns), names(df))
    if (length(cols) > 0) df <- df[, cols, drop = FALSE]
  }
  df
}

# ---- Leaflet map dispatcher (Step 3) -----------------------------------------

render_agent_leaflet <- function(chart) {
  switch(chart$type %||% "",
    "delta_map"       = render_leaflet_delta_map(chart),
    "frp_map"         = render_leaflet_frp(chart),
    "burned_area_map" = render_leaflet_burned_area(chart),
    leaflet::leaflet() %>% leaflet::addTiles()
  )
}

# Spatial NDVI change between two years (reuses the Delta Map renderer).
render_leaflet_delta_map <- function(chart) {
  aoi    <- chart$params$aoi
  sensor <- chart$params$sensor %||% "modis"
  res    <- chart$params$resolution %||% 1000
  result <- tryCatch(
    compute_ndvi_delta_hot(aoi, sensor, res,
                           chart$params$year_a, chart$params$year_b),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(leaflet::leaflet() %>% leaflet::addTiles() %>%
             leaflet::addPopups(0, 0, "Delta map data unavailable."))
  }
  plot_annual_ndvi_leaflet(result)
}

# Fire return period (reuses build_ba_frp_leaflet; uses the global data_dir).
render_leaflet_frp <- function(chart) {
  aoi <- chart$params$aoi
  res <- chart$params$resolution %||% 500
  result <- tryCatch(
    build_ba_frp_leaflet(data_dir = data_dir, country = aoi, resolution = res),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(leaflet::leaflet() %>% leaflet::addTiles() %>%
             leaflet::addPopups(0, 0, "FRP data unavailable."))
  }
  result$map
}

# Annual burn-frequency map (API grid + AoI outline).
render_leaflet_burned_area <- function(chart) {
  aoi  <- chart$params$aoi
  year <- chart$params$year %||% (as.integer(format(Sys.Date(), "%Y")) - 1)

  grid_result <- tryCatch(
    try_api_grid_call("/api/v1/burned-area/annual-grid", list(aoi = aoi, year = year)),
    error = function(e) NULL
  )
  aoi_files <- list.files(file.path(data_dir, "AoI"),
                          pattern = paste0("AoI.*", aoi, ".*\\.geojson$"))
  aoi_shape <- if (length(aoi_files) > 0) tryCatch(
    read_aoi_geometry(file.path(data_dir, "AoI", aoi_files[[1]]), aoi),
    error = function(e) NULL) else NULL

  if (is.null(grid_result) || is.null(aoi_shape)) {
    return(leaflet::leaflet() %>% leaflet::addTiles() %>%
             leaflet::addPopups(0, 0, "Burned area map data unavailable."))
  }
  build_ba_annual_leaflet(
    ba_df            = parse_grid_response(grid_result),
    aoi_shape        = aoi_shape,
    year             = year,
    total_burned_km2 = grid_result$metadata$total_burned_km2
  )
}

# ---- Static comparison-image renderer (Step 3) -------------------------------
# Builds a multi-year facet PNG (NDVI or burned area) from the hot builders.
render_agent_image <- function(chart, output_id) {
  aoi       <- chart$params$aoi
  sensor    <- chart$params$sensor %||% "modis"
  res       <- chart$params$resolution %||% 1000
  month     <- as.integer(chart$params$month %||% 1)
  years_vec <- as.integer(unlist(chart$params$years_vec))
  map_year  <- if (length(years_vec)) max(years_vec) else
                 as.integer(format(Sys.Date(), "%Y")) - 1
  is_ba     <- grepl("burned-area", chart$endpoint %||% "", fixed = TRUE)

  figures_dir <- file.path("www", "figures")
  if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
  filename <- paste0(output_id, ".png")

  tryCatch({
    if (is_ba) {
      generate_ba_2Dmap_hot(country_name = aoi, map_year = map_year,
                            map_month = month, figures_dir = figures_dir,
                            figure_filename = filename, years_vec = years_vec)
    } else {
      generate_2Dmap_hot(country_name = aoi, sensor = sensor, resolution = res,
                         map_year = map_year, map_month = month,
                         figures_dir = figures_dir, figure_filename = filename,
                         years_vec = years_vec)
    }
    list(src = file.path(figures_dir, filename), contentType = "image/png",
         width = "100%", height = "auto", alt = chart$title %||% "Comparison map")
  }, error = function(e) {
    list(src = "", alt = paste("Image generation failed:", conditionMessage(e)))
  })
}
