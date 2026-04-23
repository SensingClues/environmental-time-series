## Scenario Analysis helpers
##
## Shared NDVI-by-class loader plus seven scenario plotting functions.
## All plot_xxx() functions accept a pre-loaded `df` (output of .load_all_years)
## so raster I/O only happens once per country/resolution change in the server.

load_ndvi_per_class <- function(year, region, resolution, data_dir, lulc_path) {
  # Loads monthly NDVI rasters for a year and returns mean NDVI per land cover class per month.
  #
  # Returns data.frame with columns: year, month, land_cover, mean_ndvi

  year <- as.integer(year)
  if (is.na(year)) stop("load_ndvi_per_class: `year` must be coercible to integer.")
  if (!is.character(region) || length(region) != 1L || !nzchar(region)) {
    stop("load_ndvi_per_class: `region` must be a non-empty string.")
  }
  if (!dir.exists(data_dir)) stop("load_ndvi_per_class: `data_dir` does not exist: ", data_dir)
  if (!dir.exists(lulc_path)) stop("load_ndvi_per_class: `lulc_path` does not exist: ", lulc_path)

  land_cover_classes <- c(
    "Crops", "Rangeland", "Water", "Trees",
    "Flooded_vegetation", "Built_Area", "Bare_ground"
  )

  ndvi_dir <- file.path(data_dir, "NDVI", region, paste0(resolution, "m_resolution"))
  if (!dir.exists(ndvi_dir)) {
    stop("load_ndvi_per_class: NDVI directory not found: ", ndvi_dir)
  }

  ndvi_pattern <- paste0(
    "^", year, "-(0[1-9]|1[0-2])_NDVI_",
    gsub("([\\\\.^$|()\\[\\]{}*+?])", "\\\\\\1", region, perl = TRUE),
    "\\.tif$"
  )
  ndvi_files <- list.files(ndvi_dir, pattern = ndvi_pattern, full.names = TRUE)
  if (length(ndvi_files) == 0L) {
    stop("load_ndvi_per_class: no NDVI GeoTIFFs found for year=", year, " region=", region, " in ", ndvi_dir)
  }

  lc_geojson_by_class <- list()
  for (lc in land_cover_classes) {
    specific_pat <- paste0(region, "_", lc, "_", year, "\\.geojson$")
    candidates <- list.files(lulc_path, pattern = specific_pat, recursive = TRUE, full.names = TRUE)
    if (length(candidates) == 0L) {
      fallback_pat <- paste0(region, "_", lc, "_.*\\.geojson$")
      candidates <- list.files(lulc_path, pattern = fallback_pat, recursive = TRUE, full.names = TRUE)
    }
    if (length(candidates) == 0L) {
      stop("load_ndvi_per_class: no land cover GeoJSON found for class=", lc, " region=", region, " under ", lulc_path)
    }
    lc_geojson_by_class[[lc]] <- candidates[[1]]
  }

  vectors_by_class <- list()
  for (lc in land_cover_classes) {
    v_sf <- sf::st_read(lc_geojson_by_class[[lc]], quiet = TRUE)
    v_sf <- sf::st_transform(v_sf, crs = 4326)
    vectors_by_class[[lc]] <- terra::vect(v_sf)
  }

  file_month <- function(p) {
    bn <- basename(p)
    m <- regexec("^(\\d{4})-(\\d{2})_", bn)
    mm <- regmatches(bn, m)[[1]]
    if (length(mm) >= 3L) as.integer(mm[[3]]) else NA_integer_
  }

  out_year <- integer(0)
  out_month <- integer(0)
  out_land_cover <- character(0)
  out_mean <- numeric(0)

  for (fp in sort(ndvi_files)) {
    mo <- file_month(fp)
    if (is.na(mo)) next

    r <- terra::rast(fp)

    for (lc in land_cover_classes) {
      v <- vectors_by_class[[lc]]
      if (!is.na(terra::crs(r)) && !is.na(terra::crs(v)) && terra::crs(r) != terra::crs(v)) {
        v <- terra::project(v, terra::crs(r))
      }

      masked <- terra::mask(r, v)
      mval <- terra::global(masked, fun = "mean", na.rm = TRUE)[1, 1]

      out_year <- c(out_year, year)
      out_month <- c(out_month, mo)
      out_land_cover <- c(out_land_cover, lc)
      out_mean <- c(out_mean, as.numeric(mval))
    }
  }

  data.frame(
    year = out_year,
    month = out_month,
    land_cover = out_land_cover,
    mean_ndvi = out_mean,
    stringsAsFactors = FALSE
  )
}

# --- Internal helpers ---

.scenario_lulc_dir <- function(data_dir, country_name, land_use_src, lulc_path) {
  d <- file.path(data_dir, "LandUse", country_name, land_use_src)
  if (dir.exists(d)) d else lulc_path
}

.scenario_avail_years <- function(data_dir, country_name, resolution) {
  ndvi_dir <- file.path(data_dir, "NDVI", country_name, paste0(resolution, "m_resolution"))
  if (!dir.exists(ndvi_dir)) stop("NDVI directory not found: ", ndvi_dir)
  files <- list.files(ndvi_dir, pattern = "_NDVI_.*\\.tif$", full.names = FALSE)
  years <- suppressWarnings(as.integer(sub("^(\\d{4})-.*$", "\\1", files)))
  sort(unique(years[!is.na(years) & years >= 2019 & years <= 2025]))
}

.load_all_years <- function(years, country_name, resolution, data_dir, lulc_dir,
                             classes = NULL) {
  dfs <- lapply(years, function(y) {
    tryCatch(
      load_ndvi_per_class(y, country_name, resolution, data_dir, lulc_dir),
      error = function(e) { message("Skipping year ", y, ": ", e$message); NULL }
    )
  })
  df <- do.call(rbind, Filter(Negate(is.null), dfs))
  if (!is.null(classes) && !is.null(df)) df <- df[df$land_cover %in% classes, , drop = FALSE]
  df
}

# --- Sub-tab 1: Drought Impact ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

plot_drought_impact <- function(df, comparison_year, reference_years = NULL) {
  if (is.null(df) || nrow(df) == 0) stop("plot_drought_impact: df is empty.")

  comparison_year <- as.integer(comparison_year)
  avail_years     <- sort(unique(df$year))

  if (is.null(reference_years) || length(reference_years) == 0) reference_years <- avail_years
  reference_years <- as.integer(reference_years)

  comp_df <- df[df$year == comparison_year, ]
  if (nrow(comp_df) == 0) stop("plot_drought_impact: no data for comparison year ", comparison_year)

  ref_df <- df[df$year %in% reference_years, ]
  if (nrow(ref_df) == 0) stop("plot_drought_impact: no reference data for years ", paste(reference_years, collapse = ", "))

  hist_stats <- dplyr::summarise(
    dplyr::group_by(ref_df, land_cover, month),
    hist_mean = mean(mean_ndvi, na.rm = TRUE),
    hist_sd   = stats::sd(mean_ndvi, na.rm = TRUE),
    .groups = "drop"
  )
  hist_stats$hist_sd[is.na(hist_stats$hist_sd)] <- 0

  merged <- dplyr::left_join(comp_df, hist_stats, by = c("land_cover", "month"))
  merged <- dplyr::mutate(merged, deficit = mean_ndvi - hist_mean)
  merged <- dplyr::arrange(merged, land_cover, month)

  worst <- merged[which.min(merged$deficit), ]
  pct_below <- if (!is.na(worst$hist_mean) && worst$hist_mean != 0) {
    round(abs(worst$deficit / worst$hist_mean * 100), 1)
  } else NA
  summary_text <- sprintf(
    "In %s, %s NDVI was %.1f%% below the historical average in %s",
    comparison_year, worst$land_cover, pct_below, month.name[worst$month]
  )

  lc_classes <- unique(merged$land_cover)
  p_main <- plotly::plot_ly()

  for (lc in lc_classes) {
    df_lc <- merged[merged$land_cover == lc, ]

    p_main <- plotly::add_ribbons(
      p_main,
      x = df_lc$month,
      ymin = df_lc$hist_mean - df_lc$hist_sd,
      ymax = df_lc$hist_mean + df_lc$hist_sd,
      name = paste0(lc, " ±1 SD"),
      legendgroup = lc,
      showlegend = FALSE,
      hoverinfo = "none",
      opacity = 0.15
    )

    p_main <- plotly::add_lines(
      p_main,
      x = df_lc$month,
      y = df_lc$mean_ndvi,
      name = lc,
      legendgroup = lc,
      hovertemplate = paste0(
        "<b>", lc, "</b><br>Month: %{x}<br>",
        comparison_year, " NDVI: %{y:.3f}<extra></extra>"
      )
    )
  }

  p_main <- plotly::layout(
    p_main,
    title = list(text = summary_text, font = list(size = 12)),
    xaxis = list(title = "Month", tickmode = "array", tickvals = 1:12, ticktext = month.abb),
    yaxis = list(title = "NDVI"),
    legend = list(orientation = "h")
  )

  # Anomaly bar chart: annual mean deficit per class
  class_deficit <- dplyr::summarise(
    dplyr::group_by(merged, land_cover),
    mean_deficit = mean(deficit, na.rm = TRUE),
    .groups = "drop"
  )
  class_deficit <- dplyr::arrange(class_deficit, mean_deficit)
  bar_colors <- ifelse(class_deficit$mean_deficit >= 0, "#43A047", "#E53935")

  p_bar <- plotly::plot_ly(
    data = class_deficit,
    x = ~land_cover,
    y = ~mean_deficit,
    type = "bar",
    marker = list(color = bar_colors),
    hovertemplate = "<b>%{x}</b><br>Mean deficit: %{y:.3f}<extra></extra>"
  )
  p_bar <- plotly::layout(
    p_bar,
    xaxis = list(title = "Land Cover Class"),
    yaxis = list(title = "NDVI Deficit (actual − historical mean)")
  )

  plotly::subplot(p_main, p_bar,
                  nrows = 2, shareX = FALSE, titleY = TRUE,
                  heights = c(0.65, 0.35))
}

# --- Sub-tab 2: Seasonal Vegetation Cycle ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

plot_seasonal_cycle <- function(df, classes = NULL) {
  if (is.null(df) || nrow(df) == 0) stop("plot_seasonal_cycle: df is empty.")

  all_df <- df
  if (!is.null(classes)) all_df <- all_df[all_df$land_cover %in% classes, , drop = FALSE]
  if (nrow(all_df) == 0L) stop("plot_seasonal_cycle: no data after filtering.")

  summary_df <- dplyr::summarise(
    dplyr::group_by(all_df, land_cover, month),
    mean_ndvi = mean(mean_ndvi, na.rm = TRUE),
    sd_ndvi   = stats::sd(mean_ndvi, na.rm = TRUE),
    n_years   = dplyr::n_distinct(year),
    .groups   = "drop"
  )
  summary_df <- dplyr::mutate(
    summary_df,
    se      = sd_ndvi / sqrt(pmax(n_years, 1)),
    ci_low  = mean_ndvi - 1.96 * se,
    ci_high = mean_ndvi + 1.96 * se
  )
  summary_df <- dplyr::arrange(summary_df, land_cover, month)

  p <- plotly::plot_ly()
  for (lc in unique(summary_df$land_cover)) {
    df_lc <- summary_df[summary_df$land_cover == lc, ]

    p <- plotly::add_ribbons(
      p,
      x = df_lc$month, ymin = df_lc$ci_low, ymax = df_lc$ci_high,
      name = paste0(lc, " 95% CI"), legendgroup = lc,
      showlegend = FALSE, hoverinfo = "none", opacity = 0.15
    )

    p <- plotly::add_lines(
      p,
      x = df_lc$month, y = df_lc$mean_ndvi,
      name = lc, legendgroup = lc,
      hovertemplate = paste0(
        "<b>", lc, "</b><br>Month: %{x}<br>Mean NDVI: %{y:.3f}<br>",
        "95% CI: [%{customdata[0]:.3f}, %{customdata[1]:.3f}]<extra></extra>"
      ),
      customdata = cbind(df_lc$ci_low, df_lc$ci_high)
    )
  }

  plotly::layout(
    p,
    xaxis  = list(title = "Month", tickmode = "array", tickvals = 1:12, ticktext = month.name),
    yaxis  = list(title = "NDVI"),
    legend = list(orientation = "h")
  )
}

# --- Sub-tab 3: Land Cover Productivity ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

plot_productivity_comparison <- function(df, selected_year) {
  if (is.null(df) || nrow(df) == 0) stop("plot_productivity_comparison: df is empty.")

  df <- df[df$year == as.integer(selected_year), ]
  if (nrow(df) == 0) stop("plot_productivity_comparison: no data for year ", selected_year)

  stats_df <- dplyr::summarise(
    dplyr::group_by(df, land_cover),
    annual_mean = mean(mean_ndvi, na.rm = TRUE),
    min_ndvi    = min(mean_ndvi, na.rm = TRUE),
    max_ndvi    = max(mean_ndvi, na.rm = TRUE),
    ndvi_range  = max(mean_ndvi, na.rm = TRUE) - min(mean_ndvi, na.rm = TRUE),
    .groups = "drop"
  )
  stats_df <- dplyr::arrange(stats_df, dplyr::desc(annual_mean))

  p_bar <- plotly::plot_ly(
    data = stats_df,
    x    = ~reorder(land_cover, -annual_mean),
    y    = ~annual_mean,
    type = "bar",
    marker = list(color = "#1976D2"),
    hovertemplate = "<b>%{x}</b><br>Annual Mean NDVI: %{y:.3f}<extra></extra>"
  )
  p_bar <- plotly::layout(
    p_bar,
    xaxis = list(title = "Land Cover Class"),
    yaxis = list(title = "Annual Mean NDVI")
  )

  p_scatter <- plotly::plot_ly(
    data         = stats_df,
    x            = ~annual_mean,
    y            = ~ndvi_range,
    type         = "scatter",
    mode         = "markers+text",
    text         = ~land_cover,
    textposition = "top center",
    marker       = list(size = 12, color = "#E64A19"),
    hovertemplate = "<b>%{text}</b><br>Productivity: %{x:.3f}<br>Variability: %{y:.3f}<extra></extra>"
  )
  p_scatter <- plotly::layout(
    p_scatter,
    xaxis = list(title = "Productivity (Annual Mean NDVI)"),
    yaxis = list(title = "Variability (NDVI Range: max − min)")
  )

  table_out <- stats_df
  table_out$annual_mean <- round(table_out$annual_mean, 3)
  table_out$min_ndvi    <- round(table_out$min_ndvi, 3)
  table_out$max_ndvi    <- round(table_out$max_ndvi, 3)
  table_out$ndvi_range  <- round(table_out$ndvi_range, 3)
  colnames(table_out) <- c("Land Cover", "Annual Mean", "Min NDVI", "Max NDVI", "Range")

  list(bar = p_bar, scatter = p_scatter, table = table_out)
}

# --- Sub-tab 4: Agricultural Monitoring ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

plot_agricultural_monitoring <- function(df) {
  if (is.null(df) || nrow(df) == 0) stop("plot_agricultural_monitoring: df is empty.")

  all_df <- df[df$land_cover == "Crops", ]
  if (nrow(all_df) == 0) stop("plot_agricultural_monitoring: no Crops data found.")

  detect_phenology <- function(df_year) {
    df_year  <- dplyr::arrange(df_year, month)
    ndvi     <- df_year$mean_ndvi
    months   <- df_year$month

    peak_idx <- which.max(ndvi)
    peak_month <- months[peak_idx]

    green_up_month <- NA_integer_
    for (i in seq_len(length(ndvi) - 1)) {
      if ((ndvi[i + 1] - ndvi[i]) > 0.05 && ndvi[i + 1] > 0.3) {
        green_up_month <- months[i + 1]
        break
      }
    }

    senescence_month <- NA_integer_
    if (!is.na(peak_idx) && peak_idx < length(ndvi)) {
      for (i in seq(peak_idx + 1, length(ndvi))) {
        if (ndvi[i] < 0.3) { senescence_month <- months[i]; break }
      }
    }

    data.frame(
      year = df_year$year[1], green_up = green_up_month,
      peak = peak_month, senescence = senescence_month,
      peak_ndvi = round(ndvi[peak_idx], 3),
      stringsAsFactors = FALSE
    )
  }

  pheno_df <- do.call(rbind, lapply(split(all_df, all_df$year), detect_phenology))
  pheno_df <- dplyr::arrange(pheno_df, year)

  years_available <- sort(unique(all_df$year))
  p <- plotly::plot_ly()

  for (yr in years_available) {
    df_yr <- dplyr::arrange(all_df[all_df$year == yr, ], month)

    p <- plotly::add_lines(
      p,
      x    = df_yr$month,
      y    = df_yr$mean_ndvi,
      name = as.character(yr),
      legendgroup = as.character(yr),
      hovertemplate = paste0(
        "<b>", yr, "</b><br>Month: %{x}<br>Crops NDVI: %{y:.3f}<extra></extra>"
      )
    )

    ph <- pheno_df[pheno_df$year == yr, ]
    marker_list <- list(
      list(month = ph$green_up,   label = "Green-up",   color = "#43A047", symbol = "triangle-up"),
      list(month = ph$peak,       label = "Peak",       color = "#1565C0", symbol = "star"),
      list(month = ph$senescence, label = "Senescence", color = "#E65100", symbol = "triangle-down")
    )

    for (mk in marker_list) {
      if (!is.na(mk$month)) {
        ndvi_val <- df_yr$mean_ndvi[df_yr$month == mk$month]
        if (length(ndvi_val) > 0) {
          p <- plotly::add_markers(
            p, x = mk$month, y = ndvi_val[1],
            name = mk$label, legendgroup = mk$label,
            showlegend = (yr == years_available[1]),
            marker = list(size = 9, color = mk$color, symbol = mk$symbol),
            hovertemplate = paste0(
              "<b>", yr, " ", mk$label, "</b><br>Month: %{x}<br>NDVI: %{y:.3f}<extra></extra>"
            )
          )
        }
      }
    }
  }

  p <- plotly::layout(
    p,
    xaxis  = list(title = "Month", tickmode = "array", tickvals = 1:12, ticktext = month.abb),
    yaxis  = list(title = "Crops NDVI"),
    legend = list(orientation = "h")
  )

  avg_green_up <- round(mean(pheno_df$green_up, na.rm = TRUE))
  table_out <- pheno_df
  table_out$green_up    <- ifelse(is.na(table_out$green_up), "—", month.abb[table_out$green_up])
  table_out$peak        <- ifelse(is.na(table_out$peak), "—", month.abb[table_out$peak])
  table_out$senescence  <- ifelse(is.na(table_out$senescence), "—", month.abb[table_out$senescence])
  colnames(table_out) <- c("Year", "Green-up", "Peak", "Senescence", "Peak NDVI")

  list(plot = p, phenology_table = table_out, avg_green_up = avg_green_up)
}

# --- Sub-tab 5: Vegetation Trend ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

plot_vegetation_trend <- function(df) {
  if (is.null(df) || nrow(df) == 0) stop("plot_vegetation_trend: df is empty.")

  all_df      <- df
  avail_years <- sort(unique(all_df$year))

  annual_df <- dplyr::summarise(
    dplyr::group_by(all_df, land_cover, year),
    annual_mean = mean(mean_ndvi, na.rm = TRUE),
    .groups = "drop"
  )

  lc_classes <- unique(annual_df$land_cover)

  trend_results <- lapply(lc_classes, function(lc) {
    monthly_lc <- dplyr::arrange(all_df[all_df$land_cover == lc, ], year, month)
    smk_res <- tryCatch({
      start_yr <- min(monthly_lc$year)
      ts_data  <- stats::ts(monthly_lc$mean_ndvi, start = c(start_yr, 1), frequency = 12)
      smk      <- trend::smk.test(ts_data)
      sen      <- trend::sens.slope(ts_data)
      list(p_value = smk$p.value, slope = as.numeric(sen$estimates))
    }, error = function(e) list(p_value = NA_real_, slope = NA_real_))

    sig <- !is.na(smk_res$p_value) && smk_res$p_value < 0.05
    trend_dir <- if (sig) {
      if (!is.na(smk_res$slope) && smk_res$slope > 0) "↑ Increasing" else "↓ Decreasing"
    } else "→ Stable"

    data.frame(
      land_cover      = lc,
      trend_direction = trend_dir,
      slope           = round(smk_res$slope, 5),
      p_value         = round(smk_res$p_value, 4),
      stringsAsFactors = FALSE
    )
  })

  trend_df <- do.call(rbind, trend_results)
  trend_df_display <- trend_df
  colnames(trend_df_display) <- c("Land Cover", "Trend", "Slope (NDVI/yr)", "p-value")

  p <- plotly::plot_ly()
  for (lc in lc_classes) {
    df_lc <- dplyr::arrange(annual_df[annual_df$land_cover == lc, ], year)

    p <- plotly::add_lines(
      p, x = df_lc$year, y = df_lc$annual_mean,
      name = lc, legendgroup = lc,
      hovertemplate = paste0(
        "<b>", lc, "</b><br>Year: %{x}<br>Annual Mean NDVI: %{y:.3f}<extra></extra>"
      )
    )

    if (nrow(df_lc) >= 2) {
      lm_fit  <- lm(annual_mean ~ year, data = df_lc)
      trend_y <- predict(lm_fit, newdata = df_lc)
      p <- plotly::add_lines(
        p, x = df_lc$year, y = trend_y,
        name = paste0(lc, " trend"), legendgroup = lc,
        showlegend = FALSE, line = list(dash = "dot"), hoverinfo = "none"
      )
    }
  }

  p <- plotly::layout(
    p,
    xaxis  = list(title = "Year", tickmode = "array", tickvals = avail_years, tickformat = "d"),
    yaxis  = list(title = "Annual Mean NDVI"),
    legend = list(orientation = "h")
  )

  list(plot = p, trend_table = trend_df_display)
}

# --- Sub-tab 6: Rainy Season Onset ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

plot_rainy_season_onset <- function(df, selected_class = "Crops") {
  if (is.null(df) || nrow(df) == 0) stop("plot_rainy_season_onset: df is empty.")

  all_df <- df[df$land_cover == selected_class, ]
  if (nrow(all_df) == 0) {
    stop("plot_rainy_season_onset: no data for class ", selected_class)
  }

  detect_onset <- function(df_year) {
    df_year  <- dplyr::arrange(df_year, month)
    ndvi     <- df_year$mean_ndvi
    months   <- df_year$month

    dry_idx <- which(months %in% 6:8)
    dry_min <- if (length(dry_idx) > 0) min(ndvi[dry_idx], na.rm = TRUE) else min(ndvi, na.rm = TRUE)
    threshold <- dry_min + 0.05

    onset_month <- NA_integer_
    for (i in seq_len(length(ndvi) - 1)) {
      if (ndvi[i] > threshold && ndvi[i + 1] > threshold) {
        onset_month <- months[i]
        break
      }
    }
    data.frame(year = df_year$year[1], onset_month = onset_month, stringsAsFactors = FALSE)
  }

  onset_df <- do.call(rbind, lapply(split(all_df, all_df$year), detect_onset))
  onset_df <- dplyr::arrange(onset_df, year)

  years_available <- sort(unique(all_df$year))
  p_lines <- plotly::plot_ly()

  for (yr in years_available) {
    df_yr    <- dplyr::arrange(all_df[all_df$year == yr, ], month)
    onset_mo <- onset_df$onset_month[onset_df$year == yr]

    p_lines <- plotly::add_lines(
      p_lines, x = df_yr$month, y = df_yr$mean_ndvi,
      name = as.character(yr),
      hovertemplate = paste0(
        "<b>", yr, "</b><br>Month: %{x}<br>NDVI: %{y:.3f}<extra></extra>"
      )
    )

    if (length(onset_mo) > 0 && !is.na(onset_mo)) {
      ndvi_at <- df_yr$mean_ndvi[df_yr$month == onset_mo]
      if (length(ndvi_at) > 0) {
        p_lines <- plotly::add_markers(
          p_lines, x = onset_mo, y = ndvi_at[1],
          showlegend = FALSE,
          marker = list(size = 10, color = "#1565C0", symbol = "diamond"),
          hovertemplate = paste0("<b>", yr, " Onset: ", month.abb[onset_mo], "</b><extra></extra>")
        )
      }
    }
  }

  p_lines <- plotly::layout(
    p_lines,
    xaxis  = list(title = "Month", tickmode = "array", tickvals = 1:12, ticktext = month.abb),
    yaxis  = list(title = paste(selected_class, "NDVI")),
    legend = list(orientation = "h")
  )

  onset_valid <- onset_df[!is.na(onset_df$onset_month), ]

  p_bar <- plotly::plot_ly(
    data = onset_valid,
    x    = ~year, y = ~onset_month,
    type = "bar",
    marker = list(color = "#1565C0"),
    hovertemplate = "<b>%{x}</b><br>Onset Month: %{y}<extra></extra>"
  )

  trend_text <- ""
  if (nrow(onset_valid) >= 3) {
    lm_fit         <- lm(onset_month ~ year, data = onset_valid)
    slope_per_yr   <- coef(lm_fit)[["year"]]
    n_years_span   <- max(onset_valid$year) - min(onset_valid$year)
    total_shift_wk <- slope_per_yr * n_years_span * 4.33
    direction      <- if (slope_per_yr > 0) "later" else "earlier"
    trend_text     <- sprintf(
      "Onset shifted %.1f weeks %s since %d",
      abs(total_shift_wk), direction, min(onset_valid$year)
    )
  }

  p_bar <- plotly::layout(
    p_bar,
    xaxis = list(title = "Year", tickmode = "array", tickvals = onset_valid$year, tickformat = "d"),
    yaxis = list(title = "Onset Month", tickmode = "array",
                 tickvals = 1:12, ticktext = month.abb),
    title = list(text = trend_text, font = list(size = 12))
  )

  table_out <- onset_df
  table_out$onset_month <- ifelse(
    is.na(table_out$onset_month), "—", month.name[table_out$onset_month]
  )
  colnames(table_out) <- c("Year", "Onset Month")

  list(lines = p_lines, bar = p_bar, onset_table = table_out)
}

# --- Sub-tab 7: Anomaly Resilience ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

plot_anomaly_resilience <- function(df, anomaly_year) {
  if (is.null(df) || nrow(df) == 0) stop("plot_anomaly_resilience: df is empty.")

  all_df <- df

  hist_stats <- dplyr::summarise(
    dplyr::group_by(all_df, land_cover, month),
    hist_mean = mean(mean_ndvi, na.rm = TRUE),
    hist_sd   = stats::sd(mean_ndvi, na.rm = TRUE),
    .groups   = "drop"
  )
  hist_stats$hist_sd[is.na(hist_stats$hist_sd)] <- 0

  anomaly_df <- all_df[all_df$year == as.integer(anomaly_year), ]
  if (nrow(anomaly_df) == 0) stop("plot_anomaly_resilience: no data for anomaly year ", anomaly_year)

  merged <- dplyr::left_join(anomaly_df, hist_stats, by = c("land_cover", "month"))
  merged <- dplyr::mutate(merged, anomaly = mean_ndvi - hist_mean)

  lc_classes <- unique(merged$land_cover)

  recovery_results <- lapply(lc_classes, function(lc) {
    df_lc <- dplyr::arrange(merged[merged$land_cover == lc, ], month)

    worst_row    <- df_lc[which.min(df_lc$anomaly), ]
    max_deficit  <- worst_row$anomaly
    deficit_month <- worst_row$month

    subsequent <- df_lc[df_lc$month > deficit_month, ]
    recovery_months <- NA_integer_
    for (i in seq_len(nrow(subsequent))) {
      row_i <- subsequent[i, ]
      if (abs(row_i$mean_ndvi - row_i$hist_mean) <= row_i$hist_sd) {
        recovery_months <- as.integer(row_i$month - deficit_month)
        break
      }
    }

    data.frame(
      land_cover    = lc,
      max_deficit   = round(max_deficit, 3),
      deficit_month = month.abb[deficit_month],
      recovery_months = recovery_months,
      stringsAsFactors = FALSE
    )
  })

  recovery_df <- do.call(rbind, recovery_results)

  # Heatmap of anomaly per class × month
  hm_data <- tidyr::pivot_wider(
    merged[, c("land_cover", "month", "anomaly")],
    names_from = "month", values_from = "anomaly"
  )
  lc_names <- hm_data$land_cover
  hm_matrix <- as.matrix(hm_data[, -1])
  rownames(hm_matrix) <- lc_names
  col_months <- as.integer(colnames(hm_matrix))

  p_heatmap <- plotly::plot_ly(
    x = col_months, y = lc_names, z = hm_matrix,
    type = "heatmap",
    colorscale = list(c(0, "#E53935"), c(0.5, "#FFFFFF"), c(1, "#43A047")),
    zmid = 0,
    hovertemplate = "<b>%{y}</b><br>Month: %{x}<br>Anomaly: %{z:.3f}<extra></extra>"
  )
  p_heatmap <- plotly::layout(
    p_heatmap,
    xaxis = list(title = "Month", tickmode = "array",
                 tickvals = col_months, ticktext = month.abb[col_months]),
    yaxis = list(title = "")
  )

  recovery_valid <- recovery_df[!is.na(recovery_df$recovery_months), ]
  p_recovery <- plotly::plot_ly(
    data = recovery_valid,
    x    = ~land_cover,
    y    = ~recovery_months,
    type = "bar",
    marker = list(color = "#7B1FA2"),
    hovertemplate = "<b>%{x}</b><br>Recovery: %{y} months<extra></extra>"
  )
  p_recovery <- plotly::layout(
    p_recovery,
    xaxis = list(title = "Land Cover Class"),
    yaxis = list(title = "Months to Recovery")
  )

  table_out <- recovery_df
  table_out$recovery_months <- ifelse(is.na(table_out$recovery_months), "—",
                                      as.character(table_out$recovery_months))
  colnames(table_out) <- c("Land Cover", "Max Deficit", "Deficit Month", "Recovery (months)")

  list(heatmap = p_heatmap, recovery = p_recovery, summary_table = table_out)
}
