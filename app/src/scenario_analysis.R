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
  sort(unique(years[!is.na(years)]))
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

  # Summary annotation: exclude Flooded_vegetation from worst-class ranking
  merged_no_flood <- merged[merged$land_cover != "Flooded_vegetation", ]
  worst_source    <- if (nrow(merged_no_flood) > 0) merged_no_flood else merged
  worst           <- worst_source[which.min(worst_source$deficit), ]
  pct_below <- if (!is.na(worst$hist_mean) && worst$hist_mean != 0) {
    round(abs(worst$deficit / worst$hist_mean * 100), 1)
  } else NA
  summary_text <- sprintf(
    "In %s, %s NDVI was %.1f%% below the historical average in %s",
    comparison_year, worst$land_cover, pct_below, month.name[worst$month]
  )

  # Drought Severity Score: mean deficit of Rangeland, Crops, Trees only
  key_classes    <- c("Rangeland", "Crops", "Trees")
  key_df         <- merged[merged$land_cover %in% key_classes, ]
  severity_score <- if (nrow(key_df) > 0) mean(key_df$deficit, na.rm = TRUE) else NA_real_
  severity_label <- if (is.na(severity_score)) {
    "Unknown"
  } else if (severity_score > -0.02) {
    "Normal"
  } else if (severity_score > -0.05) {
    "Mild stress"
  } else if (severity_score > -0.10) {
    "Moderate drought"
  } else {
    "Severe drought"
  }

  # Multi-class agreement: max number of non-flood classes simultaneously below average
  non_flood_classes   <- unique(merged$land_cover[merged$land_cover != "Flooded_vegetation"])
  months_available    <- sort(unique(merged$month))
  agreement_by_month  <- sapply(months_available, function(mo) {
    m_data <- merged[merged$month == mo & merged$land_cover %in% non_flood_classes, ]
    sum(m_data$deficit < 0, na.rm = TRUE)
  })
  max_agreement        <- max(agreement_by_month, na.rm = TRUE)
  worst_agreement_month <- months_available[which.max(agreement_by_month)]

  # Main line chart
  lc_classes <- unique(merged$land_cover)
  p_main     <- plotly::plot_ly()

  for (lc in lc_classes) {
    df_lc        <- merged[merged$land_cover == lc, ]
    display_name <- if (lc == "Flooded_vegetation") "Flooded_vegetation ⚠" else lc

    p_main <- plotly::add_ribbons(
      p_main,
      x = df_lc$month,
      ymin = df_lc$hist_mean - df_lc$hist_sd,
      ymax = df_lc$hist_mean + df_lc$hist_sd,
      name = paste0(display_name, " ±1 SD"),
      legendgroup = lc,
      showlegend = FALSE,
      hoverinfo = "none",
      opacity = 0.15
    )

    p_main <- plotly::add_lines(
      p_main,
      x = df_lc$month,
      y = df_lc$mean_ndvi,
      name = display_name,
      legendgroup = lc,
      hovertemplate = paste0(
        "<b>", display_name, "</b><br>Month: %{x}<br>",
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

  # Bar chart: cumulative (total) NDVI deficit per class across all months
  class_deficit <- dplyr::summarise(
    dplyr::group_by(merged, land_cover),
    total_deficit = sum(deficit, na.rm = TRUE),
    .groups = "drop"
  )
  class_deficit <- dplyr::arrange(class_deficit, total_deficit)
  bar_colors    <- ifelse(class_deficit$total_deficit >= 0, "#43A047", "#E53935")

  p_bar <- plotly::plot_ly(
    data = class_deficit,
    x = ~land_cover,
    y = ~total_deficit,
    type = "bar",
    marker = list(color = bar_colors),
    hovertemplate = "<b>%{x}</b><br>Cumulative deficit: %{y:.3f}<extra></extra>"
  )
  p_bar <- plotly::layout(
    p_bar,
    xaxis = list(title = "Land Cover Class"),
    yaxis = list(title = "Cumulative NDVI Deficit (full year)")
  )

  p_combined <- plotly::subplot(p_main, p_bar,
                                nrows = 2, shareX = FALSE, titleY = TRUE,
                                heights = c(0.65, 0.35))

  list(
    plot                 = p_combined,
    severity_score       = round(severity_score, 4),
    severity_label       = severity_label,
    multi_class_agreement = max_agreement,
    worst_agreement_month = worst_agreement_month,
    n_non_flood_classes  = length(non_flood_classes),
    summary_text         = summary_text
  )
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

  # Compute avg CI width per class for insight card
  ci_width_df <- dplyr::summarise(
    dplyr::group_by(summary_df, land_cover),
    avg_ci_width = mean(ci_high - ci_low, na.rm = TRUE),
    .groups = "drop"
  )
  most_variable <- ci_width_df$land_cover[which.max(ci_width_df$avg_ci_width)]
  most_stable   <- ci_width_df$land_cover[which.min(ci_width_df$avg_ci_width)]

  p <- plotly::plot_ly()
  for (lc in unique(summary_df$land_cover)) {
    df_lc     <- summary_df[summary_df$land_cover == lc, ]
    lc_color  <- if (lc %in% names(.lc_colors)) .lc_colors[[lc]] else "#888888"
    disp_name <- .format_lc(lc)
    rgb_v     <- grDevices::col2rgb(lc_color)
    ci_fill   <- sprintf("rgba(%d,%d,%d,0.15)", rgb_v[1], rgb_v[2], rgb_v[3])

    # CI ribbon: lower bound first, then upper fills down to it
    p <- plotly::add_trace(
      p,
      x = df_lc$month, y = df_lc$ci_low, type = "scatter", mode = "lines",
      name = paste0(disp_name, " CI"), legendgroup = lc,
      showlegend = FALSE, hoverinfo = "none",
      line = list(color = "transparent", width = 0)
    )
    p <- plotly::add_trace(
      p,
      x = df_lc$month, y = df_lc$ci_high, type = "scatter", mode = "lines",
      name = paste0(disp_name, " CI"), legendgroup = lc,
      showlegend = FALSE, hoverinfo = "none",
      fill = "tonexty", fillcolor = ci_fill,
      line = list(color = "transparent", width = 0)
    )

    p <- plotly::add_lines(
      p,
      x = df_lc$month, y = df_lc$mean_ndvi,
      name = disp_name, legendgroup = lc,
      line = list(color = lc_color),
      hovertemplate = paste0(
        "<b>", disp_name, "</b><br>Month: %{x}<br>Mean NDVI: %{y:.3f}<br>",
        "95% CI: [%{customdata[0]:.3f}, %{customdata[1]:.3f}]<extra></extra>"
      ),
      customdata = cbind(df_lc$ci_low, df_lc$ci_high)
    )
  }

  p <- plotly::layout(
    p,
    xaxis  = list(title = "Month", tickmode = "array", tickvals = 1:12, ticktext = month.name),
    yaxis  = list(title = "NDVI"),
    legend = list(orientation = "h")
  )

  flood_note <- if (length(most_variable) == 1L && most_variable == "Flooded_vegetation")
    " — driven by varying flood levels rather than vegetation health" else ""
  insight_text <- if (length(most_variable) == 1L && length(most_stable) == 1L) {
    sprintf(
      "%s shows the most year-to-year variability (widest confidence interval)%s. %s shows the most consistent seasonal pattern (narrowest confidence interval).",
      .format_lc(most_variable), flood_note, .format_lc(most_stable)
    )
  } else "Seasonal pattern insight not available."

  list(plot = p, insight_text = insight_text)
}

# --- Sub-tab 3: Land Cover Productivity ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

.lc_colors <- c(
  Trees              = "#27500A",
  Rangeland          = "#1D9E75",
  Crops              = "#EF9F27",
  Flooded_vegetation = "#378ADD",
  Bare_ground        = "#888780",
  Built_Area         = "#D85A30",
  Water              = "#534AB7"
)

.format_lc <- function(x) gsub("_", " ", x)

.compute_productivity_stats <- function(df, yr) {
  # Annual mean for the selected year
  d_yr <- df[df$year == as.integer(yr), ]
  if (nrow(d_yr) == 0) stop("plot_productivity_comparison: no data for year ", yr)

  yr_stats <- dplyr::summarise(
    dplyr::group_by(d_yr, land_cover),
    annual_mean = mean(mean_ndvi, na.rm = TRUE),
    .groups = "drop"
  )

  # Inter-annual stats: CV / min / max computed from each year's annual mean across ALL years
  annual_by_year <- dplyr::summarise(
    dplyr::group_by(df, land_cover, year),
    yr_mean = mean(mean_ndvi, na.rm = TRUE),
    .groups = "drop"
  )
  inter_stats <- dplyr::summarise(
    dplyr::group_by(annual_by_year, land_cover),
    hist_min      = min(yr_mean, na.rm = TRUE),
    hist_max      = max(yr_mean, na.rm = TRUE),
    hist_sd       = stats::sd(yr_mean, na.rm = TRUE),
    hist_mean_all = mean(yr_mean, na.rm = TRUE),
    .groups = "drop"
  )
  inter_stats <- dplyr::mutate(inter_stats,
    cv = ifelse(hist_mean_all != 0, hist_sd / abs(hist_mean_all), NA_real_)
  )

  dplyr::left_join(
    yr_stats,
    inter_stats[, c("land_cover", "hist_min", "hist_max", "cv")],
    by = "land_cover"
  )
}

.lc_interpretation <- function(land_cover, cv) {
  if (is.na(cv)) return("Insufficient data")
  switch(land_cover,
    Trees              = if (cv < 0.05) "Stable & highly productive"
                         else "Moderately variable but productive",
    Rangeland          = if (cv < 0.05) "Stable & productive"
                         else "Moderately variable — responds to rainfall",
    Crops              = if (cv < 0.08) "Consistent productivity"
                         else "Highly variable — sensitive to growing conditions",
    Flooded_vegetation = "⚠ High variability — flood-driven dynamics, not vegetation stress",
    Bare_ground        = if (cv < 0.05) "Stable but low productivity — potential degradation risk"
                         else "Variable & low productivity — may indicate land cover change",
    Built_Area         = "Low & stable — built surfaces don’t respond to rainfall",
    Water              = "Not a vegetation metric — NDVI near zero",
    "—"
  )
}

plot_productivity_comparison <- function(df, selected_year, compare_year = NULL) {
  if (is.null(df) || nrow(df) == 0) stop("plot_productivity_comparison: df is empty.")

  stats_df <- .compute_productivity_stats(df, selected_year)
  stats_df  <- dplyr::arrange(stats_df, dplyr::desc(annual_mean))

  # Per-class interpretation
  stats_df$interpretation <- mapply(
    .lc_interpretation, stats_df$land_cover, stats_df$cv,
    USE.NAMES = FALSE
  )

  # Dynamic insight card text
  most_productive <- stats_df$land_cover[which.max(stats_df$annual_mean)]
  most_variable   <- stats_df$land_cover[which.max(stats_df$cv)]
  flood_note <- if (!is.na(most_variable) && most_variable == "Flooded_vegetation") {
    " — this reflects flood dynamics rather than vegetation stress"
  } else ""
  insight_text <- sprintf(
    "In %s, %s had the highest annual mean NDVI (%.3f), making it the most productive class. %s showed the greatest year-to-year variability (CV: %.2f)%s.",
    selected_year, .format_lc(most_productive), max(stats_df$annual_mean, na.rm = TRUE),
    .format_lc(most_variable), max(stats_df$cv, na.rm = TRUE), flood_note
  )

  # Bar chart — class-specific colours, grouped if compare year supplied
  bar_colors_main <- unname(.lc_colors[stats_df$land_cover])
  bar_colors_main[is.na(bar_colors_main)] <- "#888888"

  p_bar <- plotly::plot_ly()
  p_bar <- plotly::add_bars(
    p_bar,
    x    = .format_lc(stats_df$land_cover),
    y    = stats_df$annual_mean,
    name = as.character(selected_year),
    marker = list(color = bar_colors_main),
    hovertemplate = paste0(
      "<b>%{x}</b><br>%{y:.3f} NDVI in ", selected_year, ".<br>",
      "Average vegetation health for this land cover type across the selected area.",
      "<extra></extra>"
    )
  )

  comp_stats <- NULL
  if (!is.null(compare_year) && nzchar(compare_year)) {
    comp_stats <- tryCatch(.compute_productivity_stats(df, compare_year), error = function(e) NULL)
    if (!is.null(comp_stats)) {
      comp_stats     <- comp_stats[match(stats_df$land_cover, comp_stats$land_cover), ]
      bar_colors_cmp <- adjustcolor(bar_colors_main, alpha.f = 0.5)
      p_bar <- plotly::add_bars(
        p_bar,
        x    = .format_lc(stats_df$land_cover),
        y    = comp_stats$annual_mean,
        name = as.character(compare_year),
        marker = list(color = bar_colors_cmp),
        hovertemplate = paste0(
          "<b>%{x}</b><br>%{y:.3f} NDVI in ", compare_year, ".<br>",
          "Average vegetation health for this land cover type across the selected area.",
          "<extra></extra>"
        )
      )
    }
  }

  p_bar <- plotly::layout(
    p_bar,
    barmode = "group",
    xaxis   = list(title = "Land Cover Class"),
    yaxis   = list(title = "Annual Mean NDVI", range = c(0, 0.8)),
    legend  = list(orientation = "h")
  )

  # Scatter plot: X = productivity (selected year annual mean), Y = year-to-year CV
  scatter_colors <- unname(.lc_colors[stats_df$land_cover])
  scatter_colors[is.na(scatter_colors)] <- "#888888"

  cv_vals   <- stats_df$cv
  mean_vals <- stats_df$annual_mean
  x_pad <- diff(range(mean_vals, na.rm = TRUE)) * 0.1
  y_pad <- diff(range(cv_vals,   na.rm = TRUE)) * 0.1

  p_scatter <- plotly::plot_ly(
    data         = stats_df,
    x            = ~annual_mean,
    y            = ~cv,
    type         = "scatter",
    mode         = "markers+text",
    text         = ~.format_lc(land_cover),
    textposition = "top center",
    marker       = list(size = 14, color = scatter_colors),
    customdata   = ~interpretation,
    hovertemplate = paste0(
      "<b>%{text}</b><br>Productivity: %{x:.3f}<br>",
      "Year-to-Year Stability (CV): %{y:.3f}<br>%{customdata}<extra></extra>"
    )
  )
  p_scatter <- plotly::layout(
    p_scatter,
    xaxis = list(title = "Productivity (Annual Mean NDVI)",
                 range = c(min(mean_vals, na.rm = TRUE) - x_pad,
                           max(mean_vals, na.rm = TRUE) + x_pad)),
    yaxis = list(title = "Year-to-Year Stability (CV)",
                 range = c(max(0, min(cv_vals, na.rm = TRUE) - y_pad),
                           max(cv_vals, na.rm = TRUE) + y_pad))
  )

  # Summary table
  table_out <- data.frame(
    `Land Cover Class`  = .format_lc(stats_df$land_cover),
    `Annual Mean NDVI`  = round(stats_df$annual_mean, 3),
    `Historical Min`    = round(stats_df$hist_min,    3),
    `Historical Max`    = round(stats_df$hist_max,    3),
    `Year-to-Year CV <span title="SD ÷ mean of each class’s annual NDVI across all available years — lower means more consistent year to year." style="cursor:help;color:#888;font-size:0.85em;">ⓘ</span>` = round(stats_df$cv, 3),
    `Interpretation`    = stats_df$interpretation,
    check.names = FALSE, stringsAsFactors = FALSE
  )

  if (!is.null(comp_stats) && nrow(comp_stats) > 0) {
    change_vals <- round(stats_df$annual_mean - comp_stats$annual_mean, 3)
    table_out[[paste0('Change <span title="Annual mean NDVI for the selected year minus the compare year — positive means improvement, negative means decline." style="cursor:help;color:#888;font-size:0.85em;">ⓘ</span>')]] <- ifelse(
      is.na(change_vals), "—",
      ifelse(change_vals >= 0, paste0("+", change_vals), as.character(change_vals))
    )
  }

  list(bar = p_bar, scatter = p_scatter, table = table_out, insight_text = insight_text)
}

# --- Sub-tab 4: Agricultural Monitoring ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

# --- Phenology profiles (region + crop) ---
# Each profile drives all detection windows, thresholds, and confidence scoring.
# Add new profiles here as new regions are supported — no other code changes required.
.phenology_profiles <- list(
  "Zambia — Maize" = list(
    region                  = "Southern Africa (Zambia)",
    crop                    = "Maize",
    lc_class                = "Crops",
    green_up_window         = c(11L, 12L),
    green_up_baseline_month = 10L,
    green_up_threshold      = 0.08,
    green_up_conf_high      = 0.10,
    green_up_conf_med_low   = 0.06,
    green_up_conf_lo_min    = 0.03,
    peak_window             = c(1L, 3L),
    peak_conf_delta         = 0.05,
    senescence_window       = c(4L, 6L),
    senescence_threshold    = 0.30,
    notes                   = "Rainy season Oct–Apr; responds to October rains; harvest by June"
  ),
  "Spain — Wheat" = list(
    region                  = "Mediterranean (Spain)",
    crop                    = "Wheat",
    lc_class                = "Crops",
    green_up_window         = c(11L, 12L),
    green_up_baseline_month = 10L,
    green_up_threshold      = 0.10,
    green_up_conf_high      = 0.12,
    green_up_conf_med_low   = 0.08,
    green_up_conf_lo_min    = 0.04,
    peak_window             = c(2L, 4L),
    peak_conf_delta         = 0.05,
    senescence_window       = c(5L, 7L),
    senescence_threshold    = 0.35,
    notes                   = "Cool-season crop; planted in autumn, harvested early summer"
  ),
  "India — Rice" = list(
    region                  = "South Asia (India)",
    crop                    = "Rice",
    lc_class                = "Crops",
    green_up_window         = c(7L, 8L),
    green_up_baseline_month = 6L,
    green_up_threshold      = 0.12,
    green_up_conf_high      = 0.14,
    green_up_conf_med_low   = 0.10,
    green_up_conf_lo_min    = 0.06,
    peak_window             = c(8L, 9L),
    peak_conf_delta         = 0.04,
    senescence_window       = c(9L, 10L),
    senescence_threshold    = 0.28,
    notes                   = "Monsoon-driven; planted at rains, harvested before winter"
  ),
  "Brazil — Soybean" = list(
    region                  = "Central Brazil (Cerrado)",
    crop                    = "Soybean",
    lc_class                = "Crops",
    green_up_window         = c(10L, 11L),
    green_up_baseline_month = 9L,
    green_up_threshold      = 0.09,
    green_up_conf_high      = 0.11,
    green_up_conf_med_low   = 0.07,
    green_up_conf_lo_min    = 0.04,
    peak_window             = c(12L, 12L),
    peak_conf_delta         = 0.05,
    senescence_window       = c(1L, 2L),
    senescence_threshold    = 0.32,
    notes                   = "Summer crop; staggered plantings common; shorter cycle than maize"
  ),
  "Generic — Rangeland" = list(
    region                  = "Generic",
    crop                    = "Rangeland",
    lc_class                = "Rangeland",
    green_up_window         = c(10L, 12L),
    green_up_baseline_month = 9L,
    green_up_threshold      = 0.05,
    green_up_conf_high      = 0.08,
    green_up_conf_med_low   = 0.04,
    green_up_conf_lo_min    = 0.02,
    peak_window             = c(1L, 3L),
    peak_conf_delta         = 0.04,
    senescence_window       = c(4L, 7L),
    senescence_threshold    = 0.20,
    notes                   = "Generic rangeland — lower thresholds, broader senescence window"
  )
)

# Expand a month window to a vector; handles wrap-around (e.g. 11:1 → c(11,12,1))
.months_in_window <- function(start, end) {
  if (start <= end) start:end else c(start:12L, 1L:end)
}

.score_green_up <- function(rise, profile) {
  if (is.na(rise) || rise < profile$green_up_conf_lo_min) return("Not detected")
  if (rise >= profile$green_up_conf_high)    return("High")
  if (rise >= profile$green_up_conf_med_low) return("Medium")
  return("Low")
}

.score_peak <- function(peak_ndvi, hist_avg, profile) {
  if (is.na(peak_ndvi)) return("Not detected")
  if (is.na(hist_avg))  return("Medium")
  dev <- peak_ndvi - hist_avg
  if (dev >  profile$peak_conf_delta) return("High")
  if (dev < -profile$peak_conf_delta) return("Low")
  return("Medium")
}

.score_senescence <- function(sen_ndvi, profile) {
  if (is.na(sen_ndvi)) return("Not detected")
  if (sen_ndvi < (profile$senescence_threshold - 0.05)) return("High")
  return("Medium")
}

# Returns marker style list (symbol, size, opacity) keyed by confidence level.
.conf_marker_style <- function(conf, base_symbol) {
  switch(conf,
    High           = list(symbol = base_symbol,                    size = 12, opacity = 1.00),
    Medium         = list(symbol = paste0(base_symbol, "-open"),   size = 10, opacity = 0.85),
    Low            = list(symbol = paste0(base_symbol, "-open"),   size = 8,  opacity = 0.55),
    `Not detected` = NULL
  )
}

.conf_label <- function(conf) {
  switch(conf,
    High           = "✅ High",
    Medium         = "⚠️ Medium",
    Low            = "⚠️ Low",
    `Not detected` = "❌ Not detected",
    conf
  )
}

detect_phenology <- function(df_year, profile, hist_peak_avg = NA_real_) {
  df_year <- dplyr::arrange(df_year, month)
  ndvi    <- df_year$mean_ndvi
  months  <- df_year$month

  # --- Green-up ---
  baseline_idx  <- which(months == profile$green_up_baseline_month)
  baseline_ndvi <- if (length(baseline_idx) > 0L) ndvi[baseline_idx[1L]] else NA_real_

  gu_win  <- .months_in_window(profile$green_up_window[1L], profile$green_up_window[2L])
  gu_idx  <- which(months %in% gu_win)

  green_up_month <- NA_integer_
  green_up_conf  <- "Not detected"
  green_up_rise  <- NA_real_

  if (length(gu_idx) > 0L && !is.na(baseline_ndvi)) {
    max_in_win    <- max(ndvi[gu_idx], na.rm = TRUE)
    green_up_rise <- max_in_win - baseline_ndvi

    if (green_up_rise >= profile$green_up_conf_lo_min) {
      thr_val <- baseline_ndvi + profile$green_up_threshold
      crossed <- gu_idx[ndvi[gu_idx] > thr_val]
      green_up_month <- if (length(crossed) > 0L) months[crossed[1L]]
                        else months[gu_idx[which.max(ndvi[gu_idx])]]
      green_up_conf  <- .score_green_up(green_up_rise, profile)
    }
  }

  # --- Peak ---
  pk_win <- .months_in_window(profile$peak_window[1L], profile$peak_window[2L])
  pk_idx <- which(months %in% pk_win)

  peak_month    <- NA_integer_
  peak_ndvi_val <- NA_real_
  peak_conf     <- "Not detected"

  if (length(pk_idx) > 0L) {
    best          <- pk_idx[which.max(ndvi[pk_idx])]
    peak_month    <- months[best]
    peak_ndvi_val <- ndvi[best]
    peak_conf     <- .score_peak(peak_ndvi_val, hist_peak_avg, profile)
  } else {
    fallback      <- which.max(ndvi)
    peak_month    <- months[fallback]
    peak_ndvi_val <- ndvi[fallback]
    peak_conf     <- "Low"
  }

  # --- Senescence ---
  sen_win <- .months_in_window(profile$senescence_window[1L], profile$senescence_window[2L])
  sen_idx <- which(months %in% sen_win)

  senescence_month <- NA_integer_
  senescence_conf  <- "Not detected"
  senescence_ndvi  <- NA_real_

  for (k in seq_along(sen_idx)) {
    i <- sen_idx[k]
    if (!is.na(ndvi[i]) && ndvi[i] < profile$senescence_threshold) {
      senescence_month <- months[i]
      senescence_ndvi  <- ndvi[i]
      senescence_conf  <- .score_senescence(ndvi[i], profile)
      break
    }
  }

  season_length_months <- if (!is.na(green_up_month) && !is.na(senescence_month))
    as.integer((senescence_month + 12L - green_up_month) %% 12L)
  else NA_integer_

  data.frame(
    year                 = df_year$year[1L],
    green_up             = green_up_month,
    green_up_conf        = green_up_conf,
    green_up_rise        = round(green_up_rise, 3),
    peak                 = peak_month,
    peak_ndvi            = round(peak_ndvi_val, 3),
    peak_conf            = peak_conf,
    senescence           = senescence_month,
    senescence_conf      = senescence_conf,
    senescence_ndvi      = round(senescence_ndvi, 3),
    season_length_months = season_length_months,
    stringsAsFactors     = FALSE
  )
}

plot_agricultural_monitoring <- function(df, selected_class = "Crops") {
  if (is.null(df) || nrow(df) == 0) stop("plot_agricultural_monitoring: df is empty.")

  # Auto-select first profile matching the requested class
  matching <- names(.phenology_profiles)[
    vapply(.phenology_profiles, function(p) p$lc_class == selected_class, logical(1L))
  ]
  if (length(matching) == 0L) stop("plot_agricultural_monitoring: no profile for class '", selected_class, "'")
  profile_name <- matching[1L]
  profile <- .phenology_profiles[[profile_name]]

  all_df <- df[df$land_cover == selected_class, ]
  if (nrow(all_df) == 0) stop("plot_agricultural_monitoring: no ", selected_class, " data found.")

  # Pre-compute historical average peak NDVI for confidence scoring
  pk_win <- .months_in_window(profile$peak_window[1L], profile$peak_window[2L])
  hist_peak_avg <- tryCatch({
    pk_by_yr <- dplyr::summarise(
      dplyr::group_by(all_df[all_df$month %in% pk_win, ], year),
      pk = max(mean_ndvi, na.rm = TRUE), .groups = "drop"
    )
    mean(pk_by_yr$pk, na.rm = TRUE)
  }, error = function(e) NA_real_)

  pheno_df <- do.call(rbind, lapply(
    split(all_df, all_df$year),
    function(d) detect_phenology(d, profile, hist_peak_avg)
  ))
  pheno_df <- dplyr::arrange(pheno_df, year)

  avg_df <- dplyr::arrange(
    dplyr::summarise(dplyr::group_by(all_df, month),
                     avg_ndvi = mean(mean_ndvi, na.rm = TRUE), .groups = "drop"),
    month
  )

  years_available <- sort(unique(all_df$year))

  # legendrank controls order: avg(1) → years(100+) → phenology(200+)
  # Each year has its own unique legendgroup so it toggles individually.
  # Section headers come from legendgrouptitle on the first trace of each section.
  marker_meta <- list(
    "Green-up"   = list(rank = 200L, group_title = "Phenology"),
    "Peak"       = list(rank = 201L, group_title = NULL),
    "Senescence" = list(rank = 202L, group_title = NULL)
  )

  shown_in_legend <- character(0)
  p <- plotly::plot_ly()

  p <- plotly::add_lines(
    p,
    x = avg_df$month, y = avg_df$avg_ndvi,
    name        = "Multi-year avg",
    legendrank  = 1L,
    legendgroup = "reference", legendgrouptitle = list(text = "Reference"),
    line        = list(color = "#888888", dash = "dash", width = 2),
    hovertemplate = paste0(selected_class, " avg: %{y:.3f}<extra></extra>")
  )

  for (i in seq_along(years_available)) {
    yr    <- years_available[i]
    df_yr <- dplyr::arrange(all_df[all_df$year == yr, ], month)
    ph    <- pheno_df[pheno_df$year == yr, ]

    # Each year gets unique legendgroup → individually toggleable.
    # "Year" section header only on first year.
    p <- plotly::add_lines(
      p,
      x = df_yr$month, y = df_yr$mean_ndvi,
      name             = as.character(yr),
      legendrank       = 100L + i,
      legendgroup      = paste0("yr_", yr),
      legendgrouptitle = if (i == 1L) list(text = "Year") else NULL,
      hovertemplate    = paste0(
        "<b>", yr, "</b><br>Month: %{x}<br>", selected_class, " NDVI: %{y:.3f}<extra></extra>"
      )
    )

    marker_defs <- list(
      list(
        month    = ph$green_up,
        conf     = ph$green_up_conf,
        label    = "Green-up",
        color    = "#43A047",
        base_sym = "triangle-up",
        tip      = if (!is.na(ph$green_up) && !is.na(ph$green_up_rise))
                     sprintf("Green-up: %s\n%s confidence (%.2f NDVI rise above %s baseline)",
                             month.name[ph$green_up], .conf_label(ph$green_up_conf),
                             ph$green_up_rise, month.name[profile$green_up_baseline_month])
                   else "Green-up: not detected"
      ),
      list(
        month    = ph$peak,
        conf     = ph$peak_conf,
        label    = "Peak",
        color    = "#1565C0",
        base_sym = "star",
        tip      = if (!is.na(ph$peak) && !is.na(ph$peak_ndvi))
                     sprintf("Peak: %s (NDVI: %.3f)\n%s confidence",
                             month.name[ph$peak], ph$peak_ndvi, .conf_label(ph$peak_conf))
                   else "Peak: not detected"
      ),
      list(
        month    = ph$senescence,
        conf     = ph$senescence_conf,
        label    = "Senescence",
        color    = "#E65100",
        base_sym = "triangle-down",
        tip      = if (!is.na(ph$senescence) && !is.na(ph$senescence_ndvi))
                     sprintf("Senescence: %s (NDVI: %.3f, threshold: %.2f)\n%s confidence",
                             month.name[ph$senescence], ph$senescence_ndvi,
                             profile$senescence_threshold, .conf_label(ph$senescence_conf))
                   else "Senescence: not detected"
      )
    )

    for (mk in marker_defs) {
      if (is.na(mk$month) || mk$conf == "Not detected") next
      ms <- .conf_marker_style(mk$conf, mk$base_sym)
      if (is.null(ms)) next
      ndvi_val <- df_yr$mean_ndvi[df_yr$month == mk$month]
      if (length(ndvi_val) == 0L) next
      meta        <- marker_meta[[mk$label]]
      first_shown <- !(mk$label %in% shown_in_legend)
      p <- plotly::add_markers(
        p, x = mk$month, y = ndvi_val[1L],
        name             = mk$label,
        legendgroup      = mk$label,
        legendgrouptitle = if (first_shown) meta$group_title else NULL,
        legendrank       = meta$rank,
        showlegend       = first_shown,
        marker           = list(size = ms$size, color = mk$color,
                                symbol = ms$symbol, opacity = ms$opacity),
        hovertemplate    = paste0("<b>", yr, " ", mk$label, "</b><br>",
                                  gsub("\n", "<br>", mk$tip), "<extra></extra>")
      )
      if (first_shown) shown_in_legend <- c(shown_in_legend, mk$label)
    }
  }

  p <- plotly::layout(
    p,
    xaxis  = list(title = "Month", tickmode = "array", tickvals = 1:12, ticktext = month.abb),
    yaxis  = list(title = paste(selected_class, "NDVI")),
    legend = list(
      orientation   = "v",
      tracegroupgap = 10,
      font          = list(size = 12)
    )
  )

  # Season Performance insight — multi-year summary using only years with ≥6 months of data
  insight_text <- tryCatch({
    months_per_yr  <- tapply(all_df$month, all_df$year, function(x) length(unique(x)))
    complete_years <- as.integer(names(months_per_yr)[months_per_yr >= 6L])
    ok <- pheno_df[!is.na(pheno_df$peak_ndvi) & pheno_df$year %in% complete_years, ]
    if (nrow(ok) < 2L) stop("insufficient complete years")

    n_yrs      <- nrow(ok)
    yr_range   <- paste0(min(ok$year), "–", max(ok$year))
    avg_peak_ndvi <- round(mean(ok$peak_ndvi, na.rm = TRUE), 2)
    avg_peak_mo   <- round(mean(ok$peak,      na.rm = TRUE))

    gu_ok      <- ok[!is.na(ok$green_up), ]
    gu_text    <- if (nrow(gu_ok) >= 2L)
      paste0(", with green-up typically in ", month.name[round(mean(gu_ok$green_up))])
    else ""

    trend_text <- if (n_yrs >= 3L) {
      slope <- coef(lm(peak_ndvi ~ year, data = ok))[["year"]]
      if      (slope >  0.005) sprintf(" Peak NDVI is trending upward (+%.3f/yr).",  slope)
      else if (slope < -0.005) sprintf(" Peak NDVI is trending downward (%.3f/yr).", slope)
      else                     " Peak NDVI is stable across this period."
    } else ""

    sprintf(
      "Over %d years (%s), %s typically peaks at NDVI %.2f in %s%s.%s",
      n_yrs, yr_range, selected_class, avg_peak_ndvi, month.name[avg_peak_mo],
      gu_text, trend_text
    )
  }, error = function(e) "Season performance summary not available.")

  # Table
  avg_green_up    <- round(mean(pheno_df$green_up,             na.rm = TRUE))
  avg_peak_mo     <- round(mean(pheno_df$peak,                 na.rm = TRUE))
  avg_sen_mo      <- round(mean(pheno_df$senescence,           na.rm = TRUE))
  avg_peak_ndvi   <- round(mean(pheno_df$peak_ndvi,            na.rm = TRUE), 3)
  avg_season      <- round(mean(pheno_df$season_length_months, na.rm = TRUE))

  # Plain month abbreviation or "—"; confidence omitted from cells
  # (confidence already encoded in chart marker style: solid = certain, outlined = uncertain)
  .fmt_mo <- function(mo) if (is.na(mo)) "—" else month.abb[mo]

  .tip <- function(text) sprintf(
    ' <span title="%s" style="cursor:help;color:#888;font-size:0.85em;">ⓘ</span>', text
  )
  col_greenup <- paste0("Green-up", .tip(
    "Month when NDVI first rises above the dry-season baseline, signalling vegetation response to rainfall."
  ))
  col_peak    <- paste0("Peak", .tip(
    "Month of highest NDVI within the expected peak window."
  ))
  col_senes   <- paste0("Senescence", .tip(
    "First month NDVI drops below the senescence threshold, marking crop maturity or dry-season die-back."
  ))
  col_season  <- paste0("Season length (months)", .tip(
    "Months from green-up to senescence; shown only when both events are detected."
  ))

  table_out <- data.frame(
    Year                     = as.character(pheno_df$year),
    `Green-up`               = vapply(pheno_df$green_up,    .fmt_mo, character(1L)),
    Peak                     = vapply(pheno_df$peak,         .fmt_mo, character(1L)),
    Senescence               = vapply(pheno_df$senescence,  .fmt_mo, character(1L)),
    `Peak NDVI`              = round(pheno_df$peak_ndvi, 3),
    `Season length (months)` = ifelse(is.na(pheno_df$season_length_months), "—",
                                      as.character(pheno_df$season_length_months)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(table_out)[names(table_out) == "Green-up"]               <- col_greenup
  names(table_out)[names(table_out) == "Peak"]                   <- col_peak
  names(table_out)[names(table_out) == "Senescence"]             <- col_senes
  names(table_out)[names(table_out) == "Season length (months)"] <- col_season

  avg_row <- data.frame(
    Year                     = "Average",
    `Green-up`               = .fmt_mo(avg_green_up),
    Peak                     = .fmt_mo(avg_peak_mo),
    Senescence               = .fmt_mo(avg_sen_mo),
    `Peak NDVI`              = avg_peak_ndvi,
    `Season length (months)` = if (!is.na(avg_season)) as.character(avg_season) else "—",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(avg_row)[names(avg_row) == "Green-up"]               <- col_greenup
  names(avg_row)[names(avg_row) == "Peak"]                   <- col_peak
  names(avg_row)[names(avg_row) == "Senescence"]             <- col_senes
  names(avg_row)[names(avg_row) == "Season length (months)"] <- col_season
  table_out <- rbind(table_out, avg_row)

  list(plot = p, phenology_table = table_out,
       avg_green_up = avg_green_up, insight_text = insight_text,
       selected_class = selected_class)
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

  # Insight: strongest positive/negative tendency (even if not significant)
  trend_with_slope <- trend_df[!is.na(trend_df$slope), ]
  insight_text <- if (nrow(trend_with_slope) >= 2) {
    best_pos <- trend_with_slope[which.max(trend_with_slope$slope), ]
    best_neg <- trend_with_slope[which.min(trend_with_slope$slope), ]
    sprintf(
      "With %d years of data, no statistically significant trends are detected. %s shows the strongest upward tendency (+%.5f NDVI/yr) and %s the strongest downward tendency (%.5f NDVI/yr). Continue monitoring as data accumulates.",
      length(avail_years),
      .format_lc(best_pos$land_cover), best_pos$slope,
      .format_lc(best_neg$land_cover), best_neg$slope
    )
  } else "Insufficient data to compute trend statistics."

  # Display table: formatted names, NA shown as "—", note for NA rows
  trend_df_display <- data.frame(
    `Land Cover`      = .format_lc(trend_df$land_cover),
    `Trend`           = trend_df$trend_direction,
    `Slope (NDVI/yr)` = ifelse(is.na(trend_df$slope),   "—", as.character(round(trend_df$slope, 5))),
    `p-value`         = ifelse(is.na(trend_df$p_value), "—", as.character(round(trend_df$p_value, 4))),
    `Note`            = ifelse(is.na(trend_df$slope) | is.na(trend_df$p_value),
                               "Insufficient variation", ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  p <- plotly::plot_ly()
  for (lc in lc_classes) {
    df_lc    <- dplyr::arrange(annual_df[annual_df$land_cover == lc, ], year)
    lc_color <- if (lc %in% names(.lc_colors)) .lc_colors[[lc]] else "#888888"

    p <- plotly::add_lines(
      p, x = df_lc$year, y = df_lc$annual_mean,
      name = .format_lc(lc), legendgroup = lc,
      line = list(color = lc_color),
      hovertemplate = paste0(
        "<b>", .format_lc(lc), "</b><br>Year: %{x}<br>Annual Mean NDVI: %{y:.3f}<extra></extra>"
      )
    )

    if (nrow(df_lc) >= 2) {
      lm_fit  <- lm(annual_mean ~ year, data = df_lc)
      trend_y <- predict(lm_fit, newdata = df_lc)
      p <- plotly::add_lines(
        p, x = df_lc$year, y = trend_y,
        name = paste0(.format_lc(lc), " trend"), legendgroup = lc,
        showlegend = FALSE,
        line = list(dash = "dash", color = lc_color, width = 2),
        hoverinfo = "none"
      )
    }
  }

  p <- plotly::layout(
    p,
    xaxis  = list(title = "Year", tickmode = "array", tickvals = avail_years, tickformat = "d"),
    yaxis  = list(title = "Annual Mean NDVI"),
    legend = list(orientation = "h")
  )

  list(plot = p, trend_table = trend_df_display, insight_text = insight_text)
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

    dry_idx <- which(months %in% 6L:8L)
    dry_min <- if (length(dry_idx) > 0L) min(ndvi[dry_idx], na.rm = TRUE) else min(ndvi, na.rm = TRUE)
    threshold <- dry_min + 0.08  # raised; 0.05 was detecting already-rising Jan NDVI

    # Search from September onward only; require 2 consecutive months above threshold
    onset_month <- NA_integer_
    search_idx  <- which(months >= 9L)
    for (k in seq_len(max(0L, length(search_idx) - 1L))) {
      i <- search_idx[k]
      j <- search_idx[k + 1L]
      if (ndvi[i] > threshold && ndvi[j] > threshold) {
        onset_month <- months[i]; break
      }
    }
    data.frame(year = df_year$year[1L], onset_month = onset_month, stringsAsFactors = FALSE)
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

  if (nrow(onset_valid) == 0L) {
    p_bar <- plotly::layout(
      plotly::add_annotations(
        plotly::plot_ly(type = "scatter", mode = "markers"),
        x = 0.5, y = 0.5, text = "No onset months detected",
        xref = "paper", yref = "paper", showarrow = FALSE,
        font = list(size = 14, color = "#888888")
      ),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
    )
  } else {
    trend_text <- ""
    if (nrow(onset_valid) >= 3L) {
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

    p_bar <- plotly::plot_ly(
      data = onset_valid,
      x    = ~year, y = ~onset_month,
      type = "bar",
      marker = list(color = "#1565C0"),
      hovertemplate = "<b>%{x}</b><br>Onset Month: %{y}<extra></extra>"
    )
    p_bar <- plotly::layout(
      p_bar,
      xaxis = list(title = "Year", tickmode = "array",
                   tickvals = onset_valid$year, tickformat = "d"),
      yaxis = list(title = "Onset Month", tickmode = "array",
                   tickvals = 1:12, ticktext = month.abb),
      title = list(text = trend_text, font = list(size = 12))
    )
  }

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

  # Resilience score = abs(max deficit) × recovery time; lower = more resilient
  max_rec <- if (any(!is.na(recovery_df$recovery_months)))
    max(recovery_df$recovery_months, na.rm = TRUE) * 2L else 12L
  recovery_df$resilience_score <- abs(recovery_df$max_deficit) *
    ifelse(is.na(recovery_df$recovery_months), max_rec, recovery_df$recovery_months)
  recovery_df$resilience_rank  <- rank(recovery_df$resilience_score,
                                       ties.method = "min", na.last = TRUE)

  # Insight text
  insight_text <- tryCatch({
    most_affected  <- recovery_df[which.min(recovery_df$max_deficit), ]
    most_resilient <- recovery_df[which.min(recovery_df$resilience_score), ]
    slowest_valid  <- recovery_df[!is.na(recovery_df$recovery_months), ]
    slowest        <- if (nrow(slowest_valid) > 0)
      slowest_valid[which.max(slowest_valid$recovery_months), ] else NULL
    slow_text <- if (!is.null(slowest))
      sprintf(" %s took the longest to recover (%d months).",
              .format_lc(slowest$land_cover), slowest$recovery_months) else ""
    sprintf(
      "In %s, %s showed the largest deficit (%.3f NDVI) in %s.%s %s was the most resilient class (lowest combined deficit × recovery score).",
      anomaly_year, .format_lc(most_affected$land_cover), most_affected$max_deficit,
      most_affected$deficit_month, slow_text, .format_lc(most_resilient$land_cover)
    )
  }, error = function(e) "Resilience summary not available.")

  # Heatmap of anomaly per class × month
  hm_data <- tidyr::pivot_wider(
    merged[, c("land_cover", "month", "anomaly")],
    names_from = "month", values_from = "anomaly"
  )
  lc_names  <- hm_data$land_cover
  hm_matrix <- as.matrix(hm_data[, -1])
  rownames(hm_matrix) <- lc_names
  col_months <- as.integer(colnames(hm_matrix))

  p_heatmap <- plotly::plot_ly(
    x = col_months, y = lc_names, z = hm_matrix,
    type = "heatmap",
    colorscale = list(c(0, "#E53935"), c(0.5, "#FFFFFF"), c(1, "#43A047")),
    zmid = 0,
    colorbar = list(title = "NDVI deficit\n(negative = below avg)"),
    hovertemplate = "<b>%{y}</b><br>Month: %{x}<br>Anomaly: %{z:.3f}<extra></extra>"
  )
  p_heatmap <- plotly::layout(
    p_heatmap,
    xaxis = list(title = "Month", tickmode = "array",
                 tickvals = col_months, ticktext = month.abb[col_months]),
    yaxis = list(title = "")
  )

  # Recovery bar chart coloured by class
  recovery_valid    <- recovery_df[!is.na(recovery_df$recovery_months), ]
  rec_bar_colors    <- sapply(recovery_valid$land_cover, function(lc)
    if (lc %in% names(.lc_colors)) .lc_colors[[lc]] else "#888888")
  p_recovery <- plotly::plot_ly(
    data = recovery_valid,
    x    = ~.format_lc(land_cover),
    y    = ~recovery_months,
    type = "bar",
    marker = list(color = rec_bar_colors),
    hovertemplate = "<b>%{x}</b><br>Recovery: %{y} months<extra></extra>"
  )
  p_recovery <- plotly::layout(
    p_recovery,
    xaxis = list(title = "Land Cover Class"),
    yaxis = list(title = "Months to Recovery")
  )

  # Summary table: formatted names, resilience rank, ⚠️ note for Flooded_vegetation
  table_out <- recovery_df[, c("land_cover", "max_deficit", "deficit_month",
                                "recovery_months", "resilience_rank")]
  table_out$land_cover      <- .format_lc(table_out$land_cover)
  table_out$recovery_months <- ifelse(is.na(table_out$recovery_months), "—",
                                      as.character(table_out$recovery_months))
  table_out$Note <- ifelse(
    table_out$land_cover == "Flooded vegetation",
    "⚠ Anomalies reflect flood dynamics — interpret alongside Rangeland and Crops", ""
  )
  colnames(table_out) <- c("Land Cover", "Max Deficit", "Deficit Month",
                            "Recovery (months)", "Resilience Rank", "Note")

  list(heatmap = p_heatmap, recovery = p_recovery, summary_table = table_out,
       insight_text = insight_text)
}
