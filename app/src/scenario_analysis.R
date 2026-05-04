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

# --- Sub-tab 3: Land Cover Productivity ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

.lc_colors <- land_cover_class_colors()

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

# --- Sub-tab 7: Anomaly Resilience ---
# df: pre-loaded data frame (all years, all classes) from the server shared reactive

.anomaly_severity <- function(deficit) {
  if (is.na(deficit) || deficit >= 0)
    return(list(badge = "\U0001f7e2", label = "Minimal",  text = "Negligible stress, barely noticeable"))
  if (deficit < -0.15)
    return(list(badge = "\U0001f534", label = "Severe",   text = "Major stress, significant vegetation loss"))
  if (deficit < -0.10)
    return(list(badge = "\U0001f7e0", label = "Moderate", text = "Notable stress, visible vegetation decline"))
  if (deficit < -0.05)
    return(list(badge = "\U0001f7e1", label = "Mild",     text = "Minor stress, recovery expected quickly"))
  list(badge = "\U0001f7e2", label = "Minimal", text = "Negligible stress, barely noticeable")
}

.anomaly_class_interp <- function(lc, deficit) {
  if (is.na(deficit)) return("No data available")
  if (lc == "Flooded_vegetation")
    return("High variability driven by flood dynamics (water levels), not vegetation stress")
  if (lc == "Built_Area") return("Low & stable — built surfaces don't respond to rainfall")
  if (lc == "Water")      return("Not a vegetation metric — NDVI near zero for water bodies")
  if (lc == "Trees") {
    if (deficit < -0.10) return("Severe stress, unusual for this stable class")
    if (deficit < -0.05) return("Moderate stress, slower than expected recovery")
    return("Minimal stress, recovered quickly")
  }
  if (lc == "Rangeland") {
    if (deficit < -0.10) return("Severe drought impact, grass recovery sensitive to rainfall")
    if (deficit < -0.05) return("Moderate drought, typical recovery pattern")
    return("Mild stress, resilient to rainfall variability")
  }
  if (lc == "Crops") {
    if (deficit < -0.10) return("Poor growing season, crop failure risk")
    if (deficit < -0.05) return("Moderate yield loss, recovery depends on next planting season")
    return("Minimal impact, manageable for farmers")
  }
  if (lc == "Bare_ground") {
    if (deficit < -0.10) return("Variable & low productivity — may indicate active land cover change")
    if (deficit < -0.05) return("Variable but stable")
    return("Minimal change")
  }
  .anomaly_severity(deficit)$text
}

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

    worst_row         <- df_lc[which.min(df_lc$anomaly), ]
    max_deficit       <- worst_row$anomaly
    deficit_month_int <- as.integer(worst_row$month)

    subsequent <- df_lc[df_lc$month > deficit_month_int, ]
    recovery_months <- NA_integer_
    for (i in seq_len(nrow(subsequent))) {
      row_i <- subsequent[i, ]
      if (abs(row_i$mean_ndvi - row_i$hist_mean) <= row_i$hist_sd) {
        recovery_months <- as.integer(row_i$month - deficit_month_int)
        break
      }
    }

    data.frame(
      land_cover        = lc,
      max_deficit       = round(max_deficit, 3),
      deficit_month_int = deficit_month_int,
      deficit_month     = month.abb[deficit_month_int],
      recovery_months   = recovery_months,
      stringsAsFactors  = FALSE
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

  fmt_rec <- function(r) {
    if (is.na(r$recovery_months)) "—"
    else paste0(r$recovery_months, " month", ifelse(r$recovery_months == 1L, "", "s"))
  }
  fmt_rec_prose <- function(r) {
    if (is.na(r$recovery_months)) "no recovery detected this year"
    else paste0(r$recovery_months, " month", ifelse(r$recovery_months == 1L, "", "s"), " to recover")
  }

  # Helper: collapse tied class names into "A and B" or "A, B and C"
  .join_classes <- function(classes) {
    nms <- .format_lc(classes)
    if (length(nms) == 1L) nms
    else if (length(nms) == 2L) paste(nms, collapse = " and ")
    else paste0(paste(nms[-length(nms)], collapse = ", "), " and ", nms[length(nms)])
  }

  # Insight text — handle ties by finding ALL classes at min/max
  insight_text <- tryCatch({
    min_def   <- min(recovery_df$max_deficit, na.rm = TRUE)
    affected  <- recovery_df[!is.na(recovery_df$max_deficit) &
                               recovery_df$max_deficit == min_def, ]

    min_score <- min(recovery_df$resilience_score, na.rm = TRUE)
    resilient <- recovery_df[!is.na(recovery_df$resilience_score) &
                               recovery_df$resilience_score == min_score, ]

    slowest_valid <- recovery_df[!is.na(recovery_df$recovery_months), ]
    slow_text <- if (nrow(slowest_valid) > 0L) {
      max_rec_val <- max(slowest_valid$recovery_months)
      slowest_all <- slowest_valid[slowest_valid$recovery_months == max_rec_val, ]
      sprintf(" %s took the longest to recover (%s).",
              .join_classes(slowest_all$land_cover),
              fmt_rec_prose(slowest_all[1L, ]))
    } else ""

    sprintf(
      "In %s, %s showed the largest deficit (%.3f NDVI) in %s.%s %s was the most resilient class (lowest deficit %.3f NDVI, fastest recovery %s).",
      anomaly_year, .join_classes(affected$land_cover), min_def,
      affected$deficit_month[1L], slow_text,
      .join_classes(resilient$land_cover), resilient$max_deficit[1L],
      fmt_rec_prose(resilient[1L, ])
    )
  }, error = function(e) "Resilience summary not available.")

  # Resilience Ranking Card — handle ties
  ranking_card <- tryCatch({
    min_score <- min(recovery_df$resilience_score, na.rm = TRUE)
    max_score <- max(recovery_df$resilience_score, na.rm = TRUE)
    resilient_rows  <- recovery_df[!is.na(recovery_df$resilience_score) &
                                     recovery_df$resilience_score == min_score, ]
    vulnerable_rows <- recovery_df[!is.na(recovery_df$resilience_score) &
                                     recovery_df$resilience_score == max_score, ]

    resilient_lcs  <- resilient_rows$land_cover
    vulnerable_lcs <- vulnerable_rows$land_cover
    other_rows <- recovery_df[!recovery_df$land_cover %in% c(resilient_lcs, vulnerable_lcs), ]

    other_text <- if (nrow(other_rows) > 0L) {
      mid <- other_rows[which.min(abs(other_rows$resilience_score -
                                        stats::median(recovery_df$resilience_score))), ]
      sev <- .anomaly_severity(mid$max_deficit)
      sprintf("%s shows %s vulnerability — %.3f NDVI deficit, %s.",
              .format_lc(mid$land_cover), tolower(sev$label), mid$max_deficit, fmt_rec_prose(mid))
    } else ""

    list(
      resilient  = sprintf("Most Resilient: %s — smallest deficit (%.3f NDVI), %s",
                           .join_classes(resilient_lcs),
                           resilient_rows$max_deficit[1L], fmt_rec_prose(resilient_rows[1L, ])),
      vulnerable = sprintf("Most Vulnerable: %s — largest deficit (%.3f NDVI), %s",
                           .join_classes(vulnerable_lcs),
                           vulnerable_rows$max_deficit[1L], fmt_rec_prose(vulnerable_rows[1L, ])),
      other      = other_text,
      flood_note = if ("Flooded_vegetation" %in% recovery_df$land_cover)
        "⚠️ Flooded vegetation's deficit and recovery time are flood-driven, not vegetation stress." else ""
    )
  }, error = function(e) NULL)

  # Heatmap of anomaly per class × month
  hm_data <- tidyr::pivot_wider(
    merged[, c("land_cover", "month", "anomaly")],
    names_from = "month", values_from = "anomaly"
  )
  lc_names  <- hm_data$land_cover
  hm_matrix <- as.matrix(hm_data[, -1])
  rownames(hm_matrix) <- lc_names
  col_months <- as.integer(colnames(hm_matrix))

  # Per-cell hover text for heatmap
  hm_hover <- matrix("", nrow = length(lc_names), ncol = length(col_months))
  for (i in seq_along(lc_names)) {
    lc <- lc_names[i]
    for (j in seq_along(col_months)) {
      mo    <- col_months[j]
      row_m <- merged[merged$land_cover == lc & merged$month == mo, ]
      if (nrow(row_m) == 0L) next
      anom  <- row_m$anomaly[1L]
      sd_v  <- row_m$hist_sd[1L]
      status <- if (is.na(anom))         "No data"
                else if (abs(anom) <= sd_v) "Normal range"
                else if (anom < 0)     "Below normal (deficit)"
                else                   "Above normal"
      sev <- .anomaly_severity(anom)
      hm_hover[i, j] <- paste0(
        .format_lc(lc), " — ", month.abb[mo], " ", anomaly_year,
        "<br>Anomaly: ", sprintf("%.3f", anom), " NDVI (", sev$label, ")",
        "<br>Status: ", status
      )
    }
  }

  p_heatmap <- plotly::plot_ly(
    x = col_months, y = lc_names, z = hm_matrix,
    text = hm_hover,
    type = "heatmap",
    colorscale = list(c(0, "#E53935"), c(0.5, "#FFFFFF"), c(1, "#43A047")),
    zmid = 0,
    colorbar = list(title = "NDVI deficit\n(negative = below avg)"),
    hovertemplate = "%{text}<extra></extra>"
  )
  p_heatmap <- plotly::layout(
    p_heatmap,
    xaxis = list(title = "Month", tickmode = "array",
                 tickvals = col_months, ticktext = month.abb[col_months]),
    yaxis = list(title = "")
  )

  # Recovery bar chart coloured by class
  recovery_valid  <- recovery_df[!is.na(recovery_df$recovery_months), ]
  rec_bar_colors  <- sapply(recovery_valid$land_cover, function(lc)
    if (lc %in% names(.lc_colors)) .lc_colors[[lc]] else "#888888")

  rec_hover <- vapply(seq_len(nrow(recovery_valid)), function(i) {
    lc      <- recovery_valid$land_cover[i]
    rec     <- recovery_valid$recovery_months[i]
    def     <- recovery_valid$max_deficit[i]
    def_mo  <- recovery_valid$deficit_month_int[i]
    rec_mo  <- if (!is.na(def_mo) && !is.na(rec) && (def_mo + rec) <= 12L)
      month.abb[def_mo + rec] else "end of year"
    sev <- .anomaly_severity(def)
    paste0(
      .format_lc(lc), ": ", rec, " month", ifelse(rec == 1L, "", "s"), " to recovery",
      "<br>Max deficit: ", sprintf("%.3f", def), " NDVI (", sev$label, ": ", sev$text, ")",
      "<br>Recovered by: ", rec_mo, " ", anomaly_year
    )
  }, character(1L))

  p_recovery <- plotly::plot_ly(
    data = recovery_valid,
    x    = ~vapply(land_cover, .format_lc, character(1L)),
    y    = ~recovery_months,
    type = "bar",
    text = rec_hover,
    textposition = "none",
    marker = list(color = rec_bar_colors),
    hovertemplate = "%{text}<extra></extra>"
  )
  p_recovery <- plotly::layout(
    p_recovery,
    xaxis = list(title = "Land Cover Class"),
    yaxis = list(title = "Months to Recovery")
  )

  # Summary table with Interpretation column
  interp_col <- vapply(seq_len(nrow(recovery_df)), function(i) {
    lc  <- recovery_df$land_cover[i]
    def <- recovery_df$max_deficit[i]
    if (lc == "Flooded_vegetation") {
      "⚠️ Severe (flood-driven) — high variability reflects water dynamics, not drought"
    } else {
      sev    <- .anomaly_severity(def)
      interp <- .anomaly_class_interp(lc, def)
      paste0(sev$badge, " ", sev$label, " — ", interp)
    }
  }, character(1L))

  table_out <- recovery_df[, c("land_cover", "max_deficit", "deficit_month",
                                "recovery_months", "resilience_rank")]
  table_out$land_cover      <- .format_lc(table_out$land_cover)
  table_out$recovery_months <- ifelse(is.na(table_out$recovery_months), "—",
                                      as.character(table_out$recovery_months))
  table_out$Interpretation  <- interp_col
  .tip <- function(txt) sprintf(' <span title="%s" style="cursor:help;color:#888;font-size:0.85em;">ⓘ</span>', txt)
  col_deficit    <- paste0("Max Deficit (NDVI)",   .tip("How far NDVI dropped below its historical average — a larger negative value means more stress."))
  col_def_month  <- paste0("Deficit Month",        .tip("The month when vegetation stress was at its worst."))
  col_recovery   <- paste0("Recovery (months)",    .tip("How many months it took for NDVI to return to normal after the worst stress point."))
  col_rank       <- paste0("Resilience Rank",      .tip("Classes ranked from most resilient (1) to most vulnerable, based on deficit size and recovery time."))
  col_interp     <- paste0("Interpretation",       .tip("Overall stress severity and what it means for this land cover type."))
  colnames(table_out) <- c("Land Cover", col_deficit, col_def_month,
                            col_recovery, col_rank, col_interp)

  list(heatmap = p_heatmap, recovery = p_recovery, summary_table = table_out,
       insight_text = insight_text, ranking_card = ranking_card)
}
