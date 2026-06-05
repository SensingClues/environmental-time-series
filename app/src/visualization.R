
# -----------------------------------------------------------------------------
# VISUALIZATION
# -----------------------------------------------------------------------------

# Function to plot NDVI distribution
plot_ndvi_timeseries <- function(train_data = NULL, test_data = NULL,
                                 country_name = NULL, resolution = NULL,
                                 plot_width = 15, plot_height = 8,
                                 ylim_range = NULL,
                                 test_start_date = NULL, test_end_date = NULL,
                                 label_test = "NDVI 2024",
                                 label_train = "NDVI 2019-2023",
                                 label_mean = "NDVI Average 2019-2023",
                                 save_path = NULL,
                                 filename = "NDVI_timeseries.png") {
  
  # Set y value range for plot
  if (is.null(ylim_range)) {
    ylim_range <- c(min(train_data$upper_ci) - 0.25, max(train_data$upper_ci) + 0.15)
  }
  
  # Add Month Name to dataframes
  test_data$Month_Name <- month.name[as.numeric(test_data$Month)]
  train_data$Month_Name <- month.name[as.numeric(train_data$Month)]
  
  # Make month name vector, to customize order of x axis 
  invisible(Sys.setlocale("LC_TIME", "C")) # or "English"
  month_vector <- format(seq(test_start_date, test_end_date, by="month"), "%B")
  
  # Set plot size
  options(repr.plot.width = plot_width, repr.plot.height = plot_height)
  
  # Create the plot
  ts_plot <- ggplot(NULL, aes(x = factor(Month_Name, levels=month_vector),
                              y = mean_val, group = 1)) +
    geom_point(data = train_data, size = 5,
               fill = "#2781cf") + # point-average train data
    geom_line(data = train_data) +
    geom_ribbon(data = train_data, aes(ymin = lower_ci, ymax = upper_ci),
                alpha = 0.2, fill = "#2781cf") + # Shaded CI ribbon
    geom_point(data = test_data, size = 5, shape = 23,
               fill = "#9662b3") + # point-average test data
    theme_minimal() +
    labs(
      title = paste0("NDVI (", ifelse(grepl("_", resolution), sub(".*_", "", resolution), resolution), "m res)"),
      x = "Month",
      y = "Mean NDVI"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
      axis.text.y = element_text(size = 15), 
      axis.title.x = element_text(size = 20, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = 20, margin = margin(0, 15, 0, 0)),
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    ylim(ylim_range) +
    # Add texts to plot labels
    geom_text(aes(x = Inf, y = ylim_range[2] - 0.025),
            label = label_test, inherit.aes = FALSE,
            size = 6, color = "#9662b3", hjust = 1) +
    geom_text(aes(x = Inf, y = ylim_range[2] - 0.075),
              label = label_train, inherit.aes = FALSE,
              size = 6, color = "#2781cf", hjust = 1) +
    geom_text(aes(x = Inf, y = ylim_range[2] - 0.125),
              label = label_mean, inherit.aes = FALSE,
              size = 6, color = "black", hjust = 1)
    
  # Save plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the plot to the specified location
    ggsave(filename = file.path(save_path, filename),plot = ts_plot,
           width = plot_width, height = plot_height, units = "in")
  }
  
  # Return the plot
  return(ts_plot)
}

# --- Land cover: shared colors (Leaflet map + Plotly time series) --------------------

#' Named hex colors for canonical LULC class keys (aligned with map palette).
#' @noRd
land_cover_class_colors <- function() {
  c(
    Bare_ground        = "#A1887F",
    Built_Area         = "#78909C",
    Crops              = "#F9A825",
    Flooded_vegetation = "#00897B",
    Rangeland          = "#8BC34A",
    Trees              = "#2E7D32",
    Water              = "#1565C0"
  )
}

#' Stable iteration order for loops / legend (matches \code{land_cover_class_colors} names).
#' @noRd
land_cover_class_keys_ordered <- function() {
  names(land_cover_class_colors())
}

#' Convert \code{#RRGGBB} to \code{rgba(...)} for Plotly map polygons.
#' Eight-digit hex (\code{#RRGGBBAA}) is unreliable for \code{scattermapbox} \code{fillcolor}
#' in the browser; outlines still use \code{line.color}.
#' @noRd
lc_hex_to_rgba <- function(hex, alpha = 0.55) {
  hex <- trimws(hex)
  if (!grepl("^#", hex)) {
    return(sprintf("rgba(128,128,128,%g)", alpha))
  }
  h <- substr(hex, 2, 7)
  if (nchar(h) != 6L) {
    return(sprintf("rgba(128,128,128,%g)", alpha))
  }
  r <- strtoi(substr(h, 1, 2), 16L)
  g <- strtoi(substr(h, 3, 4), 16L)
  b <- strtoi(substr(h, 5, 6), 16L)
  sprintf("rgba(%d,%d,%d,%g)", r, g, b, alpha)
}

#' Map a GeoJSON filename stem (e.g. \code{Zambia_Mponda_Crops_2023}) to a class key.
#' @noRd
geojson_stem_to_class_key <- function(stem) {
  keys <- land_cover_class_keys_ordered()
  keys_by_len <- keys[order(-nchar(keys), keys)]
  for (k in keys_by_len) {
    if (grepl(k, stem, fixed = TRUE)) {
      return(k)
    }
  }
  NA_character_
}

#' Human-readable legend labels for class keys.
#' @noRd
land_cover_class_legend_labels <- function() {
  c(
    Bare_ground        = "Bare ground",
    Built_Area         = "Built area",
    Crops              = "Crops",
    Flooded_vegetation = "Flooded vegetation",
    Rangeland          = "Rangeland",
    Trees              = "Trees",
    Water              = "Water"
  )
}

#' Multi-line Plotly NDVI by land cover + AoI historic CI ribbon (NDVI TS–style layout).
#' @param for_subplot If TRUE, tighter margins for stacking under a map panel.
#' @noRd
plot_ndvi_landcover_multiline <- function(train_ndvi_summary_aoi = NULL,
                                          land_cover_summaries = NULL,
                                          name_ribbon = "Historical range",
                                          for_subplot = FALSE) {
  # Extra padding + absolute margin so ribbon CIs and sharp dips stay inside the frame.
  pad_y_range <- function(lo, hi, pad = 0.14, abs_margin = 0.035) {
    if (!is.finite(lo) || !is.finite(hi)) {
      return(NULL)
    }
    if (lo > hi) {
      return(NULL)
    }
    if (abs(hi - lo) < 1e-9) {
      pad_abs <- max(0.05, abs(lo) * 0.08 + 0.02)
      return(c(lo - pad_abs, hi + pad_abs))
    }
    span <- hi - lo
    low  <- lo - pad * span - abs_margin
    high <- hi + pad * span + abs_margin
    # Keep axis within plausible NDVI bounds while preserving headroom
    low  <- max(-1.05, low)
    high <- min(1.05, high)
    if (low >= high) {
      return(NULL)
    }
    c(low, high)
  }

  lc_colors <- land_cover_class_colors()
  lc_labels <- land_cover_class_legend_labels()

  ribbon_df <- train_ndvi_summary_aoi %>%
    dplyr::mutate(
      month_int   = as.integer(Month),
      month_label = month.name[month_int],
      month_label = factor(month_label, levels = month.name)
    ) %>%
    dplyr::filter(!is.na(month_label))

  test_df <- land_cover_summaries %>%
    dplyr::filter(period == "test") %>%
    dplyr::mutate(
      land_cover  = as.character(land_cover),
      month_int   = as.integer(Month),
      month_label = month.name[month_int],
      month_label = factor(month_label, levels = month.name)
    ) %>%
    dplyr::filter(!is.na(month_label))

  vals_y <- c(ribbon_df$lower_ci, ribbon_df$upper_ci, test_df$mean_val)
  vals_y <- vals_y[is.finite(vals_y)]
  rng_ndvi <- if (length(vals_y)) {
    pad_y_range(min(vals_y), max(vals_y))
  } else {
    NULL
  }

  ndvi_y_axis <- list(title = "Mean NDVI", showgrid = TRUE, gridcolor = "rgba(0,0,0,0.08)")
  if (!is.null(rng_ndvi)) {
    ndvi_y_axis$range <- rng_ndvi
    ndvi_y_axis$autorange <- FALSE
  }

  xaxis <- list(
    title          = "Month",
    type           = "category",
    categoryorder  = "array",
    categoryarray  = month.name,
    showgrid       = FALSE
  )

  p <- plotly::plot_ly() %>%
    plotly::add_ribbons(
      data        = ribbon_df,
      x           = ~month_label,
      ymin        = ~lower_ci,
      ymax        = ~upper_ci,
      name        = name_ribbon,
      legendgroup = "baseline",
      fillcolor   = "rgba(39, 129, 207, 0.2)",
      line        = list(color = "transparent"),
      hoverinfo   = "skip"
    )

  for (lc in land_cover_class_keys_ordered()) {
    sub <- test_df %>% dplyr::filter(land_cover == lc)
    if (nrow(sub) == 0) {
      next
    }
    col <- unname(lc_colors[lc])
    if (is.na(col)) {
      next
    }
    lab <- unname(lc_labels[lc])
    sub <- sub %>% dplyr::mutate(
      hover_line = paste0(
        lab, "<br>",
        "Month: ", as.character(month_label), "<br>",
        "Mean NDVI: ", sprintf("%.3f", mean_val)
      )
    )
    p <- p %>% plotly::add_lines(
      data          = sub,
      x             = ~month_label,
      y             = ~mean_val,
      name          = lab,
      legendgroup   = lc,
      line          = list(width = 3, color = col),
      marker        = list(size = 7, color = col),
      text          = ~hover_line,
      hovertemplate = "%{text}<extra></extra>"
    )
  }

  leg_y <- if (isTRUE(for_subplot)) 1.05 else 1.08
  margin_t <- if (isTRUE(for_subplot)) 80 else 118
  margin_b <- if (isTRUE(for_subplot)) 50 else 60
  p %>% plotly::layout(
    xaxis = xaxis,
    yaxis = ndvi_y_axis,
    template = "plotly_white",
    hovermode = "x unified",
    legend = list(
      orientation      = "h",
      x                = 0.5,
      xanchor          = "center",
      y                = leg_y,
      yanchor          = "bottom",
      traceorder       = "normal",
      # ~3 entries per row → 3 wrapped rows for 8 traces (avoids long ribbon vs. class overlap)
      entrywidth       = 0.32,
      entrywidthmode   = "fraction",
      itemsizing       = "constant",
      tracegroupgap    = 8,
      bgcolor          = "rgba(255,255,255,0.92)",
      bordercolor      = "rgba(0,0,0,0.08)",
      borderwidth      = 1
    ),
    margin = list(t = margin_t, r = 30, l = 60, b = margin_b)
  )
}

#' Lightly simplify geometries for Plotly (fewer vertices, faster client rendering).
#' Uses metre tolerance on geographic CRS when sf S2 is enabled; otherwise degree tolerance.
#' @noRd
lc_simplify_wgs84_for_plot <- function(x, dTolerance_m = 75, dTolerance_deg = 0.00025) {
  if (!inherits(x, "sf") || nrow(x) == 0L) {
    return(x)
  }
  g <- sf::st_geometry(x)
  if (all(sf::st_is_empty(g))) {
    return(x)
  }
  dtol <- if (isTRUE(sf::sf_use_s2()) && sf::st_is_longlat(x)) dTolerance_m else dTolerance_deg
  sg <- tryCatch(
    sf::st_simplify(g, dTolerance = dtol, preserveTopology = TRUE),
    error = function(e) g
  )
  # Simplifying an already-simplified or very small polygon can collapse it into
  # an empty geometry or a GEOMETRYCOLLECTION, which plotly::add_sf cannot draw
  # ("not implemented for objects of class sfc_GEOMETRYCOLLECTION"). Keep the
  # original geometry for any feature that degenerated this way.
  bad <- sf::st_is_empty(sg) | sf::st_geometry_type(sg) == "GEOMETRYCOLLECTION"
  if (any(bad)) sg[bad] <- g[bad]
  sf::st_set_geometry(x, sg)
}

#' Empty mapbox figure for \code{add_sf} without \code{plot_mapbox()} (avoids MAPBOX_TOKEN error for OSM).
#' \code{plot_mapbox()} calls \code{mapbox_token()}; open-street-map tiles work in the browser without a token.
#' @noRd
lc_plot_mapbox_init_osm <- function() {
  p <- plotly::plot_ly()
  if (is.null(p$x$layout)) {
    p$x$layout <- list()
  }
  p$x$layout$mapType <- "mapbox"
  getFromNamespace("geo2cartesian", "plotly")(p)
}

#' Expand a WGS84 bbox for framing the map: enough margin to fit the full AoI in the panel
#' (used with \code{lc_mapbox_center_zoom_from_bounds}, not as Plotly \code{bounds}, which limits zoom).
#' @noRd
lc_mapbox_bounds_from_bbox <- function(xmin, xmax, ymin, ymax,
                                        pad_frac = 0.14,
                                        pad_min_deg = 0.0028,
                                        north_extra_frac = 0.1,
                                        south_extra_frac = 0.06) {
  lon_range <- range(c(xmin, xmax))
  lat_range <- range(c(ymin, ymax))
  dx <- diff(lon_range)
  dy <- diff(lat_range)
  if (!is.finite(dx) || dx < 1e-6) {
    dx <- 0.05
  }
  if (!is.finite(dy) || dy < 1e-6) {
    dy <- 0.05
  }
  pl <- max(dx * pad_frac, pad_min_deg)
  pb <- max(dy * pad_frac, pad_min_deg)
  lon_range <- lon_range + c(-1, 1) * pl
  lat_range <- lat_range + c(-1, 1) * pb
  lat_range[1] <- lat_range[1] - dy * south_extra_frac
  lat_range[2] <- lat_range[2] + dy * north_extra_frac
  list(
    west  = lon_range[1],
    east  = lon_range[2],
    south = lat_range[1],
    north = lat_range[2]
  )
}

#' Initial \code{center} + \code{zoom} for \code{layout.mapbox} (do not set \code{bounds}: in Plotly.js that
#' restricts pan/zoom like Mapbox \code{maxBounds}, so the wheel cannot zoom out past the study area).
#' @noRd
lc_mapbox_center_zoom_from_bounds <- function(west, east, south, north, zoom_pad = 0.45) {
  lon_c <- (west + east) / 2
  lat_c <- (south + north) / 2
  lon_span <- max(east - west, 1e-8)
  lat_span <- max(north - south, 1e-8)
  lat_rad <- lat_c * pi / 180
  span <- max(lat_span, lon_span * abs(cos(lat_rad)))
  z <- log2(360 / span) - zoom_pad
  z <- max(1, min(20, z))
  list(center = list(lon = lon_c, lat = lat_c), zoom = z)
}

#' Plotly mapbox map (OpenStreetMap) of LULC polygons from a folder; \code{legendgroup} matches NDVI lines.
#' @param aoi_sf Optional study-area polygon(s); non-legend outline under LULC.
#' @return List with \code{p} (plotly), \code{bbox_by_stem}.
#' @noRd
plot_lulc_map_plotly_from_folder <- function(folder_path, aoi_sf = NULL) {
  message(paste("Plotting LULC GeoJSON for Plotly map:", folder_path))
  geojson_files <- list.files(folder_path, pattern = "\\.geojson$", full.names = TRUE)
  if (length(geojson_files) == 0) {
    stop("No GeoJSON files found in the specified folder.")
  }
  landuse_types <- tools::file_path_sans_ext(basename(geojson_files))
  ord <- order(landuse_types)
  geojson_files <- geojson_files[ord]
  landuse_types <- landuse_types[ord]
  # AoI name for the geometry hot path; folder is .../LandUse/{aoi}/S2_10m_LULC_2023.
  # Probe once so a down API costs a single 1s health check, not one 2s timeout
  # per class across the loops below.
  aoi_name <- basename(dirname(folder_path))
  lc_api_up <- api_is_available()
  pal_named <- land_cover_class_colors()
  bbox_by_stem <- vector("list", length(landuse_types))
  names(bbox_by_stem) <- landuse_types
  p <- lc_plot_mapbox_init_osm()
  all_xmin <- all_xmax <- all_ymin <- all_ymax <- numeric(0)
  aoi_bbox_for_frame <- NULL
  aoi_ok <- !is.null(aoi_sf) && inherits(aoi_sf, "sf") && nrow(aoi_sf) > 0L &&
    !all(sf::st_is_empty(sf::st_geometry(aoi_sf)))
  total_study_area_ha <- NA_real_
  if (isTRUE(aoi_ok)) {
    aoi_wgs <- lc_simplify_wgs84_for_plot(sf::st_transform(aoi_sf, 4326))
    total_study_area_ha <- sum(as.numeric(sf::st_area(aoi_wgs)), na.rm = TRUE) / 10000
    bb_aoi <- sf::st_bbox(aoi_wgs)
    aoi_bbox_for_frame <- bb_aoi
    all_xmin <- c(all_xmin, bb_aoi[["xmin"]])
    all_xmax <- c(all_xmax, bb_aoi[["xmax"]])
    all_ymin <- c(all_ymin, bb_aoi[["ymin"]])
    all_ymax <- c(all_ymax, bb_aoi[["ymax"]])
    p <- p %>% plotly::add_sf(
      data = aoi_wgs,
      inherit = FALSE,
      name = "Study area",
      showlegend = FALSE,
      fill = "toself",
      mode = "lines",
      fillcolor = "rgba(0,0,0,0)",
      line = list(color = "#222222", width = 2),
      hoverinfo = "text",
      text = "Study area"
    )
  }
  if (is.na(total_study_area_ha) || total_study_area_ha <= 0) {
    total_study_area_ha <- sum(vapply(geojson_files, function(fp) {
      g <- read_landcover_geometry(
        fp, aoi_name, geojson_stem_to_class_key(tools::file_path_sans_ext(basename(fp))),
        api_available = lc_api_up
      )
      g <- sf::st_transform(g, crs = 4326)
      sum(as.numeric(sf::st_area(g)), na.rm = TRUE) / 10000
    }, numeric(1)), na.rm = TRUE)
  }
  for (i in seq_along(geojson_files)) {
    stem <- landuse_types[i]
    geojson_data <- read_landcover_geometry(geojson_files[i], aoi_name,
                                            geojson_stem_to_class_key(stem),
                                            api_available = lc_api_up)
    geojson_data <- lc_simplify_wgs84_for_plot(sf::st_transform(geojson_data, crs = 4326))
    bbox_by_stem[[stem]] <- sf::st_bbox(geojson_data)
    lab_short <- geojson_stem_to_class_key(stem)
    pop_lab <- if (!is.na(lab_short) && lab_short %in% names(land_cover_class_legend_labels())) {
      unname(land_cover_class_legend_labels()[[lab_short]])
    } else {
      stem
    }
    col <- if (!is.na(lab_short) && lab_short %in% names(pal_named)) {
      unname(pal_named[[lab_short]])
    } else {
      "#999999"
    }
    area_ha <- sum(as.numeric(sf::st_area(geojson_data))) / 10000
    area_ha_label <- format(round(area_ha), big.mark = ",", scientific = FALSE)
    pct_label <- if (!is.na(total_study_area_ha) && total_study_area_ha > 0) {
      paste0(" (", round((area_ha / total_study_area_ha) * 100), "% of study area)")
    } else {
      ""
    }
    htxt <- paste0(
      "<b>", htmltools::htmlEscape(pop_lab), "</b> - ",
      area_ha_label, " ha", pct_label
    )
    bb <- bbox_by_stem[[stem]]
    all_xmin <- c(all_xmin, bb[["xmin"]])
    all_xmax <- c(all_xmax, bb[["xmax"]])
    all_ymin <- c(all_ymin, bb[["ymin"]])
    all_ymax <- c(all_ymax, bb[["ymax"]])
    lg <- if (!is.na(lab_short)) lab_short else stem
    p <- p %>% plotly::add_sf(
      data = geojson_data,
      inherit = FALSE,
      name = pop_lab,
      legendgroup = lg,
      showlegend = FALSE,
      fill = "toself",
      mode = "lines",
      fillcolor = lc_hex_to_rgba(col, alpha = 0.55),
      line = list(color = col, width = 2),
      hoverinfo = "text",
      text = htxt
    )
  }
  if (!is.null(aoi_bbox_for_frame)) {
    bnds <- lc_mapbox_bounds_from_bbox(
      aoi_bbox_for_frame[["xmin"]],
      aoi_bbox_for_frame[["xmax"]],
      aoi_bbox_for_frame[["ymin"]],
      aoi_bbox_for_frame[["ymax"]]
    )
  } else {
    bnds <- lc_mapbox_bounds_from_bbox(
      min(all_xmin, na.rm = TRUE),
      max(all_xmax, na.rm = TRUE),
      min(all_ymin, na.rm = TRUE),
      max(all_ymax, na.rm = TRUE)
    )
  }
  cz <- lc_mapbox_center_zoom_from_bounds(bnds$west, bnds$east, bnds$south, bnds$north)
  p <- p %>% plotly::layout(
    mapbox = list(
      style = "open-street-map",
      center = cz$center,
      zoom = cz$zoom
    ),
    margin = list(l = 4, r = 4, t = 24, b = 4),
    title = list(text = "Land cover", font = list(size = 12), y = 0.98, yref = "paper")
  )
  list(p = p, bbox_by_stem = bbox_by_stem)
}

#' NDVI chart left, Plotly mapbox map right (two columns); shared \code{legendgroup} links legend to lines + polygons.
#' @param aoi_sf Optional study-area polygon(s); passed to the map layer as a persistent outline.
#' @noRd
plot_ndvi_landcover_with_map <- function(train_ndvi_summary_aoi = NULL,
                                         land_cover_summaries = NULL,
                                         name_ribbon = "Historical range",
                                         lulc_map_folder = NULL,
                                         aoi_sf = NULL) {
  if (is.null(lulc_map_folder) || !nzchar(lulc_map_folder) || !dir.exists(lulc_map_folder)) {
    stop("lulc_map_folder must be an existing directory with GeoJSON files.")
  }
  p_ts <- plot_ndvi_landcover_multiline(
    train_ndvi_summary_aoi = train_ndvi_summary_aoi,
    land_cover_summaries   = land_cover_summaries,
    name_ribbon            = name_ribbon,
    for_subplot            = TRUE
  )
  mp <- plot_lulc_map_plotly_from_folder(lulc_map_folder, aoi_sf = aoi_sf)
  out <- plotly::subplot(
    p_ts,
    mp$p,
    nrows = 1,
    widths = c(0.5, 0.5),
    margin = 0.06,
    titleY = TRUE
  )
  out <- out %>%
    plotly::layout(dragmode = "zoom") %>%
    plotly::config(scrollZoom = TRUE, displayModeBar = TRUE)
  list(plot = out, bbox_by_stem = mp$bbox_by_stem)
}

#' Land cover seasonal-pattern table (real DOM; works in Chrome without JS tooltip HTML quirks).
#' @noRd
lc_land_cover_explorer_tooltip_table_tags <- function() {
  row <- function(a, b) {
    shiny::tags$tr(shiny::tags$td(a), shiny::tags$td(b))
  }
  shiny::tags$table(
    class = "lc-landcover-tooltip-table",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("Land cover"),
        shiny::tags$th("Seasonal pattern")
      )
    ),
    shiny::tags$tbody(
      row("Trees", "Highest NDVI. Stable year-round, peaks in rainy season (Feb–Mar)."),
      row("Rangeland", "Seasonal — rises with rainfall, drops in dry season."),
      row("Crops", "Sharp rise at planting (Nov–Dec), drops at harvest (May–Jun)."),
      row("Flooded vegetation", "Low when flooded early in year, rises as water recedes."),
      row("Bare ground", "Lowest NDVI. Slight rise after rain due to sparse vegetation."),
      row("Built area", "Low and stable — built surfaces don't respond to rainfall."),
      row("Water", "Near-zero NDVI. Higher values indicate riverside mixed pixels.")
    )
  )
}

#' Shiny UI: title row above Land Cover Plotly chart.
#' @param year Calendar year of the test NDVI series (same as server \code{end_year}); optional.
#' @noRd
ndvi_landcover_titles_ui <- function(resolution = NULL, year = NULL) {
  res_suffix <- ndvi_resolution_title_suffix(resolution)
  yr_part <- if (!is.null(year)) {
    paste0(", ", as.integer(year))
  } else {
    ""
  }
  shiny::tagList(
    shiny::tags$div(
      class = "ndvi-anomaly-title-wrap",
      shiny::tags$h4(
        class = "ndvi-anomaly-title-h4 ndvi-landcover-title-h4",
        paste0("NDVI by land cover", res_suffix, yr_part),
        shiny::tags$span(
          class = "lc-landcover-tooltip-wrap",
          tabindex = "0",
          shiny::tags$span(class = "ndvi-help-icon", `aria-label` = "Land cover seasonal patterns", "ⓘ"),
          shiny::tags$div(
            class = "lc-landcover-tooltip-panel",
            role = "tooltip",
            lc_land_cover_explorer_tooltip_table_tags()
          )
        )
      )
    ),
    lc_landcover_tooltip_flip_script()
  )
}

#' One-time JS: flip land-cover tooltip panel to the left when it would overflow the viewport.
#' @noRd
lc_landcover_tooltip_flip_script <- function() {
  shiny::tags$script(
    shiny::HTML(
      "(function(){if(window.__lcLandcoverTooltipFlip)return;window.__lcLandcoverTooltipFlip=true;\nfunction setFlip(wrap){\nvar panel=wrap.querySelector('.lc-landcover-tooltip-panel');\nif(!panel)return;\nrequestAnimationFrame(function(){\nrequestAnimationFrame(function(){\nvar pw=panel.getBoundingClientRect().width;\nif(!pw||pw<10)pw=Math.min(560,window.innerWidth-48);\nvar rect=wrap.getBoundingClientRect();\nvar margin=10;\nvar pad=4;\nvar overflowRight=rect.right+margin+pw>window.innerWidth-pad;\nvar spaceLeft=rect.left-margin;\nif(overflowRight&&spaceLeft>=pw){\nwrap.classList.add('lc-flip-left');\n}else{\nwrap.classList.remove('lc-flip-left');\n}\n});});\n}\nif(typeof jQuery!=='undefined'){\njQuery(document).on('mouseenter.lcflip focusin.lcflip','.lc-landcover-tooltip-wrap',function(){setFlip(this);});\njQuery(document).on('mouseleave.lcflip','.lc-landcover-tooltip-wrap',function(){jQuery(this).removeClass('lc-flip-left');});\njQuery(document).on('focusout.lcflip','.lc-landcover-tooltip-wrap',function(){\nvar w=this;setTimeout(function(){if(!w.contains(document.activeElement))jQuery(w).removeClass('lc-flip-left');},0);\n});\njQuery(window).on('resize.lcflip',function(){jQuery('.lc-landcover-tooltip-wrap').each(function(){var w=this;if(jQuery(w).is(':hover')||jQuery(w).find(':focus').length)setFlip(w);});});\n}\n})();"
    )
  )
}

# --- NDVI anomaly (Plotly): monthly NDVI vs climatology + historic min/max band ----

aggregate_monthly_ndvi <- function(df) {
  df %>%
    dplyr::mutate(YearMonth = as.Date(YearMonth)) %>%
    dplyr::filter(!is.na(YearMonth), !is.na(NDVI)) %>%
    dplyr::group_by(YearMonth) %>%
    dplyr::summarise(NDVI = mean(NDVI, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(YearMonth) %>%
    dplyr::mutate(
      Year  = lubridate::year(YearMonth),
      Month = lubridate::month(YearMonth)
    )
}

get_monthly_climatology <- function(train_df) {
  train_df %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(climatology = mean(NDVI, na.rm = TRUE), .groups = "drop")
}

get_monthly_historic_range <- function(train_monthly) {
  train_monthly %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(
      lower = min(NDVI, na.rm = TRUE),
      upper = max(NDVI, na.rm = TRUE),
      .groups = "drop"
    )
}

make_anomaly_data <- function(test_df, climatology_df) {
  test_df %>%
    dplyr::left_join(climatology_df, by = "Month") %>%
    dplyr::mutate(
      anomaly   = NDVI - climatology,
      bar_color = ifelse(anomaly >= 0, "#009E73", "#D55E00")
    )
}

ndvi_resolution_title_suffix <- function(resolution) {
  if (is.null(resolution) || !nzchar(as.character(resolution))) {
    return("")
  }
  paste0(
    " (",
    ifelse(grepl("_", resolution), sub(".*_", "", resolution), resolution),
    "m res)"
  )
}

# Tooltip for NDVI help icon (HTML title attribute; \\n for line breaks in native tooltip).
ndvi_anomaly_help_tooltip_text <- function() {
  paste(
    "NDVI measures how green vegetation is from satellite data.",
    "Higher values (max 1) = more vegetation.",
    "Lower values (min -1) = less vegetation.",
    "This chart shows whether current conditions are better or worse than usual.",
    sep = "\n"
  )
}

# Shiny UI: tags$h4 title above plotlyOutput.
ndvi_anomaly_titles_ui <- function(resolution = NULL, land_cover_class = NULL, view = "monthly") {
  res_suffix <- ndvi_resolution_title_suffix(resolution)
  lc_suffix  <- if (!is.null(land_cover_class) && nzchar(land_cover_class))
    paste0(" — ", gsub("_", " ", land_cover_class)) else ""
  base_title <- if (identical(view, "annual")) "Annual NDVI trend" else "NDVI time series"
  shiny::tags$div(
    class = "ndvi-anomaly-title-wrap",
    shiny::tags$h4(
      class = "ndvi-anomaly-title-h4",
      paste0(base_title, lc_suffix, res_suffix)
    )
  )
}

# --- NDVI Explorer insight cards (Wilcoxon + Seasonal Mann–Kendall) -----------------

ndvi_insight_wilcox_tooltip <- function() {
  paste(
    "This result is based on a Wilcoxon signed-rank test applied to monthly NDVI anomalies for the selected year.",
    "It checks whether vegetation conditions in the selected year are significantly different from the historical monthly average.",
    sep = "\n"
  )
}

ndvi_insight_smk_tooltip <- function() {
  paste(
    "This result is based on the Seasonal Mann–Kendall test.",
    "It evaluates whether vegetation greenness shows a consistent long-term increase or decrease over multiple years while accounting for seasonal patterns.",
    "The test is run only when at least 60 monthly samples (5 years) are available.",
    sep = "\n"
  )
}

#' Wilcoxon (monthly anomalies vs 0) and Seasonal Mann–Kendall on full monthly series.
#' SMK/Sen run only when there are at least 60 monthly points (5 years).
#' @return list(wilcox_p, wilcox_median, smk_p, sen_slope, smk_n_months)
compute_ndvi_explorer_stats <- function(train_ndvi_df, test_ndvi_df) {
  train_monthly <- aggregate_monthly_ndvi(train_ndvi_df %>% dplyr::select(YearMonth, NDVI))
  test_monthly  <- aggregate_monthly_ndvi(test_ndvi_df %>% dplyr::select(YearMonth, NDVI))
  climatology_df <- get_monthly_climatology(train_monthly)
  plot_df <- make_anomaly_data(test_monthly, climatology_df)

  anom <- stats::na.omit(plot_df$anomaly)
  wilcox_p <- NA_real_
  wilcox_median <- NA_real_
  if (length(anom) >= 3L) {
    wt <- stats::wilcox.test(anom, mu = 0)
    wilcox_p <- unname(wt$p.value)
    wilcox_median <- stats::median(anom)
  }

  smk_p <- NA_real_
  sen_slope <- NA_real_
  ndvi_monthly_full <- dplyr::bind_rows(train_monthly, test_monthly) %>%
    dplyr::distinct(YearMonth, .keep_all = TRUE) %>%
    dplyr::arrange(YearMonth)
  smk_n_months <- nrow(ndvi_monthly_full)
  smk_min_months <- 60L
  if (smk_n_months >= smk_min_months) {
    st <- ndvi_monthly_full$YearMonth[1]
    ndvi_ts <- stats::ts(
      ndvi_monthly_full$NDVI,
      start = c(lubridate::year(st), lubridate::month(st)),
      frequency = 12
    )
    smk <- tryCatch(trend::smk.test(ndvi_ts), error = function(e) NULL)
    sen <- tryCatch(trend::sens.slope(ndvi_ts), error = function(e) NULL)
    if (!is.null(smk)) smk_p <- unname(smk$p.value)
    if (!is.null(sen)) sen_slope <- as.numeric(sen$estimates)[1]
  }

  list(
    wilcox_p = wilcox_p,
    wilcox_median = wilcox_median,
    smk_p = smk_p,
    sen_slope = sen_slope,
    smk_n_months = smk_n_months
  )
}

ndvi_insight_main_class <- function(col) {
  if (identical(col, "#009E73")) {
    "ndvi-insight-card__main ndvi-insight-card__main--positive"
  } else if (identical(col, "#D55E00")) {
    "ndvi-insight-card__main ndvi-insight-card__main--negative"
  } else {
    "ndvi-insight-card__main ndvi-insight-card__main--neutral"
  }
}

#' Shiny UI: Current Year Condition card (uses compute_ndvi_explorer_stats output).
ndvi_insight_wilcox_card_ui <- function(stats, land_cover_class = NULL) {
  if (is.null(stats)) return(NULL)
  p <- stats$wilcox_p
  med <- stats$wilcox_median
  lc_label <- if (!is.null(land_cover_class) && nzchar(land_cover_class))
    gsub("_", " ", land_cover_class) else NULL
  subject <- if (!is.null(lc_label)) paste0(lc_label, " NDVI") else "Vegetation health"
  area_phrase <- if (!is.null(lc_label)) paste0("for ", lc_label, " in this area") else "for this area"
  plain_text <- paste0(subject, " this year is within the expected range ", area_phrase, ".")
  if (is.na(p)) {
    main <- "Not enough data for this summary"
    col <- "#555555"
    p_lab <- "p-value: N/A"
    plain_text <- "There is not enough monthly data to compare this year with usual conditions."
  } else if (!is.na(p) && p < 0.05 && !is.na(med) && med > 0) {
    main <- "Above normal vegetation"
    col <- "#009E73"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
    plain_text <- paste0(subject, " this year is notably better than usual.")
  } else if (!is.na(p) && p < 0.05 && !is.na(med) && med < 0) {
    main <- "Below normal vegetation"
    col <- "#D55E00"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
    plain_text <- paste0(subject, " this year is notably worse than usual - this may indicate drought, land degradation, or other stress.")
  } else {
    main <- "No significant difference from normal"
    col <- "#555555"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
  }
  shiny::tags$div(
    class = "ndvi-insight-card",
    shiny::tags$h4(
      class = "ndvi-insight-card__heading",
      "Current Year Condition",
      shiny::tags$span(
        title = ndvi_insight_wilcox_tooltip(),
        class = "ndvi-help-icon",
        "ⓘ"
      )
    ),
    shiny::tags$div(
      class = ndvi_insight_main_class(col),
      main
    ),
    shiny::tags$div(
      class = "ndvi-insight-card__footer",
      shiny::tags$div(p_lab),
      shiny::tags$div(class = "ndvi-insight-card__plain", plain_text)
    )
  )
}

#' Shiny UI: Long-Term Trend card (Seasonal Mann–Kendall + Sen slope sign).
ndvi_insight_smk_card_ui <- function(stats, source_label = NULL, year_range_label = NULL, land_cover_class = NULL) {
  if (is.null(stats)) return(NULL)
  p <- stats$smk_p
  slope <- stats$sen_slope
  n_m <- stats$smk_n_months
  lc_label <- if (!is.null(land_cover_class) && nzchar(land_cover_class))
    gsub("_", " ", land_cover_class) else NULL
  source_label <- if (!is.null(source_label) && nzchar(source_label)) source_label else "the selected data source"
  year_phrase <- if (!is.null(year_range_label) && nzchar(year_range_label)) {
    paste0(" over ", year_range_label)
  } else {
    " over the available data period"
  }
  modis_hint <- if (!identical(source_label, "MODIS")) " Switch to MODIS for a longer-term view." else ""
  if (is.null(n_m) || !is.numeric(n_m)) n_m <- NA_integer_
  smk_min_months <- 60L
  subject <- if (!is.null(lc_label)) paste0(lc_label, " vegetation health") else "Vegetation health"
  plain_text <- paste0(subject, " has been broadly consistent", year_phrase, ".", modis_hint)
  if (!is.na(n_m) && n_m < smk_min_months) {
    main <- "Long-term trend not shown (insufficient series length)"
    col <- "#555555"
    p_lab <- paste0(
      "Seasonal Mann–Kendall applies only with ≥5 years of monthly data (",
      smk_min_months,
      " months). Current series: ",
      n_m,
      " month",
      if (n_m == 1L) "" else "s",
      "."
    )
    plain_text <- paste0("There is not enough monthly data from ", source_label,
                         " to show a reliable long-term trend.", modis_hint)
  } else if (is.na(p) || is.na(slope)) {
    main <- "Long-term trend cannot be assessed from this series"
    col <- "#555555"
    p_lab <- "p-value: N/A"
    plain_text <- paste0("The available ", source_label,
                         " data could not produce a reliable trend summary.", modis_hint)
  } else if (p < 0.05 && slope > 0) {
    main <- "Significant increasing trend"
    col <- "#009E73"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
    plain_text <- paste0(subject, " has been improving over time", year_phrase,
                         " - a positive sign for this landscape.")
  } else if (p < 0.05 && slope < 0) {
    main <- "Significant decreasing trend"
    col <- "#D55E00"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
    plain_text <- paste0(subject, " has been declining over time", year_phrase,
                         " - this may indicate long-term degradation and warrants closer monitoring.")
  } else {
    main <- "No significant long-term trend"
    col <- "#555555"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
  }
  shiny::tags$div(
    class = "ndvi-insight-card",
    shiny::tags$h4(
      class = "ndvi-insight-card__heading",
      "Long-Term Trend",
      shiny::tags$span(
        title = ndvi_insight_smk_tooltip(),
        class = "ndvi-help-icon",
        "ⓘ"
      )
    ),
    shiny::tags$div(
      class = ndvi_insight_main_class(col),
      main
    ),
    shiny::tags$div(
      class = "ndvi-insight-card__footer",
      shiny::tags$div(p_lab),
      shiny::tags$div(class = "ndvi-insight-card__plain", plain_text)
    )
  )
}

ndvi_monthly_year_span_label <- function(monthly_df) {
  if (is.null(monthly_df) || nrow(monthly_df) == 0L) return("")
  ym <- monthly_df$YearMonth
  ym <- ym[!is.na(ym)]
  if (length(ym) == 0L) return("")
  y_lo <- lubridate::year(min(ym))
  y_hi <- lubridate::year(max(ym))
  if (is.na(y_lo) || is.na(y_hi)) return("")
  if (y_lo == y_hi) as.character(y_lo) else paste0(y_lo, "\u2013", y_hi)
}

#' Interactive NDVI time series vs training climatology and historic range (plotly).
#' Titles with help icon: use ndvi_anomaly_titles_ui() in Shiny above plotlyOutput.
plot_ndvi_anomaly <- function(train_ndvi_df = NULL, test_ndvi_df = NULL, land_cover_class = NULL) {
  train_monthly <- aggregate_monthly_ndvi(train_ndvi_df %>% dplyr::select(YearMonth, NDVI))
  test_monthly  <- aggregate_monthly_ndvi(test_ndvi_df %>% dplyr::select(YearMonth, NDVI))

  train_yr <- ndvi_monthly_year_span_label(train_monthly)
  test_yr <- ndvi_monthly_year_span_label(test_monthly)
  lc_label <- if (!is.null(land_cover_class) && nzchar(land_cover_class))
    paste0(" — ", gsub("_", " ", land_cover_class)) else ""
  name_ribbon  <- if (nzchar(train_yr)) paste0("NDVI historic range (", train_yr, ")") else "NDVI historic range"
  name_current <- if (nzchar(test_yr)) paste0("Current NDVI (", test_yr, ")") else "Current NDVI"
  name_clim    <- if (nzchar(train_yr)) {
    paste0("Historical monthly average (", train_yr, ")")
  } else {
    "Historical monthly average"
  }

  climatology_df <- get_monthly_climatology(train_monthly)
  historic_range   <- get_monthly_historic_range(train_monthly)

  plot_df <- make_anomaly_data(test_monthly, climatology_df) %>%
    dplyr::left_join(historic_range, by = "Month") %>%
    dplyr::mutate(
      hover_bar = paste0(
        "Date: ", format(YearMonth, "%b %Y"), "<br>",
        "Anomaly: ", sprintf("%.3f", anomaly), "<br>",
        "Current NDVI: ", sprintf("%.3f", NDVI), "<br>",
        "Historical average: ", sprintf("%.3f", climatology)
      )
    )

  common_xaxis <- list(
    title      = "Month / Year",
    tickmode   = "linear",
    dtick      = "M1",
    tickformat = "%b %Y",
    tickangle  = -45,
    showgrid   = FALSE
  )

  # Y limits from train + test so the scale stays stable when only the test year changes
  pad_y_range <- function(lo, hi, pad = 0.05) {
    if (!is.finite(lo) || !is.finite(hi)) return(NULL)
    if (lo > hi) return(NULL)
    if (abs(hi - lo) < 1e-9) {
      pad_abs <- max(0.02, abs(lo) * 0.05 + 0.01)
      return(c(lo - pad_abs, hi + pad_abs))
    }
    span <- hi - lo
    c(lo - pad * span, hi + pad * span)
  }
  vals_ndvi_y <- c(
    train_monthly$NDVI,
    test_monthly$NDVI,
    historic_range$lower,
    historic_range$upper,
    climatology_df$climatology
  )
  vals_ndvi_y <- vals_ndvi_y[is.finite(vals_ndvi_y)]
  rng_ndvi <- if (length(vals_ndvi_y)) {
    pad_y_range(min(vals_ndvi_y), max(vals_ndvi_y))
  } else {
    NULL
  }

  train_anom_df <- make_anomaly_data(train_monthly, climatology_df)
  test_anom_df <- make_anomaly_data(test_monthly, climatology_df)
  vals_anom_y <- c(train_anom_df$anomaly, test_anom_df$anomaly)
  vals_anom_y <- vals_anom_y[is.finite(vals_anom_y)]
  rng_anom <- if (length(vals_anom_y)) {
    pad_y_range(min(vals_anom_y), max(vals_anom_y))
  } else {
    NULL
  }

  ndvi_y_axis <- list(
    title = "NDVI", showgrid = TRUE, gridcolor = "rgba(0,0,0,0.08)"
  )
  if (!is.null(rng_ndvi)) {
    ndvi_y_axis$range <- rng_ndvi
    ndvi_y_axis$autorange <- FALSE
  }

  ndvi_anomaly_y_axis <- list(
    title = "NDVI Anomaly", zeroline = TRUE, zerolinewidth = 2, zerolinecolor = "gray50",
    showgrid = TRUE, gridcolor = "rgba(0,0,0,0.08)"
  )
  if (!is.null(rng_anom)) {
    ndvi_anomaly_y_axis$range <- rng_anom
    ndvi_anomaly_y_axis$autorange <- FALSE
  }

  p1 <- plotly::plot_ly() %>%
    plotly::add_ribbons(
      data      = plot_df,
      x         = ~YearMonth,
      ymin      = ~lower,
      ymax      = ~upper,
      name      = name_ribbon,
      legendgroup = "historic",
      fillcolor = "rgba(39, 129, 207, 0.2)",
      line      = list(color = "transparent"),
      hoverinfo = "skip"
    ) %>%
    plotly::add_lines(
      data            = plot_df,
      x               = ~YearMonth, y = ~NDVI,
      type            = "scatter", mode = "lines+markers",
      name            = name_current,
      line            = list(width = 3, color = "#0072B2"),
      marker          = list(size = 7, color = "#0072B2"),
      hovertemplate   = "Date: %{x|%b %Y}<br>NDVI: %{y:.3f}<extra></extra>"
    ) %>%
    plotly::add_lines(
      data            = plot_df,
      x               = ~YearMonth, y = ~climatology,
      name            = name_clim,
      line            = list(width = 2.5, dash = "dash", color = "#E69F00"),
      hovertemplate   = "Date: %{x|%b %Y}<br>Historical average: %{y:.3f}<extra></extra>"
    ) %>%
    plotly::layout(
      xaxis = common_xaxis,
      yaxis = ndvi_y_axis,
      template = "plotly_white",
      hovermode  = "x unified",
      legend = list(
        orientation = "h",
        x           = 1,
        y           = 1.05,
        xanchor     = "right",
        yanchor     = "top"
      ),
      margin = list(t = 50, r = 30, l = 60, b = 60)
    )

  # Inline hover content (avoid %{text}: plotly.R bar + single point omits `text` → literal "%{text}")
  p2 <- plotly::plot_ly(
    data    = plot_df,
    x       = ~YearMonth,
    y       = ~anomaly,
    type    = "bar",
    marker  = list(color = plot_df$bar_color),
    hovertemplate = ~paste0(hover_bar, "<extra></extra>"),
    showlegend = FALSE
  ) %>%
    plotly::layout(
      xaxis = common_xaxis,
      yaxis = ndvi_anomaly_y_axis,
      template = "plotly_white",
      margin   = list(t = 30, r = 30, l = 60, b = 80)
    )

  plotly::subplot(
    p1, p2,
    nrows   = 2,
    shareX  = TRUE,
    heights = c(0.58, 0.42),
    titleY  = TRUE
  ) %>%
    plotly::layout(
      margin = list(t = 20, r = 30, l = 60, b = 80)
    )
}

#' Compute annual-view stats from complete years only (12 months of data).
#' @return list(mk_p, mk_slope, n_years, year_range)
compute_ndvi_annual_stats <- function(annual_df) {
  # Only use complete years for all statistics
  complete_df <- if ("is_complete" %in% names(annual_df)) {
    annual_df[annual_df$is_complete == TRUE, ]
  } else {
    annual_df
  }
  n_years  <- nrow(complete_df)
  mk_p     <- NA_real_
  mk_slope <- NA_real_
  min_yr   <- if (n_years > 0L) min(complete_df$year) else NA_integer_
  max_yr   <- if (n_years > 0L) max(complete_df$year) else NA_integer_
  year_range <- if (!is.na(min_yr) && !is.na(max_yr)) {
    if (min_yr == max_yr) as.character(min_yr) else paste0(min_yr, "–", max_yr)
  } else ""
  if (n_years >= 5L) {
    mk  <- tryCatch(trend::mk.test(complete_df$mean_ndvi),    error = function(e) NULL)
    sen <- tryCatch(trend::sens.slope(complete_df$mean_ndvi), error = function(e) NULL)
    if (!is.null(mk))  mk_p     <- unname(mk$p.value)
    if (!is.null(sen)) mk_slope <- as.numeric(sen$estimates)[1]
  }
  list(mk_p = mk_p, mk_slope = mk_slope, n_years = n_years, year_range = year_range)
}

#' Interactive annual NDVI trend chart (plotly) with box/whisker reference.
#' Complete years (12 months) only used for statistics and the blue line.
#' Incomplete years shown as orange open-circle markers.
plot_ndvi_annual <- function(annual_df, land_cover_class = NULL) {

  # Split complete vs incomplete years
  has_complete <- "is_complete" %in% names(annual_df)
  if (has_complete) {
    complete_df   <- annual_df[annual_df$is_complete == TRUE,  ]
    incomplete_df <- annual_df[annual_df$is_complete == FALSE, ]
  } else {
    complete_df   <- annual_df
    incomplete_df <- annual_df[0L, ]
  }
  n_complete <- nrow(complete_df)

  # Box-plot statistics from complete years only
  ndvi_mean <- if (n_complete > 0L) mean(complete_df$mean_ndvi,   na.rm = TRUE) else NA_real_
  ndvi_q1   <- if (n_complete > 0L) unname(quantile(complete_df$mean_ndvi, 0.25, na.rm = TRUE)) else NA_real_
  ndvi_q3   <- if (n_complete > 0L) unname(quantile(complete_df$mean_ndvi, 0.75, na.rm = TRUE)) else NA_real_
  ndvi_min  <- if (n_complete > 0L) min(complete_df$mean_ndvi,    na.rm = TRUE) else NA_real_
  ndvi_max  <- if (n_complete > 0L) max(complete_df$mean_ndvi,    na.rm = TRUE) else NA_real_

  yr_all_min <- min(annual_df$year)
  yr_all_max <- max(annual_df$year)
  yr_min     <- if (n_complete > 0L) min(complete_df$year) else yr_all_min
  yr_max     <- if (n_complete > 0L) max(complete_df$year) else yr_all_max
  x_pad      <- 0.4
  # Dense x sequence gives good ribbon hover coverage at every year position
  x_ribbon   <- seq(yr_min - x_pad, yr_max + x_pad, length.out = 60L)

  # Mann-Kendall trend from complete years only
  mk_result <- list(p = NA_real_, slope = NA_real_)
  if (n_complete >= 3L) {
    mk  <- tryCatch(trend::mk.test(complete_df$mean_ndvi),    error = function(e) NULL)
    sen <- tryCatch(trend::sens.slope(complete_df$mean_ndvi), error = function(e) NULL)
    if (!is.null(mk))  mk_result$p     <- unname(mk$p.value)
    if (!is.null(sen)) mk_result$slope <- as.numeric(sen$estimates)[1]
  }

  if (n_complete > 0L) {
    complete_df$hover <- paste0(
      "Year: ", complete_df$year, "<br>",
      "Annual mean NDVI: ", sprintf("%.3f", complete_df$mean_ndvi), "<br>",
      "Long-term average: ", sprintf("%.3f", ndvi_mean)
    )
  }

  pad_y <- function(lo, hi, pad = 0.09) {
    if (!is.finite(lo) || !is.finite(hi) || lo > hi) return(NULL)
    if (abs(hi - lo) < 1e-9) {
      p <- max(0.03, abs(lo) * 0.06 + 0.01)
      return(c(lo - p, hi + p))
    }
    span <- hi - lo
    c(max(-1.05, lo - pad * span - 0.02), min(1.05, hi + pad * span + 0.02))
  }
  vals_y <- c(complete_df$mean_ndvi,
              if (nrow(incomplete_df) > 0L) incomplete_df$mean_ndvi else NULL,
              ndvi_min, ndvi_max)
  vals_y <- vals_y[is.finite(vals_y)]
  rng_y  <- if (length(vals_y) > 0L) pad_y(min(vals_y), max(vals_y)) else NULL

  y_axis <- list(title = "Annual Mean NDVI", showgrid = TRUE, gridcolor = "rgba(0,0,0,0.08)")
  if (!is.null(rng_y)) { y_axis$range <- rng_y; y_axis$autorange <- FALSE }

  note_text <- if (n_complete > 0L) {
    sprintf("Annual data: %d–%d (complete years only)", min(complete_df$year), max(complete_df$year))
  } else {
    "No complete years (12 months) found in data"
  }

  fig <- plotly::plot_ly()

  # Layer 1: IQR box (Q1-Q3), light grey fill
  if (all(is.finite(c(ndvi_q1, ndvi_q3)))) {
    iqr_hover_txt <- paste0("Typical range (25th-75th percentile):<br>",
                             sprintf("%.3f", ndvi_q1), " to ", sprintf("%.3f", ndvi_q3))
    fig <- fig %>% plotly::add_ribbons(
      x             = x_ribbon,
      ymin          = rep(ndvi_q1, length(x_ribbon)),
      ymax          = rep(ndvi_q3, length(x_ribbon)),
      name          = sprintf("Typical range (Q1-Q3: %.3f-%.3f)", ndvi_q1, ndvi_q3),
      fillcolor     = "rgba(150,150,150,0.22)",
      line          = list(color = "rgba(130,130,130,0.45)", width = 0.8),
      text          = rep(iqr_hover_txt, length(x_ribbon)),
      hovertemplate = "%{text}<extra></extra>"
    )
  }

  # Layer 2: Min/Max dotted whisker lines
  if (all(is.finite(c(ndvi_min, ndvi_max)))) {
    fig <- fig %>%
      plotly::add_segments(
        x = yr_min - x_pad, xend = yr_max + x_pad,
        y = ndvi_min, yend = ndvi_min,
        line = list(color = "rgba(100,100,100,0.5)", dash = "dot", width = 1.3),
        name = sprintf("Min/Max range (%.3f-%.3f)", ndvi_min, ndvi_max),
        hovertemplate = sprintf("Historical minimum: %.3f<extra></extra>", ndvi_min)
      ) %>%
      plotly::add_segments(
        x = yr_min - x_pad, xend = yr_max + x_pad,
        y = ndvi_max, yend = ndvi_max,
        line = list(color = "rgba(100,100,100,0.5)", dash = "dot", width = 1.3),
        showlegend    = FALSE,
        hovertemplate = sprintf("Historical maximum: %.3f<extra></extra>", ndvi_max)
      )
  }

  # Layer 3: Long-term mean dashed line
  if (is.finite(ndvi_mean)) {
    fig <- fig %>% plotly::add_segments(
      x = yr_min - x_pad, xend = yr_max + x_pad,
      y = ndvi_mean, yend = ndvi_mean,
      line = list(color = "rgba(80,80,80,0.65)", dash = "dash", width = 1.5),
      name = sprintf("Long-term average (%.3f)", ndvi_mean),
      hoverinfo = "skip"
    )
  }

  # Layer 4: Trend line if Mann-Kendall p < 0.1
  if (!is.na(mk_result$p) && mk_result$p < 0.1 && !is.na(mk_result$slope) && n_complete > 0L) {
    mid_yr    <- mean(complete_df$year)
    intercept <- ndvi_mean - mk_result$slope * (mid_yr - yr_min)
    trend_y   <- intercept + mk_result$slope * (complete_df$year - yr_min)
    trend_dir <- if (mk_result$slope > 0) "increasing" else "decreasing"
    fig <- fig %>% plotly::add_lines(
      x    = complete_df$year, y = trend_y,
      line = list(color = "rgba(180,60,60,0.65)", dash = "dash", width = 1.8),
      name = sprintf("Trend (%s, p = %.3f)", trend_dir, mk_result$p),
      hoverinfo = "skip"
    )
  }

  # Layer 5: Blue line + filled dots for complete years only
  if (n_complete > 0L) {
    fig <- fig %>%
      plotly::add_lines(
        data = complete_df, x = ~year, y = ~mean_ndvi,
        line = list(width = 2.5, color = "#0072B2"),
        name = "Annual mean NDVI",
        hoverinfo = "skip"
      ) %>%
      plotly::add_markers(
        data          = complete_df, x = ~year, y = ~mean_ndvi,
        marker        = list(color = "#0072B2", size = 8),
        showlegend    = FALSE,
        hovertemplate = ~paste0(hover, "<extra></extra>")
      )
  }

  # Layer 6: Orange open-circle markers for incomplete years
  if (nrow(incomplete_df) > 0L) {
    n_mo_vec <- if ("n_months" %in% names(incomplete_df)) incomplete_df$n_months
                else rep(NA_integer_, nrow(incomplete_df))
    incomplete_df$hover_inc <- paste0(
      "Year: ", incomplete_df$year, " (incomplete - excluded from statistics)<br>",
      "Annual mean NDVI: ", sprintf("%.3f", incomplete_df$mean_ndvi), "<br>",
      "Months available: ", n_mo_vec, " of 12"
    )
    inc_label <- if (nrow(incomplete_df) == 1L && !is.na(n_mo_vec[1])) {
      sprintf("Incomplete year (%d/12 months)", n_mo_vec[1])
    } else {
      "Incomplete years (< 12 months)"
    }
    fig <- fig %>% plotly::add_markers(
      data          = incomplete_df, x = ~year, y = ~mean_ndvi,
      marker        = list(color = "#E69F00", size = 9, symbol = "circle-open",
                           line = list(width = 2.5, color = "#E69F00")),
      name          = inc_label,
      hovertemplate = ~paste0(hover_inc, "<extra></extra>")
    )
  }

  fig %>% plotly::layout(
    xaxis = list(
      title    = "Year",
      tickmode = "linear",
      dtick    = 1,
      showgrid = FALSE,
      range    = c(yr_all_min - x_pad - 0.1, yr_all_max + x_pad + 0.1)
    ),
    yaxis     = y_axis,
    template  = "plotly_white",
    hovermode = "closest",
    legend    = list(orientation = "h", x = 1, y = 1.08, xanchor = "right", yanchor = "top"),
    margin    = list(t = 65, r = 30, l = 60, b = 60),
    annotations = list(list(
      x = 0, xref = "paper", y = 1.055, yref = "paper",
      text      = note_text,
      showarrow = FALSE,
      font      = list(size = 10, color = "#666666"),
      xanchor   = "left",
      yanchor   = "bottom"
    ))
  )
}

#' Shiny UI: Long-Term Annual Trend card (Mann-Kendall on annual means).
ndvi_annual_trend_card_ui <- function(stats, land_cover_class = NULL) {
  if (is.null(stats)) return(NULL)
  p      <- stats$mk_p
  slope  <- stats$mk_slope
  n_yrs  <- stats$n_years
  yr_rng <- stats$year_range
  lc_label   <- if (!is.null(land_cover_class) && nzchar(land_cover_class))
    gsub("_", " ", land_cover_class) else NULL
  subject     <- if (!is.null(lc_label)) paste0(lc_label, " vegetation health") else "Vegetation health"
  year_phrase <- if (!is.null(yr_rng) && nzchar(yr_rng)) paste0(" over ", yr_rng) else " over the available data period"
  min_yrs <- 5L

  if (is.na(n_yrs) || n_yrs < min_yrs) {
    main       <- "Long-term trend not shown (insufficient data)"
    col        <- "#555555"
    n_yrs_show <- if (!is.null(n_yrs) && !is.na(n_yrs)) n_yrs else 0L
    p_lab      <- paste0("At least ", min_yrs, " years required. Current: ", n_yrs_show,
                         " year", if (n_yrs_show == 1L) "" else "s", ".")
    plain_text <- "Not enough annual data to show a reliable long-term trend."
  } else if (is.na(p) || is.na(slope)) {
    main       <- "Long-term trend cannot be assessed"
    col        <- "#555555"
    p_lab      <- "p-value: N/A"
    plain_text <- "The available data could not produce a reliable trend summary."
  } else if (p < 0.05 && slope > 0) {
    main       <- "Significant increasing trend"
    col        <- "#009E73"
    p_lab      <- paste0("Mann–Kendall p-value: ", format(round(p, 3), nsmall = 3))
    plain_text <- paste0(subject, " has been improving over time", year_phrase,
                         " — a positive sign for this landscape.")
  } else if (p < 0.05 && slope < 0) {
    main       <- "Significant decreasing trend"
    col        <- "#D55E00"
    p_lab      <- paste0("Mann–Kendall p-value: ", format(round(p, 3), nsmall = 3))
    plain_text <- paste0(subject, " has been declining over time", year_phrase,
                         " — this may indicate long-term degradation and warrants monitoring.")
  } else {
    main       <- "No significant long-term trend"
    col        <- "#555555"
    p_lab      <- paste0("Mann–Kendall p-value: ", format(round(p, 3), nsmall = 3))
    plain_text <- paste0(subject, " has been broadly consistent", year_phrase, ".")
  }

  shiny::tags$div(
    class = "ndvi-insight-card",
    shiny::tags$h4(
      class = "ndvi-insight-card__heading",
      "Long-Term Annual Trend",
      shiny::tags$span(
        title = paste(
          "Based on the Mann–Kendall test applied to annual mean NDVI values.",
          "Checks whether vegetation shows a consistent increase or decrease over the full data record.",
          "Requires at least 5 years of data."
        ),
        class = "ndvi-help-icon",
        "ⓘ"
      )
    ),
    shiny::tags$div(class = ndvi_insight_main_class(col), main),
    shiny::tags$div(
      class = "ndvi-insight-card__footer",
      shiny::tags$div(p_lab),
      shiny::tags$div(class = "ndvi-insight-card__plain", plain_text)
    )
  )
}

# Function to plot Burned Area distribution
plot_ba_timeseries <- function(train_data = NULL, test_data = NULL,
                               country_name = NULL, resolution = NULL,
                               plot_width = 15, plot_height = 8,
                               ylim_range = NULL,
                               test_start_date = NULL, test_end_date = NULL,
                               label_test = "Burned Area 2024",
                               label_train = "Burned Area 2019-2023",
                               label_mean = "Burned Area Average 2019-2023",
                               save_path = NULL,
                               filename = "BurnedArea_timeseries.png") {
  
  # Set y value range for plot
  if (is.null(ylim_range)) {
    ylim_range <- c(min(train_data$upper_ci)-10, max(train_data$upper_ci)+250)
  }
  
  # Add Month Name to dataframes
  test_data$Month_Name <- month.name[as.numeric(test_data$Month)]
  train_data$Month_Name <- month.name[as.numeric(train_data$Month)]
  
  # Make month name vector, to customize order of x axis 
  invisible(Sys.setlocale("LC_TIME", "C")) # or "English"
  month_vector <- format(seq(test_start_date, test_end_date, by="month"), "%B")
  
  # Set plot size
  options(repr.plot.width = plot_width, repr.plot.height = plot_height)
  
  # Create the plot
  ts_plot <- ggplot(NULL, aes(x = factor(Month_Name, levels=month_vector),
                              y = mean_val, group = 1)) +
    geom_point(data = train_data, size = 5,
               fill = "#2781cf") + # point-average train data
    geom_line(data = train_data) +
    geom_ribbon(data = train_data, aes(ymin = lower_ci, ymax = upper_ci),
                alpha = 0.2, fill = "#2781cf") + # Shaded CI ribbon
    geom_point(data = test_data, size = 5, shape = 23,
               fill = "#9662b3") + # point-average test data
    theme_minimal() +
    labs(
      title = paste0("Burned Area size (", ifelse(grepl("_", resolution), sub(".*_", "", resolution), resolution), "m res)"),
      x = "Month",
      y = "Mean Burned Area Size (km2)"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
      axis.text.y = element_text(size = 15), 
      axis.title.x = element_text(size = 20, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = 20, margin = margin(0, 15, 0, 0)),
      plot.title = element_text(size = 20, hjust = 0.5)
    ) +
    ylim(ylim_range) +
    geom_text(aes(x = Inf, y = ylim_range[2] - 10),
              label = label_test, inherit.aes = FALSE,
              size = 6, color = "#9662b3", hjust = 1) +
    geom_text(aes(x = Inf, y = ylim_range[2] - 40),
              label = label_train, inherit.aes = FALSE,
              size = 6, color = "#2781cf", hjust = 1) +
    geom_text(aes(x = Inf, y = ylim_range[2] - 70),
              label = label_mean, inherit.aes = FALSE,
              size = 6, color = "black", hjust = 1)
  
  # Save plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the plot to the specified location
    ggsave(filename = file.path(save_path, filename),plot = ts_plot,
           width = plot_width, height = plot_height, units = "in")
  }
  
  # Return the plot
  return(ts_plot)
}

# Interactive Plotly BA time series matching NDVI TS style: ribbon + historical mean + current year line.
plot_ba_timeseries_plotly <- function(train_data = NULL, test_data = NULL,
                                       test_year = NULL) {
  train_data <- train_data %>%
    dplyr::mutate(
      date       = as.Date(paste0(test_year, "-", Month, "-01")),
      hover_text = paste0("Month: ", format(as.Date(paste0(test_year, "-", Month, "-01")), "%b"),
                          "<br>Historic range: ", round(lower_ci, 1), " – ", round(upper_ci, 1), " km²")
    )
  test_data <- test_data %>%
    dplyr::mutate(date = as.Date(paste0(test_year, "-", Month, "-01")))

  plotly::plot_ly() %>%
    plotly::add_ribbons(
      data          = train_data,
      x             = ~date,
      ymin          = ~lower_ci,
      ymax          = ~upper_ci,
      text          = ~hover_text,
      name          = "Historic range",
      legendgroup   = "historic",
      fillcolor     = "rgba(39, 129, 207, 0.2)",
      line          = list(color = "transparent"),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
    plotly::add_lines(
      data          = train_data,
      x             = ~date, y = ~mean_val,
      name          = "Historical monthly average",
      line          = list(width = 2.5, dash = "dash", color = "#E69F00"),
      hovertemplate = "Month: %{x|%b}<br>Historical avg: %{y:.1f} km²<extra></extra>"
    ) %>%
    plotly::add_lines(
      data          = test_data,
      x             = ~date, y = ~mean_val,
      name          = paste0("Burned Area ", test_year),
      line          = list(width = 3, color = "#0072B2"),
      marker        = list(size = 7, color = "#0072B2"),
      mode          = "lines+markers",
      hovertemplate = "Month: %{x|%b %Y}<br>Burned Area: %{y:.1f} km²<extra></extra>"
    ) %>%
    plotly::layout(
      xaxis     = list(
        title      = "Month",
        tickmode   = "linear",
        dtick      = "M1",
        tickformat = "%b",
        tickangle  = -45,
        showgrid   = FALSE
      ),
      yaxis     = list(
        title     = "Burned Area (km²)",
        showgrid  = TRUE,
        gridcolor = "rgba(0,0,0,0.08)"
      ),
      template  = "plotly_white",
      hovermode = "x unified",
      legend    = list(orientation = "h", x = 1, y = 1.05,
                       xanchor = "right", yanchor = "top"),
      margin    = list(t = 50, r = 30, l = 60, b = 60)
    )
}

# Interactive Plotly daily burn activity chart — one filled line per year.
# daily_data: data frame with columns date, km2, year (character).
# selected_years: character vector of years to plot (determines legend order).
plot_ba_daily_activity <- function(daily_data, selected_years) {
  year_colors <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7")
  names(year_colors) <- as.character(selected_years[seq_len(min(length(selected_years), 4))])

  p <- plotly::plot_ly()

  for (i in seq_along(selected_years)) {
    yr    <- as.character(selected_years[i])
    col   <- year_colors[[yr]]
    ydata <- if (!is.null(daily_data)) dplyr::filter(daily_data, year == yr) else NULL

    if (is.null(ydata) || nrow(ydata) == 0) {
      p <- p %>%
        plotly::add_lines(
          x    = as.Date(NA), y = as.numeric(NA),
          name = paste0(yr, " — no fire activity detected"),
          line = list(color = col, width = 2),
          showlegend = TRUE
        )
    } else {
      ydata <- dplyr::arrange(ydata, date)
      r_val <- strtoi(substr(col, 2, 3), 16L)
      g_val <- strtoi(substr(col, 4, 5), 16L)
      b_val <- strtoi(substr(col, 6, 7), 16L)
      fill_col <- sprintf("rgba(%d,%d,%d,0.15)", r_val, g_val, b_val)
      p <- p %>%
        plotly::add_lines(
          data          = ydata,
          x             = ~date, y = ~km2,
          name          = yr,
          line          = list(color = col, width = 2.5),
          fill          = "tozeroy",
          fillcolor     = fill_col,
          hovertemplate = paste0("%{x|%d %B %Y} — %{y:.2f} km² burned<extra>", yr, "</extra>")
        )
    }
  }

  p %>%
    plotly::layout(
      xaxis     = list(title = "Date", tickformat = "%b %d", tickangle = -45, showgrid = FALSE),
      yaxis     = list(title = "Burned Area (km²) per day", showgrid = TRUE,
                       gridcolor = "rgba(0,0,0,0.08)"),
      template  = "plotly_white",
      hovermode = "x unified",
      legend    = list(orientation = "h", x = 1, y = 1.05,
                       xanchor = "right", yanchor = "top"),
      margin    = list(t = 50, r = 30, l = 60, b = 70)
    )
}

# Function to plot NDVI distribution per crop type, with confidence intervals. Assumes land_use column in train_data_grouped
plot_grouped_training_ndvi_timeseries <- function(train_data_grouped = NULL,
                                                  country_name = NULL, resolution = NULL,
                                                  plot_width = 15, plot_height = 8,
                                                  ylim_range = c(0.15, 0.75),
                                                  save_path = NULL, filename = "NDVI_grouped_timeseries.png") {
  # Set plot size
  options(repr.plot.width = plot_width, repr.plot.height = plot_height)
  
  # Create the plot
  ts_plot <- ggplot(train_data_grouped, aes(x = Month, y = mean_val, color = land_use, group = land_use, fill = land_use)) +
    geom_point(size = 5) +
    geom_line() +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, color = NA) +
    theme_minimal() +
    labs(
      title = paste0("NDVI (", ifelse(grepl("_", resolution), sub(".*_", "", resolution), resolution), "m res)"),
      x = "Month", 
      y = "Mean NDVI",
      color = "Land Use Type",
      fill = "Land Use Type"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
      axis.text.y = element_text(size = 15), 
      axis.title.x = element_text(size = 20, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = 20, margin = margin(0, 15, 0, 0)),
      plot.title = element_text(size = 20, hjust = 0.5)
    ) +
    ylim(ylim_range)
  
  # Save plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the plot to the specified location
    ggsave(filename = file.path(save_path, filename),plot = ts_plot,
           width = plot_width, height = plot_height, units = "in")
  }
  
  # Return the plot
  return(ts_plot)
}

# Function to plot 2D maps for a specific month over several years
plot_ndvi_maps <- function(data = NULL, month_to_plot = "01",
                           plot_width = 15, plot_height = 8,
                           zlim_range = c(-0.7, 0.7), ncol = 6,
                           save_path = NULL, filename = "NDVI_maps.png") {
  # Set plot size
  options(repr.plot.width = plot_width, repr.plot.height = plot_height)
  
  # Define a color map from brown to green
  brgr_colors <- colorRampPalette(c("chocolate4", "darkgoldenrod1",
                                    "darkgray", "yellowgreen", "forestgreen"))
  
  # Filter the data for the specified month
  data_filtered <- data[data$Month == month_to_plot, ]
  
  # Filter data to only the select year and the previous one
  this_and_last_year <- data_filtered %>%
    dplyr::select(Year) %>%
    unique() %>%
    pull()
  this_and_last_year <- tail(this_and_last_year, n = 4)
  data_filtered <- data_filtered %>%
    dplyr::filter(Year %in% this_and_last_year)
  
  # Generate the plot
  map_plot <- ggplot(data_filtered, aes(x = x, y = y, fill = NDVI)) +
    geom_raster() +
    scale_fill_gradientn(colors = brgr_colors(10), limits = zlim_range,
                         oob = scales::squish) +
    facet_wrap(~ YearMonth, ncol = ncol) +
    labs(
      title = paste0("NDVI development over the years - ",
                     month.name[as.numeric(month_to_plot)]),
      fill = "NDVI",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 2.5, # Keep a consistent aspect ratio
      panel.spacing = unit(1, "lines"), # Space between panels
      strip.text = element_text(size = 12), # Adjust facet labels
      axis.text.x = element_text(hjust = 1, size = 15), 
      axis.text.y = element_text(size = 15), 
      axis.title.x = element_text(size = 20, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = 20, margin = margin(0, 15, 0, 0)),
      plot.title = element_text(size = 20, hjust = 0.5)
    )
  
  # Save the plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the plot to the specified location
    ggsave(filename = file.path(save_path, filename),
           plot = map_plot, width = plot_width,
           height = plot_height, units = "in")
  }
  
  # Return the plot
  return(map_plot)
}

# Function to plot 2D maps for a specific month over several years
plot_ba_maps <- function(data = NULL, month_to_plot = "01",
                         plot_width = NULL, plot_height = NULL,
                         zlim_range = c(0, 1), ncol = NULL, n_years = 4,
                         save_path = NULL, filename = "BA_maps.png") {
  # Define a color map from brown to green
  colors <- c("lightgrey", "darkred")

  # Filter the data for the specified month
  data_filtered <- data[data$Month == month_to_plot, ]
  data_filtered$BurnedArea <- factor(data_filtered$BurnedArea)

  # Select the last n_years available
  all_years <- sort(unique(data_filtered$Year))
  selected_years <- tail(all_years, n = n_years)
  n_shown <- length(selected_years)
  data_filtered <- data_filtered %>%
    dplyr::filter(Year %in% selected_years)

  # Compute layout dimensions dynamically based on number of panels
  if (is.null(ncol))        ncol        <- if (n_shown <= 3) n_shown else 2
  if (is.null(plot_width))  plot_width  <- if (n_shown <= 3) 8 * n_shown else 16
  if (is.null(plot_height)) plot_height <- if (n_shown <= 2) 8 else if (n_shown == 3) 8 else 16
  options(repr.plot.width = plot_width, repr.plot.height = plot_height)
  
  # Summarize area sizes and percentages per Year
  area_labels <- data_filtered %>%
    distinct(Year, BurnedArea_Size, Percentage_Burned) %>%
    mutate(label = ifelse(BurnedArea_Size <= 1, 
                          paste0(Year, "\nBurned Area: 0 km² (0%)"), 
                          paste0(Year, "\nBurned Area: ", round(BurnedArea_Size, 1), " km² (", round(Percentage_Burned, 1), "%)"))) %>%
    select(Year, label) %>%
    tibble::deframe()
  
  ba_change <- data_filtered %>%
    distinct(Year, BurnedArea_Size) %>%
    arrange(Year) %>%
    mutate(changeBurnedArea = ifelse(BurnedArea_Size <= 1 & lag(BurnedArea_Size) <= 1, 
                                     0,
                                     ifelse(BurnedArea_Size <= 1 & lag(BurnedArea_Size) > 1, 
                                            0 - lag(BurnedArea_Size),
                                            ifelse(BurnedArea_Size > 1 & lag(BurnedArea_Size) <= 1,
                                                   BurnedArea_Size - 0,
                                                   BurnedArea_Size - lag(BurnedArea_Size))))) %>%
           # changeBurnedArea = BurnedArea_Size - lag(BurnedArea_Size)) %>%
    select(changeBurnedArea) %>%
    tibble::deframe()
  BurnedArea_change <- round(ba_change[length(ba_change)], 1)
  year_range_label <- if (n_shown >= 2) paste0(min(selected_years), "–", max(selected_years)) else as.character(selected_years)
  change_label <- ifelse(BurnedArea_change == 0, "No difference",
                         paste0(abs(BurnedArea_change), " km² burned ", ifelse(BurnedArea_change > 0, "more", "less")))

  # Generate the plot
  map_plot <- ggplot(data_filtered, aes(x = x, y = y, fill = BurnedArea)) +
    geom_raster() +
    scale_fill_manual(values = colors,
                      labels = c("Unburned", "Burned")) +
    facet_wrap(~ Year, ncol = ncol, labeller = labeller(Year = area_labels)) +
    labs(
      title = paste0("Burned Area in ", month.name[as.numeric(month_to_plot)],
                     " (", year_range_label, ") — year-on-year: ", change_label),
      fill = "Burned Area",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 1.6, # Keep a consistent aspect ratio
      panel.spacing = unit(2, "lines"), # Space between panels
      strip.text = element_text(size = 15), # Adjust facet labels
      axis.text.x = element_text(hjust = 1, size = 15), 
      axis.text.y = element_text(size = 15), 
      axis.title.x = element_text(size = 20, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = 20, margin = margin(0, 15, 0, 0)),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      plot.title = element_text(size = 20, hjust = 0.5)
    )
  
  # Save the plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the plot to the specified location
    ggsave(filename = file.path(save_path, filename),
           plot = map_plot, width = plot_width,
           height = plot_height, units = "in")
  }
  
  # Return the plot
  return(map_plot)
}

# Function to plot delta NDVI in a 2D map
plot_delta_ndvi_map <- function(data = NULL, month_to_plot = "01",
                                plot_width = 15, plot_height = 8,
                                zlim_range = c(-.25, .25),
                                save_path = NULL,
                                filename = "deltaNDVI_maps.png") {
  # Set plot size
  options(repr.plot.width = plot_width, repr.plot.height = plot_height)
  
  # Define a color map from brown to green
  brgr_colors <- colorRampPalette(c("darkred", "firebrick1",
                                    "darkgray", "yellowgreen", "darkgreen"))
  
  # Filter the data for the specified month
  data_filtered <- data[data$Month == month_to_plot, ]
  
  # Generate the plot
  map_plot <- ggplot(data_filtered, aes(x = x, y = y, fill = delta_ndvi)) +
    geom_raster() +
    scale_fill_gradientn(colors = brgr_colors(10), limits = zlim_range,
                         oob = scales::squish) +
    labs(
      title = paste0("Delta NDVI - ",
                     month.name[as.numeric(month_to_plot)]),
      fill = "Delta NDVI",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 2.5, # Keep a consistent aspect ratio
      panel.spacing = unit(1, "lines"), # Space between panels
      strip.text = element_text(size = 12), # Adjust facet labels
      axis.text.x = element_text(hjust = 1, size = 15), 
      axis.text.y = element_text(size = 15), 
      axis.title.x = element_text(size = 20, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = 20, margin = margin(0, 15, 0, 0)),
      plot.title = element_text(size = 20, hjust = 0.5)
    )
  
  # Save the plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the plot to the specified location
    ggsave(filename = file.path(save_path, filename),
           plot = map_plot, width = plot_width,
           height = plot_height, units = "in")
  }
  
  # Return the plot
  return(map_plot)
}

plot_geojsons_from_a_folder <- function(folder_path, save_path = NULL, filename = NULL, basemap = "OpenStreetMap") {
  
  message(paste("Plotting GeoJSON files from folder:", folder_path))
  # Get a list of all GeoJSON files in the folder
  geojson_files <- list.files(folder_path, pattern = "\\.geojson$", full.names = TRUE)
  
  # Check if there are any GeoJSON files in the folder
  if (length(geojson_files) == 0) {
    stop("No GeoJSON files found in the specified folder.")
  }
  
  # Extract filenames; stable order for overlay group names
  landuse_types <- tools::file_path_sans_ext(basename(geojson_files))
  ord <- order(landuse_types)
  geojson_files <- geojson_files[ord]
  landuse_types <- landuse_types[ord]
  
  # AoI name for the geometry hot path; folder is .../LandUse/{aoi}/S2_10m_LULC_2023.
  # Probe the API once (avoids a per-class 2s timeout when it is down).
  aoi_name <- basename(dirname(folder_path))
  lc_api_up <- api_is_available()

  # Pre-calculate total study area (Improvement 7: for percentage in tooltip)
  total_study_area_ha <- 0
  for (file in geojson_files) {
    gj <- read_landcover_geometry(
      file, aoi_name, geojson_stem_to_class_key(tools::file_path_sans_ext(basename(file))),
      api_available = lc_api_up
    )
    gj <- sf::st_transform(gj, crs = 4326)
    total_study_area_ha <- total_study_area_ha + sum(as.numeric(sf::st_area(gj)) / 10000)
  }
  
  pal_named <- land_cover_class_colors()
  hex_by_stem <- vapply(landuse_types, function(stem) {
    k <- geojson_stem_to_class_key(stem)
    if (is.na(k) || !k %in% names(pal_named)) {
      return("#999999")
    }
    unname(pal_named[[k]])
  }, character(1))
  colors <- leaflet::colorFactor(palette = hex_by_stem, domain = landuse_types)
  
  # Create a leaflet map with the specified basemap
  map <- leaflet() %>%
    addProviderTiles(providers[[basemap]])
  
  bbox_by_stem <- vector("list", length(landuse_types))
  names(bbox_by_stem) <- landuse_types
  
  # Loop through each GeoJSON file and add it to the map
  for (i in seq_along(geojson_files)) {
    file <- geojson_files[i]
    landuse_type <- landuse_types[i]
    
    # Read the GeoJSON file (API hot path, file fallback)
    geojson_data <- read_landcover_geometry(file, aoi_name,
                                            geojson_stem_to_class_key(landuse_type),
                                            api_available = lc_api_up)

    # Transform the GeoJSON data to WGS 84 (EPSG:4326)
    geojson_data <- sf::st_transform(geojson_data, crs = 4326)
    bbox_by_stem[[landuse_type]] <- sf::st_bbox(geojson_data)
    
    # layerId + group: Shiny Leaflet shape_click identifies the land-cover layer
    lab_short <- geojson_stem_to_class_key(landuse_type)
    pop_lab <- if (!is.na(lab_short) && lab_short %in% names(land_cover_class_legend_labels())) {
      unname(land_cover_class_legend_labels()[[lab_short]])
    } else {
      landuse_type
    }
    
    # Improvement 7: Format tooltip with class name, hectares (with commas), and percentage
    areas_ha <- as.numeric(sf::st_area(geojson_data)) / 10000
    pct <- if (total_study_area_ha > 0) round((areas_ha / total_study_area_ha) * 100) else 0
    popup_text <- paste0(
      htmltools::htmlEscape(pop_lab),
      " — ",
      format(round(areas_ha), big.mark = ","),
      " ha (",
      pct,
      "% of study area)"
    )
    
    map <- map %>%
      addPolygons(
        data         = geojson_data,
        color        = colors(landuse_type),
        weight       = 2,
        opacity      = 0.6,
        fillOpacity  = 0.3,
        group        = landuse_type,
        layerId      = rep(landuse_type, nrow(geojson_data)),
        popup        = popup_text
      )
  }
  
  # Save the plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the map as an HTML file
    saveWidget(map, file.path(save_path, filename), selfcontained = TRUE)
  }
  
  # Return map plus per-layer bboxes for leafletProxy fitBounds (Plotly -> map)
  return(list(map = map, bbox_by_stem = bbox_by_stem))
}

#' Map human legend label to GeoJSON stem using class keys (see bbox_by_stem names).
#' @noRd
landcover_label_to_stem <- function(label, bbox_by_stem) {
  if (is.null(label) || !nzchar(label)) {
    return(NA_character_)
  }
  if (is.null(bbox_by_stem) || !length(bbox_by_stem)) {
    return(NA_character_)
  }
  labs <- land_cover_class_legend_labels()
  for (k in names(labs)) {
    if (identical(unname(labs[[k]]), label)) {
      for (st in names(bbox_by_stem)) {
        if (identical(geojson_stem_to_class_key(st), k)) {
          return(st)
        }
      }
      return(NA_character_)
    }
  }
  NA_character_
}

#' Pan combined NDVI+map Plotly figure to a WGS84 bbox (\code{mapbox*.center}/\code{zoom}; legacy \code{geo*}).
#' @noRd
lc_plotly_relayout_geo_bbox <- function(session, plotly_output_id, plot_obj, bbox) {
  if (is.null(bbox)) {
    return(invisible(NULL))
  }
  xmin <- bbox[["xmin"]]
  xmax <- bbox[["xmax"]]
  ymin <- bbox[["ymin"]]
  ymax <- bbox[["ymax"]]
  if (any(!is.finite(c(xmin, xmax, ymin, ymax)))) {
    return(invisible(NULL))
  }
  bnds <- lc_mapbox_bounds_from_bbox(xmin, xmax, ymin, ymax)
  cz <- lc_mapbox_center_zoom_from_bounds(bnds$west, bnds$east, bnds$south, bnds$north, zoom_pad = 0.35)
  pb <- plotly::plotly_build(plot_obj)
  lay <- pb$x$layout
  mb_keys <- grep("^mapbox[0-9]*$", names(lay), value = TRUE)
  geo_keys <- grep("^geo[0-9]*$", names(lay), value = TRUE)
  rl <- list()
  if (length(mb_keys)) {
    gk <- mb_keys[length(mb_keys)]
    rl[[paste0(gk, ".center")]] <- cz$center
    rl[[paste0(gk, ".zoom")]] <- cz$zoom
  } else if (length(geo_keys)) {
    gk <- geo_keys[length(geo_keys)]
    rl[[paste0(gk, ".lonaxis.range")]] <- c(bnds$west, bnds$east)
    rl[[paste0(gk, ".lataxis.range")]] <- c(bnds$south, bnds$north)
  } else {
    rl[["mapbox2.center"]] <- cz$center
    rl[["mapbox2.zoom"]] <- cz$zoom
  }
  prx <- plotly::plotlyProxy(plotly_output_id, session)
  plotly::plotlyProxyInvoke(prx, "relayout", rl)
  invisible(NULL)
}

#' Sync Land Cover plot/map clicks with Plotly line emphasis (lc_ndvi_plot_output).
#' @noRd
lc_landcover_emphasize_plotly_traces <- function(session, plotly_output_id, plot_obj, highlight_label = NULL) {
  if (is.null(plot_obj)) {
    return(invisible(NULL))
  }
  proxy <- plotly::plotlyProxy(plotly_output_id, session)
  pb <- plotly::plotly_build(plot_obj)
  d <- pb$x$data
  n <- length(d)
  if (n == 0L) {
    return(invisible(NULL))
  }
  for (ii in seq_len(n)) {
    idx0 <- ii - 1L
    tr <- d[[ii]]
    ttype <- tr[["type"]]
    if (is.null(ttype)) {
      ttype <- ""
    } else {
      ttype <- as.character(ttype)[1]
    }
    if (ttype %in% c("scattermapbox", "scattergeo")) {
      next
    }
    nm <- tr$name
    if (is.null(nm) || length(nm) == 0L) {
      nm <- ""
    } else {
      nm <- as.character(nm)[1]
    }
    is_ribbon <- grepl("^Historical range", nm)
    if (is_ribbon) {
      op <- if (is.null(highlight_label)) 1 else 0.42
    } else {
      op <- if (is.null(highlight_label)) {
        1
      } else if (identical(nm, highlight_label)) {
        1
      } else {
        0.18
      }
    }
    plotly::plotlyProxyInvoke(proxy, "restyle", list(opacity = op), idx0)
  }
  invisible(NULL)
}

plot_ba_geojson_from_a_folder <- function(input_file_path, data_dir = data_dir, country = country_name, 
                                          save_path = NULL, filename = NULL, basemap = "OpenStreetMap") {
  
  message(paste("Plotting GeoJSON file from folder:", input_file_path))
  # Check if the GeoJSON file exists in the folder
  if (!file.exists(input_file_path)) {
    stop("No GeoJSON files found in the specified folder.")
  }
  
  # Load and transform AoI file to plot in case there is no burned area
  aoi_files <- list.files(file.path(data_dir, "AoI"), pattern = paste0("AoI_.*", country, ".*\\.geojson$"))
  if (length(aoi_files) == 0) {
    stop("No Area of Interest file found for the selected country.")
  }
  aoi_shape <- read_aoi_geometry(file.path(data_dir, "AoI", aoi_files[[1]]), country)
  aoi_shape <- sf::st_transform(aoi_shape, crs = 4326)

  # Create a leaflet map with the specified basemap
  map <- leaflet() %>%
    addProviderTiles(providers[[basemap]]) %>%
    # Always plot AOI
    addPolygons(data = aoi_shape,
                color = "green",
                weight = 2,
                opacity = 0.5,
                fillOpacity = 0,    # transparent fill
                group = "AOI")
  
  # Read and transform the GeoJSON file to WGS 84 (EPSG:4326)
  geojson_data <- sf::st_read(input_file_path)
  geojson_data <- sf::st_transform(geojson_data, crs = 4326)
  
  # Identify the date column (first non-geometry column)
  date_col <- setdiff(names(geojson_data), attr(geojson_data, "sf_column"))  
  
  # Filter only burned patches (values > 0)
  burned_area_data <- geojson_data[geojson_data[[date_col]] > 0, ]  
  burned_area_data <- st_union(burned_area_data)
  
  if (!sf::st_is_empty(burned_area_data)) {
    # Add the GeoJSON data to the map with a different color
    map <- map %>%
      addPolygons(data = burned_area_data, 
                  color = "#ED022A", 
                  weight = 2, 
                  opacity = 0.6, 
                  fillOpacity = 0.3,
                  group = "Burned Area")
  }
  
  # Add the layers control and legend to the map
  map <- map %>%
    addLayersControl(
      overlayGroups = c("Burned Area"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend("bottomright", 
              colors = "#ED022A", 
              labels = "Burned Area", 
              title = "Legend",
              opacity = 1)
  
  # Save the plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the map as an HTML file
    saveWidget(map, file.path(save_path, filename), selfcontained = TRUE)
  }
  
  # Return the map
  return(map)
}

# Build interactive Leaflet map for a single month's burned area footprint.
# geojson_path: path to the GeoJSON produced by generate_ba_export().
# country: e.g. "Zambia_Mponda". year: 4-digit integer. month_num: 1-12.
build_ba_monthly_leaflet <- function(geojson_path = NULL, data_dir = NULL,
                                     country = NULL, year = NULL, month_num = NULL) {
  aoi_files <- list.files(file.path(data_dir, "AoI"),
                          pattern = paste0("AoI.*", country, ".*\\.geojson$"))
  if (length(aoi_files) == 0) stop("No Area of Interest file found for the selected country.")
  aoi_shape  <- read_aoi_geometry(file.path(data_dir, "AoI", aoi_files[[1]]), country)
  aoi_shape  <- sf::st_transform(aoi_shape, crs = 4326)
  aoi_bounds <- sf::st_bbox(aoi_shape)

  month_label <- format(as.Date(paste0(year, "-", sprintf("%02d", month_num), "-01")), "%B %Y")

  m <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap,      group = "Street Map") %>%
    addProviderTiles(providers$Esri.WorldImagery,  group = "Satellite") %>%
    addPolygons(data = aoi_shape, color = "#1B5E20", weight = 2,
                opacity = 0.9, fillOpacity = 0, label = "Study area boundary",
                group = "Study Area") %>%
    fitBounds(aoi_bounds[[1]], aoi_bounds[[2]], aoi_bounds[[3]], aoi_bounds[[4]])

  has_burned <- FALSE

  if (!is.null(geojson_path) && file.exists(geojson_path)) {
    geojson_data <- sf::st_read(geojson_path, quiet = TRUE)
    geojson_data <- sf::st_transform(geojson_data, crs = 4326)
    date_col     <- setdiff(names(geojson_data), attr(geojson_data, "sf_column"))[1]
    burned       <- geojson_data[!is.na(geojson_data[[date_col]]) & geojson_data[[date_col]] > 0, ]

    if (nrow(burned) > 0) {
      has_burned <- TRUE
      burned$burn_date <- format(
        as.Date(burned[[date_col]] - 1, origin = paste0(year, "-01-01")), "%B %d, %Y"
      )
      m <- m %>%
        addPolygons(
          data        = burned,
          fillColor   = "#E25822", fillOpacity = 0.7,
          color       = "#8B2500", weight = 1.5,
          label       = lapply(paste0(
            "<b>Burned on:</b> ", burned$burn_date, "<br>",
            "<b>Area:</b> 0.25 km² per cell"
          ), htmltools::HTML),
          labelOptions = labelOptions(style = list("font-size" = "12px"), direction = "auto"),
          group = "Burned Area"
        ) %>%
        addLegend("bottomright", colors = "#E25822", labels = "Burned Area",
                  title = month_label, opacity = 0.85)
    }
  }

  if (!has_burned) {
    m <- m %>%
      addControl(
        html = paste0(
          '<div style="background:white;padding:10px 14px;border-radius:4px;',
          'border:1px solid #ddd;font-size:13px;color:#555;">',
          'No burned area detected in ', month_label, ' for this area.</div>'
        ),
        position = "topright"
      )
  }

  m %>% addLayersControl(
    baseGroups    = c("Street Map", "Satellite"),
    overlayGroups = c("Study Area", "Burned Area"),
    options       = layersControlOptions(collapsed = FALSE),
    position      = "topleft"
  )
}

# Build interactive Leaflet map showing fire return period across all available years.
# Returns a list: map (leaflet), years_label (character), n_years (integer).
build_ba_frp_leaflet <- function(data_dir = NULL, country = NULL, resolution = NULL) {
  data_path  <- file.path(data_dir, "BurnedArea", country, paste0(resolution, "m_resolution"))
  cache_path <- file.path(cache_dir, paste0(country, "_", resolution, "_frp.rds"))

  aoi_files <- list.files(file.path(data_dir, "AoI"),
                          pattern = paste0("AoI.*", country, ".*\\.geojson$"))
  if (length(aoi_files) == 0) stop("No Area of Interest file found for the selected country.")
  aoi_shape  <- read_aoi_geometry(file.path(data_dir, "AoI", aoi_files[[1]]), country)
  aoi_shape  <- sf::st_transform(aoi_shape, crs = 4326)
  aoi_bounds <- sf::st_bbox(aoi_shape)

  # HOT PATH: pre-computed FRP polygons + year-range metadata from the API. The
  # GeoJSON carries return period in `frp_years`; the year range comes from the
  # response metadata. Returns early; the TIF computation below is the fallback
  # and runs unchanged when the API is unavailable.
  frp_api <- try_api_geometry(
    endpoint        = "/api/v1/geometry/fire-return-period",
    params          = list(aoi = country),
    return_metadata = TRUE
  )
  if (!is.null(frp_api) && inherits(frp_api$geometry, "sf") &&
      "frp_years" %in% names(frp_api$geometry) && nrow(frp_api$geometry) > 0 &&
      !is.null(frp_api$metadata$n_years)) {
    frp_sf <- sf::st_transform(frp_api$geometry, 4326)
    frp_sf$return_period <- frp_sf$frp_years
    years_label <- paste(frp_api$metadata$year_start, "to", frp_api$metadata$year_end)
    n_years     <- frp_api$metadata$n_years

    pal <- leaflet::colorNumeric(
      palette  = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
      domain   = c(1, max(frp_sf$return_period, na.rm = TRUE)),
      na.color = "transparent"
    )
    rp_vals <- round(frp_sf$return_period, 1)
    m <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap,     group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addPolygons(
        data         = frp_sf,
        fillColor    = ~pal(return_period), fillOpacity = 0.8,
        color        = NA, weight = 0,
        label        = lapply(paste0(
          "<b>Burns approximately every ", rp_vals, " year",
          ifelse(rp_vals == 1, "", "s"), "</b>"
        ), htmltools::HTML),
        labelOptions = labelOptions(style = list("font-size" = "12px"), direction = "auto"),
        group        = "Fire Return Period"
      ) %>%
      addPolygons(data = aoi_shape, color = "#1B5E20", weight = 2,
                  opacity = 0.9, fillOpacity = 0, group = "Study Area") %>%
      fitBounds(aoi_bounds[[1]], aoi_bounds[[2]], aoi_bounds[[3]], aoi_bounds[[4]]) %>%
      addLegend("bottomright", pal = pal, values = frp_sf$return_period,
                title   = "Fire Return Period<br>(years)", opacity = 0.85,
                labFormat = labelFormat(suffix = " yrs", digits = 1)) %>%
      addLayersControl(
        baseGroups    = c("Street Map", "Satellite"),
        overlayGroups = c("Study Area", "Fire Return Period"),
        options       = layersControlOptions(collapsed = FALSE),
        position      = "topleft"
      )

    return(list(map = m, years_label = years_label, n_years = n_years))
  }

  ba_files <- list.files(data_path, pattern = "\\.tif$", full.names = TRUE)
  if (length(ba_files) == 0) {
    stop("No burned area data found for this area and resolution. Please try a different selection.")
  }
  n_files_now <- length(ba_files)

  polys_rp    <- NULL
  years_label <- NULL
  n_years     <- NULL

  if (file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    if (isTRUE(cached$n_files == n_files_now)) {
      polys_rp    <- cached$data
      years_label <- cached$years_label
      n_years     <- cached$n_years
    }
  }

  if (is.null(polys_rp)) {
    years_available <- sort(unique(sub("^(\\d{4})-.*", "\\1", basename(ba_files))))
    n_years         <- length(years_available)
    years_label     <- paste(min(years_available), "to", max(years_available))

    yearly_burned <- lapply(years_available, function(yr) {
      yr_files <- ba_files[grepl(paste0("^", yr, "-"), basename(ba_files))]
      if (length(yr_files) == 0) return(NULL)
      yr_max <- terra::app(terra::rast(yr_files), fun = max, na.rm = TRUE)
      terra::ifel(yr_max > 0, 1, 0)
    })
    yearly_burned <- Filter(Negate(is.null), yearly_burned)

    burn_count    <- terra::app(terra::rast(yearly_burned), fun = sum, na.rm = TRUE)
    return_period <- terra::ifel(burn_count > 0, n_years / burn_count, NA)
    return_period <- terra::project(return_period, "EPSG:4326")

    polys_rp <- terra::as.polygons(return_period) |> sf::st_as_sf()
    names(polys_rp)[1] <- "return_period"
    polys_rp <- sf::st_transform(polys_rp, 4326)
    polys_rp <- polys_rp[!is.na(polys_rp$return_period), ]

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    saveRDS(list(data = polys_rp, n_files = n_files_now,
                 n_years = n_years, years_label = years_label), cache_path)
  }

  if (nrow(polys_rp) == 0) stop("No burned pixels found for this selection.")

  pal <- leaflet::colorNumeric(
    palette  = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
    domain   = c(1, max(polys_rp$return_period, na.rm = TRUE)),
    na.color = "transparent"
  )

  rp_vals <- round(polys_rp$return_period, 1)
  m <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap,     group = "Street Map") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addPolygons(
      data         = polys_rp,
      fillColor    = ~pal(return_period), fillOpacity = 0.8,
      color        = NA, weight = 0,
      label        = lapply(paste0(
        "<b>Burns approximately every ", rp_vals, " year",
        ifelse(rp_vals == 1, "", "s"), "</b>"
      ), htmltools::HTML),
      labelOptions = labelOptions(style = list("font-size" = "12px"), direction = "auto"),
      group        = "Fire Return Period"
    ) %>%
    addPolygons(data = aoi_shape, color = "#1B5E20", weight = 2,
                opacity = 0.9, fillOpacity = 0, group = "Study Area") %>%
    fitBounds(aoi_bounds[[1]], aoi_bounds[[2]], aoi_bounds[[3]], aoi_bounds[[4]]) %>%
    addLegend("bottomright", pal = pal, values = polys_rp$return_period,
              title   = "Fire Return Period<br>(years)", opacity = 0.85,
              labFormat = labelFormat(suffix = " yrs", digits = 1)) %>%
    addLayersControl(
      baseGroups    = c("Street Map", "Satellite"),
      overlayGroups = c("Study Area", "Fire Return Period"),
      options       = layersControlOptions(collapsed = FALSE),
      position      = "topleft"
    )

  list(map = m, years_label = years_label, n_years = n_years)
}

# Function to plot delta NDVI on a Leaflet map
plot_delta_ndvi_streetview <- function(data = NULL, month_to_plot = "01",
                                       zlim_range = c(-.25, .25),
                                       basemap = "OpenStreetMap",
                                       save_path = NULL, filename = "deltaNDVI_heatmap.html") {
  
  if (is.null(data)) stop("The input data cannot be NULL.")
  
  # Filter the data for the specified month
  data_filtered <- data %>%
    filter(Month == month_to_plot)
  
  # Check if there is data for the specified month
  if (nrow(data_filtered) == 0) stop(paste("No data available for month:", month_to_plot))
  
  # Define a color palette for Delta NDVI values
  brgr_colors <- colorNumeric(
    palette = c("darkred", "firebrick1", "darkgray", "yellowgreen", "darkgreen"),
    domain = zlim_range,
    na.color = NA
  )
  
  # Add squishing to clamp out-of-bound values
  data_filtered <- data_filtered %>%
    mutate(delta_ndvi_clamped = scales::squish(delta_ndvi, zlim_range))  # Clamp values
  
  # Create the Leaflet map
  map <- leaflet(data_filtered) %>%
    addProviderTiles(providers[[basemap]]) %>%  # Add the basemap
    addCircleMarkers(
      lng = ~x,  # Longitude
      lat = ~y,  # Latitude
      radius = 4,  # Marker size
      color = ~brgr_colors(delta_ndvi_clamped),  # Use clamped values for color
      fillOpacity = 0.7,  # Circle transparency
      popup = ~paste0(
        "<b>Delta NDVI:</b> ", round(delta_ndvi, 2), "<br>",
        "<b>Longitude:</b> ", round(x, 2), "<br>",
        "<b>Latitude:</b> ", round(y, 2)
      )  # Add popup for each point
    ) %>%
    addLegend(
      "bottomright",  # Position of the legend
      pal = brgr_colors,  # Use the same color palette
      values = zlim_range,  # Range of Delta NDVI
      title = "Delta NDVI",
      opacity = 1
    )
  
  # Save the plot if save_path is provided
  if (!is.null(save_path)) {
    # Ensure the save directory exists
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    
    # Save the map as an HTML file
    saveWidget(map, file.path(save_path, filename), selfcontained = TRUE)
  }
  
  return(map)  # Return the Leaflet map
}

# Compute per-pixel annual mean NDVI delta between two years
compute_annual_ndvi_change <- function(year_a, year_b, country_name, resolution, data_dir) {
  data_path <- file.path(data_dir, "NDVI", country_name, paste0(resolution, "m_resolution"))
  aoi_path  <- file.path(data_dir, "AoI")
  aoi_files <- get_filenames(aoi_path, "AoI", ".geojson", country_name)
  aoi_proj    <- get_aoi_vector(aoi_files[[1]], aoi_path, "EPSG:4326")

  ndvi_files <- get_filenames(data_path, "NDVI", ".tif", country_name)
  files_df   <- get_filename_df(ndvi_files)

  load_year_mean <- function(yr) {
    yr_files <- files_df[files_df$year == as.integer(yr), ]
    if (nrow(yr_files) == 0L) stop("No NDVI files for year ", yr)
    rast <- get_ndvi_raster(yr_files$filenames, data_path, "EPSG:4326", yr_files$dates, aoi_proj)
    terra::mean(rast, na.rm = TRUE)
  }

  rast_a <- load_year_mean(year_a)
  rast_b <- load_year_mean(year_b)
  delta_rast <- rast_b - rast_a

  df_a     <- as.data.frame(rast_a,    xy = TRUE); names(df_a)[3]     <- "ndvi_a"
  df_b     <- as.data.frame(rast_b,    xy = TRUE); names(df_b)[3]     <- "ndvi_b"
  df_delta <- as.data.frame(delta_rast, xy = TRUE); names(df_delta)[3] <- "delta"

  delta_df <- merge(df_a, df_b, by = c("x", "y"))
  delta_df <- merge(delta_df, df_delta, by = c("x", "y"))
  delta_df <- delta_df[!is.na(delta_df$delta), ]

  pos_rast <- terra::ifel(delta_rast > 0, 1, NA)
  neg_rast <- terra::ifel(delta_rast < 0, 1, NA)
  valid_rast <- terra::ifel(!is.na(delta_rast), 1, NA)
  pos_km2  <- round(sum(terra::expanse(pos_rast, unit = "km"), na.rm = TRUE), 1)
  neg_km2  <- round(sum(terra::expanse(neg_rast, unit = "km"), na.rm = TRUE), 1)
  total_km2 <- round(sum(terra::expanse(valid_rast, unit = "km"), na.rm = TRUE), 1)

  list(delta_df = delta_df, pos_km2 = pos_km2, neg_km2 = neg_km2,
       total_km2 = total_km2,
       year_a = year_a, year_b = year_b, country_name = country_name)
}

# Render annual NDVI change as a Leaflet map with diverging colour scale
plot_annual_ndvi_leaflet <- function(result) {
  delta_df <- result$delta_df
  year_a   <- result$year_a
  year_b   <- result$year_b

  quants   <- quantile(delta_df$delta, c(0.02, 0.98), na.rm = TRUE)
  max_abs  <- max(abs(quants), na.rm = TRUE)
  if (max_abs == 0 || is.na(max_abs)) max_abs <- 0.1

  pal <- leaflet::colorNumeric(
    palette  = c("#8B0000", "#CC3300", "#FFFFFF", "#66BB6A", "#1B5E20"),
    domain   = c(-max_abs, max_abs),
    na.color = "transparent"
  )

  leaflet::leaflet(delta_df) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~x, lat = ~y,
      radius = 3, stroke = FALSE, fillOpacity = 0.8,
      fillColor = ~pal(scales::squish(delta, c(-max_abs, max_abs))),
      popup = ~paste0(
        "<b>Delta NDVI:</b> ", round(delta, 3), "<br>",
        "<b>Baseline NDVI (", year_a, "):</b> ", round(ndvi_a, 3), "<br>",
        "<b>Comparison NDVI (", year_b, "):</b> ", round(ndvi_b, 3)
      )
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      pal      = pal,
      values   = ~scales::squish(delta, c(-max_abs, max_abs)),
      title    = paste0("Annual NDVI Change<br>", year_a, " → ", year_b),
      opacity  = 0.8
    )
}

# Plain-language legend variant for annual NDVI change. Defined after the
# original renderer so this version is the one used by the app.
plot_annual_ndvi_leaflet <- function(result) {
  delta_df <- result$delta_df
  year_a   <- result$year_a
  year_b   <- result$year_b

  quants   <- quantile(delta_df$delta, c(0.02, 0.98), na.rm = TRUE)
  max_abs  <- max(abs(quants), na.rm = TRUE)
  if (max_abs == 0 || is.na(max_abs)) max_abs <- 0.1

  pal <- leaflet::colorNumeric(
    palette  = c("#8B0000", "#CC3300", "#FFFFFF", "#66BB6A", "#1B5E20"),
    domain   = c(-max_abs, max_abs),
    na.color = "transparent"
  )

  leaflet::leaflet(delta_df) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~x, lat = ~y,
      radius = 3, stroke = FALSE, fillOpacity = 0.8,
      fillColor = ~pal(scales::squish(delta, c(-max_abs, max_abs))),
      popup = ~paste0(
        "<b>Delta NDVI:</b> ", round(delta, 3), "<br>",
        "<b>Baseline NDVI (", year_a, "):</b> ", round(ndvi_a, 3), "<br>",
        "<b>Comparison NDVI (", year_b, "):</b> ", round(ndvi_b, 3)
      )
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      colors   = c(pal(-max_abs), pal(0), pal(max_abs)),
      labels   = c("Large loss", "No change", "Large gain"),
      title    = paste0("Annual vegetation change<br>", year_a, " to ", year_b),
      opacity  = 0.8
    )
}
