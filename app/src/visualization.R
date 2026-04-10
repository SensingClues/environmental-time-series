
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
    Bare_ground        = "#EDE9E4",
    Built_Area         = "#ED022A",
    Crops              = "#FFDB5C",
    Flooded_vegetation = "#87D19E",
    Rangeland          = "#A7D282",
    Trees              = "#358221",
    Water              = "#1A5BAB"
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
  pal_named <- land_cover_class_colors()
  bbox_by_stem <- vector("list", length(landuse_types))
  names(bbox_by_stem) <- landuse_types
  p <- lc_plot_mapbox_init_osm()
  all_xmin <- all_xmax <- all_ymin <- all_ymax <- numeric(0)
  aoi_bbox_for_frame <- NULL
  aoi_ok <- !is.null(aoi_sf) && inherits(aoi_sf, "sf") && nrow(aoi_sf) > 0L &&
    !all(sf::st_is_empty(sf::st_geometry(aoi_sf)))
  if (isTRUE(aoi_ok)) {
    aoi_wgs <- lc_simplify_wgs84_for_plot(sf::st_transform(aoi_sf, 4326))
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
  for (i in seq_along(geojson_files)) {
    stem <- landuse_types[i]
    geojson_data <- sf::st_read(geojson_files[i], quiet = TRUE)
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
    htxt <- paste0(
      "<b>", htmltools::htmlEscape(pop_lab), "</b><br>",
      "Area (hectares): ", round(area_ha, 3)
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

# Shiny UI: tags$h4 title + tags$span circled-i with native multiline tooltip above plotlyOutput.
ndvi_anomaly_titles_ui <- function(resolution = NULL) {
  res_suffix <- ndvi_resolution_title_suffix(resolution)
  tip <- ndvi_anomaly_help_tooltip_text()
  shiny::tags$div(
    class = "ndvi-anomaly-title-wrap",
    shiny::tags$h4(
      class = "ndvi-anomaly-title-h4",
      paste0("NDVI Anomaly", res_suffix),
      shiny::tags$span(
        title = tip,
        class = "ndvi-help-icon",
        "ⓘ"
      )
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
ndvi_insight_wilcox_card_ui <- function(stats) {
  if (is.null(stats)) return(NULL)
  p <- stats$wilcox_p
  med <- stats$wilcox_median
  if (is.na(p)) {
    main <- "Not enough data for this summary"
    col <- "#555555"
    p_lab <- "p-value: N/A"
  } else if (!is.na(p) && p < 0.05 && !is.na(med) && med > 0) {
    main <- "Above normal vegetation"
    col <- "#009E73"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
  } else if (!is.na(p) && p < 0.05 && !is.na(med) && med < 0) {
    main <- "Below normal vegetation"
    col <- "#D55E00"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
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
      p_lab
    )
  )
}

#' Shiny UI: Long-Term Trend card (Seasonal Mann–Kendall + Sen slope sign).
ndvi_insight_smk_card_ui <- function(stats) {
  if (is.null(stats)) return(NULL)
  p <- stats$smk_p
  slope <- stats$sen_slope
  n_m <- stats$smk_n_months
  if (is.null(n_m) || !is.numeric(n_m)) n_m <- NA_integer_
  smk_min_months <- 60L
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
  } else if (is.na(p) || is.na(slope)) {
    main <- "Long-term trend cannot be assessed from this series"
    col <- "#555555"
    p_lab <- "p-value: N/A"
  } else if (p < 0.05 && slope > 0) {
    main <- "Significant increasing trend"
    col <- "#009E73"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
  } else if (p < 0.05 && slope < 0) {
    main <- "Significant decreasing trend"
    col <- "#D55E00"
    p_lab <- paste0("p-value: ", format(round(p, 3), nsmall = 3))
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
      p_lab
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
plot_ndvi_anomaly <- function(train_ndvi_df = NULL, test_ndvi_df = NULL) {
  train_monthly <- aggregate_monthly_ndvi(train_ndvi_df %>% dplyr::select(YearMonth, NDVI))
  test_monthly  <- aggregate_monthly_ndvi(test_ndvi_df %>% dplyr::select(YearMonth, NDVI))

  train_yr <- ndvi_monthly_year_span_label(train_monthly)
  test_yr <- ndvi_monthly_year_span_label(test_monthly)
  name_ribbon <- if (nzchar(train_yr)) paste0("NDVI historic range (", train_yr, ")") else "NDVI historic range"
  name_current <- if (nzchar(test_yr)) paste0("Current NDVI (", test_yr, ")") else "Current NDVI"
  name_clim <- if (nzchar(train_yr)) {
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

  p2 <- plotly::plot_ly(
    data    = plot_df,
    x       = ~YearMonth,
    y       = ~anomaly,
    type    = "bar",
    marker  = list(color = plot_df$bar_color),
    text    = ~hover_bar,
    textposition = "none",
    hovertemplate = "%{text}<extra></extra>",
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
                         plot_width = 15, plot_height = 8,
                         zlim_range = c(0, 1), ncol = 6,
                         save_path = NULL, filename = "BA_maps.png") {
  # Set plot size
  options(repr.plot.width = plot_width, repr.plot.height = plot_height)
  
  # Define a color map from brown to green
  colors <- c("lightgrey", "darkred")
  
  # Filter the data for the specified month
  data_filtered <- data[data$Month == month_to_plot, ]
  data_filtered$BurnedArea <- factor(data_filtered$BurnedArea)
  
  # Filter data to only the select year and the previous one
  this_and_last_year <- data_filtered %>%
    dplyr::select(Year) %>%
    unique() %>%
    pull()
  this_and_last_year <- tail(this_and_last_year, n = 2)
  data_filtered <- data_filtered %>%
    dplyr::filter(Year %in% this_and_last_year)
  
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
  BurnedArea_change <- round(ba_change[2], 1)
  
  # Generate the plot
  map_plot <- ggplot(data_filtered, aes(x = x, y = y, fill = BurnedArea)) +
    geom_raster() +
    scale_fill_manual(values = colors,
                      labels = c("Unburned", "Burned")) +
    facet_wrap(~ Year, ncol = ncol, labeller = labeller(Year = area_labels)) +
    labs(
      title = paste0("Burned Area development over ", month.name[as.numeric(month_to_plot)], 
                     " in the past year - ", ifelse(BurnedArea_change == 0, "", abs(BurnedArea_change)), ifelse(BurnedArea_change == 0, 
                                                                                                                "No difference",
                                                                                                                ifelse(BurnedArea_change > 0, 
                                                                                                                       " km² burned more",
                                                                                                                       " km² burned less"))),
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
    
    # Read the GeoJSON file
    geojson_data <- sf::st_read(file)
    
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
    map <- map %>%
      addPolygons(
        data         = geojson_data,
        color        = colors(landuse_type),
        weight       = 2,
        opacity      = 0.6,
        fillOpacity  = 0.3,
        group        = landuse_type,
        layerId      = rep(landuse_type, nrow(geojson_data)),
        popup        = paste0(
          "<strong>", htmltools::htmlEscape(pop_lab), "</strong><br>",
          "Area (hectares): ", round(as.numeric(sf::st_area(geojson_data)) / 10000, 3)
        )
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
  aoi_shape <- sf::st_read(file.path(data_dir, "AoI", aoi_files[[1]]))
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
