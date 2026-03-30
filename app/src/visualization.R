
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
    sep = "\n"
  )
}

#' Wilcoxon (monthly anomalies vs 0) and Seasonal Mann–Kendall on full monthly series.
#' @return list(wilcox_p, wilcox_median, smk_p, sen_slope)
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
  if (nrow(ndvi_monthly_full) >= 24L) {
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
    sen_slope = sen_slope
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
  if (is.na(p) || is.na(slope)) {
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

#' Interactive NDVI time series vs training climatology and historic range (plotly).
#' Titles with help icon: use ndvi_anomaly_titles_ui() in Shiny above plotlyOutput.
plot_ndvi_anomaly <- function(train_ndvi_df = NULL, test_ndvi_df = NULL) {
  train_monthly <- aggregate_monthly_ndvi(train_ndvi_df %>% dplyr::select(YearMonth, NDVI))
  test_monthly  <- aggregate_monthly_ndvi(test_ndvi_df %>% dplyr::select(YearMonth, NDVI))

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

  p1 <- plotly::plot_ly() %>%
    plotly::add_ribbons(
      data      = plot_df,
      x         = ~YearMonth,
      ymin      = ~lower,
      ymax      = ~upper,
      name      = "NDVI historic range",
      legendgroup = "historic",
      fillcolor = "rgba(39, 129, 207, 0.2)",
      line      = list(color = "transparent"),
      hoverinfo = "skip"
    ) %>%
    plotly::add_lines(
      data            = plot_df,
      x               = ~YearMonth, y = ~NDVI,
      type            = "scatter", mode = "lines+markers",
      name            = "Current NDVI",
      line            = list(width = 3, color = "#0072B2"),
      marker          = list(size = 7, color = "#0072B2"),
      hovertemplate   = "Date: %{x|%b %Y}<br>NDVI: %{y:.3f}<extra></extra>"
    ) %>%
    plotly::add_lines(
      data            = plot_df,
      x               = ~YearMonth, y = ~climatology,
      name            = "Historical monthly average",
      line            = list(width = 2.5, dash = "dash", color = "#E69F00"),
      hovertemplate   = "Date: %{x|%b %Y}<br>Historical average: %{y:.3f}<extra></extra>"
    ) %>%
    plotly::layout(
      xaxis = common_xaxis,
      yaxis = list(title = "NDVI", showgrid = TRUE, gridcolor = "rgba(0,0,0,0.08)"),
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
      yaxis = list(
        title           = "NDVI Anomaly",
        zeroline        = TRUE,
        zerolinewidth   = 2,
        zerolinecolor   = "gray50",
        showgrid        = TRUE,
        gridcolor       = "rgba(0,0,0,0.08)"
      ),
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
  
  # Extract filenames 
  landuse_types <- tools::file_path_sans_ext(basename(geojson_files))
  
  # Define a set of colors for the different GeoJSON files
  colors <- colorFactor(c("#EDE9E4", "#ED022A", "#FFDB5C", "#87D19E", "#A7D282", "#358221", "#1A5BAB"), domain = landuse_types) # LULC colors
  # Create a leaflet map with the specified basemap
  map <- leaflet() %>%
    addProviderTiles(providers[[basemap]])
  
  # Loop through each GeoJSON file and add it to the map
  for (i in seq_along(geojson_files)) {
    file <- geojson_files[i]
    landuse_type <- landuse_types[i]
    
    # Read the GeoJSON file
    geojson_data <- sf::st_read(file)
    
    # Transform the GeoJSON data to WGS 84 (EPSG:4326)
    geojson_data <- sf::st_transform(geojson_data, crs = 4326)
    
    # Add the GeoJSON data to the map with a different color
    map <- map %>%
      addPolygons(data = geojson_data, color = colors(landuse_type), weight = 2, 
                  opacity = 0.6, fillOpacity = 0.3, group = landuse_type,
                  popup = paste("Area (hectares):", round(as.numeric(sf::st_area(geojson_data)) / 10000, 3)))
  }
  
  # Add the layers control to the map
  map <- map %>%
    addLayersControl(
      overlayGroups = landuse_types,
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Add legend to the map
  map <- map %>%
    addLegend("bottomright", 
              pal = colors, 
              values = landuse_types, 
              title = "Land Use Type",
              labFormat = labelFormat(transform = function(x) x),
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
