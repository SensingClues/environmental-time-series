# Data Visualization Functions
#
#
# This file contains functions to build useful plots

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
    ylim_range <- c(min(train_data$upper_ci)-0.25, max(train_data$upper_ci)+0.15)
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
      title = paste0(country_name, " NDVI (", ifelse(grepl("_", resolution), sub(".*_", "", resolution), resolution), "m res)"),
      x = "Month",
      y = "Mean NDVI"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
      axis.text.y = element_text(size = 15), 
      axis.title.x = element_text(size = 20, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = 20, margin = margin(0, 15, 0, 0)),
      plot.title = element_text(size = 20, hjust = 0.5)
    ) +
    ylim(ylim_range) +
    geom_text(x = 8, y = ylim_range[2] - 0.025, label = label_test, size = 6,
              color = "#9662b3", hjust = 0) + # add text to label plot
    geom_text(x = 8, y = ylim_range[2] - 0.075, label = label_train, size = 6,
              color = "#2781cf", hjust = 0) + # add text to label plot
    geom_text(x = 8, y = ylim_range[2] - 0.125, label = label_mean, size = 6,
              color = "black", hjust = 0) # add text to label plot

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
      title = paste0(strsplit(country_name, "_")[[1]][1], " Burned Area size (", ifelse(grepl("_", resolution), sub(".*_", "", resolution), resolution), "m res)"),
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
    geom_text(x = 8, y = ylim_range[2] - 10, label = label_test, size = 6,
              color = "#9662b3", hjust = 0) + # add text to label plot
    geom_text(x = 8, y = ylim_range[2] - 30, label = label_train, size = 6,
              color = "#2781cf", hjust = 0) + # add text to label plot
    geom_text(x = 8, y = ylim_range[2] - 50, label = label_mean, size = 6,
              color = "black", hjust = 0) # add text to label plot
  
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
      title = paste0(country_name, " NDVI (", ifelse(grepl("_", resolution), sub(".*_", "", resolution), resolution), "m res)"),
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
    mutate(changeBurnedArea = BurnedArea_Size - lag(BurnedArea_Size)) %>%
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
                     " in the past year - ", abs(BurnedArea_change), ifelse(BurnedArea_change > 0, 
                                                                            " km² burned more", 
                                                                            " km² burned less")),
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