
# -----------------------------------------------------------------------------
# UTILITY FUNCTIONS
# -----------------------------------------------------------------------------

# get system language from locale (Windows)
get_sys_language <- function(OS) {
  default_locale <- Sys.getlocale("LC_CTYPE")
  # Extract the language part from the locale
  language <- sub("_.*", "", default_locale) # on a MAC(OS=Darwin) this is the "short" language, e.g. "en""or "fr"
  # in Windows we still need to convert:
  if(OS=="Windows") {
    lang <- case_when(language == "Dutch" ~ "nl",
                      language == "English" ~ "en",
                      language == "French" ~ "fr",
                      language == "Spanish" ~ "es",
                      .default = "en")
  } else { #if(OS=="Darwin") {
    lang <- language
  } 
  list(lang_long = language, lang_short = lang)
}

#' General Purpose Utility Functions
#'
#' This file contains utility functions for common tasks such as listing files in a directory.
#' Each function is documented with a clear description, arguments, return values, and examples.

#' List Files in a Folder
#'
#' This function retrieves a list of all files in a specified folder. It includes the option
#' to return either the complete file paths or just the file names.
#'
#' @param folder_path A string specifying the folder path to list files from.
#' @param include_full_path Logical. If `TRUE`, returns complete file paths. Defaults to `TRUE`.
#' @return A character vector containing the file names or full file paths, depending on the 
#' value of `include_full_path`.
#' @details The function validates the existence of the folder before attempting to list files. 
#' It uses the base R `list.files()` function for retrieval.
#' @examples
#' # Example 1: List all files with full paths
#' list_files_in_folder("path/to/your/folder", TRUE)
#'
#' # Example 2: List all files with only file names
#' list_files_in_folder("path/to/your/folder", FALSE)
#'
#' # Example 3: Print the results
#' files <- list_files_in_folder("path/to/your/folder", TRUE)
#' print(files)
#' @export
list_files_in_folder <- function(folder_path, include_full_path = TRUE) {
  # Validate that the folder exists
  if (!dir.exists(folder_path)) {
    stop("The specified folder does not exist. Please check the folder path.")
  }
  
  # Retrieve the list of files
  files <- list.files(
    path = folder_path,       # Path to the folder
    full.names = include_full_path # Option to include full file paths
  )
  
  return(files)
}

## get list of NDVI or AoI filenames for specific country
get_filenames <- function(filepath = NULL, data_type = "NDVI",
                          file_extension = ".tif", country_name = NULL) {
  
  if (data_type == "NDVI") {
    out_files <- get_ndvi_filenames(data_path = filepath,
                                    file_extension = file_extension,
                                    country_name = country_name)
  }
  
  if (data_type == "AoI") {
    out_files <- get_aoi_filenames(aoi_path = filepath,
                                   file_extension = file_extension,
                                   country_name = country_name)
  }
  
  if (data_type == "LandUseVector") {
    out_files <- get_landuse_filenames(landuse_path = filepath,
                                       file_extension = file_extension,
                                       country_name = country_name)
  }
  
  if (data_type == "BurnedArea") {
    out_files <- get_ba_filenames(ba_path = filepath,
                                  file_extension = file_extension,
                                  country_name = country_name)
  }
  
  cat("\nLoading", data_type, "data for", country_name, "\n", sep = " ")
  return(out_files)
}

## get list of NDVI filenames in folder
get_ndvi_filenames <- function(data_path = NULL, file_extension = ".tif", country_name = NULL) {
  
  ndvi_files <- list.files(data_path,
                           pattern = paste0("NDVI", ".*", country_name, ".*", file_extension, "$")
  )
  
  return(ndvi_files)
}

## get list of land use filenames in folder
get_landuse_filenames <- function(landuse_path = NULL, file_extension = ".geojson",
                                  country_name = NULL) {
  
  landuse_files <- list.files(landuse_path,
                              pattern = paste0(country_name, "_.*", file_extension, "$")
  )
  
  return(landuse_files)
}

## get list of Burned Area filenames in folder
get_ba_filenames <- function(ba_path = NULL, file_extension = ".tif",
                             country_name = NULL) {
  
  ba_files <- list.files(ba_path,
                         pattern = paste0("BurnedArea", ".*", country_name, ".*", file_extension, "$")
  )
  
  return(ba_files)
}

## get list of aoi filenames in folder
get_aoi_filenames <- function(aoi_path = NULL, file_extension = ".geojson",
                              country_name = NULL) {
  
  aoi_files <- list.files(aoi_path,
                          pattern = paste0("AoI", ".*", country_name, file_extension, "$")
  )
  
  return(aoi_files)
}

## given a list of filenames, extract date as YYYY-MM 
## and return list of date strings
extract_dates <- function(file_list = NULL) {
  
  dates <- gsub("(\\d{4}-\\d{2})_.*", "\\1", file_list)
  
  cat("\nFound data for", length(dates),
      "months, from", min(dates), "to", max(dates), "\n", sep = " ")
  
  return(dates)
}

## order file list by date
order_by_date <- function(file_list = NULL, dates = NULL, decreasing = FALSE) {
  
  file_list <- file_list[order(as.Date(paste0(dates, "-01")),
                               decreasing = decreasing)]
  
  return(file_list)
}

## get vector AoI data
get_aoi_vector <- function(aoi_files = NULL, aoi_path = NULL,
                           projection = "EPSG:4326") {
  
  # load input Area of Interest (AoI) to later mask data
  aoi_vec <- sf::st_read(file.path(aoi_path, aoi_files))
  
  # transform (by projecting) AoI data to useful coordinate system
  aoi_proj <- sf::st_transform(aoi_vec, projection)
  
  return(aoi_proj)
}

## get raster NDVI data 
get_ndvi_raster <- function(ndvi_files = NULL, data_path = NULL,
                            projection = "EPSG:4326", dates = NULL,
                            aoi_proj = NULL) {
  
  # load raster data for all months, and stack
  ndvi_rast <- terra::rast(file.path(data_path, ndvi_files))
  
  # transform (by projecting) the raster data to useful coordinate system
  ndvi_out <- terra::project(ndvi_rast, projection)
  
  if (!is.null(aoi_proj)) {
    # Mask the raster, to remove background values (if any).
    ndvi_out <- terra::mask(ndvi_out, aoi_proj)
  }
  
  # change layer names for plotting
  names(ndvi_out) <- c(dates)
  
  # add time info for transformations
  time(ndvi_out) <- as.Date(paste0(dates, "-01"))
  
  return(ndvi_out)
}

## get raster BA data 
get_ba_raster <- function(ba_files = NULL, data_path = NULL,
                          projection = "EPSG:4326", dates = NULL,
                          aoi_proj = NULL) {
  
  # load raster data for all months, and stack
  ba_rast <- terra::rast(file.path(data_path, ba_files))
  
  # transform (by projecting) the raster data to useful coordinate system
  ba_out <- terra::project(ba_rast, projection)
  
  if (!is.null(aoi_proj)) {
    # Mask the raster, to remove background values (if any).
    ba_out <- terra::mask(ba_out, aoi_proj)
  }
  
  # change layer names for plotting
  names(ba_out) <- c(dates)
  
  # add time info for transformations
  time(ba_out) <- as.Date(paste0(dates, "-01"))
  
  return(ba_out)
}

## convert raster to dataframe
raster_to_df <- function(raster, date) {
  out_df <- as.data.frame(raster, xy = TRUE) %>%
    rename(Value = 3) %>%
    mutate(YearMonth = date)
  
  return(out_df)
}

get_ndvi_df <- function(ndvi_rast = NULL, dates = NULL) {
  
  # Extract raster layers for each date and store in dataframe
  raster_dfs <- lapply(as.Date(paste0(dates, "-01")), function(date_key) {
    raster_layer <- ndvi_rast[[time(ndvi_rast) == date_key]]
    raster_to_df(raster_layer, date_key)
  })
  
  # Combine all data frames into one bigger one
  ndvi_df <- bind_rows(raster_dfs)
  
  # split dates into month and year columns
  ndvi_df <- transform(ndvi_df,
                       Year = format(YearMonth, "%Y"),
                       Month = format(YearMonth, "%m"))
  
  # change column name for plotting
  colnames(ndvi_df)[3] <- "NDVI"
  
  return(ndvi_df)
}

get_ba_df <- function(ba_rast = NULL, dates = NULL) {
  
  # Extract raster layers for each date and store in dataframe
  raster_dfs <- lapply(as.Date(paste0(dates, "-01")), function(date_key) {
    raster_layer <- ba_rast[[time(ba_rast) == date_key]]
    raster_to_df(raster_layer, date_key)
  })
  
  # Combine all data frames into one bigger one
  ba_df <- bind_rows(raster_dfs)
  
  # split dates into month and year columns
  ba_df <- transform(ba_df,
                     Year = format(YearMonth, "%Y"),
                     Month = format(YearMonth, "%m"))
  
  # change column name for plotting
  colnames(ba_df)[3] <- "BurnDate"
  
  # Create boolean for burned yes/no
  ba_df$BurnedArea <- ifelse(ba_df$BurnDate > 0, 1, 0)
  
  burned_area_sizes <- lapply(unique(ba_df$YearMonth), function(date_key) {
    raster_layer <- ba_rast[[time(ba_rast) == as.Date(date_key)]]
    TotalArea_Size <- sum(expanse(raster_layer, unit="km"))
    burned_rast <- classify(raster_layer, matrix(c(-Inf, 0, NA), ncol = 3, byrow = TRUE))
    BurnedArea_Size <- sum(expanse(burned_rast, unit = "km"), na.rm = TRUE)
    Percentage_Burned <- ifelse(BurnedArea_Size <= 1, 0, (BurnedArea_Size/TotalArea_Size) * 100)
    data.frame(YearMonth = as.Date(date_key), 
               BurnedArea_Size = BurnedArea_Size, 
               TotalArea_Size = TotalArea_Size, 
               Percentage_Burned = Percentage_Burned)
  })
  
  # Join burned area sizes back to ba_df
  burned_area_df <- bind_rows(burned_area_sizes)
  ba_df <- left_join(ba_df, burned_area_df, by = "YearMonth")
  
  return(ba_df)
}

# calculate NDVI modulation in 2D space
get_delta_ndvi_df <- function(train_ndvi_df = NULL, test_ndvi_df = NULL) {
  
  ## Calculate mean value for each coordinate
  # train data
  train_ndvi_summary <- train_ndvi_df %>%
    group_by(x,y, Month) %>%
    summarize(mean_ndvi = mean(NDVI))
  
  # test data
  test_ndvi_summary <- test_ndvi_df %>%
    group_by(x,y, Month) %>%
    summarize(mean_ndvi = mean(NDVI))
  
  # Join the two summaries
  ndvi_comparison <- train_ndvi_summary %>%
    inner_join(test_ndvi_summary, 
               by = c("x", "y", "Month"), suffix = c("_train", "_test"))
  
  # Get delta NDVI
  delta_ndvi_df <- ndvi_comparison %>%
    mutate(delta_ndvi = (mean_ndvi_test - mean_ndvi_train) / (mean_ndvi_test + mean_ndvi_train)) # normalized difference
  
  return(delta_ndvi_df)
}

# calculate NDVI mean, SD, and confidence intervals per month
get_summary_ndvi_df <- function(ndvi_df = NULL) {
  
  summary_ndvi_df <- ndvi_df %>%
    group_by(Year, Month) %>%
    summarize(
      mean_ym_ndvi = mean(NDVI)
    ) %>% # 1st get the monthly mean NDVI, for each year separately
    group_by(Month) %>%
    summarize(
      mean_val = mean(mean_ym_ndvi),
      lower_ci = mean(mean_ym_ndvi) - 1.96 * sd(mean_ym_ndvi) / sqrt(length(mean_ym_ndvi)), # 95% CI lower bound
      upper_ci = mean(mean_ym_ndvi) + 1.96 * sd(mean_ym_ndvi) / sqrt(length(mean_ym_ndvi)) # 95% CI upper bound
    )
  
  return(summary_ndvi_df)
}

# calculate BA area mean, SD, and confidence intervals per month
get_summary_ba_df <- function(ba_df = NULL) {
  
  summary_ba_df <- ba_df %>%
    group_by(Year, Month) %>%
    summarize(
      mean_ym_ba = mean(BurnedArea_Size)
    ) %>% # 1st get the monthly mean BA, for each year separately
    group_by(Month) %>%
    summarize(
      mean_val = mean(mean_ym_ba),
      lower_ci = mean(mean_ym_ba) - 1.96 * sd(mean_ym_ba) / sqrt(length(mean_ym_ba)), # 95% CI lower bound
      upper_ci = mean(mean_ym_ba) + 1.96 * sd(mean_ym_ba) / sqrt(length(mean_ym_ba)) # 95% CI upper bound
    ) %>%
    mutate(
      lower_ci = if_else(lower_ci < 0, 0, lower_ci)
    )
  
  return(summary_ba_df)
}

# convert list of NDVI filenames to data frame
get_filename_df <- function(ndvi_files = NULL) {
  
  # Assert that ndvi_files is not empty
  if (is.null(ndvi_files) || length(ndvi_files) == 0) {
    stop("No NDVI files provided.")
  }
  ## put filenames in table, with info for year and month
  files_df <- tibble(filenames = ndvi_files) %>%
    mutate(dates = gsub("(\\d{4}-\\d{2})_.*", "\\1", filenames))
  files_df <- separate(files_df, "dates", c("year", "month"), sep="-", remove=F)
  
  # put dates into time operator
  files_df$dates <- as.Date(paste0(files_df$dates, "-01"))
  
  # turn year and month into int, for easier operations
  files_df$year <- as.integer(files_df$year)
  files_df$month <- as.integer(files_df$month)
  
  return(files_df)
}

# convert list of BA filenames to data frame
get_ba_filename_df <- function(ba_files = NULL) {
  
  # Assert that ba_files is not empty
  if (is.null(ba_files) || length(ba_files) == 0) {
    stop("No BA files provided.")
  }
  ## put filenames in table, with info for year and month
  files_df <- tibble(filenames = ba_files) %>%
    mutate(dates = gsub("(\\d{4}-\\d{2})_.*", "\\1", filenames))
  files_df <- separate(files_df, "dates", c("year", "month"), sep="-", remove=F)
  
  # put dates into time operator
  files_df$dates <- as.Date(paste0(files_df$dates, "-01"))
  
  # turn year and month into int, for easier operations
  files_df$year <- as.integer(files_df$year)
  files_df$month <- as.integer(files_df$month)
  
  return(files_df)
}



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



# -----------------------------------------------------------------------------
# GENERATE PLOTS
# -----------------------------------------------------------------------------

# Function to create NDVI timeseries plot
generate_timeseries <- function(country_name = NULL, resolution = NULL,
                                end_year = NULL, end_month = NULL,
                                figures_dir = NULL, data_dir = NULL,
                                return_plot = FALSE, figure_filename = NULL
) {
  
  ### Set paths and define parameters
  
  # figure path
  figure_path <- file.path(figures_dir, figure_filename)
  
  data_type <- "NDVI"
  
  # Input NVDI basemaps stored in country folder. 
  data_path <- file.path(data_dir, paste0(data_type, "/", 
                                          country_name, "/", 
                                          resolution, "m_resolution/"))
  # Area of Interest (AoI) files in AoI folder
  aoi_path <- file.path(data_dir, "AoI")
  
  ## define end and start date for test data
  end_date <- as.Date(paste(end_year, end_month, 1, sep="-"))
  start_date <- as.Date(paste(end_year, 1, 1, sep="-"))
  
  ### Create lists with relevant filenames.
  # NDVI filenames
  ndvi_files <- get_filenames(filepath = data_path, data_type = data_type, 
                              file_extension = ".tif", country_name = country_name)
  
  # AoI filenames
  aoi_files <- get_filenames(filepath = aoi_path, data_type = "AoI", 
                             file_extension = ".geojson", country_name = country_name)
  
  ### Subselect filenames according to date
  # get NDVI filenames dataframe (includes date info)
  files_df <- get_filename_df(ndvi_files = ndvi_files)
  
  # Given date selected, split file into test data and train data
  # test filenames
  test_files_df <- filter(files_df, between(dates, start_date, end_date))
  
  # get train filenames (train interval: prior to test interval start)
  months_in_test <- c(test_files_df$month)
  year_in_test   <- test_files_df$year
  train_files_df <- files_df %>% 
    dplyr::filter(month %in% months_in_test & year < year_in_test)
  #train_files_df <- files_df[(files_df$dates< start_date),]
  
  ### Load raster and vector objects - Aoi, train data and test data
  # load input Area of Interest (AoI) to later mask data
  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path,
                             projection = "EPSG:4326")
  
  test_ndvi_msk <- get_ndvi_raster(ndvi_files = test_files_df$filenames, data_path = data_path,
                                   projection = "EPSG:4326", dates = test_files_df$dates,
                                   aoi_proj = aoi_proj)
  
  train_ndvi_msk <- get_ndvi_raster(ndvi_files = train_files_df$filenames, data_path = data_path,
                                    projection = "EPSG:4326", dates = train_files_df$dates,
                                    aoi_proj = aoi_proj)
  
  ### Calculate mean NDVI for each month
  # Extract raster layers for each date
  # and store in dataframe
  test_ndvi_df <- get_ndvi_df(ndvi_rast = test_ndvi_msk, dates = test_files_df$dates) 
  train_ndvi_df <- get_ndvi_df(ndvi_rast = train_ndvi_msk, dates = train_files_df$dates) 
  
  ## Compute mean, SD, and confidence intervals
  # test data
  test_ndvi_summary <- get_summary_ndvi_df(ndvi_df = test_ndvi_df)
  # train data
  train_ndvi_summary <- get_summary_ndvi_df(ndvi_df = train_ndvi_df)
  
  ## Make plot - distribution of NDVI values throughout the year.
  ndvi_ts_plot <- plot_ndvi_timeseries(train_data = train_ndvi_summary, 
                                       test_data = test_ndvi_summary,
                                       country_name = country_name, 
                                       resolution = resolution,
                                       plot_width = 15, 
                                       plot_height = 8,
                                       ylim_range = NULL,
                                       test_start_date = start_date,
                                       test_end_date = end_date,
                                       label_test = paste0("NDVI ", paste(format(c(start_date, end_date), "%b %Y"),collapse=" - ") ),
                                       label_train = paste0("NDVI historic range until ", format(start_date, "%b %Y") ),
                                       label_mean = paste0("NDVI monthly average until ", format(start_date, "%b %Y") ),
                                       save_path = figures_dir,
                                       filename = figure_filename
  )
  
  # if we want to return the ggplot object
  if (return_plot == TRUE) {
    
    return(ndvi_ts_plot)
    
  }
  
}

# Function to create Burned Area timeseries plot
generate_ba_timeseries <- function(country_name = NULL, resolution = NULL,
                                   end_year = NULL, end_month = NULL,
                                   figures_dir = NULL, data_dir = NULL,
                                   return_plot = FALSE, figure_filename = NULL
) {
  
  ### Set paths and define parameters
  
  # figure path
  figure_path <- file.path(figures_dir, figure_filename)
  
  data_type <- "BurnedArea"
  
  # Input NVDI basemaps stored in country folder. 
  data_path <- file.path(data_dir, paste0(data_type, "/", 
                                          country_name, "/", 
                                          resolution, "m_resolution/"))
  # Area of Interest (AoI) files in AoI folder
  aoi_path <- file.path(data_dir, "AoI")
  
  ## define end and start date for test data
  end_date <- as.Date(paste(end_year, end_month, 1, sep="-"))
  start_date <- seq(end_date, length = 2, by = "-11 months")[2]
  
  ### Create lists with relevant filenames
  # BA filenames
  ba_files <- get_filenames(filepath = data_path, data_type = data_type, 
                            file_extension = ".tif", country_name = country_name)
  
  # AoI filenames
  aoi_files <- get_filenames(filepath = aoi_path, data_type = "AoI", 
                             file_extension = ".geojson", country_name = country_name)
  
  ### Subselect filenames according to date
  # get NDVI filenames dataframe (includes date info)
  files_df <- get_ba_filename_df(ba_files = ba_files)
  
  ### Given date selected, split file into test data and train data
  # test filenames
  test_files_df <- filter(files_df, between(dates, start_date, end_date))
  
  # get train filenames (train interval: prior to test interval start)
  train_files_df <- files_df[(files_df$dates< start_date),]
  
  ### Load raster and vector objects - Aoi, train data and test data
  # load input Area of Interest (AoI) to later mask data
  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path,
                             projection = "EPSG:4326")
  
  test_ba_msk <- get_ba_raster(ba_files = test_files_df$filenames, data_path = data_path,
                               projection = "EPSG:4326", dates = test_files_df$dates,
                               aoi_proj = aoi_proj)
  
  train_ba_msk <- get_ba_raster(ba_files = train_files_df$filenames, data_path = data_path,
                                projection = "EPSG:4326", dates = train_files_df$dates,
                                aoi_proj = aoi_proj)
  
  ### Calculate mean BA for each month
  # Extract raster layers for each date
  # and store in dataframe
  test_ba_df <- get_ba_df(ba_rast = test_ba_msk, dates = test_files_df$dates) 
  train_ba_df <- get_ba_df(ba_rast = train_ba_msk, dates = train_files_df$dates) 
  
  ## Compute mean, SD, and confidence intervals
  # test data
  test_ba_summary <- get_summary_ba_df(ba_df = test_ba_df)
  # train data
  train_ba_summary <- get_summary_ba_df(ba_df = train_ba_df)
  
  ## Make plot - distribution of NDVI values throughout the year.
  ba_ts_plot <- plot_ba_timeseries(train_data = train_ba_summary, 
                                   test_data = test_ba_summary,
                                   country_name = country_name, 
                                   resolution = resolution,
                                   plot_width = 15, 
                                   plot_height = 8,
                                   ylim_range = NULL,
                                   test_start_date = start_date,
                                   test_end_date = end_date,
                                   label_test = paste0("Burned Area ", paste(format(c(start_date, end_date), "%b %Y"),collapse=" - ") ),
                                   label_train = paste0("Burned Area until ", format(start_date, "%b %Y") ),
                                   label_mean = paste0("Burned Area monthly average until ", format(start_date, "%b %Y") ),
                                   save_path = figures_dir,
                                   filename = figure_filename
  )
  
  # if we want to return the ggplot object
  if (return_plot == TRUE) {
    
    return(ba_ts_plot)
    
  }
}

# Function to create NDVI 2D map 
generate_2Dmap <- function(country_name = NULL, resolution = NULL,
                           map_year = NULL, map_month = NULL,
                           figures_dir = NULL, data_dir = NULL,
                           plot_delta = FALSE,
                           return_plot = FALSE, figure_filename = NULL
) {
  
  ### Set paths and define parameters
  
  # figure path
  figure_path <- file.path(figures_dir, figure_filename)
  
  Sys.setlocale("LC_TIME", "C")
  
  data_type <- "NDVI"
  
  # Input NVDI basemaps stored in country folder. 
  data_path <- file.path(data_dir, paste0(data_type, "/", 
                                          country_name, "/", 
                                          resolution, "m_resolution/"))
  # Area of Interest (AoI) files in AoI folder
  aoi_path <- file.path(data_dir, "AoI/")
  
  # define date for which to load data 
  map_date <- as.Date(paste(map_year, map_month, 1, sep="-"))
  
  ### Create lists with relevant filenames.
  # NDVI filenames
  ndvi_files <- get_filenames(filepath = data_path, data_type = data_type, 
                              file_extension = ".tif", country_name = country_name)
  
  # AoI filenames
  aoi_files <- get_filenames(filepath = aoi_path, data_type = "AoI", 
                             file_extension = ".geojson", country_name = country_name)

  ### Subselect filenames according to date
  # get NDVI filenames dataframe (includes date info)
  files_df <- get_filename_df(ndvi_files = ndvi_files)
  
  # Given date selected, split file into test data and train data
  # test filenames
  test_files_df <- files_df[(files_df$dates == map_date),]
  
  # get train filenames (same month, all years prior to test date)
  train_files_df <- files_df[(files_df$dates< map_date & months(files_df$dates) %in% month.name[map_month]),]
  
  ### Load raster and vector objects - Aoi, train data and test data
  # load input Area of Interest (AoI) to later mask data
  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path,
                             projection = "EPSG:4326")
  
  test_ndvi_msk <- get_ndvi_raster(ndvi_files = test_files_df$filenames, data_path = data_path,
                                   projection = "EPSG:4326", dates = test_files_df$dates,
                                   aoi_proj = aoi_proj)
  
  train_ndvi_msk <- get_ndvi_raster(ndvi_files = train_files_df$filenames, data_path = data_path,
                                    projection = "EPSG:4326", dates = train_files_df$dates,
                                    aoi_proj = aoi_proj)
  
  ### Calculate mean NDVI for each month
  # Extract raster layers for each date
  # and store in dataframe
  test_ndvi_df <- get_ndvi_df(ndvi_rast = test_ndvi_msk, dates = test_files_df$dates) 
  train_ndvi_df <- get_ndvi_df(ndvi_rast = train_ndvi_msk, dates = train_files_df$dates) 
  
  
  # if we want to make the delta NDVI plot
  if (plot_delta == TRUE) {
    
    ## compute NDVI deviation from baseline in 2D space
    delta_ndvi_df <- get_delta_ndvi_df(train_ndvi_df = train_ndvi_df,
                                       test_ndvi_df = test_ndvi_df)
    
    ## Make plot - delta NDVI values for the selected month, over street view (leaflet)
    ndvi_map <- plot_delta_ndvi_streetview(data = delta_ndvi_df, 
                                           month_to_plot = sprintf("%02d", map_month),
                                           save_path = figures_dir,
                                           filename = figure_filename
    )
  } else {
    
    ## Make plot - NDVI values for the selected month, throughout the years
    ndvi_map <- plot_ndvi_maps(data = bind_rows(train_ndvi_df,test_ndvi_df), 
                               month_to_plot = sprintf("%02d", map_month),
                               plot_width = 15, 
                               plot_height = 8,
                               zlim_range = c(-0.7, 0.7), 
                               ncol = dim(train_files_df)[1] + 1,
                               save_path = figures_dir,
                               filename = figure_filename
    )
  }

  # if we want to return the ggplot object
  if (return_plot == TRUE) {
    
    return(ndvi_map)
    
  }
  
}

# Function to create BA 2D map 
generate_ba_2Dmap <- function(country_name = NULL, resolution = NULL,
                              map_year = NULL, map_month = NULL,
                              figures_dir = NULL, data_dir = NULL,
                              plot_delta = FALSE,
                              return_plot = FALSE, figure_filename = NULL
) {
  
  ### Set paths and define parameters
  
  # figure path
  figure_path <- file.path(figures_dir, figure_filename)
  
  data_type <- "BurnedArea"
  Sys.setlocale("LC_TIME", "C") # Otherwise creates language inconsistencies, at least locally
  
  # Input NVDI basemaps stored in country folder. 
  data_path <- file.path(data_dir, paste0(data_type, "/", 
                                          country_name, "/", 
                                          resolution, "m_resolution/"))
  # Area of Interest (AoI) files in AoI folder
  aoi_path <- file.path(data_dir, "AoI/")
  
  # define date for which to load data 
  map_date <- as.Date(paste(map_year, map_month, 1, sep="-"))
  
  ### Create lists with relevant filenames.
  # BA filenames
  ba_files <- get_filenames(filepath = data_path, data_type = data_type, 
                            file_extension = ".tif", country_name = country_name)
  
  # AoI filenames
  aoi_files <- get_filenames(filepath = aoi_path, data_type = "AoI", 
                             file_extension = ".geojson", country_name = country_name)
  
  ### Subselect filenames according to date
  # get BA filenames dataframe (includes date info)
  files_df <- get_ba_filename_df(ba_files = ba_files)
  
  # Given date selected, split file into test data and train data
  # test filenames
  test_files_df <- files_df[(files_df$dates == map_date),]
  
  # get train filenames (same month, all years prior to test date)
  train_files_df <- files_df[(files_df$dates< map_date & months(files_df$dates) %in% month.name[map_month]),]
  
  ### Load raster and vector objects - Aoi, train data and test data
  # load input Area of Interest (AoI) to later mask data
  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path,
                             projection = "EPSG:4326")
  
  test_ba_msk <- get_ba_raster(ba_files = test_files_df$filenames, data_path = data_path,
                               projection = "EPSG:4326", dates = test_files_df$dates,
                               aoi_proj = aoi_proj)
  
  train_ba_msk <- get_ba_raster(ba_files = train_files_df$filenames, data_path = data_path,
                                projection = "EPSG:4326", dates = train_files_df$dates,
                                aoi_proj = aoi_proj)
  
  ### Calculate mean BA size for each month
  # Extract raster layers for each date and store in dataframe
  test_ba_df <- get_ba_df(ba_rast = test_ba_msk, dates = test_files_df$dates) 
  train_ba_df <- get_ba_df(ba_rast = train_ba_msk, dates = train_files_df$dates) 
  
  ## Make plot - BA values for the selected month, throughout the years
  ba_map <- plot_ba_maps(data = bind_rows(train_ba_df, test_ba_df), 
                         month_to_plot = sprintf("%02d", map_month),
                         plot_width = 15, 
                         plot_height = 8,
                         zlim_range = c(-0.7, 0.7), 
                         ncol = dim(train_files_df)[1] + 1,
                         save_path = figures_dir,
                         filename = figure_filename
  )
  #}
  
  # Get output for burned area geoJSON export 
  # burned_area_raster <- test_ba_msk
  # burn_mask <- ifel(burned_area_raster > 0, 1, NA)
  # 
  # burned_area_raster_masked <- as.polygons(mask(burned_area_raster, burn_mask))
  # 
  # geojson_export_path <- file.path(figures_dir, paste0(tools::file_path_sans_ext(figure_filename), ".geojson"))
  # writeVector(burned_area_raster_masked, filename = geojson_export_path, filetype = "GeoJSON", overwrite = TRUE)
  # 
  # return(geojson_export_path)
  
}

# Function to create BA geoJSON export
generate_ba_export <- function(country_name = NULL, resolution = NULL,
                               map_year = NULL, map_month = NULL,
                               figures_dir = NULL, data_dir = NULL,
                               figure_filename = NULL
) {
  
  ### Set paths and define parameters
  
  # figure path
  figure_path <- file.path(figures_dir, figure_filename)
  
  data_type <- "BurnedArea"
  Sys.setlocale("LC_TIME", "C") # Otherwise creates language inconsistencies, at least locally
  
  # Input BA basemaps stored in country folder. 
  data_path <- file.path(data_dir, paste0(data_type, "/", 
                                          country_name, "/", 
                                          resolution, "m_resolution/"))
  # Area of Interest (AoI) files in AoI folder
  aoi_path <- file.path(data_dir, "AoI/")
  
  # define date for which to load data 
  map_date <- as.Date(paste(map_year, map_month, 1, sep="-"))
  
  ### Create lists with relevant filenames.
  # BA filenames
  ba_files <- get_filenames(filepath = data_path, data_type = data_type, 
                            file_extension = ".tif", country_name = country_name)
  
  # AoI filenames
  aoi_files <- get_filenames(filepath = aoi_path, data_type = "AoI", 
                             file_extension = ".geojson", country_name = country_name)
  
  ### Subselect filenames according to date
  # get BA filenames dataframe (includes date info)
  files_df <- get_ba_filename_df(ba_files = ba_files)
  
  # Given date selected, select corresponding month
  ba_export_df <- files_df[(files_df$dates == map_date),]
  
  ### Load raster and vector objects - Aoi, and desired data
  # load input Area of Interest (AoI) to later mask data
  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path,
                             projection = "EPSG:4326")
  
  ba_export_msk <- get_ba_raster(ba_files = ba_export_df$filenames, data_path = data_path,
                                 projection = "EPSG:4326", dates = ba_export_df$dates,
                                 aoi_proj = aoi_proj)
  
  # Get output for burned area geoJSON export 
  burned_area_raster <- ba_export_msk
  burn_mask <- ifel(burned_area_raster > 0, 1, NA)
  
  burned_area_raster_masked <- as.polygons(mask(burned_area_raster, burn_mask))
  
  geojson_export_path <- file.path(figures_dir, paste0(tools::file_path_sans_ext(figure_filename), ".geojson"))
  writeVector(burned_area_raster_masked, filename = geojson_export_path, filetype = "GeoJSON", overwrite = TRUE)
  
  return(geojson_export_path)
}

# Function to create NDVI timeseries plot
generate_timeseries_landcover <- function(country_name = NULL, resolution = NULL, land_cover_type = NULL, land_use_src = NULL,
                                          end_year = NULL, end_month = NULL,
                                          figures_dir = NULL, data_dir = NULL,
                                          return_plot = FALSE, figure_filename = NULL
) {
  
  ### Set paths and define parameters
  
  # figure path
  figure_path <- file.path(figures_dir, figure_filename)
  
  data_type <- "NDVI"
  Sys.setlocale("LC_TIME", "C") # Otherwise creates language inconsistencies, at least locally
  
  
  # Input NVDI basemaps stored in country folder. 
  data_path <- file.path(data_dir, paste0(data_type, "/", 
                                          country_name, "/", 
                                          resolution, "m_resolution/"))
  # Area of Interest (AoI) files in AoI folder
  aoi_path <- file.path(data_dir, "AoI/")
  
  # Land use files path
  lulc_path <- file.path(data_dir, paste0("LandUse/", country_name, "/", land_use_src, "/"))
  
  ## define end and start date for test data
  end_date <- as.Date(paste(end_year, end_month, 1, sep="-"))
  start_date <- as.Date(paste(end_year, 1, 1, sep="-"))
  
  ### Create lists with relevant filenames.
  # NDVI filenames
  ndvi_files <- get_filenames(filepath = data_path, data_type = data_type, 
                              file_extension = ".tif", country_name = country_name)
  
  # AoI filenames
  aoi_files <- get_filenames(filepath = aoi_path, data_type = "AoI", 
                             file_extension = ".geojson", country_name = country_name)
  
  # LULC filenames
  lulc_files <- get_filenames(filepath = lulc_path, data_type = "LandUseVector", 
                              file_extension = ".geojson", country_name = country_name)
  
  # Get the desired land cover file
  land_cover_file <- lulc_files[grepl(land_cover_type, lulc_files)][1]
  
  ### Subselect filenames according to date
  # get NDVI filenames dataframe (includes date info)
  files_df <- get_filename_df(ndvi_files = ndvi_files)
  
  # Given date selected, split file into test data and train data
  # test filenames
  test_files_df <- filter(files_df, between(dates, start_date, end_date))
  
  # get train filenames (train interval: prior to test interval start)
  months_in_test <- c(test_files_df$month)
  year_in_test   <- test_files_df$year
  train_files_df <- files_df %>% 
    dplyr::filter(month %in% months_in_test & year < year_in_test)

  ### Load raster and vector objects - Aoi, train data and test data
  # load input Area of Interest (AoI) to later mask data
  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path,
                             projection = "EPSG:4326")
 
  land_use <- get_aoi_vector(aoi_files = land_cover_file, aoi_path = lulc_path,
                             projection = "EPSG:4326")
  
  test_ndvi_msk <- get_ndvi_raster(ndvi_files = test_files_df$filenames, data_path = data_path,
                                   projection = "EPSG:4326", dates = test_files_df$dates,
                                   aoi_proj = aoi_proj)
  
  train_ndvi_msk <- get_ndvi_raster(ndvi_files = train_files_df$filenames, data_path = data_path,
                                    projection = "EPSG:4326", dates = train_files_df$dates,
                                    aoi_proj = aoi_proj)
  
  test_ndvi_land_use <- mask(test_ndvi_msk, land_use)
  
  train_ndvi_land_use <- mask(train_ndvi_msk, land_use)
  
  
  ### Calculate mean NDVI for each month
  # Extract raster layers for each date
  # and store in dataframe
  test_ndvi_df <- get_ndvi_df(ndvi_rast = test_ndvi_land_use, dates = test_files_df$dates) 
  train_ndvi_df <- get_ndvi_df(ndvi_rast = train_ndvi_land_use, dates = train_files_df$dates) 
  
  ## Compute mean, SD, and confidence intervals
  # test data
  test_ndvi_summary <- get_summary_ndvi_df(ndvi_df = test_ndvi_df)
  # train data
  train_ndvi_summary <- get_summary_ndvi_df(ndvi_df = train_ndvi_df)
  
  ## Make plot - distribution of NDVI values throughout the year.
  ndvi_ts_plot <- plot_ndvi_timeseries(train_data = train_ndvi_summary, 
                                       test_data = test_ndvi_summary,
                                       country_name = country_name, 
                                       resolution = resolution,
                                       plot_width = 15, 
                                       plot_height = 8,
                                       ylim_range = NULL,
                                       test_start_date = start_date,
                                       test_end_date = end_date,
                                       label_test = paste0(land_cover_type, " NDVI ", paste(format(c(start_date, end_date), "%b %Y"),collapse=" - ") ),
                                       label_train = paste0(land_cover_type, " NDVI until ", format(start_date, "%b %Y") ),
                                       label_mean = paste0(land_cover_type, " NDVI monthly average until ", format(start_date, "%b %Y") ),
                                       save_path = figures_dir,
                                       filename = figure_filename
  )
  
  # if we want to return the ggplot object
  if (return_plot == TRUE) {
    
    return(ndvi_ts_plot)
    
  }
  
}


