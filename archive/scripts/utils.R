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
                                    file_extension = file_extension)
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

  cat("\nLoading", data_type, "data for", country_name, "\n", sep = " ")
  return(out_files)
}

## get list of NDVI filenames in folder
get_ndvi_filenames <- function(data_path = NULL, file_extension = ".tif") {

  ndvi_files <- list.files(data_path,
    pattern = paste0("NDVI", ".*", file_extension, "$")
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

## get list of aoi filenames in folder
get_aoi_filenames <- function(aoi_path = NULL, file_extension = ".geojson",
                              country_name = NULL) {

  aoi_files <- list.files(aoi_path,
    pattern = paste0("AoI", ".*", country_name, ".*", file_extension, "$")
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
  aoi_vec <- sf::st_read(paste0(aoi_path, aoi_files))

  # transform (by projecting) AoI data to useful coordinate system
  aoi_proj <- sf::st_transform(aoi_vec, projection)

  return(aoi_proj)
}

## get raster NDVI data 
get_ndvi_raster <- function(ndvi_files = NULL, data_path = NULL,
                            projection = "EPSG:4326", dates = NULL,
                            aoi_proj = NULL) {

  # load raster data for all months, and stack
  ndvi_rast <- terra::rast(paste0(data_path, ndvi_files))

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

# convert list of NDVI filenames to data frame
get_filename_df <- function(ndvi_files = NULL) {

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