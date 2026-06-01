
# -----------------------------------------------------------------------------
# UTILITY FUNCTIONS
# -----------------------------------------------------------------------------

#' Map app resolution key to pipeline sensor and resolution number
#'
#' The app uses resolution keys like "Sentinel_1000", "MODIS_1000", "100" etc.
#' The pipeline stores Parquet files by sensor name and numeric resolution.
#' This function bridges the two naming conventions.
#'
#' @param resolution_key Character. One of the values from resolutionchoices_rv.
#' @return Named list with 'sensor' (character) and 'resolution' (integer),
#'         or NULL if the key is not recognised.
get_sensor_resolution <- function(resolution_key) {
  mapping <- list(
    "100"          = list(sensor = "sentinel2", resolution = 100L),
    "Sentinel_1000"= list(sensor = "sentinel2", resolution = 1000L),
    "250"          = list(sensor = "modis",     resolution = 250L),
    "500"          = list(sensor = "modis",     resolution = 500L),
    "MODIS_1000"   = list(sensor = "modis",     resolution = 1000L)
  )
  mapping[[resolution_key]]
}

#' Build the full path to a pre-processed Parquet file
#'
#' @param aoi Character. AoI name matching folder name, e.g. "Zambia_Mponda".
#' @param resolution_key Character. App resolution key, e.g. "Sentinel_1000".
#' @param table_name Character. Parquet table name without extension,
#'        e.g. "ndvi_monthly", "ndvi_monthly_by_class", "ba_monthly".
#' @param sensor_override Character or NULL. If not NULL, use this sensor
#'        instead of deriving from resolution_key. Used for burned_area.
#' @return Full file path as character, or NULL if resolution_key not recognised.
get_parquet_path <- function(aoi, resolution_key, table_name,
                             sensor_override = NULL) {
  if (!is.null(sensor_override)) {
    sensor <- sensor_override
    resolution <- as.integer(sub("[^0-9]", "", resolution_key))
  } else {
    sr <- get_sensor_resolution(resolution_key)
    if (is.null(sr)) return(NULL)
    sensor <- sr$sensor
    resolution <- sr$resolution
  }

  file.path(
    "www/data/processed",
    aoi,
    sensor,
    paste0(resolution, "m"),
    paste0(table_name, ".parquet")
  )
}

#' Try to read a Parquet file, return NULL if it doesn't exist
#'
#' @param path Character. Full path from get_parquet_path().
#' @return data.frame from arrow::read_parquet(), or NULL if file not found.
try_read_parquet <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(
    arrow::read_parquet(path),
    error = function(e) {
      warning(paste("Failed to read Parquet:", path, "-", e$message))
      NULL
    }
  )
}

# Remove land_cover classes that have NO valid (non-NA) mean_ndvi across all
# rows.  At coarse resolutions tiny classes (e.g. Flooded_vegetation < 0.1 km²)
# contain no raster pixels, producing all-NA mean_ndvi.  plot_anomaly_resilience
# crashes when which.min() receives an all-NA vector, so we drop these classes
# before the data reaches any plot function.
.drop_empty_lc_classes <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  valid_lc <- unique(df$land_cover[!is.na(df$mean_ndvi)])
  df[df$land_cover %in% valid_lc, , drop = FALSE]
}

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

get_data_path <- function(data_dir = NULL, data_type = "NDVI",
                          country_name = NULL, resolution = NULL) {
  file.path(data_dir, data_type, country_name, paste0(resolution, "m_resolution"))
}

get_available_dates <- function(data_dir = NULL, data_type = "NDVI",
                                country_name = NULL, resolution = NULL) {
  data_path <- get_data_path(data_dir, data_type, country_name, resolution)
  if (!dir.exists(data_path)) {
    return(data.frame(year = integer(0), month = integer(0), dates = as.Date(character(0))))
  }

  files <- tryCatch(
    get_filenames(
      filepath = data_path,
      data_type = data_type,
      file_extension = ".tif",
      country_name = country_name
    ),
    error = function(e) character(0)
  )
  if (length(files) == 0L) {
    return(data.frame(year = integer(0), month = integer(0), dates = as.Date(character(0))))
  }

  files_df <- tryCatch(
    if (identical(data_type, "BurnedArea")) {
      get_ba_filename_df(files)
    } else {
      get_filename_df(files)
    },
    error = function(e) NULL
  )
  if (is.null(files_df) || nrow(files_df) == 0L) {
    return(data.frame(year = integer(0), month = integer(0), dates = as.Date(character(0))))
  }

  files_df %>%
    dplyr::distinct(year, month, dates) %>%
    dplyr::arrange(year, month)
}

get_available_years <- function(data_dir = NULL, data_type = "NDVI",
                                country_name = NULL, resolution = NULL,
                                decreasing = FALSE) {
  years <- sort(unique(get_available_dates(data_dir, data_type, country_name, resolution)$year))
  if (isTRUE(decreasing)) rev(years) else years
}

get_available_month_names <- function(data_dir = NULL, data_type = "NDVI",
                                      country_name = NULL, resolution = NULL,
                                      year = NULL) {
  dates_df <- get_available_dates(data_dir, data_type, country_name, resolution)
  year <- suppressWarnings(as.integer(year))
  months <- sort(unique(dates_df$month[dates_df$year == year]))
  month.name[months]
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
                          pattern = paste0("AoI.*", country_name, ".*", file_extension, "$")
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

#' Spatial mean NDVI per layer via \code{terra::global} (fast path).
#'
#' Returns a small data frame with \code{Year}, \code{Month}, \code{NDVI} per
#' layer, compatible with \code{get_summary_ndvi_df()} — same inter-annual CI
#' logic as the pixel-based \code{get_ndvi_df()} path when means match
#' \code{mean(NDVI, na.rm = TRUE)} over cells.
#'
#' @noRd
get_ndvi_global_means_df <- function(ndvi_rast = NULL, dates = NULL) {
  if (is.null(ndvi_rast) || is.null(dates)) {
    stop("ndvi_rast and dates are required.")
  }
  nlyr <- terra::nlyr(ndvi_rast)
  if (nlyr == 0L) {
    stop("ndvi_rast has no layers.")
  }
  if (nlyr != length(dates)) {
    stop(
      "Number of raster layers (", nlyr, ") does not match length(dates) (",
      length(dates), ")."
    )
  }
  dates_key <- if (inherits(dates, "Date")) {
    dates
  } else {
    as.Date(paste0(dates, "-01"))
  }
  gm <- terra::global(ndvi_rast, fun = "mean", na.rm = TRUE)
  vals <- as.numeric(gm[[1L]])
  data.frame(
    YearMonth = dates_key,
    NDVI      = vals,
    Year      = format(dates_key, "%Y"),
    Month     = format(dates_key, "%m"),
    stringsAsFactors = FALSE
  )
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

# Extract daily burned area (km²) for one year from a set of monthly TIF files.
# pixel_area_km2: area per raster cell (e.g. 0.25 for 500m).
get_ba_daily_activity <- function(files_df, data_path, year_val, pixel_area_km2 = 0.25) {
  year_files <- files_df %>% dplyr::filter(year == year_val)
  if (nrow(year_files) == 0) return(NULL)

  results <- lapply(seq_len(nrow(year_files)), function(i) {
    r    <- terra::rast(file.path(data_path, year_files$filenames[i]))
    vals <- as.numeric(terra::values(r, na.rm = TRUE))
    vals <- vals[vals > 0]
    if (length(vals) == 0) return(NULL)
    dates      <- as.Date(vals - 1, origin = paste0(year_val, "-01-01"))
    date_counts <- table(dates)
    data.frame(
      date = as.Date(names(date_counts)),
      km2  = as.numeric(date_counts) * pixel_area_km2,
      year = as.character(year_val),
      stringsAsFactors = FALSE
    )
  })
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) return(NULL)
  dplyr::bind_rows(results)
}

# Fast per-month burned area size (km²) — one file at a time, no pixel dataframe.
# Replaces get_ba_raster + get_ba_df for the TS Plotly path.
get_ba_summary_fast <- function(files_df, data_path, aoi_proj) {
  aoi_vect <- terra::vect(aoi_proj)
  results <- lapply(seq_len(nrow(files_df)), function(i) {
    r <- terra::rast(file.path(data_path, files_df$filenames[i]))
    r <- terra::project(r, "EPSG:4326")
    r <- terra::mask(r, aoi_vect)
    burned_r    <- terra::ifel(r > 0, 1L, NA)
    burned_size <- sum(terra::expanse(burned_r, unit = "km"), na.rm = TRUE)
    data.frame(
      YearMonth       = files_df$dates[i],
      Year            = format(files_df$dates[i], "%Y"),
      Month           = sprintf("%02d", files_df$month[i]),
      BurnedArea_Size = burned_size
    )
  })
  dplyr::bind_rows(results)
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
