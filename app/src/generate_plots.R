
# -----------------------------------------------------------------------------
# GENERATE PLOTS
# -----------------------------------------------------------------------------

# Function to create NDVI timeseries plot
generate_timeseries <- function(country_name = NULL, resolution = NULL,
                                end_year = NULL, end_month = NULL,
                                figures_dir = NULL, data_dir = NULL,
                                return_plot = FALSE, figure_filename = NULL,
                                land_cover_class = NULL,
                                view = "monthly"
) {

  data_type  <- "NDVI"
  data_path  <- file.path(data_dir, paste0(data_type, "/", country_name, "/", resolution, "m_resolution/"))
  aoi_path   <- file.path(data_dir, "AoI")

  ndvi_files <- get_filenames(filepath = data_path, data_type = data_type,
                              file_extension = ".tif", country_name = country_name)
  aoi_files  <- get_filenames(filepath = aoi_path, data_type = "AoI",
                              file_extension = ".geojson", country_name = country_name)

  files_df <- get_filename_df(ndvi_files = ndvi_files)
  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path, projection = "EPSG:4326")

  ### Setup land cover mask (before annual/monthly branch)
  use_lc      <- !is.null(land_cover_class) && nzchar(land_cover_class)
  land_use_lc <- NULL
  if (use_lc) {
    lulc_path  <- file.path(data_dir, "LandUse", country_name, "S2_10m_LULC_2023")
    lulc_files <- get_filenames(filepath = lulc_path, data_type = "LandUseVector",
                                file_extension = ".geojson", country_name = country_name)
    lc_file    <- lulc_files[grepl(land_cover_class, lulc_files)][1]
    if (is.na(lc_file) || !nzchar(lc_file))
      stop("No LULC GeoJSON found for class: ", land_cover_class)
    land_use_lc <- get_aoi_vector(aoi_files = lc_file, aoi_path = lulc_path, projection = "EPSG:4326")
  }

  ### Annual view: load each year independently, compute annual means
  if (identical(view, "annual")) {
    all_years <- sort(unique(files_df$year))

    # Count unique months per year to detect incomplete years
    months_per_year <- tapply(files_df$month, files_df$year,
                              function(m) length(unique(m)))

    rows_list <- lapply(all_years, function(yr) {
      yr_files <- files_df[files_df$year == yr, ]
      yr_rast  <- get_ndvi_raster(ndvi_files = yr_files$filenames, data_path = data_path,
                                  projection = "EPSG:4326", dates = yr_files$dates,
                                  aoi_proj = aoi_proj)
      if (use_lc) yr_rast <- terra::mask(yr_rast, land_use_lc)
      layer_means <- terra::global(yr_rast, "mean", na.rm = TRUE)$mean
      n_mo <- as.integer(months_per_year[as.character(yr)])
      data.frame(year = yr, mean_ndvi = mean(layer_means, na.rm = TRUE), n_months = n_mo)
    })
    annual_df             <- do.call(rbind, rows_list)
    annual_df$is_complete <- annual_df$n_months == 12L
    annual_stats <- compute_ndvi_annual_stats(annual_df)
    annual_stats$view <- "annual"
    annual_plot  <- plot_ndvi_annual(annual_df, land_cover_class = if (use_lc) land_cover_class else NULL)
    return(list(plot = annual_plot, stats = annual_stats, view = "annual"))
  }

  ### Monthly view (original logic)
  end_date   <- as.Date(paste(end_year, end_month, 1, sep = "-"))
  start_date <- as.Date(paste(end_year, 1, 1, sep = "-"))

  test_files_df  <- filter(files_df, between(dates, start_date, end_date))
  months_in_test <- c(test_files_df$month)
  year_in_test   <- test_files_df$year
  train_files_df <- files_df %>%
    dplyr::filter(month %in% months_in_test & year < year_in_test)

  test_ndvi_msk  <- get_ndvi_raster(ndvi_files = test_files_df$filenames,  data_path = data_path,
                                    projection = "EPSG:4326", dates = test_files_df$dates,
                                    aoi_proj = aoi_proj)
  train_ndvi_msk <- get_ndvi_raster(ndvi_files = train_files_df$filenames, data_path = data_path,
                                    projection = "EPSG:4326", dates = train_files_df$dates,
                                    aoi_proj = aoi_proj)

  if (use_lc) {
    test_ndvi_msk  <- terra::mask(test_ndvi_msk,  land_use_lc)
    train_ndvi_msk <- terra::mask(train_ndvi_msk, land_use_lc)
  }

  if (use_lc) {
    test_ndvi_df  <- get_ndvi_global_means_df(ndvi_rast = test_ndvi_msk,  dates = test_files_df$dates)
    train_ndvi_df <- get_ndvi_global_means_df(ndvi_rast = train_ndvi_msk, dates = train_files_df$dates)
  } else {
    test_ndvi_df  <- get_ndvi_df(ndvi_rast = test_ndvi_msk,  dates = test_files_df$dates)
    train_ndvi_df <- get_ndvi_df(ndvi_rast = train_ndvi_msk, dates = train_files_df$dates)
  }

  ndvi_ts_plot <- plot_ndvi_anomaly(
    train_ndvi_df    = train_ndvi_df,
    test_ndvi_df     = test_ndvi_df,
    land_cover_class = if (use_lc) land_cover_class else NULL
  )

  if (isTRUE(return_plot)) {
    stats <- compute_ndvi_explorer_stats(train_ndvi_df, test_ndvi_df)
    stats$view <- "monthly"
    return(list(plot = ndvi_ts_plot, stats = stats, view = "monthly"))
  }

  invisible(NULL)
}

# Function to create Burned Area timeseries plot
generate_ba_timeseries <- function(country_name = NULL, resolution = NULL,
                                   end_year = NULL, end_month = NULL,
                                   figures_dir = NULL, data_dir = NULL,
                                   return_plot = FALSE, figure_filename = NULL,
                                   season_months = 1:12
) {

  ### Set paths and define parameters

  # Fire season (months) the user selected; everything below (plot, ribbon,
  # climatology and statistics) is restricted to these months.
  season_months <- normalize_season_months(season_months)

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
  start_date <- as.Date(paste(end_year, 1, 1, sep="-"))
  
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

  # Keep only the selected fire season months
  files_df <- files_df %>% dplyr::filter(month %in% season_months)
  if (nrow(files_df) == 0) {
    stop("No burned area data available for the selected fire season (months ",
         paste(range(season_months), collapse = "-"), ").")
  }

  # Given date selected, split file into test data and train data
  # test filenames
  test_files_df <- filter(files_df, between(dates, start_date, end_date))
  if (nrow(test_files_df) == 0) {
    stop("No burned area data available for ", end_year,
         " within the selected fire season (months ",
         paste(range(season_months), collapse = "-"), ").")
  }

  # get train filenames (train interval: prior to test interval start)
  months_in_test <- c(test_files_df$month)
  year_in_test   <- max(test_files_df$year)
  train_files_df <- files_df %>%
    dplyr::filter(month %in% months_in_test & year < year_in_test)
  #train_files_df <- files_df[(files_df$dates< start_date),]
  if (nrow(train_files_df) == 0) {
    stop("No historical burned area data available before ", year_in_test,
         " within the selected fire season (months ",
         paste(range(season_months), collapse = "-"), ").")
  }

  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path,
                             projection = "EPSG:4326")

  if (return_plot == TRUE) {
    # Fast path: read one file at a time, skip pixel-level raster→dataframe conversion.
    # Cache train summary + raw monthly series (historical data changes only when new
    # files are added). The raw series feeds the long-term (Seasonal Mann–Kendall) stat.
    # Season is part of the cache key: a different season means a different
    # train subset (and equal file counts across seasons would otherwise
    # silently reuse the wrong cache).
    cache_path <- file.path(cache_dir,
                            paste0("ba_ts_", country_name, "_", resolution,
                                   "_m", paste(range(season_months), collapse = "-"),
                                   "_train.rds"))

    train_raw        <- NULL
    train_ba_summary <- NULL
    if (file.exists(cache_path)) {
      cached <- readRDS(cache_path)
      if (isTRUE(cached$n_files == nrow(train_files_df)) && !is.null(cached$raw)) {
        train_ba_summary <- cached$data
        train_raw        <- cached$raw
      }
    }
    if (is.null(train_raw)) {
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }
      train_raw        <- get_ba_summary_fast(train_files_df, data_path, aoi_proj)
      train_ba_summary <- get_summary_ba_df(ba_df = train_raw)
      saveRDS(list(data = train_ba_summary, raw = train_raw,
                   n_files = nrow(train_files_df)), cache_path)
    }

    test_raw        <- get_ba_summary_fast(test_files_df, data_path, aoi_proj)
    test_ba_summary <- get_summary_ba_df(ba_df = test_raw)

    ba_plot <- plot_ba_timeseries_plotly(
      train_data    = train_ba_summary,
      test_data     = test_ba_summary,
      test_year     = end_year,
      season_months = season_months
    )
    ba_stats <- compute_ba_explorer_stats(train_raw = train_raw, test_raw = test_raw,
                                          season_months = season_months)

    return(list(plot = ba_plot, stats = ba_stats))
  }

  # Slow path (PNG): full pixel-level raster processing
  test_ba_msk <- get_ba_raster(ba_files = test_files_df$filenames, data_path = data_path,
                               projection = "EPSG:4326", dates = test_files_df$dates,
                               aoi_proj = aoi_proj)
  train_ba_msk <- get_ba_raster(ba_files = train_files_df$filenames, data_path = data_path,
                                projection = "EPSG:4326", dates = train_files_df$dates,
                                aoi_proj = aoi_proj)
  test_ba_df   <- get_ba_df(ba_rast = test_ba_msk,  dates = test_files_df$dates)
  train_ba_df  <- get_ba_df(ba_rast = train_ba_msk, dates = train_files_df$dates)
  test_ba_summary  <- get_summary_ba_df(ba_df = test_ba_df)
  train_ba_summary <- get_summary_ba_df(ba_df = train_ba_df)
  test_end_month   <- test_files_df[max(test_files_df$month), ]$dates

  plot_ba_timeseries(train_data = train_ba_summary,
                     test_data = test_ba_summary,
                     country_name = country_name,
                     resolution = resolution,
                     plot_width = 15,
                     plot_height = 8,
                     ylim_range = NULL,
                     test_start_date = start_date,
                     test_end_date = end_date,
                     label_test = paste0("Burned Area ", paste(format(c(start_date, test_end_month), "%b %Y"), collapse = " - ")),
                     label_train = paste0("Burned Area until ", format(start_date, "%b %Y")),
                     label_mean  = paste0("Burned Area monthly average until ", format(start_date, "%b %Y")),
                     save_path = figures_dir,
                     filename  = figure_filename
  )
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
                               ncol = 2,
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
                         n_years = 2,
                         zlim_range = c(-0.7, 0.7),
                         save_path = figures_dir,
                         filename = figure_filename
  )
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
  burn_mask <- ifel(burned_area_raster > 0, 1, 0)
  
  burned_area_raster_masked <- as.polygons(mask(burned_area_raster, burn_mask))
  
  geojson_export_path <- file.path(figures_dir, paste0(tools::file_path_sans_ext(figure_filename), ".geojson"))
  writeVector(burned_area_raster_masked, filename = geojson_export_path, filetype = "GeoJSON", overwrite = TRUE)
  
  return(geojson_export_path)
}

# Function to create NDVI timeseries by land cover (all classes, Plotly when return_plot = TRUE)
generate_timeseries_landcover <- function(country_name = NULL, resolution = NULL, land_use_src = NULL,
                                          end_year = NULL, end_month = NULL,
                                          figures_dir = NULL, data_dir = NULL,
                                          return_plot = FALSE, figure_filename = NULL,
                                          lulc_map_folder_path = NULL,
                                          land_cover_classes = c(
                                            "Crops", "Rangeland", "Water", "Trees",
                                            "Flooded_vegetation", "Built_Area", "Bare_ground"
                                          )
) {
  
  data_type <- "NDVI"
  Sys.setlocale("LC_TIME", "C") # Otherwise creates language inconsistencies, at least locally
  
  data_path <- file.path(data_dir, paste0(data_type, "/",
                                          country_name, "/",
                                          resolution, "m_resolution/"))
  aoi_path <- file.path(data_dir, "AoI/")
  lulc_path <- file.path(data_dir, paste0("LandUse/", country_name, "/", land_use_src, "/"))
  
  end_date <- as.Date(paste(end_year, end_month, 1, sep = "-"))
  start_date <- as.Date(paste(end_year, 1, 1, sep = "-"))
  
  ndvi_files <- get_filenames(filepath = data_path, data_type = data_type,
                              file_extension = ".tif", country_name = country_name)
  
  aoi_files <- get_filenames(filepath = aoi_path, data_type = "AoI",
                             file_extension = ".geojson", country_name = country_name)
  
  lulc_files <- get_filenames(filepath = lulc_path, data_type = "LandUseVector",
                              file_extension = ".geojson", country_name = country_name)
  
  files_df <- get_filename_df(ndvi_files = ndvi_files)
  
  test_files_df <- filter(files_df, between(dates, start_date, end_date))
  
  months_in_test <- c(test_files_df$month)
  year_in_test   <- test_files_df$year
  train_files_df <- files_df %>%
    dplyr::filter(month %in% months_in_test & year < year_in_test)
  
  aoi_proj <- get_aoi_vector(aoi_files = aoi_files, aoi_path = aoi_path,
                             projection = "EPSG:4326")
  
  test_ndvi_msk <- get_ndvi_raster(ndvi_files = test_files_df$filenames, data_path = data_path,
                                   projection = "EPSG:4326", dates = test_files_df$dates,
                                   aoi_proj = aoi_proj)
  
  train_ndvi_msk <- get_ndvi_raster(ndvi_files = train_files_df$filenames, data_path = data_path,
                                    projection = "EPSG:4326", dates = train_files_df$dates,
                                    aoi_proj = aoi_proj)
  
  # AoI-wide historic ribbon (not masked by land cover class)
  train_ndvi_df_aoi <- get_ndvi_global_means_df(ndvi_rast = train_ndvi_msk, dates = train_files_df$dates)
  train_ndvi_summary_aoi <- get_summary_ndvi_df(ndvi_df = train_ndvi_df_aoi)
  
  land_cover_summaries_list <- vector("list", length(land_cover_classes))
  names(land_cover_summaries_list) <- land_cover_classes
  for (lc in land_cover_classes) {
    land_cover_file <- lulc_files[grepl(lc, lulc_files)][1]
    if (is.na(land_cover_file) || !nzchar(land_cover_file)) {
      message("Skipping land cover class ", lc, ": no matching GeoJSON in ", lulc_path)
      next
    }
    land_use_lc <- get_aoi_vector(aoi_files = land_cover_file, aoi_path = lulc_path,
                                  projection = "EPSG:4326")
    test_ndvi_lc <- mask(test_ndvi_msk, land_use_lc)
    train_ndvi_lc <- mask(train_ndvi_msk, land_use_lc)
    test_ndvi_df_lc <- get_ndvi_global_means_df(ndvi_rast = test_ndvi_lc, dates = test_files_df$dates)
    train_ndvi_df_lc <- get_ndvi_global_means_df(ndvi_rast = train_ndvi_lc, dates = train_files_df$dates)
    test_ndvi_summary_lc <- get_summary_ndvi_df(ndvi_df = test_ndvi_df_lc)
    train_ndvi_summary_lc <- get_summary_ndvi_df(ndvi_df = train_ndvi_df_lc)
    land_cover_summaries_list[[lc]] <- dplyr::bind_rows(
      dplyr::mutate(train_ndvi_summary_lc, land_cover = lc, period = "train"),
      dplyr::mutate(test_ndvi_summary_lc, land_cover = lc, period = "test")
    )
  }
  land_cover_summaries <- dplyr::bind_rows(land_cover_summaries_list)
  
  name_ribbon <- paste0("Historical range (until ", format(start_date, "%b %Y"), ")")
  use_map <- !is.null(lulc_map_folder_path) && nzchar(lulc_map_folder_path) &&
    dir.exists(lulc_map_folder_path)
  if (isTRUE(use_map)) {
    combo <- plot_ndvi_landcover_with_map(
      train_ndvi_summary_aoi = train_ndvi_summary_aoi,
      land_cover_summaries   = land_cover_summaries,
      name_ribbon            = name_ribbon,
      lulc_map_folder        = lulc_map_folder_path,
      aoi_sf                 = aoi_proj
    )
    ndvi_ts_plot <- combo$plot
    bbox_stem <- combo$bbox_by_stem
  } else {
    ndvi_ts_plot <- plot_ndvi_landcover_multiline(
      train_ndvi_summary_aoi = train_ndvi_summary_aoi,
      land_cover_summaries   = land_cover_summaries,
      name_ribbon            = name_ribbon
    )
    bbox_stem <- NULL
  }
  
  if (isTRUE(return_plot)) {
    return(list(plot = ndvi_ts_plot, bbox_by_stem = bbox_stem))
  }
  
  invisible(NULL)
}
