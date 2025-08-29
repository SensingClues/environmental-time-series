
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
  aoi_path <- file.path(data_dir, "AoI/")

  ## define end and start date for test data
  end_date <- as.Date(paste(end_year, end_month, 1, sep="-"))
  start_date <- seq(end_date, length = 2, by = "-11 months")[2]

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
  train_files_df <- files_df[(files_df$dates< start_date),]

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
                                        label_train = paste0("NDVI < ", format(start_date, "%b %Y") ),
                                        save_path = figures_dir,
                                        filename = figure_filename
                                        )
  
  # if we want to return the ggplot object
  if (return_plot == TRUE) {

    return(ndvi_ts_plot)

  }

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
  start_date <- seq(end_date, length = 2, by = "-11 months")[2]

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
  train_files_df <- files_df[(files_df$dates< start_date),]

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
                                        label_train = paste0(land_cover_type, " NDVI < ", format(start_date, "%b %Y") ),
                                        save_path = figures_dir,
                                        filename = figure_filename
                                        )
  
  # if we want to return the ggplot object
  if (return_plot == TRUE) {

    return(ndvi_ts_plot)

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

