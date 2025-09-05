# ----------------------------------------------------------------
# Short script for changing the country suffix for input NDVI Data
# ----------------------------------------------------------------

path <- "app/www/data/NDVI/Zambia_Mponda/100m_resolution" # Deze was voor lokaal, op de server zal het iets als "/home/timeseries/NDVI/Zambia_Mponda/Sentinel_1000m_resolution" zijn
country_name <- "Zambia" # Oude country name
new_suffix <- "Mponda" # Nieuwe suffix, e.g. "Zambia" >>> "Zambia_Mponda"

# Function
rename_country_files <- function(path, country_name, new_suffix) {
  # list files
  files <- list.files(path, full.names = TRUE)
  
  # get only the file names without path
  fnames <- basename(files)
  
  # build the replacement: e.g. "Zambia" -> "Zambia_Mponda"
  replacement <- paste0(country_name, "_", new_suffix)
  
  # replace only the *first* occurrence of country_name
  new_fnames <- sub(country_name, replacement, fnames)
  
  # create full paths
  new_files <- file.path(path, new_fnames)
  
  # rename
  ok <- file.rename(files, new_files)
  
  # return a data.frame so you see what happened
  data.frame(
    old = fnames,
    new = new_fnames,
    renamed = ok
  )
}

# Function call
rename_country_files(path, country_name, new_suffix)
