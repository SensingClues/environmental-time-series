// Script to define the necessary parameters
// to load and save composite data from Google Earth Engine

// Country name
var countryName = 'Zambia';
exports.countryName = countryName;

// Specify the years to process (if looking at 1 month only, set endYear = null)
exports.startYear = 2020; 
exports.endYear = null; //2024 

// Specify the month to process (if looking at 1 month only, set endMonth = null)
exports.startMonth = 2; 
exports.endMonth = null; //12 

// Resolution in meters
var resolution = 10; 
exports.resolution = resolution;

// CRS for projection 
exports.crs = 'EPSG:4326'; // Specify CRS as EPSG:4326

// Paths
var aoiPath = 'projects/ee-sensingclues-timeseries/assets/'; // path to get AoI mask
exports.aoiPath = aoiPath;
exports.outputFolder = 'GEE_' + countryName + '_' + resolution+'m_resolution'; // Output folder in Google Drive <GEE_Zambia_10m_resolution>

// make dict of aoi filenames
exports.aoiPathDict = {
  'Bulgaria':'AoI_Bulgaria_BAS', 
  'Kenya': 'AoI_Kenya_Wildlife_Works', 
  'Spain': 'AoI_Spain_3eData',
  'Zambia': 'AoI_Zambia_By_Life_Connected'};

// Preprocessing parameters
exports.filterCloud = true; // if we want to mask out clouds

// Visualize the NDVI composite.
exports.ndviParams = {
    min: -1,
    max: 0.8,
    palette: ['brown', 'white', 'green']
  };



