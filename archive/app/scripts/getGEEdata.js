// Script to collect NDVI composites from Google Earth Engine
// for a given time range (minumum is a single month)
//  - includes cloud removal using the Scene Classification Layer (SCL)
//  - script requires an AoI file to be available in the assets

// Import the utilities module
var utils = require('users/sverissimoines/default:utilsGEE.js'); // set path to GEE scripts folder <users/username/repository:utilsGEE.js>

// Import the preprocessing module
var preproc = require('users/sverissimoines/default:preprocGEE.js'); // set path to GEE scripts folder <users/username/repository:preprocGEE.js>

// load main parameters from file
var params = require('users/sverissimoines/default:paramsGEE.js'); // set path to GEE scripts folder <users/username/repository:paramsGEE.js>

// Load the area of interest (AOI) from an asset.
var aoi = utils.getAoI(params.aoiPathDict, params.aoiPath, params.countryName);

// Define the start and end dates
var timeRange = utils.getDateRange(params.startYear, params.endYear, params.startMonth, params.endMonth);

// Initialize the "current" date (i.e., month that is being processed)
var currentDate = timeRange.startDate;

// Loop through each month
while (currentDate.difference(timeRange.endDate, 'days').getInfo() < 0){
    // Print the month that is being processed
    print('Processing date:', currentDate.format('YYYY-MM'));

    // Define the end of the "current" month
    var currentMonthEnd = currentDate.advance(1, 'month').advance(-1, 'second');
    
    // Preprocess data
    //
    // Load the Sentinel-2 surface reflectance image collection.
    var collection = preproc.getCollection(aoi, currentDate, currentMonthEnd, params.filterCloud);

    // Print image count, for bookeeping
    print('Image count:', collection.size());

    // Mask clouds and other unwanted pixels using the SCL band.
    var cloudFreeCollection = collection.map(preproc.sclMasker);

    // Map the NDVI calculation over the cloud-free collection.
    var withNDVI = cloudFreeCollection.map(preproc.calcNDVI);

    // Create a composite by applying a mosaic to the NDVI values for the month.
    var ndviComposite = withNDVI.select('NDVI').mosaic();

    // // Add layers to the map.
    // Map.centerObject(aoi, 10);
    // Map.addLayer(ndviComposite.clip(aoi), params.ndviParams, 'SCL Cloud-Free NDVI Composite');
    // //Map.addLayer(aoi, {color: 'red'}, 'AOI');

    // Export NDVI Image
    Export.image.toDrive({
        image: ndviComposite.clip(aoi),
        description: currentDate.format('YYYY-MM').getInfo() + '_NDVI_' + params.countryName,
        folder: params.outputFolder,
        fileNamePrefix: currentDate.format('YYYY-MM').getInfo() + '_NDVI_' + params.countryName,
        region: aoi,
        scale: params.resolution,
        crs: params.crs, 
        maxPixels: 1e9
    });
    
    // Increment the date to the next month
    currentDate = currentDate.advance(1, 'month');
}

