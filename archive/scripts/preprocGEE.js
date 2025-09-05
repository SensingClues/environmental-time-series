// Export preprocessing functions

// Function to load the Sentinel-2 surface reflectance image collection.
exports.getCollection = function getCollection(aoi, startDate, endDate, filterCloud) {
    // if we want to filter for initial cloud coverage.
    if (filterCloud === true) {
        var collection = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
        .filterBounds(aoi) // Filter by the AOI.
        .filterDate(startDate, endDate) // Filter by the date range.
        .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 100));
    } else {
        var collection = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
        .filterBounds(aoi) // Filter by the AOI.
        .filterDate(startDate, endDate); // Filter by the date range.
    }
    return collection
}

// Function to mask clouds and other unwanted pixels using the SCL band.
exports.sclMasker = function sclMasker(image) {
    // Scene Classification Layer.
    var scl = image.select('SCL');
    
    // Create a mask for valid pixels (exclude clouds, cloud shadows, snow/ice).
    var mask = scl.neq(1) // 1 = Defective
                  .and(scl.neq(7))  // 2 = Dark area
                  .and(scl.neq(7))  // 3 = Cloud shadow
                  .and(scl.neq(7))  // 7 = Unclassified
                  .and(scl.neq(8))  // 8 = Clouds
                  .and(scl.neq(9))  // 9 = Clouds
                  .and(scl.neq(10)) // 10 = Cirrus
                  .and(scl.neq(11)); // 11 = Snow/ice
  
    // Apply the mask.
    return image.updateMask(mask);
  };

// Function to calculate NDVI for each image.
exports.calcNDVI = function calcNDVI(image) {
    var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
    return image.addBands(ndvi);
  };