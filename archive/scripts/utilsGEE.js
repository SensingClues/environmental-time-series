// Export useful functions

// Function to get the AoI using a predefined dict of asset paths
exports.getAoI = function getAoI(aoiPathDict, aoiPath, countryName) {

    return ee.FeatureCollection(aoiPath+aoiPathDict[countryName]);
}

// Function to get date range
exports.getDateRange = function getDateRange(startYear, endYear, startMonth, endMonth) {
    // Get start date
    var startDate = ee.Date.fromYMD(startYear, startMonth, 1);

    // Get end date
    if (endYear === null || endMonth === null) {
        var endDate = startDate.advance(1, 'month');
    } else {
        var endDate = ee.Date.fromYMD(endYear, endMonth, 1);
    }
    return {startDate:startDate,endDate:endDate}
}