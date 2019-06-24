// ### Extract Daymet
// # Alec Robitaille
// # June 2019

// ### Image Collection Import
// NDVI
var daymet = ee.ImageCollection('NASA/ORNL/DAYMET_V3');


// ### Variables
// Set type
var type = 'observed';

// Import feature collections
var observed = ee.FeatureCollection('users/emilie/observed');
var random = ee.FeatureCollection('users/emilie/random');


if(type == 'observed'){
  var dt = observed;
} else {
  var dt = random;
}

var outputString  = type + '-daymet';

// Year range to extract
var lowYear = 2007; 
var highYear = 2013;

var lowJul = 100;
var highJul = 150;

// ### Filter
ndvi = daymet
  .filter(ee.Filter.calendarRange(lowYear, highYear, 'year'))
  .filter(ee.Filter.dayOfYear(lowJul, highJul));