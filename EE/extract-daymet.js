// ### Extract Daymet
// # Alec Robitaille
// # June 2019
 
// ### Image Collection Import
// daymet
var daymet = ee.ImageCollection('NASA/ORNL/DAYMET_V3');


// ### Variables
// Set type
var type = 'observed';

// Import feature collections
var observed = ee.FeatureCollection('users/ededeban/Observed-emilie-nlcaribou');
// var random = ee.FeatureCollection('users/ededeban/Randoms-emilie-nlcaribou');


if(type == 'observed'){
  var points = observed;
} else {
  var points = random;
}

var outputString  = type + '-daymet';

// Year range to extract
var lowYear = 2007; 
var highYear = 2013;

var lowJul = 100;
var highJul = 150;

// ### Filter
daymet = daymet
  .filter(ee.Filter.calendarRange(lowYear, highYear, 'year'))
  .filter(ee.Filter.dayOfYear(lowJul, highJul));

// ### Add year
var addYear = function(img) {
  return(img.addBands(ee.Image(img.date().get('year')).float().rename('yr')));
};

daymet = daymet.map(addYear);
Map.addLayer(daymet)

// ### Sample
var samplePts = function(img) {
  return img.reduceRegions(points, ee.Reducer.mean())
            .copyProperties(points);
};

var reduced = daymet.map(samplePts)
									.flatten()
									.filter(ee.Filter.neq('daymet', null));

// ### Filter
// TODO: filter based off jul of point and jul of pic (and year)
print(reduced.limit(10))

// ### Export
// Export.table.toDrive(reduced, outputString, 'IRG');