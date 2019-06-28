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

// ### Add year and julian day
var addJulian = function(img) {
  return(img.addBands(
    ee.Image(img.date()
                .getRelative('day', 'year'))
                .float()
                .rename('jul')));
};

var addYear = function(img) {
  return(img.addBands(
    ee.Image(img.date()
                .get('year'))
                .float()
                .rename('yr')));
};

daymet = daymet
  .map(addJulian)
  .map(addYear);
  
Map.addLayer(daymet);

// ### Sample
var samplePts = function(img) {
  return img.reduceRegions(points, ee.Reducer.mean())
            .copyProperties(points);
};

var reduced = daymet.map(samplePts)
								  	.flatten()
							  		.filter(ee.Filter.neq('daymet', null));

// ### Filter
var matchDay = function(ft) {
  var imgDay = ee.Number(ft.get('jul'));
  var ptDay = ee.Number(ft.get('JDate'));
  var diff = imgDay.subtract(ptDay);
  return ft.set('matchDay', diff);
};


// add the difference day column and 
//   filter to where the days are the same
reduced = reduced
  .map(matchDay)
  .filter(ee.Filter.eq('matchDay', 0));

print(reduced.limit(10))

// ### Export
// Export.table.toDrive(reduced, outputString, 'IRG');