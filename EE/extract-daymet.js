// ### Extract Daymet
// # Alec Robitaille
// # June 2019
 
// ### Image Collection Import
// daymet
var daymet = ee.ImageCollection('NASA/ORNL/DAYMET_V3');


// ### Variables
// Set type
var type = 'random1';

// Import feature collections
var observed = ee.FeatureCollection('users/ededeban/Observed-emilie-nlcaribou');
var random1 = ee.FeatureCollection('users/ededeban/Randoms1-emilie-nlcaribou');
var random2 = ee.FeatureCollection('users/ededeban/Randoms2-emilie-nlcaribou');


if(type == 'observed'){
  var points = observed;
} else if (type == 'random1'){
  var points = random1;
} else if (type == 'random2'){
  var points = random1;
}

var outputString  = type + '-daymet';

// Year range to extract
var lowYear = 2010; 
var highYear = 2013;

var lowJul = 50;
var highJul = 120;

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
                    .flatten();

// ### Filter
var matchDate = function(ft) {
  var imgDay = ee.Number(ft.get('jul'));
  var ptDay = ee.Number(ft.get('JDate'));
  var diffDay = imgDay.subtract(ptDay);
  
  var imgYr = ee.Number(ft.get('yr'));
  var ptYr = ee.Number(ft.get('Year'));
  var diffYr = imgYr.subtract(ptYr);
  return ft.set({
    'diffDay': diffDay, 
    'diffYr': diffYr});
};


// add the difference day column and 
//   filter to where the days are the same
reduced = reduced
  .map(matchDate)
  // .filter(ee.Filter.neq('daymet', null))
  .filter(ee.Filter.eq('diffDay', 0))
  .filter(ee.Filter.eq('diffYr', 0));

// ### Export
Export.table.toDrive(reduced, outputString, 'Daymet');