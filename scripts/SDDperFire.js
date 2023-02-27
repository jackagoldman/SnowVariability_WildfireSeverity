// ---------- Snow Disappearance Date Script -------------//

// modified from: Crumley, R. L., R. T. Palomaki, A. W. Nolin, E. A. Sproles, and E. J. Mar. 2020. SnowCloudMetrics: Snow Information for Everyone. Remote Sensing 12:3341.
// https://dx.doi.org/10.3390/rs12203341

// --------------- Inputs ------------------------//
// ----------- fire perimeters ------------------//
var fire = ee.FeatureCollection("users/jandrewgoldman/OnShield_FirePerim_01to2020").filter(ee.Filter.eq('Fire_Year', 2013));
// set images folder
var imagesFolder = 'SDDperFire'
// create two lists: one with fire names and the other with fire IDs 
var fireID    = ee.List(fire.aggregate_array('Fire_ID')).getInfo();
var nFires = fireID.length;
//------------------ SDD function inputs ------------//
// Change the year only and run
var wyr = 2013;
// Set the month and day of year for the Water Year to begin
var wyStartDay = '-10-01';

var NDSI_threshold = '15'

// Set the lower end of the SCF values for masking
// This is OPTIONAL. Setting this value to 0 will not mask any values.
// This value equals about 30 days of snow or less that is being masked. 
// This is set because without it there are 'snow' designations all across the southern U.S., Mexico, Central America, Africa, etc.
// My guess is that agricultural water bodies are being mistaken for snow, rice growing, flooded fields, and etc.
var low_value = 0.07;

// Set the prior_days parameter for the SDD calculation.
// This gets fed into the 'min_sdays_prior_to_SDD' image and variable.
// Setting this value means there are [prior_days] days that snow must be seen/designated in the pixel before the SDD date can be set.
var prior_days = 5;

// Set the parameter below for the SDD calculation.
// This gets fed into the 'minNoSnow' image and variable.
// Setting this value means there are [minNoSnowdays] days that snow must be absent from the pixel after the SDD date is set.
var minNoSnowdays = 5;

// Set the projection for the geotiff export
var projection = 'EPSG:4326';

//////////////////////////////////////////////
////////    END USER INPUT      //////////////
//////////////////////////////////////////////


// Call the getMetrics variable to run the function on the water year defined above.
var getMetrics = function(wyr){
  
    
    
    // Take the end of the water year and create a start and end date.
    var start = (wyr - 1) + wyStartDay;
    print(start,'start');
    var end = wyr + wyStartDay;

    // Take the end date of the water year and increase it by 30 days.
    var start_adv = end;
    var end_adv = ee.Date(end).advance(30,'day');
    print(start_adv, 'start_adv')
    print(end_adv,'end_adv');
    
    // Advance the days of interest by 30 days into the future
    var adv_days = ee.ImageCollection("MODIS/006/MOD10A1")
      .filterDate(start_adv,end_adv)
      .size().getInfo();
    print(adv_days,'adv_days')
    
    
    /////////////// ACCESS the DATA and do some things  //////////////////////////
    ///////////////////////////  SATELLITE DATASETS  /////////////////////////////
    //////////////////////////////////////////////////////////////////////////////

    
    // This makes a list of images where the MOD10A1v6 designation equals cloud(250).
    // See printout to console for more info.
    // This creates a yes/no (0/1) image of cloud locations each day of the year.
    var MODIS_cloud_list = ee.ImageCollection("MODIS/006/MOD10A1")
      .filterDate(start,end_adv)
      .map(function(img) {
        return img.expression("(BAND==250)",{BAND:img.select('NDSI_Snow_Cover_Class')})})
      .toList(400);
      print(MODIS_cloud_list,'MODIS_cloud_list');
    
    // This makes a list of images where the snow value is between 1 and 100 based on the snow 
    // cover band. This creates an yes/no (0/1) image of snow locations each day of the year.
    // See printout to console for more info.
    var MODIS_snow_list = ee.ImageCollection("MODIS/006/MOD10A1")
      .filterDate(start,end_adv)
      .map(function(img) {
        return img.expression('(BAND>='+ NDSI_threshold +'&&BAND<=100)',{BAND:img.select('NDSI_Snow_Cover')})})
      .toList(400);
      print(MODIS_snow_list,'MODIS_snow_list');
      
    // Access the 30m Landsat datset for water loctions. I don't want these metrics
    // to be calculated over inland water bodies!
    // See printout to console for more info.
    var water = ee.ImageCollection("GLCF/GLS_WATER")
        .map(function(img) {
          return img.expression("(BAND==2)",{BAND:img.select('water')})}).or().not()
    print(water,'This is water from LandSat')
    // Map.addLayer(water,{'palette':palette_snow,'min':0, 'max':1}, 'Inland Water 0=water, 1=not water');
    
    
    // Get number of days in the Water Year
    var ndays = MODIS_snow_list.length().getInfo() - adv_days;
    print(ndays,'ndays');
    
    //% This is a correction for leap years.
    var sddCorrection = (ndays < 365 + (wyr % 4 ? 0 : 1)) ? 365 + (wyr % 4 ? 0 : 1) - ndays : 0;
    print('Processed ' + ndays + ' days in ' + wyr);
    
    ///////////////////////////////////////////////////////////////////////////
    // Open a bunch of image variables with zero values, or other defined values
    
    var scfImage = 'MODIS_SCF_' + wyr;
    print(scfImage,'scfImage');
    var sddImage = 'MODIS_SDD_' + wyr;
    print(sddImage,'sddImage');
    sddImage = ee.Image(0);
    
    var future_snow = ee.Image(0);
    var Snow = ee.Image(0);
    var accSnow = ee.Image(0);
    var ephemeral_counter = ee.Image(0);
    var noSnowCounter = ee.Image(0);
    var minNoSnow = ee.Image(minNoSnowdays);
    var min_sdays_prior_to_SDD = ee.Image(prior_days);
    var sddDetected = ee.Image(0);
    var MODIS_max_ndays_nosnow = ee.Image(0);
    
    //////////////////////////////////////////////////////////
    ///////////////  Do The STUFFFFFF  ///////////////////////
    //////////////////////////////////////////////////////////
    // Increment backwards from the end of the water year + 30 days
    // REMEMBER: This is complex because it is moving backwards in time. Its really easy to forget. 
    // All the commenting is interpreted through the backwards-through-time lens.
    for(var current_day =ndays + adv_days-1; current_day>=0; current_day--) {
      
      // During those 30 days after the water year, build some 'memory' of where snow is and is not in the variables
      // Take cloud locations from MODIS and create cloud image of 1=cloud, 0=everything else
      var Cloud = ee.Image(MODIS_cloud_list.get(current_day)).unmask();
      
      // If there is a cloud designation TODAY and future_snow variable=1, from tomorrow or snow=1 tomorrow, set to 1. 
      // Essentially, 'remembering' where snow is located in the future
      future_snow = Cloud.and(future_snow.or(Snow));
      
      // Take snow locations from MODIS TODAY and create snow image of 1=snow, 0=everything else for TODAY
      Snow = ee.Image(MODIS_snow_list.get(current_day)).unmask();
      
      // If TODAY is in the water year of interest do all these things
      // Because of advanced days process above, we have snow 'memory' of the future
      if (current_day < ndays) {
        
        // If TODAY is snow in the pixel or if there is future snow in the pixel, snow_occurrence = 1 
        var snow_occurrence = future_snow.or(Snow);
        
        // Count the number of days snow has occurred in the future in the pixel and then accumulate it to this variable.
        // This is the last time we use accsnow to create the final SCF_image
        accSnow = accSnow.add(snow_occurrence);
        
        ////////////////////////////////////////////////////////////////////
        // Everything below in the for loop is to deal with the SDD issues.
        // This accesses the variable defined as prior_days and creates a conditional statement
        // If prior_days is greater than the ephemeral counter, and snow does not occur in the future, reset the value of this variable to ephemeral counter value
        // Purpose:
        min_sdays_prior_to_SDD = min_sdays_prior_to_SDD.where(snow_occurrence.eq(0).and(ephemeral_counter.gt(min_sdays_prior_to_SDD)), ephemeral_counter);
        
        // The number of days after the the TODAY that has snow_occurrence=1 (snow or previously snow)
        ephemeral_counter = (ephemeral_counter.add(snow_occurrence)).multiply(snow_occurrence);
        
        // A running count of the number of days when snow_occurrence was equal to 0 from end of water year to now
        // SDD cannot occur until some number of days without snow (minNoSnow) happen at the end of the year
        noSnowCounter = (noSnowCounter.add(snow_occurrence.eq(0))).multiply(sddDetected.eq(0));
        
        // Day of snow disappearance
        // Number of days of snow prior to CURRENT day must excede a value set by prior_days variable
        // And there must be a minimum number of days without snow after the CUURENT day (minNoSnow)
        sddDetected = (ephemeral_counter.gt(min_sdays_prior_to_SDD)).and(noSnowCounter.gt(minNoSnow));
        
        
        minNoSnow = minNoSnow.where(sddDetected, noSnowCounter);
        
        // Find the day of snow disappearance by adding some values to the CURRENT day
        var sdd1 = current_day+sddCorrection;
        var sdd = min_sdays_prior_to_SDD.add(ee.Image(sdd1));
        sddImage = sddImage.where(sddDetected, sdd);
        
      }
    }
    
    sddImage = sddImage.where(accSnow.eq(ndays-1), ndays+sddCorrection);
    
    
    // Make the final scf image by dividing the number of snow days by the number of days in the year
    // Also, mask out all the inland water bodies using the Landsat dataset (its more precise than the MODIS land mask, for sure)
    scfImage = accSnow.divide(ndays).updateMask(accSnow).rename('ccSCF').updateMask(water);
    print(scfImage);
    
    // Make the final scf image
    // Also, mask out all the inland water bodies
    sddImage = sddImage.updateMask(sddImage).rename('SDD').updateMask(water);
    print(sddImage);
    
    ////////////////////////////////////////////////////
    /////////       Mask the images         ////////////
    ////////////////////////////////////////////////////
    // Mask the low_values in SCF.
    var scfimg = scfImage;
    print(scfimg, 'SCF');
    var low_end = ee.Image(low_value);
    var lower_mask = scfimg.gte(low_end);
    var SCF = scfimg.updateMask(lower_mask);
    //Map.addLayer(SCF,{'palette':palette_snow,'min':low_value, 'max':1},'SCF (post-mask)');
    
    // Mask the SDD image by the low_values image above, just so we have the same coverage
    var sddimg = sddImage;
    var SDD = sddimg.updateMask(lower_mask);
    //Map.addLayer(SDD,{'palette':palette_sdd,'min':0, 'max':365},'SDD (post-mask)');
    
    
    // Uncomment below for western_US or any other user defined area
    // Export your domain of interest to your drive
    // Export.image.toDrive({
    // image: SCF,
    // description: 'SCF'+ wyr + 'Ontario_Shield',
    // scale: 500,
    // crs: projection,
    // region: my_domain,
    // folder: imageryFolderSCF,
    // maxPixels:1e12,
    // });

    // Export.image.toDrive({
    // image: SDD,
    // description: 'SDD'+ wyr + 'Ontario_Shield',
    // scale: 500,
    // crs: projection,
    // region: my_domain,
    // folder: imageryFolderSDD,
    // maxPixels:1e12,
    // });

  return(SDD)
};

var snowReturn = getMetrics(wyr)

for( var i = 0; i < nFires; i++) {
  var id = fireID[i];
  var Name = id;
  var fiya = fire.filter(ee.Filter.eq('Fire_ID', id))
  var my_region = fiya.geometry()
  var snowClip = snowReturn.clip(my_region)

  // for (var j = 0; j < nFires; j++){
    //var fireExport = forestCover.filter(ee.Filter.eq('fireID', id))
  Export.image.toDrive({
    image: snowClip,
    description: id + '_' + 'SDD',
    scale: 500,
    crs: 'EPSG: 4326',
    region: my_region,
    folder: imagesFolder,
    maxPixels:1e12,
    });
}