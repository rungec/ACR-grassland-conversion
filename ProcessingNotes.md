#PROCESSING NOTES for study of revised methods for American Carbon Registry 


------------
##Processing of spatial layers - Cropland conversion
###Map of private land in each county
*manual in ArcGIS*  
PADUSCBIv2_Private_Land_AllCounty selected by attributes only private lands (this dataset has private lands overlaid with county boundaries). Export to 
> PADUSCBIv2_Private_Land_only_AllCounty.shp  

Then dissolve by state and county name to get a single, multipart, polygon per county  
> PADUSCBIv2_Private_Land_only_AllCounty_diss.shp  

Removed sliver polygons from edges of usa & border with canada (editor, then delete any rows with ADMIN_FIPS blank). Copied 'District of Columbia' from ADMIN_NAME into NAME field

###How much cropland was converted from forest
*manual in ArcGIS*  
Tabulate by area (Zonal toolbox) of class_before_crop.tif using class_before_crop as zonal raster. This gives the area of each lulc type that was converted into cropland between 2008 and 2012
> Class_before_crop.txt  

Saved as .xlsx and edited to include the name of each class

< 3% of land converted to cropland came from forest, about 40% from shrubland or grassland and about 40% from pasture/hay. So I can safely ignore conversion from forest and/or developed land. Given the classification errors, it may be best to treat pasture/hay as if it were grassland or had the potential to be grassland.

###Calculate area of land converted to cropland in each county
*manual in ArcGIS*  
Tabulate by area (Zonal toolbox) of Multitemporal_Results_FF2.tif using PADUSCBIv2_Private_Land_only_AllCounty_diss.shp ADMIN_FIPS as zones, and with cell size set to Multitemporal_Results_FF2.tif. Classes of Multitemporal_Results_FF2.tif are 1=stable noncrop, 2= stable crop, 3= converted to crop, 4= abandoned, 5=intermittent cropland. Joined to PADUSCBIv2_Private_Land_only_AllCounty_diss by ADMIN_FIPS and exported table   
> LarkCDL_PrivateArea_byCounty.csv / xlsx  

Areas are in m2. The sum of values 0 to 5 is the sum area of private land in each county.

###Calculate area of land in forest & developed in each county
*manual in ArcGIS*  
We assume that the area of forest and developed land did not change from the 2011 NLCD layer, and that forest & developed land are not converted to cropland. Under these assumptions, the area of grassland (or pasture/hay) in each county is equivalent to difference between the stable area under non-cropland from the lark cdl, minus the area of forest or developed land from the nlcd.  

Tabulate by area (Zonal toolbox) of nlcd_2011_landcover_2011_edition_2014_10_10.img using PADUSCBIv2_Private_Land_only_AllCounty_diss.shp ADMIN_FIPS as zones, and with cell size set to nlcd_2011_landcover_2011_edition_2014_10_10.img (30m)  
Joined to PADUSCBIv2_Private_Land_only_AllCounty_diss by ADMIN_FIPS and exported table  
> NLCD_PrivateArea_byCounty.csv  

Areas are in m2. 

###Mask Forest, Water and Developed land from Lark's CDL data
*manual in ArcGIS*  
Now using CDL as a grassland mask instead of the NLCD - conversion probabilities are calculated for land that is not classified as water, forest or developed in the 2015 CDL (i.e. grassland mask includes only pixels classified as shrubland, wetlands, native grassland and pasture/hay).  

Use 'Con' tool (conditional). If Class_Name in 2015 CDL layer 2015_30m_cdls.img is
"Class_Name" = 'Clouds/No Data' OR "Class_Name" = 'Deciduous Forest' OR "Class_Name" ='Developed'OR "Class_Name" = 'Developed/High Intensity'OR "Class_Name" = 'Developed/Low Intensity' OR "Class_Name" ='Developed/Med Intensity'OR "Class_Name" = 'Developed/Open Space' OR "Class_Name" ='Evergreen Forest' OR "Class_Name" ='Forest'OR "Class_Name" = 'Mixed Forest' OR "Class_Name" ='Open Water'OR "Class_Name" = 'Perennial Ice/Snow' OR "Class_Name" ='Water'
then reclassify Multitemporal_Results_FF2.tif as value 15 (no data). Snap to and extent of Multitemporal_Results_FF2.tif
> LarkCDL_grassland.tif

###Mask Forest, Water and Developed land from Lark's CDL data - year_from_crop_ff2.tif
*manual in ArcGIS*  
Use 'Con' tool (conditional). If Class_Name in 2015 CDL layer 2015_30m_cdls.img is
"Class_Name" = 'Clouds/No Data' OR "Class_Name" = 'Deciduous Forest' OR "Class_Name" ='Developed'OR "Class_Name" = 'Developed/High Intensity'OR "Class_Name" = 'Developed/Low Intensity' OR "Class_Name" ='Developed/Med Intensity'OR "Class_Name" = 'Developed/Open Space' OR "Class_Name" ='Evergreen Forest' OR "Class_Name" ='Forest'OR "Class_Name" = 'Mixed Forest' OR "Class_Name" ='Open Water'OR "Class_Name" = 'Perennial Ice/Snow' OR "Class_Name" ='Water'
then reclassify year_from_crop_ff2.tif as value 65535 (no data). Snap to and extent of year_from_crop_ff2.tif
> LarkCDL_yearfromcrop_grassland.tif

###Calculate area of grassland converted to cropland in each county
*manual in ArcGIS*  
Tabulate by area (Zonal toolbox) of LarkCDL_grassland.tif using PADUSCBIv2_Private_Land_only_AllCounty_diss.shp ADMIN_FIPS as zones, and with cell size set to Multitemporal_Results_FF2.tif  
Classes of LarkCDL_grassland.tif are 1=stable noncrop, 2= stable crop, 3= converted to crop, 4= abandoned, 5=intermittent cropland , 15=forest,water or developed  
Joined to PADUSCBIv2_Private_Land_only_AllCounty_diss by ADMIN_FIPS and exported table    
> LarkCDL_GrasslandPrivateArea_byCounty.csv  

Areas are in m2. The sum of values 0 to 5 is the sum area of private grassland or cropland in each county.

###Calculate area of grassland converted to cropland in each county in each year
*manual in ArcGIS*  
Tabulate by area (Zonal toolbox) of LarkCDL_yearfromcrop_grassland.tif using PADUSCBIv2_Private_Land_only_AllCounty_diss.shp ADMIN_FIPS as zones, and with cell size set to Multitemporal_Results_FF2.tif  
Joined to PADUSCBIv2_Private_Land_only_AllCounty_diss by ADMIN_FIPS and exported table    
> LarkCDL_GrasslandPrivateYearConverted_byCounty.csv  
Areas are in m2 of private grassland converted to cropland in each year in each county.



------------
##Processing of land rental rates
*ACR_raster_processing.r*  
Calculated average, min and max rental rate for cropland & pasture in each county. Counties without data were filled from the combined county data for the district and state which that county falls within.  
Data from https://quickstats.nass.usda.gov/results/8275FFAE-5319-3417-AA97-F95A3384A3AD
see README_NASSdata.txt for more info
Data input
> NASS_LandRents_2008_2016.csv  

Data output  
> NASS_LandRents_2008_2012_allRents.csv
> NASS_LandRents_2008_2012_IrrigatedCropland.csv
> NASS_LandRents_2008_2012_NonirrigatedCropland.csv
> NASS_LandRents_2008_2012_Pasture.csv

------------
##Combine datasets
*ACR_raster_processing.r*  
Combined LarkCDL_Grassland_PrivateArea_byCounty.csv and NASS_LandRents_2008_2012_allRents.csv based on FIPS code.  
Calculated land conversion probabilites, ignoring land that is abandoned or intermittent cropland.  


------------
##NOTES on DATASETS

###NLCD metadata
values 52, 71, 90, 95 includes grassland , shrubland, wetland

class       : RasterLayer 
dimensions  : 104424, 161190, 16832104560  (nrow, ncol, ncell)
resolution  : 30, 30  (x, y)
extent      : -2493045, 2342655, 177285, 3310005  (xmin, xmax, ymin, ymax)
coord. ref. : +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
data source : ~ NCEAS_Postdoc\P1 Sage Grouse\Data\Original\LULC\National Land Cover Database 2011\nlcd_2011_landcover_2011_edition_2014_10_10\nlcd_2011_landcover_2011_edition_2014_10_10.img 
names       : nlcd_2011_landcover_2011_edition_2014_10_10 
values      : 0, 95  (min, max)
attributes  :
        ID      COUNT Red Green Blue NLCD.2011.Land.Cover.Class Opacity
 from:   0 7854240512   0     0    0               Unclassified     255
 to  : 255          0   0     0    0                                  0

###CDL
64=shrubland
87=wetlands
152=shrubland
176=grass/pasture
190=woody wetlands
195=herbaceous wetlands
