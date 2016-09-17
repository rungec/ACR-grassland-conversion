#Processing and analysis associated with proposed new ACR methodology.

#test=function() {source("C:/Claire/NCEAS_Postdoc/P4 ACR revised methods/Analysis/github/ACR_raster_processing.r")}

##Preliminary
library(raster)
library(rgdal)


# Define your temp folder
#my_tmpdir='~/tmpRaster'
my_tmpdir = 'C:/Claire/tmpRaster/'

# Create it (handles the case where the folder already exists)
dir.create(my_tmpdir, showWarnings=F)

# Set the raster option to this folder
rasterOptions(tmpdir= my_tmpdir)

inpNLCD <- "Z:/Data/NCEAS_Postdoc/P1 Sage Grouse/Data/Original/LULC/National Land Cover Database 2011/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img"
outDirNLCD <- "C:/Claire/NCEAS_Postdoc/P4 ACR revised methods/Analysis/NLCD mask"


#################
# #NLCD extract a raster containing only only pixels classified as grassland, shrubland & wetland
NLCDRast <- raster(inpNLCD)
newNCLD <- subs(NLCDRast, y=data.frame(vals=c(52, 71, 90, 95), newval=rep(1,4), by=1, which=2, subsWithNA=TRUE, filename=paste0(outDir, "/NLCD_grassland.tif"), format=GTiff) #replaces any values not in 'vals' with NAs
