#Processing and analysis associated with proposed new ACR methodology.

#test=function() {source("C:/Claire/NCEAS_Postdoc/P4 ACR revised methods/Analysis/github/ACR_raster_processing.r")}

library(tidyr) #for spread() which is like reshape
library(raster)
library(rgdal)

wd <- "Z:/Data/NCEAS_Postdoc/P4 ACR revised methods"
wd <- "C:/Claire/NCEAS_Postdoc/P4 ACR revised methods"
wd <- "D:/P4 ACR revised methods"
setwd(wd)

# Define your temp folder
my_tmpdir = paste0(wd, '/tmpRaster/')

# Create it (handles the case where the folder already exists)
dir.create(my_tmpdir, showWarnings=F)

# Set the raster option to this folder
rasterOptions(tmpdir= my_tmpdir)

#set up dirs
	rentDir <- paste0(wd, "/Data/NASS county rental rates/NASS_LandRents_2008_2016.csv")

	outDir <- paste0(wd, "/Analysis/")

#################
#Preprocessing of spatial data
#Not done in R - see README.txt for processing that was done in ArcGIS instead
	# inpNLCD <- "Z:/Data/NCEAS_Postdoc/P1 Sage Grouse/Data/Original/LULC/National Land Cover Database 2011/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img"
	# privateDir <- "Z:/Data/NCEAS_Postdoc/P1 Sage Grouse/Data/Original/Land Ownership and Protected Areas/PAD-US CBI Version 2/Private land/PADUSCBIv2_Private_only_AllCounty"
	# cdlDir <- "Z:/Data/NCEAS_Postdoc/P1 Sage Grouse/Data/Original/LULC/Cropland CDL/Lark et al 2015 data/Multitemporal_Results_FF2.tif"
	# countyDir <- "Z:/Data/NCEAS_Postdoc/P1 Sage Grouse/Data/Original/General maps/USA_administrative/USGS_digital_boundaries_USETHISONE/USGS_County_boundaries_USContinental_albers.shp"
	#outDirNLCD <- paste0(wd, "/Analysis/NLCD mask")
	#outDirCDL <- paste0(wd, "/Analysis/Processed CDL rasters")
	
# ###NLCD extract a raster containing only only pixels classified as grassland, shrubland & wetland #TOO SLOW - faster in ArcGIS
	# # NLCDRast <- raster(inpNLCD)
	# # newNCLD <- subs(NLCDRast, y=data.frame(vals=c(52, 71, 90, 95), newval=rep(1,4)), by=1, which=2, subsWithNA=TRUE, filename=paste0(outDirNLCD, "/NLCD_grassland.tif"), format='GTiff') #replaces any values not in 'vals' with NAs

# ###Mask out areas that are not on private land (private land comes from PADUSCBIv2 - any unprotected private land or inholding)
	# privateShp <- readOGR(dirname(privateDir), basename(privateDir))
	# maskRast <- mask(newNLCD, privateShp, updatevalue=NA, inverse=FALSE, filename=paste0(outDirNLCD, "/NLCD_grassland_private.tif"), format='GTiff')

# ###Mask out only private grassland from the cdl data (cdl from Lark et al. 2015)
	# cdlRast <- raster(cdlDir)
	# #classes are 1=stable noncrop, 2= stable crop, 3= converted to crop, 4= abandoned, 5=intermittent cropland
	# cdlmasked <- mask(cdlRast, maskRast, updatevalue=NA, inverse=FALSE, filename=paste0(outDirCDL, "/Lark_CDL_onlyprivategrassland.tif"), format='GTiff')

# ###Convert county boundary shp into a zonal raster
	# countyShp <- readOGR(dirname(countyDir), basename(countyDir))
	# countyRast <- rasterize(countyShp, cdlmasked, field=ADMIN_FIPS, filename=paste0(basename(countyDir), "USGS_County.tif"), format='GTiff')

#################
#Extract the area of cropland, non-cropland & converted cropland for each county
#Do this in ArcGIS using Tabulate area which is really quick and easy
#################


#################
###Preprocessing of NASS land rent data
	rentDat <- read.csv(rentDir, header=TRUE, stringsAsFactors=TRUE)
	rentDat$County <- as.character(rentDat$County)
	#function to convert caps to camel case
	camel <- function(x) {
		x <- tolower(x)
		s <- strsplit(x, " ")[[1]]
		paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
	}
	#convert county names in rent dataset to camel case
	rentDat$NewCounty <- sapply(rentDat$County, function(x) camel(x))

	Rents <- list(	Pasture_rent="RENT, CASH, PASTURELAND - EXPENSE, MEASURED IN $ / ACRE",
				NonIrrigatedCropland_rent="RENT, CASH, CROPLAND, NON-IRRIGATED - EXPENSE, MEASURED IN $ / ACRE", 
				IrrigatedCropland_rent="RENT, CASH, CROPLAND, IRRIGATED - EXPENSE, MEASURED IN $ / ACRE")
	Years <- c(2008, 2009, 2010, 2011, 2012)


	rentDat <- rentDat[rentDat$Data.Item %in% unlist(Rents) & rentDat$Year %in% Years ,c("State", "State.ANSI", "Ag.District", "Ag.District.Code", "NewCounty", "County", "County.ANSI", "Year", "Data.Item", "Value")]
	rentDat$Value <- as.numeric(gsub(",", "", rentDat$Value)) #remove commas from values and convert to numeric

	###Reshape data
	rentWide <- spread(rentDat, Year, value=Value, fill=NA)
	rentWide$Av <- round(rowMeans(rentWide[,c("2008", "2009", "2010", "2011", "2012")], na.rm=TRUE), 2)
	rentWide$Min <- apply(rentWide[,c("2008", "2009", "2010", "2011", "2012")],1, min, na.rm=TRUE)
	rentWide$Max <- apply(rentWide[,c("2008", "2009", "2010", "2011", "2012")],1, max, na.rm=TRUE)
	head(rentWide)
	names(rentWide)[which(names(rentWide)=="State")] <- "STATE"
	names(rentWide)[which(names(rentWide)=="County")] <- "COUNTY"

	#save reshaped data
	write.csv(rentWide, paste0(outDir, "NASS_LandRents_2008_2012_reshaped.csv"), row.names=FALSE)

	###Fill in gaps for counties with no data
	#Table of which district counties are in to fill in gaps
		countyList <- read.csv(paste0(dirname(rentDir), "/county_list_r.csv"), stringsAsFactors=FALSE)
		countyList <- countyList[countyList$Flag==1,]#pull only current counties
		countyList <- countyList[countyList$County<888 & countyList$County>0,]#exclude combined counties & state totals
		countyList <- countyList[countyList$County<888,]#exclude combined counties & state totals
		#Table of data to fill with
		#filldf <- rentWide[rentWide$COUNTY=="OTHER (COMBINED) COUNTIES",]

#Make a list of dataframes (one list item per Rent)
rentList <- lapply(Rents, function(currRent){
	currSub <- droplevels(rentWide[rentWide$Data.Item==currRent,])
	currdf <- merge(countyList[,1:4], currSub, by.x=c("Name", "State"), by.y=c("NewCounty", "State.ANSI"), all.x=TRUE)
		
	#fill counties with no data using the average for the Ag.District they fall within
	filldf <- currSub[currSub$COUNTY=="OTHER (COMBINED) COUNTIES",]#Table of which values to fill with
	nodata <- which(is.na(currdf$Av)) #find gaps
	print(names(Rents[which(Rents %in% currRent)]))
	length(nodata)
	#print(nodata)
	
	#print(currdf[nodata,1:3])
	
	#fill in rows
		for(i in nodata){
			currRow <- currdf[i,]
			currFill <- filldf[	filldf$Ag.District.Code==currRow[["District"]] & 
								filldf$State.ANSI==currRow[["State"]] ,
								c("STATE", "Ag.District", "Ag.District.Code", "COUNTY", "County.ANSI", "Data.Item", "2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max")	]
			if(nrow(currFill)>0){
			currdf[i,]<- data.frame(currdf[i, 1:4], currFill)
			}
		}
	write.csv(currdf, paste0(outDir, "NASS_LandRents_2008_2012_", names(Rents[which(Rents %in% currRent)]), ".csv"), row.names=FALSE) 
	return(currdf)
	})

	countyList$NASS_FIPS <- paste0(countyList$State, formatC(countyList$County, width=3, format="d", flag="0"))
	
	rentDF <- data.frame(countyList, rentList[[1]][c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max")], rentList[[2]][c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max")], rentList[[3]][c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max")])
	names(rentDF) <- c(names(countyList), paste(rep(names(Rents), each=length(c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max"))), c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max"), sep="_"))

	write.csv(rentDF, paste0(outDir, "NASS_LandRents_2008_2012_allRents.csv"), row.names=FALSE)

##################

#Read in data
rentDF <- read.csv(paste0(outDir, "NASS_LandRents_2008_2012_allRents.csv"), header=TRUE)
cdlDF <- read.csv(paste0(outDir, "LarkCDL_PrivateArea_byCounty.csv"), header=TRUE)
nlcdDF <- read.csv(paste0(outDir, "NLCD_PrivateArea_byCounty.csv"), header=TRUE)

names(cdlDF) <- sub("VALUE", "CDL", names(cdlDF))
names(nlcdDF) <- sub("VALUE", "NLCD", names(nlcdDF))

#Join all data and summarise data
allDat <- data.frame(
				cdlDF[,which(!names(cdlDF) %in% c("FID", "Rowid_", "ADMIN_FIPS_1"))], #throw out some junk columns
				nlcdDF[,10:26], #just pull rows with area values
				rentDF[match(cdlDF$ADMIN_FIPS, rentDF$NASS_FIPS),grep(c("Av|Min|Max"), names(rentDF))]) #sort rows in rentDF by order of cdlDF, select columns containing Av, Min, Max

#Calculate delta rent
allDat$DeltaRent <- allDat$NonIrrigatedCropland_rent_Av - allDat$Pasture_rent_Av

#Calculate conversion probability of grassland (including pasture/hay)
# = Area converted between 2008 and 2012 (ignore area reverted as it is hard to get native grassland back) / Area of grassland available for conversion
#cdl classes are 1=stable noncrop, 2= stable crop, 3= converted to crop, 4= abandoned, 5=intermittent cropland

allDat$ConversionProb <- allDat$CDL_3 / (allDat$CDL_1 - rowSums(allDat[, which(names(allDat) %in% paste0("NLCD_", c("0", "11", "12", "21", "22", "23", "24", "31", "41", "42", "43")))]))


rowSums(allDat[, which(names(allDat) %in% paste0("NLCD_", c("0", "11", "12", "21", "22", "23", "24", "31", "41", "42", "43")))])/rowSums(allDat[, grep("NLCD_", names(allDat))])

rowSums(allDat[, grep("NLCD_", names(allDat))])/rowSums(allDat[, grep("C_", names(allDat))])

"NLCD_0", 
"NLCD_11", "NLCD_12", "NLCD_21", "NLCD_22", "NLCD_23", "NLCD_24", 
"NLCD_31", "NLCD_41", "NLCD_42", "NLCD_43", "NLCD_52", "NLCD_71", 
"NLCD_81", "NLCD_82", "NLCD_90", "NLCD_95",



