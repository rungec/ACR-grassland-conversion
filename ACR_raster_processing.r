#Processing and analysis associated with proposed new ACR methodology.

#test=function() {source("C:/Claire/NCEAS_Postdoc/P4 ACR revised methods/Analysis/github/ACR_raster_processing.r")}

library(tidyr) #for spread() which is like reshape
library(raster)
library(rgdal)
library(ggplot2)

#wd <- "Z:/Data/NCEAS_Postdoc/P4 ACR revised methods"
wd <- "C:/Claire/NCEAS_Postdoc/P4 ACR revised methods"
#wd <- "D:/P4 ACR revised methods"
setwd(wd)

# Define your temp folder
my_tmpdir = paste0(wd, '/tmpRaster/')

# Create it (handles the case where the folder already exists)
dir.create(my_tmpdir, showWarnings=F)

# Set the raster option to this folder
rasterOptions(tmpdir= my_tmpdir)

#set up dirs
	rentDir <- paste0(wd, "/Data/NASS county rental rates/NASS_LandRents_2008_2016.csv")

	outDir <- paste0(wd, "/Analysis/tables/")

#################
#Preprocessing of spatial data
#Not done in R - see ProcessingNotes.md for processing that was done in ArcGIS instead
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
	write.csv(rentWide, paste0(outDir, "processed NASS/NASS_LandRents_2008_2012_reshaped.csv"), row.names=FALSE)

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
	write.csv(currdf, paste0(outDir, "processed NASS/NASS_LandRents_2008_2012_", names(Rents[which(Rents %in% currRent)]), ".csv"), row.names=FALSE) 
	return(currdf)
	})

	countyList$NASS_FIPS <- paste0(countyList$State, formatC(countyList$County, width=3, format="d", flag="0"))
	
	rentDF <- data.frame(countyList, rentList[[1]][c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max")], rentList[[2]][c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max")], rentList[[3]][c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max")])
	names(rentDF) <- c(names(countyList), paste(rep(names(Rents), each=length(c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max"))), c("2008", "2009", "2010", "2011", "2012", "Av", "Min", "Max"), sep="_"))

	write.csv(rentDF, paste0(outDir, "processed NASS/NASS_LandRents_2008_2012_allRents.csv"), row.names=FALSE)

####################
#ANALYSIS
####################
#Set up data
rentDF <- read.csv(paste0(outDir, "processed NASS/NASS_LandRents_2008_2012_allRents.csv"), header=TRUE)

#for conversion on all land capability classes
#cdlDF <- read.csv(paste0(outDir, "land use by area/LarkCDL_GrasslandPrivateArea_byCounty.csv"), header=TRUE) 
#cdlDF <- cdlDF[,which(!names(cdlDF) %in% c("FID", "Rowid_", "ADMIN_FIPS_1"))], #throw out some junk columns
#names(cdlDF) <- sub("VALUE", "CDL", names(cdlDF))
#Calculate conversion probability of grassland (including pasture/hay)
# = Area converted between 2008 and 2012 (ignore area reverted as it is hard to get native grassland back) / Area of grassland available for conversion
#cdl classes are 1=stable noncrop, 2= stable crop, 3= converted to crop, 4= abandoned, 5=intermittent cropland, 15=forest, developed or water
# allDat$ConversionProb <- allDat$CDL_3 / (allDat$CDL_1 + allDat$CDL_3)
# allDat$Total_Area_m2 = rowSums(allDat[,c("CDL_1", "CDL_2", "CDL_3", "CDL_4", "CDL_5")])
# allDat$PropGrasslandRemaining = allDat$CDL_1/allDat$Total_Area_m2
# allDat$PropCropland = (allDat$CDL_2+allDat$CDL_3)/allDat$Total_Area_m2

##for conversion on individual land capability classes 
cdlfilelist <- paste0(rep(paste0(outDir, "land use by area/LarkCDL_GrasslandPrivateArea_LCC"), 10), c(1:8, "1to6", "7to8"), rep("_byCounty.csv",10))
cdlDF <- docall(cbind, lapply(cdlfilelist, function(x) {
								currlcc <- strsplit(basename(x[[1]][1]), "_")[[1]][3]
								currdf <- read.csv(x, header=TRUE) #read in csv
								#summarise the data
								currdf$ConversionProb_VALUE <- currdf$VALUE_3 / (currdf$VALUE_1 + currdf$VALUE_3)
								currdf$VALUE_Total_Area_m2 = rowSums(currdf[,c("VALUE_1", "VALUE_2", "VALUE_3", "VALUE_4", "VALUE_5")])
								currdf$VALUE_PropGrasslandRemaining = currdf$VALUE_1/currdf$VALUE_Total_Area_m2
								currdf$VALUE_PropCropland = (currdf$VALUE_2+currdf$VALUE_3)/currdf$VALUE_Total_Area_m2
								names(currdf) <- sub("VALUE", currlcc, names(currdf)) #rename columns
								}))
cdlDF <- cdlDF[,c(2:7, grep("LCC", names(currdf)))] #just pull "VALUE" columns
				
#load year converted to crop data
yearDF <- read.csv(paste0(outDir, "land use by area/LarkCDL_GrasslandPrivateYeartoCrop_byCounty.csv"), header=TRUE)
names(yearDF) <- sub("VALUE", "Converted", names(yearDF))

#Join all data and summarise data
allDat <- data.frame(
				cdlDF,
				yearDF[, c(paste0(rep("Converted_", 4), c(2009:2012)))],
				rentDF[match(cdlDF$ADMIN_FIPS, rentDF$NASS_FIPS),grep(c("Av|Min|Max"), names(rentDF))]) #sort rows in rentDF by order of cdlDF, select columns containing Av, Min, Max

#Calculate delta rent
allDat$DeltaRent <- allDat$NonIrrigatedCropland_rent_Av - allDat$Pasture_rent_Av
allDat$PropRent <- allDat$Pasture_rent_Av/allDat$NonIrrigatedCropland_rent_Av

#allDat <- allDat[!is.na(allDat$ConversionProb),]
write.csv(allDat, paste0(outDir, "all data combined/LandConversion_combinedData_allUSStates.csv"), row.names=FALSE)

###############
#Clean up data
#there are 151 NAs in the DeltaRent, and 51 negative numbers
#there is one NA in ConversionProb
#subDat <- allDat[!is.na(allDat$DeltaRent) & !is.na(allDat$ConversionProb) & allDat$DeltaRent>=0, ]
subDat <- allDat[!is.na(allDat$DeltaRent) & allDat$DeltaRent>=0, ]


##################
#Make nice plots for ACR

#Plot conversion prob against delta rents
p <- ggplot(subDat, aes(DeltaRent, ConversionProb)) +
	geom_point(colour='black', size=1) +
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Delta non-irrigated cropland & pasture rents", y="Proportion of grassland converted to cropland 2008-2012")+
	scale_x_continuous(expand = c(0.05, 0.05)) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend
	
outPath <- paste0(dirname(outDir), "/figures/ConversionProb_vs_DeltaRents2.png")
	ggsave(filename=outPath)
	
#Plot log conversion prob against delta rents	
p <- ggplot(subDat, aes(DeltaRent, log(ConversionProb+1))) +
	geom_point(colour='black', size=1) +
	#geom_abline(coefficients(mod1)[[1]][1], slope=coefficients(mod1)[[2]][1], colour="red")+ #add trendline
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Non-irrigated cropland rent minus pasture rent", y="Log proportion of grassland converted to cropland 2008-2012")+
	scale_x_continuous(expand = c(0.05, 0.05), limits=c(0, max(subDat$DeltaRent))) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend
	
outPath <- paste0(dirname(outDir), "/figures/ConversionProbLog_vs_DeltaRents3.png")
	ggsave(filename=outPath)

#Plot conversion prob against delta rents, include trendline and only non-zero data	
subDat2 <- subDat[subDat$ConversionProb>0 & subDat$DeltaRent>0,]
mod1 <- lm(subDat2$ConversionProb ~ subDat2$DeltaRent) 
summary(lm(subDat2$ConversionProb ~ subDat2$DeltaRent))
plot(lm(subDat2$ConversionProb ~ subDat2$DeltaRent)) #terrible residuals
	
	
p <- ggplot(subDat2, aes(DeltaRent, ConversionProb)) +
	geom_point(colour='grey70', size=1) +
	geom_abline(intercept=coefficients(mod1)[[1]][1], slope=coefficients(mod1)[[2]][1], colour="red")+ #add trendline
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Non-irrigated cropland rent minus pasture rent", y="Proportion of grassland converted to cropland 2008-2012")+
	scale_x_continuous(expand = c(0.05, 0.05), limits=c(0, max(subDat$DeltaRent))) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend
	
outPath <- paste0(dirname(outDir), "/figures/ConversionProb_vs_DeltaRentsNonZero.png")
	ggsave(filename=outPath)
	
#Plot conversion prob against prop rents, include trendline and only non-zero data	
mod1 <- lm(subDat2$ConversionProb ~ subDat2$PropRent) 
summary(lm(subDat2$ConversionProb ~ subDat2$PropRent))
plot(lm(subDat2$ConversionProb ~ subDat2$PropRent)) #terrible residuals
	
	
p <- ggplot(subDat2, aes(PropRent, ConversionProb)) +
	geom_point(colour='grey70', size=1) +
	geom_abline(intercept=coefficients(mod1)[[1]][1], slope=coefficients(mod1)[[2]][1], colour="red")+ #add trendline
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Ratio of pasture rent to non-irrigated cropland", y="Proportion of grassland converted to cropland 2008-2012")+
	scale_x_continuous(expand = c(0.05, 0.05), limits=c(0, max(subDat$PropRent))) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend
	
outPath <- paste0(dirname(outDir), "/figures/ConversionProb_vs_PropRentsNonZero.png")
	ggsave(filename=outPath)
	
#Plot conversion prob against cropland area, include trendline
mod1 <- lm(allDat$ConversionProb ~ allDat$PropCropland) 
summary(lm(allDat$ConversionProb ~ allDat$PropCropland))
plot(lm(allDat$ConversionProb ~ allDat$PropCropland)) #terrible residuals

	
p <- ggplot(allDat, aes(PropCropland, ConversionProb)) +
	geom_point(colour='grey70', size=1) +
	geom_abline(intercept=coefficients(mod1)[[1]][1], slope=coefficients(mod1)[[2]][1], colour="red")+ #add trendline
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Proportion of cropland in county in 2008", y="Proportion of grassland converted to cropland 2008-2012")+
	scale_x_continuous(expand = c(0.05, 0.05)) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend
	
outPath <- paste0(dirname(outDir), "/figures/ConversionProb_vs_PropCropland.png")
	ggsave(filename=outPath)
 
#Plot conversion prob against cropland area, include trendline, nonzero data
mod1 <- lm(subDat2$ConversionProb ~ subDat2$PropCropland) 
summary(lm(subDat2$ConversionProb ~ subDat2$PropCropland))
#plot(lm(subDat2$ConversionProb ~ subDat2$PropCropland)) #terrible residuals

	
p <- ggplot(subDat2, aes(PropCropland, ConversionProb)) +
	geom_point(colour='grey70', size=1) +
	geom_abline(intercept=coefficients(mod1)[[1]][1], slope=coefficients(mod1)[[2]][1], colour="red")+ #add trendline
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Proportion of cropland in county in 2008", y="Proportion of grassland converted to cropland 2008-2012")+
	scale_x_continuous(expand = c(0.05, 0.05)) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend
	
outPath <- paste0(dirname(outDir), "/figures/ConversionProb_vs_PropCropland_NonZero.png")
	ggsave(filename=outPath)
 
 
#Histogram of conversion prob
p <- ggplot(subDat, aes((ConversionProb))) +
	geom_histogram() +
	theme_bw(17) + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Proportion of grassland converted to cropland 2008-2012", y="Number of counties")+
	scale_x_continuous(expand = c(0.0, 0.0)) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend

outPath <- paste0(dirname(outDir), "/figures/Histogram_ConversionProb.png")
	ggsave(filename=outPath)
	
#Histogram of conversion prob - zoom
p <- ggplot(subDat, aes((ConversionProb))) +
	geom_histogram() +
	theme_bw(17) + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Proportion of grassland converted to cropland 2008-2012", y="Number of counties")+
	scale_x_continuous(expand = c(0.0, 0.0)) + scale_y_continuous(expand = c(0.01, 0.001), limits=c(0, 200))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend

outPath <- paste0(dirname(outDir), "/figures/Histogram_ConversionProb_zoom.png")
	ggsave(filename=outPath)
	
#Histogram of log conversion prob
p <- ggplot(subDat, aes((log(ConversionProb+1)))) +
	geom_histogram() +
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Proportion of grassland converted to cropland 2008-2012", y="Number of counties")+
	scale_x_continuous(expand = c(0.0, 0.0)) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend

outPath <- paste0(dirname(outDir), "/figures/Histogram_ConversionProbLog.png")
	ggsave(filename=outPath)
	
#Histogram of delta rents
p <- ggplot(subDat, aes((DeltaRent))) +
	geom_histogram() +
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Delta non-irrigated cropland & pasture rents", y="Number of counties")+
	scale_x_continuous(expand = c(0.0, 0.0)) + scale_y_continuous(expand = c(0.01, 0.001))+ #set x and y limits
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend

outPath <- paste0(dirname(outDir), "/figures/Histogram_DeltaRents.png")
	ggsave(filename=outPath)
	
#
#Plot delta rents and conversion probability against cumulative area of grassland in county
subDat$binConvProb <- cut(subDat$ConversionProb, breaks = c(seq(0, 0.3, by = .01)), labels = 0:29)
subDat$binDeltaRent <- cut(subDat$DeltaRent, breaks = c(-51, seq(0, 230, by = 10)), labels = c("<0", seq(0, 210, by = 10), ">220"))
cumAreaConvProp <- stats::aggregate(subDat$CDL_1, list(subDat$binConvProb), sum)
cumAreaConvProp$Area_mha <- round((cumAreaConvProp$x)/(10000*1000000), 0)
sum(cumAreaConvProp$Area_mha)
cumAreaDeltaRent <- stats::aggregate(subDat$CDL_1, list(subDat$binDeltaRent), sum)
cumAreaDeltaRent$Area_mha <- round((cumAreaDeltaRent$x)/(10000*1000000), 0)

#Histogram of conversion prob against cumulative area of grassland (in million ha) in county
p <- ggplot(cumAreaConvProp, aes(Group.1, rev(cumsum(rev(Area_mha))))) +
#p <- ggplot(cumAreaConvProp, aes(Group.1, x)) +
	geom_bar(stat='identity')+
	geom_line(aes(group=1)) +
	theme_bw(17) + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	#scale_x_discrete(limits = rev(levels(cumAreaConvProp$Group.1)))+
	scale_x_discrete(breaks=c(29, 25, 20, 15, 10, 5, 0), limits = rev(levels(cumAreaConvProp$Group.1)))+
	scale_y_continuous(expand = c(0.01, 0.001))+
	labs(x="Percent of grassland converted to cropland 2008-2012", y="Cumulative area of grassland remaining (million ha)")+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend

outPath <- paste0(dirname(outDir), "/figures/AreaGrassland_vs_ConversionProb.png")
	ggsave(filename=outPath)

#Histogram of conversion prob against cumulative area of grassland (in million ha) in county
#zoomed in
p <- ggplot(cumAreaConvProp[2:11,], aes(Group.1, rev(cumsum(rev(Area_mha))))) +
#p <- ggplot(cumAreaConvProp, aes(Group.1, x)) +
	geom_bar(stat='identity')+
	geom_line(aes(group=1)) +
	theme_bw(17) + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	scale_x_discrete(breaks=c(10, 8,6,4,2),limits = rev(levels(cumAreaConvProp$Group.1)[1:11]))+
	scale_y_continuous(limits=c(0, 75),expand = c(0.01, 0.001))+
	labs(x="Percent of grassland converted to cropland 2008-2012", y="Cumulative area of grassland remaining (million ha)")+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend

outPath <- paste0(dirname(outDir), "/figures/AreaGrassland_vs_ConversionProb_zoom.png")
	ggsave(filename=outPath)

cumsum(rev(cumAreaConvProp$Area_mha))


#Histogram of delta rents against cumulative area of grassland in county
p <- ggplot(cumAreaDeltaRent, aes(Group.1, cumsum(x))) +
	geom_line(aes(group=1)) +
	geom_point()+
	theme_bw() + #get rid of grey bkg and gridlines
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
	labs(x="Delta non-irrigated cropland & pasture rents ($)", y="Cumulative area of grassland remaining (million ha)")+
	theme(axis.title.x = element_text(vjust=-0.6),axis.title.y = element_text(vjust=1))+	#move xylabels away from graph
	theme(legend.position="none")#gets rid of legend

outPath <- paste0(dirname(outDir), "/figures/AreaGrassland_vs_DeltaRents.png")
	ggsave(filename=outPath)
	


#########

plot(allDat$DeltaRent, allDat$ConversionProb)
plot(allDat$DeltaRent, log(allDat$ConversionProb))
plot(log(allDat$DeltaRent), log(allDat$ConversionProb))
hist(log(allDat$DeltaRent)

#Conversion Probability against area of grassland remaining in county
plot(allDat$ConversionProb ~ allDat$PropGrasslandRemaining) 
summary(lm(allDat$ConversionProb ~ allDat$PropGrasslandRemaining)) #significant
plot(lm(allDat$ConversionProb ~ allDat$PropGrasslandRemaining)) #not the worst model, residuals skewed
#Conversion Probability against area of cropland in county
plot(allDat$ConversionProb ~ allDat$PropCropland)
summary(lm(allDat$ConversionProb ~ allDat$PropCropland)) #conversion probability increases significantly as amount of cropland in county increases (slope=0.032)
plot(lm(allDat$ConversionProb ~ allDat$PropCropland)) #not the worst, residuals skewed

glm(allDat$ConversionProb ~ allDat$DeltaRent, family='gaussian',)


plot(log(subDat$ConversionProb) ~ subDat$PropGrasslandRemaining) 
plot(log(subDat$ConversionProb) ~ log(subDat$DeltaRent) )
summary(lm(log(subDat$ConversionProb) ~ log(subDat$DeltaRent) ))
summary(lm(log(subDat$ConversionProb) ~ subDat$PropGrasslandRemaining*subDat$DeltaRent)) #deltarent not significant
plot(lm(log(subDat$ConversionProb) ~ subDat$PropGrasslandRemaining*subDat$DeltaRent)) #looks ok
plot(lm(allDat$ConversionProb ~ allDat$PropGrasslandRemaining)) 
summary(lm(log(subDat$ConversionProb) ~ subDat$PropGrasslandRemaining)) 
plot(lm(log(subDat$ConversionProb) ~ subDat$PropGrasslandRemaining)) 
plot(log(subDat$ConversionProb) ~ subDat$PropGrasslandRemaining) 
plot(allDat$ConversionProb ~ allDat$PropGrasslandRemaining) 
hist(allDat$PropGrasslandRemaining)

#Model conversion probabilities against rental rates
plot(allDat$DeltaRent, allDat$ConversionProb)
plot(allDat$PropRent, allDat$ConversionProb)
plot(allDat$PropRent, log(allDat$ConversionProb))
plot(allDat$DeltaRent, log(allDat$ConversionProb))
mod1 <- lm(allDat$ConversionProb~allDat$DeltaRent)
mod1 <- lm(allDat$ConversionProb~allDat$PropRent) #model is terrible, but line is flat - no relationship between ratios and conversion probability
mod2 <- lm(log(allDat$ConversionProb+1)~allDat$DeltaRent)
summary(mod2)
plot(mod2)
mod2b <- lm(allDat$ConversionProb~log(allDat$DeltaRent+1))
summary(mod2b)
plot(mod2b)
mod2c <- lm(log(allDat$ConversionProb+1)~log(allDat$DeltaRent+1))
summary(mod2c) #not significant
plot(mod2c)

#Model just the non-zeros
subDat2 <- allDat[allDat$ConversionProb>0 & allDat$DeltaRent>0,]
plot(log(subDat2$ConversionProb)~log(subDat2$DeltaRent))
plot(lm(log(subDat2$ConversionProb)~log(subDat2$DeltaRent))) #looks good
summary(lm(log(subDat2$ConversionProb)~log(subDat2$DeltaRent))) #slope not significant


plot(allDat$ConversionProb ~ allDat$CDL_1)
plot(log(allDat$ConversionProb+1) ~ log(allDat$CDL_1+1))
mod3 <- lm(allDat$ConversionProb ~ allDat$CDL_1)
plot(mod3)#terrible
summary(mod3) #significant
mod4 <- lm(allDat$ConversionProb ~ poly(allDat$CDL_1, 2))
plot(mod4) #terrible
summary(mod4) #not significant


