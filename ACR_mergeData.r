#This script summarises and combines dataset for the American Carbon Registry crop conversion prediction models
#combines (1) Lark CDL conversion rates (range to range, range to crop) 
# (2) Control variables, including a. area of urban land in a county, b. annual county population c. Year CRP expires
# Land conversion data isfor private grassland (not forest, developed or water, not public land, not already protected)

#################################################################################################################
#wd <- "/home/runge/Data/NCEAS_Postdoc/P1 Sage Grouse/Analysis/1_PEOG_grazing_economics/Inputs/""
wd <- "Y:/Data/NCEAS_Postdoc/P1 Sage Grouse/Analysis/1_PEOG_grazing_economics/1 Inputs/"
setwd(wd)
options(stringsAsFactors=FALSE) # turn off automatic factor coersion
options(scipen=9999)            # turn off plotting axis lables in scientific notation

library(plyr)

#######################
###CONVERSION PROBABILITIES FROM LARK CDL BY LCC
#######################
#Annual conversion probabilites calculated from remotely sensed data on area of crop/range/pasture from 2008-2015
#data from Tyler Lark (rangeland, cropland)
#Land Capability Class from USDA (gSSURGO)
# The Lark CDL rasters were summarised (tabulate by area) to give tables of how much land was converted or present as cropland in each year, and this analysis is detailed in:
# https://github.nceas.ucsb.edu/rungec/ACR-grassland-conversion/blob/master/ProcessingNotes.md
# Tables used were:
# LarkCDL_GrasslandPrivateArea(_LCC...)_byCounty).csv
# LarkCDL_GrasslandPrivateYearFromCrop(_LCC...)_byCounty.csv
# LarkCDL_GrasslandPrivateYearToCrop(_LCC...)_byCounty.csv
# I then used these tables made for the ACR project to get the area of cropland in each county in each year, for use in the conversion models.
# Calculations were performed in Cropland_and_Rangeland_Area_byYear_ha.xlsx (all LCC) & LarkCDL_GrasslandPrivateArea_byLCC_byCounty_andYear.xlsx (split by LCC)
# The area of cropland in each year is the are of cropland at the beginning of the time period (2008; the sum of the stable cropland and the area of cropland that was lost between 2008 and 2012), plus any cropland added in that year and previous years, minus any cropland reverted in that year or previous years.
# These values are for private, unprotected land, and excludes land that is classified in the 2015 CDL as water, forest or developed. ie. only represents cropland transitions to or from rangeland.
# Abandoned cropland was ignored.
# Areas are in hectares.
# Classes of LarkCDL_grassland.tif are 1=stable noncrop, 2= stable crop, 3= converted to crop, 4= abandoned, 5=intermittent cropland , 15=forest,water or developed 
# Tables exported were:
# Cropland_Area_byYear_ha.csv
# Cropland_Area_byYear_LCC....csv
# Rangeland_Area_byYear_ha.csv
# Rangeland_Area_byYear_ha_LCC....csv

#######################
###CONVERSION PROBABILITIES FROM LARK CDL BY LCC
#######################
#Annual conversion probabilites calculated from remotely sensed data on area of crop/range/pasture from 2008-2012
#For grassland (not forest, urban or water) on private land
#data from Tyler Lark (rangeland, cropland)
#Land Capability Class from USDA

#area of cropland from LARK et al. 2015
	croplandAreaDF <- read.csv("Cropland and pasture/Cropland and Rangeland by year/Cropland_Area_byYear_ha_2008_2015.csv")
	croplandAreaAllcounties <- croplandAreaDF[, c("ADMIN_FIPS", "STATE", "NAME", "Area_ha", "X2008_Cropland_area_ha", "X2009_Cropland_area_ha", "X2010_Cropland_area_ha", "X2011_Cropland_area_ha", "X2012_Cropland_area_ha", "X2013_Cropland_area_ha", "X2014_Cropland_area_ha", "X2015_Cropland_area_ha", "X2009_RangetoCropland_area_ha", "X2010_RangetoCropland_area_ha", "X2011_RangetoCropland_area_ha", "X2012_RangetoCropland_area_ha", "X2013_RangetoCropland_area_ha", "X2014_RangetoCropland_area_ha", "X2015_RangetoCropland_area_ha")] #select only relevant columns
	croplandAreaAllcounties <- croplandAreaAllcounties[order(croplandAreaAllcounties$ADMIN_FIPS),] #order data by fips

#area of rangeland from LARK et al. 2015
	rangelandAreaDF <- read.csv("Cropland and pasture/Cropland and Rangeland by year/Rangeland_Area_byYear_ha_2008to2015.csv")
	rangelandAreaAllcounties <- rangelandAreaDF[ , c("ADMIN_FIPS", "Stable.area.of.noncrop", "Rangeland_2008_ha", "Rangeland_2009_ha", "Rangeland_2010_ha", "Rangeland_2011_ha", "Rangeland_2012_ha", "Rangeland_2013_ha", "Rangeland_2014_ha", "Rangeland_2015_ha", "X2009_RangetoRange_ha", "X2010_RangetoRange_ha", "X2011_RangetoRange_ha", "X2012_RangetoRange_ha", "X2013_RangetoRange_ha", "X2014_RangetoRange_ha", "X2015_RangetoRange_ha", "TotalArea_RangeorCrop_ha")] #select only relevant columns
	rangelandAreaAllcounties <- rangelandAreaAllcounties[order(rangelandAreaAllcounties$ADMIN_FIPS),] #order data by fips
CropRangeDF <- merge(croplandAreaAllcounties, rangelandAreaAllcounties, by="ADMIN_FIPS", all.x=TRUE)


###BY YEAR #FOR LCC1to4, 1to6 vs 5or6
LCC1to4RangeDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_LCC1to4.csv", header=TRUE)
LCC1to4CropDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_LCC1to4.csv", header=TRUE)
LCC1to4RangeDFnew <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_2015_LCC1to4.csv", header=TRUE)
LCC1to4CropDFnew <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_2015_LCC1to4.csv", header=TRUE)
LCC1to4Merge <- merge(LCC1to4RangeDF, LCC1to4CropDF[, !names(LCC1to4CropDF) %in% c("NAME")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)
LCC1to4Mergenew <- merge(LCC1to4RangeDFnew, LCC1to4CropDFnew[, !names(LCC1to4CropDFnew) %in% c("NAME")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)
LCC1to4Merge <- merge(LCC1to4Merge, LCC1to4Mergenew[, !names(LCC1to4Mergenew) %in% c("Total_Area_RangeorCrop_inLCC1to4", "Stable_cropland")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)

LCC1to6RangeDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_LCC1to6.csv", header=TRUE)
LCC1to6CropDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_LCC1to6.csv", header=TRUE)
LCC1to6RangeDFnew <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_2015_LCC1to6.csv", header=TRUE)
LCC1to6CropDFnew <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_2015_LCC1to6.csv", header=TRUE)
LCC1to6Merge <- merge(LCC1to6RangeDF, LCC1to6CropDF[, !names(LCC1to6CropDF) %in% c("NAME")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)
LCC1to6Mergenew <- merge(LCC1to6RangeDFnew, LCC1to6CropDFnew[, !names(LCC1to6CropDFnew) %in% c("NAME")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)
LCC1to6Merge <- merge(LCC1to6Merge, LCC1to6Mergenew[, !names(LCC1to6Mergenew) %in% c("Total_Area_RangeorCrop_inLCC1to6", "Stable_cropland", "Stable.area.of.noncrop")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)

LCC5or6RangeDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_LCC5or6.csv", header=TRUE)
LCC5or6CropDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_LCC5or6.csv", header=TRUE)
LCC5or6RangeDFnew <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_2015_LCC5or6.csv", header=TRUE)
LCC5or6CropDFnew <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_2015_LCC5or6.csv", header=TRUE)
LCC5or6Merge <- merge(LCC5or6RangeDF, LCC5or6CropDF[, !names(LCC5or6CropDF) %in% c("NAME")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)
LCC5or6Mergenew <- merge(LCC5or6RangeDFnew, LCC5or6CropDFnew[, !names(LCC5or6CropDFnew) %in% c("NAME")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)
LCC5or6Merge <- merge(LCC5or6Merge, LCC5or6Mergenew[, !names(LCC5or6Mergenew) %in% c("Total_Area_RangeorCrop_inLCC5or6", "Stable_cropland", "Stable.area.of.noncrop")], by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)

CropRangeLCCLong <- rbind(with(CropRangeDF,	
								data.frame(
									ADMIN_FIPS=rep(ADMIN_FIPS, 7), 
									State=rep(STATE, 7), 
									Year=rep(c(2009, 2010, 2011, 2012, 2013, 2014, 2015), each=nrow(CropRangeDF)),	
									StableArea_noncrop= rep(Stable.area.of.noncrop, 7),TotalRangeorCropAreainLCC_ha = rep(TotalArea_RangeorCrop_ha, 7),
									LCC = rep("AllLCC", 7*nrow(CropRangeDF)),
									Area_cropland = c(X2008_Cropland_area_ha, X2009_Cropland_area_ha, X2010_Cropland_area_ha, X2011_Cropland_area_ha, X2012_Cropland_area_ha, X2013_Cropland_area_ha, X2014_Cropland_area_ha),
									Area_Ranget0_to_Cropt1= c(X2009_RangetoCropland_area_ha, X2010_RangetoCropland_area_ha, X2011_RangetoCropland_area_ha, X2012_RangetoCropland_area_ha, X2013_RangetoCropland_area_ha, X2014_RangetoCropland_area_ha, X2015_RangetoCropland_area_ha),
									Area_Ranget0_to_Ranget1= c(X2009_RangetoRange_ha, X2010_RangetoRange_ha, X2011_RangetoRange_ha, X2012_RangetoRange_ha, X2013_RangetoRange_ha, X2014_RangetoRange_ha, X2015_RangetoRange_ha), 
									Area_Ranget0 = c(Rangeland_2008_ha, Rangeland_2009_ha, Rangeland_2010_ha, Rangeland_2011_ha, Rangeland_2012_ha, Rangeland_2013_ha, Rangeland_2014_ha)
									)),
							with(LCC1to6Merge,	
								data.frame(
									ADMIN_FIPS=rep(ADMIN_FIPS, 7), 
									State=rep(STATE, 7), 
									Year=rep(c(2009, 2010, 2011, 2012, 2013, 2014, 2015), each=nrow(CropRangeDF)),	
									StableArea_noncrop= rep(Stable.area.of.noncrop, 7),TotalRangeorCropAreainLCC_ha = rep(Total_Area_RangeorCrop_inLCC1to6, 7),
									LCC = rep("LCC1to6", 7*nrow(LCC1to6Merge)),
									Area_cropland = c(X2008_Cropland_area_ha, X2009_Cropland_area_ha, X2010_Cropland_area_ha, X2011_Cropland_area_ha, X2012_Cropland_area_ha, X2013_Cropland_area_ha, X2014_Cropland_area_ha),
									Area_Ranget0_to_Cropt1= c(X2009_RangetoCropland_area_ha, X2010_RangetoCropland_area_ha, X2011_RangetoCropland_area_ha, X2012_RangetoCropland_area_ha, X2013_RangetoCropland_area_ha, X2014_RangetoCropland_area_ha, X2015_RangetoCropland_area_ha),
									Area_Ranget0_to_Ranget1= c(X2009_RangetoRange_ha, X2010_RangetoRange_ha, X2011_RangetoRange_ha, X2012_RangetoRange_ha, X2013_RangetoRange_ha, X2014_RangetoRange_ha, X2015_RangetoRange_ha),
									Area_Ranget0 = c(Rangeland_2008_ha, Rangeland_2009_ha, Rangeland_2010_ha, Rangeland_2011_ha, Rangeland_2012_ha.y, Rangeland_2013_ha, Rangeland_2014_ha)
									)),
							with(LCC5or6Merge, 
								data.frame(
									ADMIN_FIPS=rep(ADMIN_FIPS, 7), 
									State=rep(STATE, 7), 
									Year=rep(c(2009, 2010, 2011, 2012, 2013, 2014, 2015), each=nrow(CropRangeDF)),	
									StableArea_noncrop= rep(Stable.area.of.noncrop, 7),TotalRangeorCropAreainLCC_ha = rep(Total_Area_RangeorCrop_inLCC5or6, 7),
									LCC = rep("LCC5or6", 7*nrow(LCC5or6Merge)),
									Area_cropland = c(X2008_Cropland_area_ha, X2009_Cropland_area_ha, X2010_Cropland_area_ha, X2011_Cropland_area_ha, X2012_Cropland_area_ha, X2013_Cropland_area_ha, X2014_Cropland_area_ha),
									Area_Ranget0_to_Cropt1= c(X2009_RangetoCropland_area_ha, X2010_RangetoCropland_area_ha, X2011_RangetoCropland_area_ha, X2012_RangetoCropland_area_ha, X2013_RangetoCropland_area_ha, X2014_RangetoCropland_area_ha, X2015_RangetoCropland_area_ha),
									Area_Ranget0_to_Ranget1= c(X2009_RangetoRange_ha, X2010_RangetoRange_ha, X2011_RangetoRange_ha, X2012_RangetoRange_ha, X2013_RangetoRange_ha, X2014_RangetoRange_ha, X2015_RangetoRange_ha),
									Area_Ranget0 = c(Rangeland_2008_ha, Rangeland_2009_ha, Rangeland_2010_ha, Rangeland_2011_ha, Rangeland_2012_ha.y, Rangeland_2013_ha, Rangeland_2014_ha)
									)),						
							with(LCC1to4Merge, 
								data.frame(
									ADMIN_FIPS=rep(ADMIN_FIPS, 7), 
									State=rep(STATE, 7), 
									Year=rep(c(2009, 2010, 2011, 2012, 2013, 2014, 2015), each=nrow(CropRangeDF)),	
									StableArea_noncrop= rep(Stable_noncrop, 7),
									TotalRangeorCropAreainLCC_ha = rep(Total_Area_RangeorCrop_inLCC1to4, 7),
									LCC = rep("LCC1to4", 7*nrow(LCC1to4Merge)),
									Area_cropland = c(X2008_Cropland_area_ha, X2009_Cropland_area_ha, X2010_Cropland_area_ha, X2011_Cropland_area_ha, X2012_Cropland_area_ha, X2013_Cropland_area_ha, X2014_Cropland_area_ha),
									Area_Ranget0_to_Cropt1= c(X2009_RangetoCropland_area_ha, X2010_RangetoCropland_area_ha, X2011_RangetoCropland_area_ha, X2012_RangetoCropland_area_ha, X2013_RangetoCropland_area_ha, X2014_RangetoCropland_area_ha, X2015_RangetoCropland_area_ha),
									Area_Ranget0_to_Ranget1= c(X2009_RangetoRange_ha, X2010_RangetoRange_ha, X2011_RangetoRange_ha, X2012_RangetoRange_ha, X2013_RangetoRange_ha, X2014_RangetoRange_ha, X2015_RangetoRange_ha),
									Area_Ranget0 = c(Rangeland_2008_ha, Rangeland_2009_ha, Rangeland_2010_ha, Rangeland_2011_ha, Rangeland_2012_ha.y, Rangeland_2013_ha, Rangeland_2014_ha)
									))
	)


##########################
#Calculate and transform RESPONSE VARIABLES
##########################
setwd("Y:/Data/NCEAS_Postdoc/P4 ACR revised methods/")
CropRangeLCCLong$PercentArea_CropStatic <- with(CropRangeLCCLong, 1-(StableArea_noncrop/TotalRangeorCropAreainLCC_ha)) #static. area of cropland is 1- area of rangeland that stays rangeland 2008 to 2012
CropRangeLCCLong$PercentArea_Crop <- with(CropRangeLCCLong, Area_cropland/TotalRangeorCropAreainLCC_ha)	#time-varying. Ignores area that is intermittent cropland or uncertain classification
CropRangeLCCLong[CropRangeLCCLong$Area_cropland==0, "PercentArea_Crop"] <- 0 #0 where there is no cropland in that county
CropRangeLCCLong[is.na(CropRangeLCCLong$Area_cropland), "PercentArea_Crop"] <- 0 #0 where there is cropland is NA in that county
CropRangeLCCLong[CropRangeLCCLong$TotalRangeorCropAreainLCC_ha==0, "PercentArea_Crop"] <- NA #NA where there is no land in that LCC category in that county
CropRangeLCCLong[CropRangeLCCLong$TotalRangeorCropAreainLCC_ha==0, "PercentArea_CropStatic"] <- NA #NA where there is no land in that LCC category in that county

CropRangeLCCLong$ConversionPropCropRangeforACR <- with(CropRangeLCCLong, Area_Ranget0_to_Cropt1/Area_Ranget0)
write.csv(CropRangeLCCLong, paste0(getwd(), "/Analysis/tables/all data combined/LarkConversion_byLCC_GrasslandPrivateArea_forACR_allCounties.csv"), row.names=FALSE)

#########################
#Merge with CONTROL VARIABLES
#########################
controlVariablesDF <- CropRangeLCCLong

###Time-varying
#1. Add Area of CRP expiring in previous year
CRP <- read.csv("Data/CPR_acres_expiring_byYear/AcresleavingCRP_byCountyandYear.csv")
controlVariablesDF <- merge(controlVariablesDF, CRP, by=c("ADMIN_FIPS","Year"), all.x=TRUE)
controlVariablesDF$PercRange_left_CRP <- with(controlVariablesDF, Acres_left_CRP*0.404686/Area_Ranget0)
controlVariablesDF$PercRange_left_CRP[is.infinite(controlVariablesDF$PercRange_left_CRP)] <- 0
#"2014", which has land exiting in Fall 2013, is associated as a predictor for conversion in year 2014 of our data 

###Time-varying
#2. Add annual population change
library(tidyr)
popnDFto2009 <- read.csv("Data/Population_USCensus/co-est2009-alldata.csv", header=TRUE, stringsAsFactors=FALSE)
	popnDFto2009$ADMIN_FIPS <- paste0(popnDFto2009$STATE, sapply(popnDFto2009$COUNTY, function(x) formatC(x, width = 3, format = "d", flag = "0")))
	popnDFto2016 <- read.csv("Data/Population_USCensus/co-est2016-alldata.csv", header=TRUE, stringsAsFactors=FALSE)
	popnDFto2016$ADMIN_FIPS <- paste0(popnDFto2016$STATE, sapply(popnDFto2016$COUNTY, function(x) formatC(x, width = 3, format = "d", flag = "0")))
	popnDF <- merge(popnDFto2009, popnDFto2016, by="ADMIN_FIPS", all=TRUE) 
	popnDF1 <- popnDF[,c("ADMIN_FIPS", paste0("POPESTIMATE", 2008:2014))]
	popnDF2 <- popnDF[, c("ADMIN_FIPS", paste0("NPOPCHG_", 2008:2014))]
	names(popnDF1) <- c("ADMIN_FIPS", 2008:2014)
	names(popnDF2) <- c("ADMIN_FIPS", 2008:2014)
	popn1 <- gather(popnDF1, "Year", "Popn", 2:8)
	popn2 <- gather(popnDF2, "Year", "Popn_Chg", 2:8)
	popnDF <- merge(popn1, popn2, by=c("ADMIN_FIPS", "Year"), all=TRUE)	
	popnDF$PopnChg_Perc <- 100*popnDF$Popn_Chg/popnDF$Popn
	popnDF$nextYear = as.numeric(popnDF$Year)+1
	names(popnDF)[2] <- "censusYear"
write.csv(popnDF, "Data/Population_USCensus/Population_by_county_2008to2015.csv")	
controlVariablesDF <- merge(controlVariablesDF, popnDF, by.x=c("ADMIN_FIPS","Year"), by.y=c("ADMIN_FIPS","nextYear"), all.x=TRUE)

###Static
#3a. Add area of irrigated (harvested ie cropland, not pastureland) land in each county
# This is only available for whole county, not by lcc
irrigatedAreaDF <- read.csv("Data/USDA_IrrigatedArea/CroplandArea_Irrigation_USDACensus_byCounty_formatted.csv", header=TRUE)
controlVariablesDF <- merge(controlVariablesDF, irrigatedAreaDF, by="ADMIN_FIPS",all.x=TRUE)
#3b. Area irrigated is also available in the agDat, though this is the percent irrigated for all agricultural land (cropland, rangeland & pasture)
agDat <- read.csv("Data/USDA AgCensus/AllCountyData_AgCensus2012.csv", header=TRUE, stringsAsFactors=FALSE)
	agNameLookup <- read.csv("Data/USDA AgCensus/Ag_Census_Map_data_variablelookup_07172015.csv", header=TRUE, stringsAsFactors=FALSE) 
	#rename columns
	for (i in 2:ncol(agDat)){
		names(agDat)[i] <- agNameLookup[which(agNameLookup$MapID == strsplit(names(agDat)[i], "_valueNumeric")[[1]][1]), "MAPTITLE"]
		} 
		names(agDat)[which(names(agDat)=="Acres of Irrigated Land as Percent of Land in Farms Acreage:  2012")] <- "PercentLandIrrigated"
controlVariablesDF <- merge(controlVariablesDF, agDat[, c("FipsNumeric","PercentLandIrrigated")], by.x="ADMIN_FIPS", by.y="FipsNumeric", all.x=TRUE)


###Static
#4. Add Area & % urban (urbanised plus urban clusters)
#data from US Census http://www.census.gov/geo/reference/ua/urban-rural-2010.html
urbanDF <- read.csv("Data/USCensus_urban/PctUrbanRural_County.csv", header=TRUE)
urbanDF$ADMIN_FIPS <- paste0(urbanDF$STATE, sapply(urbanDF$COUNTY, function(x) formatC(x, width = 3, format = "d", flag = "0")))
urbanDF$AREA_URBAN_ha <- urbanDF$AREA_URBAN/10000
controlVariablesDF <- merge(controlVariablesDF, urbanDF[,c("ADMIN_FIPS","AREA_URBAN_ha","AREAPCT_URBAN")], by="ADMIN_FIPS", all.x=TRUE)


######################
#Merge CONTROL VARIABLES with RESPONSE VARIABLES
#####################
controlVariablesDF$PercentCroplandthatisIrrigated[is.na(controlVariablesDF$PercentCroplandthatisIrrigated)] <- 0
controlVariablesDF$PercentLandIrrigated[is.na(controlVariablesDF$PercentLandIrrigated)] <- 0
write.csv(controlVariablesDF, paste0(getwd(), "/Analysis/tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables.csv"), row.names=FALSE)

######################
#Set up TIME LAGGED DATASETs
#####################
#using one year to predict the next year, two-year averages to predict subsequent two-year averages, three-year averages to predict subsequent three-year averages, and four-year averages to predict subsequent four-year averages
 #controlVariablesDF <- read.csv("/Analysis/tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables.csv", header=TRUE)
controlVariablesDF$TwoyrAverage <- sapply(controlVariablesDF$Year, function(x){
if(x %% 2 !=0){
		as.character(x) 
		} else { as.character(x-1)
		}})

controlVariablesDF$ThreeyrAverage <- sapply(controlVariablesDF$Year, function(x){
if(x %in% c(2009:2011)){
		"2009_2011" 
		} else { "2012_2014"
		}})
		
controlVariablesDF$FouryrAverage <- sapply(controlVariablesDF$Year, function(x){
if(x %in% c(2008:2011)){
		"2008_2011" 
		} else { "2012_2015"
		}})
	

#aggregate(controlVariablesDF[,c("TotalRangeorCropAreainLCC_ha", "Area_cropland", "Area_Ranget0_to_Cropt1", "Area_Ranget0_to_Ranget1", "Area_Ranget0", "PercentArea_Crop", "ConversionPropCropRangeforACR", "Acres_left_CRP", "Popn", "Popn_Chg", "PopnChg_Perc", "PercentCroplandthatisIrrigated", "AREA_URBAN_ha", "AREAPCT_URBAN")], by = controlVariablesDF[,c("ADMIN_FIPS", "LCC", "TwoyrAverage")], FUN=mean, na.rm=TRUE)

#Time-varying proportional values are expressed as annual change i.e. Conversion rate is expressed as the annual rate, averaged across the time period, as is PopnChg_Perc & PercentArea_Crop. Area values that are measured across the time period (numerators) are summed across the whole time period i.e. Area_ranget0_to_Cropt1, Popn_Chng is summed across 2008 to 2012 for instance. Area values that are measured at the beginning of the time period (denominators) are time-averaged i.e. Area_ranget0, Popn.
	
#TWO Year lags		
twoYearMeans <-  ddply(controlVariablesDF, .(ADMIN_FIPS, LCC, TwoyrAverage), summarize,
		ConversionPropCropRangeforACR = mean(ConversionPropCropRangeforACR, na.rm=TRUE),
		TotalRangeorCropAreainLCC_ha = round(mean(TotalRangeorCropAreainLCC_ha, na.rm=TRUE), 2),
		Area_Ranget0_to_Cropt1 = sum(Area_Ranget0_to_Cropt1, na.rm=TRUE),
		Area_cropland = round(mean(Area_cropland, na.rm=TRUE), 2),
		Area_Ranget0_to_Ranget1 = round(mean(Area_Ranget0_to_Ranget1, na.rm=TRUE), 2),
		Area_Ranget0 = round(mean(Area_Ranget0, na.rm=TRUE), 2),
		PercentArea_CropStatic = round(mean(PercentArea_CropStatic, na.rm=TRUE), 4),
		PercentArea_Crop = round(mean(PercentArea_Crop, na.rm=TRUE), 4),
		Acres_left_CRP = sum(Acres_left_CRP, na.rm=TRUE),
		PercRange_left_CRP = mean(PercRange_left_CRP, na.rm=TRUE),
		Popn = round(mean(Popn, na.rm=TRUE), 0),
		Popn_Chg = sum(Popn_Chg, na.rm=TRUE),
		PopnChg_Perc = mean(PopnChg_Perc, na.rm=TRUE),
		PercentLandIrrigated = mean(PercentLandIrrigated, na.rm=TRUE),
		PercentCroplandthatisIrrigated = mean(PercentCroplandthatisIrrigated, na.rm=TRUE),
		Cropland_Harvested_Irrigated_Acres = mean(Cropland_Harvested_Irrigated_Acres, na.rm=TRUE),
		AREA_URBAN_ha = round(mean(AREA_URBAN_ha, na.rm=TRUE), 2),
		AREAPCT_URBAN = round(mean(AREAPCT_URBAN, na.rm=TRUE), 4)
		)
write.csv(twoYearMeans, paste0(getwd(), "/Analysis/tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_2yraverages.csv"), row.names=FALSE)


#THREE Year lags		
threeYearMeans <-  ddply(controlVariablesDF, .(ADMIN_FIPS, LCC, ThreeyrAverage), summarize,
		ConversionPropCropRangeforACR = mean(ConversionPropCropRangeforACR, na.rm=TRUE),
		TotalRangeorCropAreainLCC_ha = round(mean(TotalRangeorCropAreainLCC_ha, na.rm=TRUE), 2),
		Area_Ranget0_to_Cropt1 = sum(Area_Ranget0_to_Cropt1, na.rm=TRUE),
		Area_cropland = round(mean(Area_cropland, na.rm=TRUE), 2),
		Area_Ranget0_to_Ranget1 = round(mean(Area_Ranget0_to_Ranget1, na.rm=TRUE), 2),
		Area_Ranget0 = round(mean(Area_Ranget0, na.rm=TRUE), 2),
		PercentArea_CropStatic = round(mean(PercentArea_CropStatic, na.rm=TRUE), 4),
		PercentArea_Crop = round(mean(PercentArea_Crop, na.rm=TRUE), 4),
		PercRange_left_CRP = mean(PercRange_left_CRP, na.rm=TRUE),
		Acres_left_CRP = sum(Acres_left_CRP, na.rm=TRUE),
		Popn = round(mean(Popn, na.rm=TRUE), 0),
		Popn_Chg = sum(Popn_Chg, na.rm=TRUE),
		PopnChg_Perc = mean(PopnChg_Perc, na.rm=TRUE),
		PercentLandIrrigated = mean(PercentLandIrrigated, na.rm=TRUE),
		PercentCroplandthatisIrrigated = mean(PercentCroplandthatisIrrigated, na.rm=TRUE),
		Cropland_Harvested_Irrigated_Acres = mean(Cropland_Harvested_Irrigated_Acres, na.rm=TRUE),
		AREA_URBAN_ha = round(mean(AREA_URBAN_ha, na.rm=TRUE), 2),
		AREAPCT_URBAN = round(mean(AREAPCT_URBAN, na.rm=TRUE), 4)
		)
write.csv(threeYearMeans, paste0(getwd(), "/Analysis/tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_3yraverages.csv"), row.names=FALSE)

#FOUR Year lags		
fourYearMeans <-  ddply(controlVariablesDF, .(ADMIN_FIPS, LCC, FouryrAverage), summarize,
		ConversionPropCropRangeforACR = mean(ConversionPropCropRangeforACR, na.rm=TRUE),
		TotalRangeorCropAreainLCC_ha = round(mean(TotalRangeorCropAreainLCC_ha, na.rm=TRUE), 2),
		Area_Ranget0_to_Cropt1 = sum(Area_Ranget0_to_Cropt1, na.rm=TRUE),
		Area_cropland = round(mean(Area_cropland, na.rm=TRUE), 2),
		Area_Ranget0_to_Ranget1 = round(mean(Area_Ranget0_to_Ranget1, na.rm=TRUE), 2),
		Area_Ranget0 = round(mean(Area_Ranget0, na.rm=TRUE), 2),
		PercentArea_CropStatic = round(mean(PercentArea_CropStatic, na.rm=TRUE), 4),
		PercentArea_Crop = round(mean(PercentArea_Crop, na.rm=TRUE), 4),
		PercRange_left_CRP = mean(PercRange_left_CRP, na.rm=TRUE),
		Acres_left_CRP = sum(Acres_left_CRP, na.rm=TRUE),
		Popn = round(mean(Popn, na.rm=TRUE), 0),
		Popn_Chg = sum(Popn_Chg, na.rm=TRUE),
		PopnChg_Perc = mean(PopnChg_Perc, na.rm=TRUE),
		PercentLandIrrigated = mean(PercentLandIrrigated, na.rm=TRUE),PercentCroplandthatisIrrigated = mean(PercentCroplandthatisIrrigated, na.rm=TRUE),
		Cropland_Harvested_Irrigated_Acres = mean(Cropland_Harvested_Irrigated_Acres, na.rm=TRUE),
		AREA_URBAN_ha = round(mean(AREA_URBAN_ha, na.rm=TRUE), 2),
		AREAPCT_URBAN = round(mean(AREAPCT_URBAN, na.rm=TRUE), 4)
		)
write.csv(fourYearMeans, paste0(getwd(), "/Analysis/tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_4yraverages.csv"), row.names=FALSE)

###########################






