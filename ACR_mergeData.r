#This script summarises and combines dataset for the American Carbon Registry crop conversion prediction models
#combines (1) Lark CDL conversion rates (range to range, range to crop) 
# (2) Control variables, including a. area of urban land in a county, b. annual county population c. Year CRP expires
# Land conversion data isfor private grassland (not forest, developed or water, not public land, not already protected)

#################################################################################################################
#wd <- "/home/runge/Data/NCEAS_Postdoc/P1 Sage Grouse/Analysis/1_PEOG_grazing_economics/Inputs/""
wd <- "Z:/Data/NCEAS_Postdoc/P1 Sage Grouse/Analysis/1_PEOG_grazing_economics/1 Inputs/"
setwd(wd)
options(stringsAsFactors=FALSE) # turn off automatic factor coersion
# options(scipen=9999)            # turn off plotting axis lables in scientific notation

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

###BY YEAR, all LCC
#area of cropland from LARK et al. 2015
	croplandAreaDF <- read.csv("Cropland and pasture/Cropland and Rangeland by year/Cropland_Area_byYear_ha.csv")
	croplandAreaAllcounties <- croplandAreaDF[, c("ADMIN_FIPS", "STATE", "NAME", "Area_ha", "Stable.area.of.cropland_ha", "Area_lost_2008_2012", "Area_gained_2008_2012", "Net_change_2009", "Net_change_2010", "Net_change_2011", "Net_change_2012", "X2008_Cropland_area_ha", "X2009_Cropland_area_ha", "X2010_Cropland_area_ha", "X2011_Cropland_area_ha", "X2012_Cropland_area_ha", "X2009_RangetoCropland_area_ha", "X2010_RangetoCropland_area_ha", "X2011_RangetoCropland_area_ha", "X2012_RangetoCropland_area_ha")] #select only relevant columns
	croplandAreaAllcounties <- croplandAreaAllcounties[order(croplandAreaAllcounties$ADMIN_FIPS),] #order data by fips
#area of rangeland from LARK et al. 2015
	rangelandAreaDF <- read.csv("Cropland and pasture/Cropland and Rangeland by year/Rangeland_Area_byYear_ha.csv")
	rangelandAreaAllcounties <- rangelandAreaDF[ , c("ADMIN_FIPS", "Stable.area.of.noncrop", "Rangeland_2008_ha", "Rangeland_2009_ha", "Rangeland_2010_ha", "Rangeland_2011_ha", "Rangeland_2012_ha", "X2009_RangetoRange_ha", 
"X2010_RangetoRange_ha", "X2011_RangetoRange_ha", "X2012_RangetoRange_ha", "TotalArea_RangeorCrop_ha")] #select only relevant columns
	rangelandAreaAllcounties <- rangelandAreaAllcounties[order(rangelandAreaAllcounties$ADMIN_FIPS),] #order data by fips
	CropRangeDF <- merge(croplandAreaAllcounties, rangelandAreaAllcounties, by="ADMIN_FIPS", all.x=TRUE)

###BY YEAR LCC1to4, 1to6 vs 5or6
LCC1to4RangeDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_LCC1to4.csv", header=TRUE)
LCC1to4CropDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_LCC1to4.csv", header=TRUE)
LCC1to4Merge <- merge(LCC1to4RangeDF, LCC1to4CropDF, by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)

LCC1to6RangeDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_LCC1to6.csv", header=TRUE)
LCC1to6CropDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_LCC1to6.csv", header=TRUE)
LCC1to6Merge <- merge(LCC1to6RangeDF, LCC1to6CropDF, by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)

LCC5or6RangeDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/Rangeland_Area_byYear_LCC5or6.csv", header=TRUE)
LCC5or6CropDF <- read.csv("Cropland and pasture/Cropland and Rangeland by LCC/CroplandArea_byYear_LCC5or6.csv", header=TRUE)
LCC5or6Merge <- merge(LCC5or6RangeDF, LCC5or6CropDF, by=c("ADMIN_FIPS", "STATE"), all.x=TRUE)

CropRangeLCCLong <- rbind(with(CropRangeDF,	
								data.frame(
									ADMIN_FIPS=rep(ADMIN_FIPS, 4), 
									State=rep(STATE, 4), 
									StableArea_noncrop= rep(Stable.area.of.noncrop, 4),
									Year=rep(c(2009, 2010, 2011, 2012), each=nrow(CropRangeDF)),	
									TotalRangeorCropAreainLCC_ha = rep(TotalArea_RangeorCrop_ha, 4),
									LCC = rep("AllLCC", 4*nrow(CropRangeDF)),
									Area_Ranget0_to_Cropt1= c(X2009_RangetoCropland_area_ha, X2010_RangetoCropland_area_ha, X2011_RangetoCropland_area_ha, X2012_RangetoCropland_area_ha),
									Area_Ranget0_to_Ranget1= c(X2009_RangetoRange_ha, X2010_RangetoRange_ha, X2011_RangetoRange_ha, X2012_RangetoRange_ha)
									)),
							with(LCC1to6Merge,	
								data.frame(
									ADMIN_FIPS=rep(ADMIN_FIPS, 4), 
									State=rep(STATE, 4), 
									StableArea_noncrop= rep(Stable.area.of.noncrop, 4),
									Year=rep(c(2009, 2010, 2011, 2012), each=nrow(LCC1to6RangeDF)),	
									TotalRangeorCropAreainLCC_ha = rep(Total_Area_RangeorCrop_inLCC1to6, 4),
									LCC = rep("LCC1to6", 4*nrow(LCC1to6RangeDF)),
									Area_Ranget0_to_Cropt1= c(X2009_RangetoCropland_area_ha, X2010_RangetoCropland_area_ha, X2011_RangetoCropland_area_ha, X2012_RangetoCropland_area_ha),
									Area_Ranget0_to_Ranget1= c(X2009_RangetoRange_ha, X2010_RangetoRange_ha, X2011_RangetoRange_ha, X2012_RangetoRange_ha)
									)),
						with(LCC5or6Merge, 
								data.frame(	
									ADMIN_FIPS=rep(ADMIN_FIPS, 4), 
									State=rep(STATE, 4), 
									StableArea_noncrop= rep(Stable_noncrop, 4),
									Year=rep(c(2009, 2010, 2011, 2012), each=nrow(LCC1to6RangeDF)),	
									TotalRangeorCropAreainLCC_ha = rep(Total_Area_RangeorCrop_inLCC5or6, 4),
									LCC = rep("LCC5or6", 4*nrow(LCC1to6RangeDF)),
									Area_Ranget0_to_Cropt1= c(X2009_RangetoCropland_area_ha, X2010_RangetoCropland_area_ha, X2011_RangetoCropland_area_ha, X2012_RangetoCropland_area_ha),
									Area_Ranget0_to_Ranget1= c(X2009_RangetoRange_ha, X2010_RangetoRange_ha, X2011_RangetoRange_ha, X2012_RangetoRange_ha)
									)),						
							with(LCC1to4Merge, 
								data.frame(	
									ADMIN_FIPS=rep(ADMIN_FIPS, 4), 
									State=rep(STATE, 4), 
									StableArea_noncrop= rep(Stable_noncrop, 4),
									Year=rep(c(2009, 2010, 2011, 2012), each=nrow(LCC1to6RangeDF)),	
									TotalRangeorCropAreainLCC_ha = rep(Total_Area_RangeorCrop_inLCC1to4, 4),
									LCC = rep("LCC1to4", 4*nrow(LCC1to4RangeDF)),
									Area_Ranget0_to_Cropt1= c(X2009_RangetoCropland_area_ha, X2010_RangetoCropland_area_ha, X2011_RangetoCropland_area_ha, X2012_RangetoCropland_area_ha),
									Area_Ranget0_to_Ranget1= c(X2009_RangetoRange_ha, X2010_RangetoRange_ha, X2011_RangetoRange_ha, X2012_RangetoRange_ha)
									))
									
								)
CropRangeLCCLong$PercentArea_Crop <- with(CropRangeLCCLong, 1-(StableArea_noncrop/TotalRangeorCropAreainLCC_ha))

##########################
#Calculate and transform RESPONSE VARIABLES
##########################
								
CropRangeLCCLong$ConversionPropCropRangeV2 <- with(CropRangeLCCLong, Area_Ranget0_to_Cropt1/Area_Ranget0_to_Ranget1)
write.csv(CropRangeLCCLong, "Cropland and pasture/Allcounties_LarkConversion_byLCC_GrasslandPrivateArea.csv", row.names=FALSE)


#########################
#Merge with CONTROL VARIABLES
#########################
controlVariablesDF <- croplandAreaDF[,c("ADMIN_FIPS", "STATE", "Year")]

###
#1. Add Area of CRP expiring in previous year
CRP <- read.csv("Z:/Data/NCEAS_Postdoc/P4 ACR revised methods/Data/CPR_acres_expiring_byYear/AcresleavingCRP_byCountyandYear.csv")
controlVariablesDF <- merge(controlVariablesDF, CRP, by=c("ADMIN_FIPS","Year"), all.x=TRUE)
#"2014", which has land exiting in Fall 2013, is associated as a predictor for conversion in year 2014 of our data 

###
#2. Add annual population change
library(tidyr)
popnDFto2009 <- read.csv("Population_USCensus/co-est2009-alldata.csv", header=TRUE, stringsAsFactors=FALSE)
popnDFto2009$ADMIN_FIPS <- paste0(popnDFto2009$STATE, sapply(popnDFto2009$COUNTY, function(x) formatC(x, width = 3, format = "d", flag = "0")))
popnDFto2016 <- read.csv("Population_USCensus/co-est2016-alldata.csv", header=TRUE, stringsAsFactors=FALSE)
popnDFto2016$ADMIN_FIPS <- paste0(popnDFto2016$STATE, sapply(popnDFto2016$COUNTY, function(x) formatC(x, width = 3, format = "d", flag = "0")))
popnDF <- merge(popnDFto2009, popnDFto2016, by="ADMIN_FIPS", all=TRUE) 
popnDF1 <- popnDF[,c("ADMIN_FIPS", paste0("POPESTIMATE", 2008:2015))]
popnDF2 <- popnDF[, c("ADMIN_FIPS", paste0("NPOPCHG_", 2008:2015))]
names(popnDF1) <- c("ADMIN_FIPS", 2008:2015)
names(popnDF2) <- c("ADMIN_FIPS", 2008:2015)
popn1 <- gather(popnDF1, "Year", "Popn", 2:9)
popn2 <- gather(popnDF2, "Year", "Popn_Chg", 2:9)
popnDF <- merge(popn1, popn2, by=c("ADMIN_FIPS", "Year"), all=TRUE)	
popnDF$PopnChg_Perc <- 100*popnDF$Popn_Chg/popnDF$Popn
write.csv(popnDF, "Population_USCensus/Population_by_county_2008to2015.csv")	
controlVariablesDF <- merge(controlVariablesDF, popnDF, by=c("ADMIN_FIPS","Year"), all.x=TRUE)

###
#3. Add area of irrigated (harvested ie cropland, not pastureland) land in each county
# This is only available for whole county, not by lcc
#Note the area irrigated is also available in the agDat, though this is the percent irrigated for all agricultural land (cropland, rangeland & pasture)
irrigatedAreaDF <- read.csv("USDA_IrrigatedArea/CroplandArea_Irrigation_USDACensus_byCounty_formatted.csv", header=TRUE)
controlVariablesDF <- merge(controlVariablesDF, irrigatedAreaDF, by="ADMIN_FIPS",all.x=TRUE)

###
#4. Add Area & % urban (urbanised plus urban clusters)
#data from US Census http://www.census.gov/geo/reference/ua/urban-rural-2010.html
urbanDF <- read.csv("USCensus_urban/PctUrbanRural_County.csv", header=TRUE)
urbanDF$ADMIN_FIPS <- paste0(urbanDF$STATE, sapply(urbanDF$COUNTY, function(x) formatC(x, width = 3, format = "d", flag = "0")))
urbanDF$AREA_URBAN_ha <- urbanDF$AREA_URBAN/10000
controlVariablesDF <- merge(controlVariablesDF, urbanDF[,c("ADMIN_FIPS","AREA_URBAN_ha","AREAPCT_URBAN")], by="ADMIN_FIPS", all.x=TRUE)


######################
#Merge CONTROL VARIABLES with RESPONSE VARIABLES
#####################
CropRangeLCCLong <- merge(CropRangeLCCLong, controlVariablesDF, by=c("ADMIN_FIPS", "Year"), all.x=TRUE)
write.csv(CropRangeLCCLong, paste0(dirname(wd), "/2 Data by county/Allcounties_LarkConversion_byLCC_plusControlVariables.csv"), row.names=FALSE)

######################
#Set up TIME LAGGED DATASETs
#####################
#using one year to predict the next year, two-year averages to predict subsequent two-year averages, three-year averages to predict subsequent three-year averages, and four-year averages to predict subsequent four-year averages










