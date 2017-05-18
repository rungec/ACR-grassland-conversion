#This script uses the Random Forest model developed to predict conversion rates of native vegetations (grasslands & shrublands) to cropland across the cont. USA. This script was tested 05/16/2017 using R 3.4.0.

########################
#PRELIMINARY PROCESSING
########################

library(plyr) #for ddply
library(ggplot2)
library(randomForest)
library(rfUtilities)
library(rgl) #for nmds 3 plots

options(stringsAsFactors=TRUE) # turn off automatic factor coersion

#EDIT ME Set your working directory here - this is the place where the model is stored
wd <- "C:/MyDir/"
setwd(wd)

#EDIT ME - specify a suffix for outputs. This will be pasted onto filenames
currname <- "2014_predictions"

#EDIT ME
updateddata <- read.csv("mynewdata.csv", header=TRUE)
#This is a .csv file with one row for each county-year that you want to predict conversion rates for. 
#It should contain the following fields, which can be updated as new data becomes available, or retain the values from the 2014 dataset Data_for_RandomForestModel.csv found in the github repository. Note all variables are estimated at the county scale, and conversion rates are expressed as for only private land classified as suitable for cropping (land capability class 1 to 6), and excluding water, developed land and forest.
#Required fields:
#TotalRangeorCropAreainLCC_ha
#Area_cropland
#PercentArea_CropStatic
#PercRange_left_CRP
#Popn
#PopnChg_Perc
#PercentLandIrrigated
#AREAPCT_URBAN

########################
#PREDICT CONVERSION
########################
#Load Random Forest regression model. It should be located in folder "model" in the working directory
	load("model/RandomForestModel_LCC1to6_3yr_train0815_test1214.rda")

#Clean up the updated dataset
	print("setup data")
	vars <- c("TotalRangeorCropAreainLCC_ha", "Area_cropland", "PercentArea_CropStatic", "PercRange_left_CRP", "Popn", "PopnChg_Perc", "PercentLandIrrigated", "AREAPCT_URBAN")
	updateddata <- updateddata[, c("ADMIN_FIPS", vars)] #drop unnecessary columns
	updateddata <- updateddata[complete.cases(updateddata[,c("ADMIN_FIPS",vars)]),c("ADMIN_FIPS",vars)] #drop rows with NAs - the model will not run if this contains NAs
	nrows(updateddata) #check 
	
#Predict conversion
	print("predict conversion")
	updateddata$predicted_conversion <- predict(rfmod$rf.final, updateddata)
	
#Add county percentile rankings
	predfun <- ecdf(updateddata$predicted_conversion)
	updateddata$Prediction_percentile <- predfun(updateddata$predicted_conversion)	

#Save predictions	
	write.csv(updateddata, sprintf"ModelPredictions_%s.csv", currname), row.names=FALSE)
	
#END
########################
