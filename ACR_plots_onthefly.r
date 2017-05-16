#on the fly ACR plots

library(plyr) #for ddply
library(corrplot) #for correlation plot
library(ggplot2)
library(randomForest)
library(rfUtilities)
#library(ICEbox) #for ice partial dependency plots
library(rgl) #for nmds 3 plots
#library(ggRandomForests)
library(parallel)

options(stringsAsFactors=FALSE) # turn off automatic factor coersion

wd <- "Y:/Data/NCEAS_Postdoc/P4 ACR revised methods/Analysis"
#wd <- "/home/runge/Data/NCEAS_Postdoc/P4 ACR revised methods/Analysis/"
setwd(wd)
########################
#SET UP DATA
########################

vars <- c("TotalRangeorCropAreainLCC_ha", "Area_cropland", "PercentArea_CropStatic", "PercRange_left_CRP", "Popn", "PopnChg_Perc", "PercentLandIrrigated", "AREAPCT_URBAN")

oneyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables.csv", header=TRUE)
twoyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_2yraverages.csv", header=TRUE)
threeyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_3yraverages.csv", header=TRUE)
fouryrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_4yraverages.csv", header=TRUE)

########################
#SET UP FUNCTIONS
########################
loadfun <- function(currname){
	#load model
	load(sprintf("model_output/RandomForestModel_%s.rda", currname))
}

partialplotFun <- function(currname, trainingdata){
	
	#plot partial dependencies
	#print("plot partial dependencies")
	load(sprintf("model_output/models/RandomForestModel_%s.rda", currname))
	#orderedVars <- rfmod$selvars[order(rfmod$importance, decreasing=TRUE)]
	currtrain <- trainingdata[complete.cases(trainingdata[,vars]),vars] #drop nas
	png(filename=sprintf("model_output/figures/Plot_VariablePartialDependencies_%s.png", currname), width=2010, height=1240, pointsize=16)
	par(mfrow=c(2,4))
	imp <- importance(rfmod$rf.final)
	impvar <- rownames(imp)[order(imp[,1], decreasing=TRUE)]
	for(i in impvar){
		pp <- partialPlot(rfmod$rf.final, currtrain, eval(substitute(i)), plot=FALSE)
		plot(pp, xlab=i, ylab="Conversion probability", main=paste("Partial Dependence on", i))
		lines(pp,lty='solid', col="black") 
		points(pp, pch=19, col="grey70")	
		lines(lowess(pp), lty='solid', col="black")
		}
	dev.off()
}


predictionplotFun <- function(currname){
	predictions <- read.csv(sprintf("model_output/predictions/ModelPredictions_%s.csv", currname))
	#plot predicted against actual
	png(filename=sprintf("model_output/figures/Plot_predicted_vs_actual_%s.png", currname), width=670, height=670, pointsize=16)
	p <- ggplot(predictions, aes(y=ConversionPropCropRangeforACR, x=predicted_conversion)) +
		geom_point(alpha=0.5) +
		xlab("Predicted conversion")+ ylab("Actual conversion")+
		xlim(range(predictions$ConversionPropCropRangeforACR))+ylim(range(predictions$ConversionPropCropRangeforACR))+
		geom_abline(color="grey70") +
		theme_classic(20)+
		ggtitle(sprintf("RandomForest %s", currname))
	print(p)
	dev.off()
	}

quantileFun <- 	function(level, currname){
	predictions <- read.csv(sprintf("model_output/predictions/ModelPredictions_%s.csv", currname))
	actual_quant <- which(predictions$ConversionPropCropRangeforACR >=quantile(predictions$ConversionPropCropRangeforACR, level))
	predicted_quant <- which(predictions$predicted_conversion >=quantile(predictions$predicted_conversion, level))
	return(data.frame(numinquantile=length(actual_quant), correct=length(actual_quant[actual_quant %in% predicted_quant]), percentcorrect=100*length(actual_quant[actual_quant %in% predicted_quant])/length(actual_quant)))
}
	
listFun <- function(level, currname, filename){
	predictions <- read.csv(sprintf("model_output/predictions/ModelPredictions_%s.csv", currname))
	actuals <- predictions[which(predictions$ConversionPropCropRangeforACR >=quantile(predictions$ConversionPropCropRangeforACR, level)),c("ADMIN_FIPS", "ConversionPropCropRangeforACR", "predicted_conversion") ]
	write.csv(actuals, file=filename, row.names=FALSE)
}	

ecdfFun <- function(currname, filename){
	predictions <- read.csv(sprintf("model_output/predictions/ModelPredictions_%s.csv", currname))
	actfun <- ecdf(predictions$ConversionPropCropRangeforACR)
	predictions$Actual_percentile <- actfun(predictions$ConversionPropCropRangeforACR)
	predfun <- ecdf(predictions$predicted_conversion)
	predictions$Prediction_percentile <- actfun(predictions$predicted_conversion)
	write.csv(predictions, file=filename, row.names=FALSE)
}	
	
	
########################
#RUN FUNCTIONS
########################

predictionplotFun("LCC1to6_1yr_train0815_test14")
predictionplotFun("LCC1to6_1yr_train0812_test1315")
predictionplotFun("LCC1to4_1yr_train0812_test1315")
predictionplotFun("LCC5or6_1yr_train0812_test1315")
predictionplotFun("LCC1to6_2yr_train0811_test1215")
predictionplotFun("LCC1to4_2yr_train0811_test1215")
predictionplotFun("LCC5or6_2yr_train0811_test1215")
predictionplotFun("LCC1to6_3yr_train0911_test1214")
predictionplotFun("LCC1to4_3yr_train0911_test1214")
predictionplotFun("LCC5or6_3yr_train0911_test1214")
predictionplotFun("LCC1to6_4yr_train0911_test1214")
predictionplotFun("LCC1to4_4yr_train0911_test1214")
predictionplotFun("LCC5or6_4yr_train0911_test1214")
predictionplotFun("LCC1to6_3yr_train0815_test1214")


partialplotFun("LCC1to6_1yr_train0815_test14", trainingdata=oneyrlag[oneyrlag$LCC == "LCC1to6", ])
partialplotFun("LCC1to6_1yr_train0812_test1315", 
			trainingdata=oneyrlag[oneyrlag$LCC == "LCC1to6" & oneyrlag$Year %in% c(2008:2012), ])
partialplotFun("LCC1to4_1yr_train0812_test1315", 
			trainingdata=oneyrlag[oneyrlag$LCC == "LCC1to4" & oneyrlag$Year %in% c(2008:2012),])
partialplotFun("LCC5or6_1yr_train0812_test1315", 
			trainingdata=oneyrlag[oneyrlag$LCC == "LCC5or6" & oneyrlag$Year %in% c(2008:2012),])
partialplotFun("LCC1to6_2yr_train0811_test1215", 
			trainingdata=twoyrlag[twoyrlag$LCC == "LCC1to6" & twoyrlag$TwoyrAverage %in% c(2008:2011),])
partialplotFun("LCC1to4_2yr_train0811_test1215", 
			trainingdata=twoyrlag[twoyrlag$LCC == "LCC1to4" & twoyrlag$TwoyrAverage %in% c(2008:2011),])
partialplotFun("LCC5or6_2yr_train0811_test1215", 
			trainingdata=twoyrlag[twoyrlag$LCC == "LCC5or6" & twoyrlag$TwoyrAverage %in% c(2008:2011),])
partialplotFun("LCC1to6_3yr_train0911_test1214", 
			trainingdata=threeyrlag[threeyrlag$LCC == "LCC1to6" & threeyrlag$ThreeyrAverage=="2009_2011",])
partialplotFun("LCC1to4_3yr_train0911_test1214", 
			trainingdata=threeyrlag[threeyrlag$LCC == "LCC1to4" & threeyrlag$ThreeyrAverage=="2009_2011",])
partialplotFun("LCC5or6_3yr_train0911_test1214", 
			trainingdata=threeyrlag[threeyrlag$LCC == "LCC5or6" & threeyrlag$ThreeyrAverage=="2009_2011",])	
partialplotFun("LCC1to6_4yr_train0911_test1214", 
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC1to6" & fouryrlag$FouryrAverage=="2008_2011",])
partialplotFun("LCC1to4_4yr_train0911_test1214", 
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC1to4" & fouryrlag$FouryrAverage=="2008_2011",])
partialplotFun("LCC5or6_4yr_train0911_test1214", 
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC5or6" & fouryrlag$FouryrAverage=="2008_2011",])
partialplotFun("LCC1to6_3yr_train0815_test1214", trainingdata=threeyrlag[threeyrlag$LCC == "LCC1to6", ])

quantileFun(0.8, "LCC1to6_3yr_train0815_test1214")
quantileFun(0.85, "LCC1to6_3yr_train0815_test1214")
quantileFun(0.9, "LCC1to6_3yr_train0815_test1214")
quantileFun(0.95, "LCC1to6_3yr_train0815_test1214")
quantileFun(0.99, "LCC1to6_3yr_train0815_test1214")

#List of counties with actual conversion rates > 0.005
listFun(0.85, "LCC1to6_3yr_train0815_test1214", "model_output/predictions/Counties_in_top15percentquantile.csv")

#Export .csv listing the percentile each county falls in 
ecdfFun("LCC1to6_3yr_train0815_test1214", "model_output/predictions/Counties_bypercentile_LCC1to6_3yr_train0815_test1214.csv")
#this .csv is then linked to a .shp of counties (in ArcGIS) to make figures for the report.


