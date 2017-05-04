
library(plyr) #for ddply
library(ggplot2)
library(randomForest)
library(rUtilities)

options(stringsAsFactors=FALSE) # turn off automatic factor coersion

wd <- "Y:/Data/NCEAS_Postdoc/P4 ACR revised methods/Analysis"
setwd(wd)

########################
#SET UP DATA
########################

oneyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables.csv")
twoyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_2yraverages.csv")
threeyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_3yraverages.csv")
fouryrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_4yraverages.csv")

vars <- c("State", "TotalRangeorCropAreainLCC_ha", "Area_Ranget0", "Area_cropland", "PercentArea_Crop","PercRange_left_CRP", "Popn", "Popn_Chg", "PopnChg_Perc", "PercentCroplandthatisIrrigated", "AREA_URBAN_ha", "AREAPCT_URBAN")

########################
#CHECK CORRELATION OF VARIABLES
########################
#check correlation of control variables
cor(oneyrlag[oneyrlag$LCC=="LCC1to6",], method="pearson")
png(filename=paste0(outDir, "/Correlation of variables/CorrelationPlotofVariables.png"), width=1860, height=1440)
corrplot::corrplot(cors[[1]], method='number', type='lower')
dev.off()

#check correlation of conversion in one time period with conversion in other time period

########################
#SET UP MODEL FUNCTION
########################
#Generalized linear model with binomial - logit link
modelfun <- function(currname, trainingdata, testdata){
	
	vars <- c("State", "TotalRangeorCropAreainLCC_ha", "Area_Ranget0", "Area_cropland", "PercentArea_Crop","PercRange_left_CRP", "Popn", "Popn_Chg", "PopnChg_Perc", "PercentCroplandthatisIrrigated", "AREA_URBAN_ha", "AREAPCT_URBAN")
	#Set up training data
	trainingdata <- trainingdata[, c("ConversionPropCropRangeforACR", vars)]
	trainingdata <- trainingdata[complete.cases(trainingdata),]
	trainingdata$State <- as.factor(trainingdata$State)
	#trainingdata$Year <- as.factor(trainingdata$Year)
	
	#Set up test data
	#testdata <- testdata[,c("ADMIN_FIPS", "ConversionPropCropRangeforACR", vars)]
	#testdata <- testdata[complete.cases(testdata),]
	testdata$State <- as.factor(testdata$State)
	#testdata$Year <- as.factor(testdata$Year)
	
	#run random forest
	set.seed(415)
	#rfmod <- randomForest(ConversionPropCropRangeforACR ~., data=trainingdata, ntree=500, importance=TRUE, do.trace=100)
	rfmod <- rf.modelSel(x=trainingdata[,vars], y=trainingdata$ConversionPropCropRangeforACR, imp.scale='mir', ntree=500)
	save(rfmod, file = sprintf("model output/RandomForestModel_%s.rda", currname))
	#load("my_model1.rda") #to open
	
	#variable importance
	png(filename=sprintf("model output/Plot_randomforestvariableimportance_%s.png", currname), width=1240, height=670)
		p <- as.matrix(rfmod$importance[,3])   
		ord <- rev(order(p[,1], decreasing=TRUE)[1:dim(p)[1]]) 
		dotchart(p[ord,1], main="Scaled Variable Importance", pch=19)
	dev.off()
	
	#predict conversion
	testdata$predicted_conversion <- predict(rfmod, testdata)
	write.csv(testdata, sprintf("model output/ModelPredictions_%s.csv", currname), row.names=FALSE)
	
	#How well do the models perform
	#test of performance against random
	 rf.perm <- rf.significance(rfmod, trainingdata[, vars], nperm = 99, ntree = 1001)
	 #regression fit
	 rf.regfit <- rf.regression.fit(rfmod)
	 
	#save model summaries to a text file
	sink(sprintf("model output/Summary_randomforestmodel_%s.txt", currname))
		print(rf.perm)
		print(rf.regfit)
	sink()


	#How well do the models predict future conversion
	# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds-actuals)/actuals) 
		#http://r-statistics.co/Linear-Regression.html
		mape <- with(testdata, mean(abs((predicted_conversion - ConversionPropCropRangeforACR))/ConversionPropCropRangeforACR))  
	# mean absolute percentage deviation
		#Pearson's correlation
		pcor <- with(testdata, cor(predicted_conversion, ConversionPropCropRangeforACR))
	#R-squared
		rsq <- with(testdata, 1-sum((ConversionPropCropRangeforACR-i)^2)/sum((ConversionPropCropRangeforACR-mean(ConversionPropCropRangeforACR))^2))
	prediction_accuracy <- data.frame(mape, pcor, rsq)
	write.csv(prediction_accuracy, filename=sprintf("model output/Prediction_accuracy_%s.rda", currname))
	
	#plot predicted against actual
	png(filename=sprintf("model output/Plot_predicted_vs_actual_%s.png", currname), width=670, height=670)
	p <- ggplot(testdata, aes(x=ConversionPropCropRangeforACR, y=predicted_conversion) +
		geom_point() +
		geom_abline(color="red") +
		ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))
	print(p)
	dev.off()	
}

#http://evansmurphy.wixsite.com/evansspatial/random-forest-sdm

########################
#PREDICT and TEST MODELS
########################

#Trial models with one yr lag
modelfun("LCC1to6_1yr_train0812_test1315", 
			trainingdata=oneyrlag[oneyrlag$LCC == "LCC1to6" & oneyrlag$Year %in% c(2008:2012),]
			testdata=oneyrlag[oneyrlag$LCC == "LCC1to6" & oneyrlag$Year %in% c(2013:2015),])
			
modelfun("LCC1to4_1yr_train0812_test1315", 
			trainingdata=oneyrlag[oneyrlag$LCC == "LCC1to4" & oneyrlag$Year %in% c(2008:2012),]
			testdata=oneyrlag[oneyrlag$LCC == "LCC1to4" & oneyrlag$Year %in% c(2013:2015),])

modelfun("LCC5or6_1yr_train0812_test1315", 
			trainingdata=oneyrlag[oneyrlag$LCC == "LCC5or6" & oneyrlag$Year %in% c(2008:2012),]
			testdata=oneyrlag[oneyrlag$LCC == "LCC5or6" & oneyrlag$Year %in% c(2013:2015),])

#Trial models with two year lag			
modelfun("LCC1to6_2yr_train0811_test1215", 
			trainingdata=twoyrlag[twoyrlag$LCC == "LCC1to6" & twoyrlag$TwoYrAverage %in% c(2008:2011),]
			testdata=twoyrlag[twoyrlag$LCC == "LCC1to6" & twoyrlag$TwoYrAverage %in% c(2012:2015),])
			
modelfun("LCC1to4_2yr_train0811_test1215", 
			trainingdata=twoyrlag[twoyrlag$LCC == "LCC1to4" & twoyrlag$TwoYrAverage %in% c(2008:2011),]
			testdata=twoyrlag[twoyrlag$LCC == "LCC1to4" & twoyrlag$TwoYrAverage %in% c(2012:2015),])

modelfun("LCC5or6_2yr_train0811_test1215", 
			trainingdata=twoyrlag[twoyrlag$LCC == "LCC5or6" & twoyrlag$TwoYrAverage %in% c(2008:2011),]
			testdata=twoyrlag[twoyrlag$LCC == "LCC5or6" & twoyrlag$TwoYrAverage %in% c(2012:2015),])
			
#Trial models with three year lag			
modelfun("LCC1to6_3yr_train0911_test1214", 
			trainingdata=threeyrlag[threeyrlag$LCC == "LCC1to6" & threeyrlag$ThreeYrAverage=="2009_2011",]
			testdata=threeyrlag[threeyrlag$LCC == "LCC1to6" & threeyrlag$ThreeYrAverage=="2012_2014",])
			
modelfun("LCC1to4_3yr_train0911_test1214", 
			trainingdata=threeyrlag[threeyrlag$LCC == "LCC1to4" & threeyrlag$ThreeYrAverage=="2009_2011",]
			testdata=threeyrlag[threeyrlag$LCC == "LCC1to4" & threeyrlag$ThreeYrAverage=="2012_2014",])

modelfun("LCC5or6_3yr_train0911_test1214", 
			trainingdata=threeyrlag[threeyrlag$LCC == "LCC5or6" & threeyrlag$ThreeYrAverage=="2009_2011",]
			testdata=threeyrlag[threeyrlag$LCC == "LCC5or6" & threeyrlag$ThreeYrAverage=="2012_2014",])
			
#Trial models with four year lag			
modelfun("LCC1to6_4yr_train0911_test1214", 
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC1to6" & fouryrlag$FourYrAverage=="2009_2011",]
			testdata=fouryrlag[fouryrlag$LCC == "LCC1to6" & fouryrlag$FourYrAverage=="2012_2014",])
			
modelfun("LCC1to4_4yr_train0911_test1214", 
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC1to4" & fouryrlag$FourYrAverage=="2009_2011",]
			testdata=fouryrlag[fouryrlag$LCC == "LCC1to4" & fouryrlag$FourYrAverage=="2012_2014",])

modelfun("LCC5or6_4yr_train0911_test1214", 
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC5or6" & fouryrlag$FourYrAverage=="2009_2011",]
			testdata=fouryrlag[fouryrlag$LCC == "LCC5or6" & fouryrlag$FourYrAverage=="2012_2014",])

############################


