
library(plyr) #for ddply
library(corrplot) #for correlation plot
library(ggplot2)
library(randomForest)
library(rUtilities)
#library(ICEbox) #for ice partial dependency plots
library(rgl) #for nmds 3 plots


options(stringsAsFactors=FALSE) # turn off automatic factor coersion

wd <- "Y:/Data/NCEAS_Postdoc/P4 ACR revised methods/Analysis"
wd <- "/home/runge/Data/NCEAS_Postdoc/P4 ACR revised methods/Analysis/"
setwd(wd)

########################
#SET UP DATA
########################

oneyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables.csv", header=TRUE)
twoyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_2yraverages.csv", header=TRUE)
threeyrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_3yraverages.csv", header=TRUE)
fouryrlag <- read.csv("tables/all data combined/LandConversion_combinedData_allUSStates_byLCC_plusControlVariables_4yraverages.csv", header=TRUE)

########################
#CHECK CORRELATION OF VARIABLES
########################
#check correlation of control variables
allvars <- c("TotalRangeorCropAreainLCC_ha", "Area_Ranget0", "Area_cropland", "PercentArea_Crop","PercRange_left_CRP", "Popn", "Popn_Chg", "PopnChg_Perc", "PercentCroplandthatisIrrigated", "AREA_URBAN_ha", "AREAPCT_URBAN")
cors <- cor(oneyrlag[oneyrlag$LCC=="LCC1to6",allvars], method="pearson", use="complete.obs")
png(filename="model_output/CorrelationPlotofVariables.png", width=930, height=720, pointsize=16)
	corrplot(cors, method='number', type='lower')
dev.off()

#check correlation of conversion in one time period with conversion in other time period
#2008 to 2011 against 2012 to 2015
cors <- cor(x=fouryrlag[fouryrlag$FouryrAverage=="2008_2011", "ConversionPropCropRangeforACR"], y=fouryrlag[fouryrlag$FouryrAverage=="2012_2015", "ConversionPropCropRangeforACR"], method="pearson", use="complete.obs")
	print(cors)
png(filename="model_output/figures/Conversion_2008to2011_vs_2012to2015.png", width=670, height=670)
	plot(x=fouryrlag[fouryrlag$FouryrAverage=="2008_2011", "ConversionPropCropRangeforACR"], y=fouryrlag[fouryrlag$FouryrAverage=="2012_2015", "ConversionPropCropRangeforACR"], xlab="Conversion 2008 to 2011", ylab="Conversion 2012 to 2015")
dev.off()

#2009 to 2010 against 2013 to 2014
cors <- cor(x=twoyrlag[twoyrlag$TwoyrAverage=="2009", "ConversionPropCropRangeforACR"], y=twoyrlag[twoyrlag$TwoyrAverage=="2013", "ConversionPropCropRangeforACR"], method="pearson", use="complete.obs")
print(cors)

png(filename="model_output/figures/Conversion_2009to2010_vs_2013to2014.png", width=670, height=670)
	plot(x=twoyrlag[twoyrlag$TwoyrAverage=="2009", "ConversionPropCropRangeforACR"], y=twoyrlag[twoyrlag$TwoyrAverage=="2013", "ConversionPropCropRangeforACR"], xlab="Conversion 2009 to 2010", ylab="Conversion 2013 to 2014")
dev.off()


########################
#SET UP MODEL FUNCTION
########################
#Random Forest regression model
modelfun <- function(currname, trainingdata, testdata){
	
	vars <- c("TotalRangeorCropAreainLCC_ha","PercentArea_Crop","PercRange_left_CRP", "Popn_Chg", "PopnChg_Perc", "PercentCroplandthatisIrrigated", "AREAPCT_URBAN")
	varnames <- c("ConversionPropCropRangeforACR", "Total_ha","Percent_Crop","PercRangeleft_CRP", "Popn_Chg", "PopnChg_Perc", "PercCropIrrigated", "Perc_Urban")
	
	#Set up training data
	trainingdata <- trainingdata[, c("ConversionPropCropRangeforACR", vars)]
	trainingdata <- trainingdata[complete.cases(trainingdata),]
	names(trainingdata) <- varnames
	#trainingdata$State <- as.factor(trainingdata$State)
	#trainingdata$Year <- as.factor(trainingdata$Year)
	
	#Set up test data
	testdata <- testdata[,c("ADMIN_FIPS", "ConversionPropCropRangeforACR", vars)]
	names(testdata) <- c("ADMIN_FIPS", varnames)
	#testdata <- testdata[complete.cases(testdata),]
	#testdata$State <- as.factor(testdata$State)
	#testdata$Year <- as.factor(testdata$Year)
	
	#run random forest, select variables
	#rfmod <- randomForest(ConversionPropCropRangeforACR ~., data=trainingdata, ntree=20, importance=TRUE, do.trace=100)
	rfmod <- rf.modelSel(x=trainingdata[,vars], y=trainingdata$ConversionPropCropRangeforACR, imp.scale='mir', ntree=500, final.model=TRUE, proximity=TRUE, mse=TRUE, rsp=TRUE, seed=1234, keepforest=TRUE)
	save(rfmod, file = sprintf("model_output/RandomForestModel_%s.rda", currname))
	#load("my_model1.rda") #to open
	
	#variable importance
	png(filename=sprintf("model_output/figures/Plot_randomforestvariableimportance_%s.png", currname), width=2010, height=670)
		par(mfrow=c(1,3))
		p <- as.matrix(rfmod$importance)   
		ord <- rev(order(p[,3], decreasing=TRUE)[1:dim(p)[1]]) 
		dotchart(p[ord,3], main="Scaled Variable Importance", pch=19)
		dotchart(p[ord,1], main="Decrease in MSE", pch=19)
		dotchart(p[ord,2], main="Decrease in Node Purity", pch=19)
	dev.off()
	
	#plot partial dependencies
	png(filename=sprintf("model_output/figures/Plot_VariablePartialDependencies_%s.png", currname), width=2010, height=1440)
	par(mfrow=c(4,3))
	for(i in length(rfmod$selvars)){
		partialPlot(rfmod, trainingdata, rfmod$selvars[i], xlab=as.character(rfmod$selvars[i]), ylab="Conversion probability", main="")
	}
	dev.off()
		
	#nmds of first three dimensions
	rf.cmd <- cmdscale(1 - rfmod$proximity, eig=TRUE, k=4) 
    rf.cmd <- data.frame(rf.cmd$points)       
    plot3d(rf.cmd[,1],rf.cmd[,2],rf.cmd[,3], col=pa.col, pch=18, size=1.25, type="s", xlab="MDS dim 1", ylab="MDS dim 2", zlab="MDS dim 3")
	rgl.snapshot(filename = sprintf("model_output/figures/nmds_3d_%s.png", currname))
	writeWebGL(dir=file.path(getwd()),filename= sprintf("model_output/figures/nmds_3d_%s.html", currname), width=500)
	#movie3d(spin3d(axis=c(1,1,1), rpm=3), dir=paste0(getwd(),"model_output/movies"), movie=sprintf("nmds_3d_movie_%s", currname), duration=10)
	
	#predict conversion
	testdata$predicted_conversion <- predict(rfmod, testdata)
	write.csv(testdata, sprintf("model_output/ModelPredictions_%s.csv", currname), row.names=FALSE)
	
	#How well do the models perform
	#test of performance against random
	 rf.perm <- rf.significance(rfmod, trainingdata[, vars], nperm = 1000, ntree = 1001)
	 #regression fit
	 rf.regfit <- rf.regression.fit(rfmod)

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
	write.csv(prediction_accuracy, filename=sprintf("model_output/Prediction_accuracy_%s.rda", currname))
	 
	#save model summaries to a text file
	sink(sprintf("model_output/Summary_randomforestmodel_%s.txt", currname))
		print(paste("Random Forest Model",  currname, sep="_"))
		print("Variable Selection")
		print(rfmod)
		print("Permutation Importance")
		print(rf.perm)
		print("Regression fit")
		print(rf.regfit)
		print("Predictive ability")
		print(prediction_accuracy)
	sink()

	#plot predicted against actual
	png(filename=sprintf("model_output/figures/Plot_predicted_vs_actual_%s.png", currname), width=670, height=670)
	p <- ggplot(testdata, aes(x=ConversionPropCropRangeforACR, y=predicted_conversion) +
		geom_point() +
		geom_abline(color="red") +
		ggtitle(paste("RandomForest Regression r^2=", r2, sep=""))
	print(p)
	dev.off()

	#return the random forest model
	return(rfmod)
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
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC1to6" & fouryrlag$FourYrAverage=="2008_2011",]
			testdata=fouryrlag[fouryrlag$LCC == "LCC1to6" & fouryrlag$FourYrAverage=="2012_2015",])
			
modelfun("LCC1to4_4yr_train0911_test1214", 
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC1to4" & fouryrlag$FourYrAverage=="2008_2011",]
			testdata=fouryrlag[fouryrlag$LCC == "LCC1to4" & fouryrlag$FourYrAverage=="2012_2015",])

modelfun("LCC5or6_4yr_train0911_test1214", 
			trainingdata=fouryrlag[fouryrlag$LCC == "LCC5or6" & fouryrlag$FourYrAverage=="2008_2011",]
			testdata=fouryrlag[fouryrlag$LCC == "LCC5or6" & fouryrlag$FourYrAverage=="2012_2015",])

############################


