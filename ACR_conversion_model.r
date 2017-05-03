
library(MASS) #for boxcox, stepAIC and rlm
library(glmulti) #for glmulti
library(MuMIn) #for AICc
library(plyr) #for ddply
library(ggplot2)
library(gridExtra)#for grid.arrange

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
modelfun <- function(currname, trainingdata, testdata){
	
	mod1 <- with(trainingdata, ConversionPropCropRangeforACR ~ percCrop + percCrop^2 + urban + areaCRPexpires + popn) #full model
	mod2 <- with(trainingdata, ConversionPropCropRangeforACR ~ percCrop + urban + areaCRPexpires + popn) #without saturating function
	mod3 <- with(trainingdata, ConversionPropCropRangeforACR ~ percCrop + percCrop^2 + urban + areaCRPexpires ) #without population
	mod4 <- with(trainingdata, ConversionPropCropRangeforACR ~ percCrop + percCrop^2 + areaCRPexpires + popn) #without urban
	mod5 <- with(trainingdata, ConversionPropCropRangeforACR ~ percCrop + percCrop^2 + urban + popn) #without CRP
	mod6 <- with(trainingdata, ConversionPropCropRangeforACR ~ areaCRPexpires + urban + popn) #without percentcrop

	models <- list(mod1=mod1, mod2=mod2, mod3=mod3, mod4=mod4, mod5=mod5, mod6=mod6)
	save(models, file = sprintf("model output/Models_%s.rda", currname))
	#load("my_model1.rda") #to open
	
	# #How well do the models perform
	unlist(lapply(models, function(x) AICc(x)))
	unlist(lapply(models, function(x) BIC(x)))
	anova(models) 

	#save model summaries to a text file
	sink(sprintf("model output/Summaryofcandidatemodels_%s.txt", currname))
	for (i in seq_along(models)){
		print(names(models)[[i]])
		print(summary(models[[i]]))
		print(paste("AIC", names(models)[[i]], round(AICc(models[[i]]), 2), sep=" "))
		print(paste("BIC", names(models)[[i]], round(BIC(models[[i]]), 2), sep=" "))
		print(anova(models))#only if models are sbusets of each other
		}
	sink()

	predicted_conversion <- lapply(models, function(x) {
		preds <- predict(x, testdata, type="response")
		return(preds)
	}
	save(predicted_conversion, file = sprintf("model output/Predicted_conversion_%s.rda", currname))
	
	#How well do the models predict future conversion
	prediction_accuracy <- unlist(lapply(predicted_conversion, function(i) {
		# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds-actuals)/actuals) #http://r-statistics.co/Linear-Regression.html
		mape <- mean(abs((i - testdata$ConversionPropCropRangeforACR))/testdata$ConversionPropCropRangeforACR)  
		# mean absolute percentage deviation
		#Pearson's correlation
		pcor <- cor(i, testdata$ConversionPropCropRangeforACR)
		#R-squared
		rsq <- 1-sum((testdata$ConversionPropCropRangeforACR-i)^2)/sum((testdata$ConversionPropCropRangeforACR-mean(testdata$ConversionPropCropRangeforACR))^2)
		return(data.frame(mape, pcor, rsq))
	}
	write.csv(prediction_accuracy, filename=sprintf("model output/Prediction_accuracy_%s.rda", currname))
	return(prediction_accuracy)
}

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


