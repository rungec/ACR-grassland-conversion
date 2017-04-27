
library(MASS) #for boxcox, stepAIC and rlm
library(glmulti) #for glmulti
library(MuMIn) #for AICc
library(plyr) #for ddply
library(ggplot2)
library(gridExtra)#for grid.arrange

options(stringsAsFactors=FALSE) # turn off automatic factor coersion

wd <- "Z:/Data/NCEAS_Postdoc/P1 Sage Grouse/Analysis/1_PEOG_grazing_economics/"
setwd(wd)

########################
#SET UP DATA
########################
allCountiesDF <- read.csv("2 Data by county/Allcounties_LarkConversion_byLCC_plusControlVariables.csv", header=TRUE)

allCountiesDF$Year <- as.factor(allCountiesDF$Year)
allCountiesDF$ADMIN_FIPS <- as.factor(allCountiesDF$ADMIN_FIPS)
allCountiesDF$LCC <- as.factor(allCountiesDF$LCC)
allCountiesDF$STATE <- as.factor(allCountiesDF$STATE)
names(allCountiesDF)[which(names(allCountiesDF)=="Acres.of.Irrigated.Harvested.Cropland.as.Percent.of.All.Harvested.Cropland.Acreage...2012")] <- "PercentCroplandIrrigated"
allCountiesDF$PercentCroplandIrrigated[is.na(allCountiesDF$PercentCroplandIrrigated)] <- 0

#NEED TO SUMMARISE DATA by 1,2,3,4 year averages
exclude from your analysis: - false conversion

18087  Lagrange, IN
48249  Jim Wells, TX
########################
#CHECK CORRELATION OF VARIABLES
########################
#check correlation of control variables
cor(data, method="pearson")
png(filename=paste0(outDir, "/Correlation of variables/CorrelationPlotofEnvironmentalVariables_nowater2.png"), width=1860, height=1440)
corrplot::corrplot(cors[[1]], method='number', type='lower')
dev.off()

#check correlation of conversion in one time period with conversion in other time period

########################
#TEST MODEL FIT USING 2012-2015 data
########################
#make models for each LCC and for each lagged dataset
datasetlist <- expand.grid(LCC=c("LCC1to6", "LCC1to4", "LCC5or6"), yr=c("1yrlag", "2yrlag","3yrlag", "4yrlag"))
datasetnames <- paste(rep(c("LCC1to6", "LCC1to4", "LCC5or6"), each=4), c("1yrlag", "2yrlag","3yrlag", "4yrlag"), sep="_")

for(i in 1:nrow(datasetlist)) {
	currname <- paste(datasetlist$LCC[i], datasetlist$yr[i], sep="_")
	currdata <- subset(allCountiesDF, LCC==datasetlist$LCC[i], YearLag=datasetlist$yr[i])

	mod1 <- with(currdata, conversion ~ conversionPrevious + percCrop + percCrop^2 + urban + areaCRPexpires + popn) #full model
	mod2 <- with(currdata, conversion ~ conversionPrevious + percCrop + urban + areaCRPexpires + popn) #without saturating function
	mod3 <- with(currdata, conversion ~ conversionPrevious + percCrop + percCrop^2 + urban + areaCRPexpires ) #without population
	mod4 <- with(currdata, conversion ~ conversionPrevious + percCrop + percCrop^2 + areaCRPexpires + popn) #without urban
	mod5 <- with(currdata, conversion ~ conversionPrevious + percCrop + percCrop^2 + urban + popn) #without CRP
	mod6 <- with(currdata, conversion ~ conversionPrevious + areaCRPexpires + urban + popn) #without percentcrop
	mod7 <- with(currdata, conversion ~ percCrop + percCrop^2 + areaCRPexpires + urban + popn) #control variables only
	mod8 <- with(currdata, conversion ~ conversionPrevious) #parsimoneous

	models <- list(mod1=mod1, mod2=mod2, mod3=mod3, mod4=mod4, mod5=mod5, mod6=mod6, mod7=mod7, mod8=mod8)
	save(models, file = sprintf("3 Model output/Models_%s.rda", currname))
	#load("my_model1.rda") #to open
	
	# #How well do the models perform
	unlist(lapply(models, function(x) AICc(x)))
	unlist(lapply(models, function(x) BIC(x)))
	anova(models) 

	#save model summaries to a text file
	sink(sprintf("3 Model output/Summaryofcandidatemodels_%s.txt", currname))
	for (i in seq_along(models)){
		print(names(models)[[i]])
		print(summary(models[[i]]))
		print(paste("AIC", names(models)[[i]], round(AICc(models[[i]]), 2), sep=" "))
		print(paste("BIC", names(models)[[i]], round(BIC(models[[i]]), 2), sep=" "))
		print(anova(models))#only if models are sbusets of each other
		}
	sink()

	}
	
	
###OTHER IDEAS
	#test models against 2012-2015 conversion rates

predict(mod1, newdata, type="response")
summary...)
#standard error & Fstatisitc are measures of goodness of fit

#http://r-statistics.co/Linear-Regression.html
# Now lets calculate the Min Max accuracy and MAPE: 
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds-actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 48.38%, mean absolute percentage deviation


#library(caret)

#compare how well it predicts counties
thresh  <- 0.5            # threshold for categorizing predicted probabilities
> predFac <- cut(predTst, breaks=c(-Inf, thresh, Inf), labels=c("lo", "hi"))
> cTab    <- table(yFac[idxTst], predFac, dnn=c("actual", "predicted"))
> addmargins(cTab)
      predicted
actual lo hi Sum
   lo  12  4  16
   hi   5  9  14
   Sum 17 13  30
   
  #plot
predplot(mod1, newdata, asp=1, line=TRUE)  

# what is the proportion variation explained in the outcome of the testing data?
# i.e., what is 1-(SSerror/SStotal)
actual <- testing$Sepal.Length
rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
print(rsq)

#compare the to x% of converted cropland counties predicted vs actual
