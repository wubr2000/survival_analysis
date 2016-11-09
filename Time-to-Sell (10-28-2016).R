#Importing the listing dataset
filename = "~/Desktop/pending_SF_apr15_oct25.csv"
listingData = read.csv(filename)

#Importing OECD Consumer Confidence data
filenameCCI = "~/Desktop/Consumer_Confidence_OECD.csv"
CCI = read.csv(filenameCCI)

#Importing Bloomberg Consumer Confidence Index data
#filenameBCCI = "C:\\Users\\Sujit\\Desktop\\Surival Analysis\\BloombergConsumerConfidence.csv"
#BCCI = read.csv(filenameBCCI)

#Subsetting only "condo" and "single_family" listings
index = (1:nrow(listingData))[listingData$type %in% c("condo", "single_family")]
listingData = listingData[index,]

#Last day of data used
end_date = as.Date("2016-10-25", format="%Y-%m-%d")
listingData$listing_date = as.Date(listingData$listing_date, format="%Y-%m-%d")
listingData$pending_date = as.Date(listingData$pending_date, format="%Y-%m-%d")

#Time to pending
duration = ifelse (is.na(listingData$pending_date), 
                   end_date - listingData$listing_date, 
                   listingData$pending_date - listingData$listing_date)
#censor=0 if the house goes to pending, else censor=1
censor = ifelse (is.na(listingData$pending_date), 1, 0)

#Subsetting listings with duration > 7 
index = (1:nrow(listingData))[duration > 7]
duration = duration[index]
censor = censor[index]
listingData = listingData[index,]

#Age of the house
houseAge = as.numeric(format(end_date, "%Y")) - listingData$year_built

#Removing bad entries
houseAge[houseAge < 0] = NA
listingData$sqft[listingData$sqft <= 200] = NA
listingData$sqft[listingData$price <= 10000] = NA

#Importing Listing Cube dataset to calculate median home prices for each zip code
#marketPrices = read.csv("C:\\Users\\Sujit\\Desktop\\Surival Analysis\\ListingCube_Zip.csv")
#mktIndex = (1:nrow(marketPrices))[(!is.na(marketPrices$avgprsqft) | !is.na(marketPrices$medprsqft)) &
#                                    (marketPrices$type == "condo" | marketPrices$type == "single_family")]
#marketPrices = marketPrices[mktIndex, ]

#Calculating the difference between the listing price and the median price
#index1 = (1:nrow(listingData))[!is.na(listingData$type) & !is.na(listingData$ldp_zip)]
#str1 = paste(listingData$type[index1], listingData$ldp_zip[index1], sep = "@")
#str2 = paste(marketPrices$type, marketPrices$zip, sep="@")
#index2 = match(str1, str2)
#priceSqftDiffMedian = rep(NA, nrow(listingData))
#priceSqftDiffMedian[index1] = listingData$price[index1]/listingData$sqft[index1] - marketPrices$medprsqft[index2]
#priceAboveMkt = ifelse(priceSqftDiffMedian > 0, 1, 0)

#The month of the listing
monthofListing = format(as.POSIXct(listingData$listing_date), "%b")

#Indicator variable for May,June,July listings
monMayJunJul = ifelse(monthofListing %in% c("May", "Jun", "Jul"), 1, 0)

#Price per square feet
psf = listingData$price/listingData$sqft

#Matching consumer confidence data with the listing data
monthYr = format(listingData$listing_date, format="%Y-%m")
index = match(monthYr, CCI$TIME)
consConf = CCI$Value[index]

weekYr = format(listingData$listing_date, format="%Y-%U")
index = match(weekYr, format(as.Date(BCCI$date, format="%m/%d/%Y"), "%Y-%U"))
bConsConf = BCCI$value[index]

#Duration in weeks
durationWks = ceiling(duration/7)

#Calculating slope of a fitted exponential model
viewsMat = cbind(listingData$ldp_views_day1, 
                 listingData$ldp_views_day2 - listingData$ldp_views_day1, 
                 listingData$ldp_views_day3 - listingData$ldp_views_day2,
                 listingData$ldp_views_day4 - listingData$ldp_views_day3,
                 listingData$ldp_views_day5 - listingData$ldp_views_day4,
                 listingData$ldp_views_day6 - listingData$ldp_views_day5,
                 listingData$ldp_views_day7 - listingData$ldp_views_day6)

calcValBeta = function(v) {
  return (cov(x=log(v), y=1:7)/var(1:7))
}
beta = apply(viewsMat, 1, calcValBeta)
beta[is.nan(beta)] = NA

#Creating the final dataset
#listingData = cbind(listingData, duration, censor, houseAge, priceSqftDiffMedian, priceAboveMkt, monthofListing, 
#                    monMayJunJul, psf, consConf, bConsConf, durationWks, beta)
listingData = cbind(listingData, duration, censor, houseAge, monthofListing, 
                    monMayJunJul, psf, consConf, bConsConf, durationWks, beta)
listingData[1:10,]

#write.csv(listingData, "C:\\Users\\Sujit\\Desktop\\Surival Analysis\\SF-listingData-Apr15-to-Oct25(cleaned).csv", row.names=FALSE)

#Fitting Cox PH Model
library(survival)
summary(coxph(Surv(duration, censor) ~ photo_count + sqft + ldp_views_day1 + beta + consConf, listingData))

########################################################
######         Training/Testing the model          #####
########################################################

#Removing rows with incomplete data
index = (1:nrow(listingData))[!is.na(listingData$photo_count) &
                              !is.na(listingData$sqft) &
                              !is.na(listingData$ldp_views_day1) &
                              !is.na(listingData$beta) &
                              !is.na(listingData$consConf)]
listingData = listingData[index,]

#Creating traing/test datasets
set.seed(150)
trainIndex = sample(1:nrow(listingData), 0.9*nrow(listingData))
testIndex = (1:nrow(listingData))[!(1:nrow(listingData) %in% trainIndex )]                
listingData.train = listingData[trainIndex,]
listingData.test = listingData[testIndex,]

#Fitting model to the training data
cph.train = coxph(Surv(durationWks, censor) ~ photo_count + sqft + ldp_views_day1 + beta + consConf, listingData.train)
summary(cph.train)

#Predicting values percentiles and IQR
nrowTest = nrow(listingData.test)
survMed = rep(NA, nrowTest)
surv25th = rep(NA, nrowTest)
surv75th = rep(NA, nrowTest)
IQR = rep(NA, nrowTest)

for (rowNum in 1:nrowTest) {
  pred = survfit(cph.train, newdata = listingData.test[rowNum,])
  cph.test.summary = summary(pred)
    
  survTimes = cph.test.summary$time
  survProbs = cph.test.summary$surv
    
  cdfProbs = 1-survProbs[1:(length(survProbs)-1)] + diff(-survProbs)
  
  survMed.curr = survTimes[which(survProbs[1:(length(survProbs)-1)] >= 0.5 & cdfProbs >= 0.5)]
  survMed[rowNum] = ifelse(length(survMed.curr) == 1, survMed.curr, NA)
  
  surv25th.curr = survTimes[which(survProbs[1:(length(survProbs)-1)] >= 0.75 & cdfProbs >= 0.25)]
  surv25th[rowNum] = ifelse(length(surv25th.curr) == 1, surv25th.curr, NA)
  
  surv75th.curr = survTimes[which(survProbs[1:(length(survProbs)-1)] >= 0.25 & cdfProbs >= 0.75)]
  surv75th[rowNum] = ifelse(length(surv75th.curr) == 1, surv75th.curr, NA)
  
  IQR[rowNum] = surv75th[rowNum] - surv25th[rowNum]
}

accuracyLevel = 0.3
correctPred = ifelse(abs((survMed - listingData.test$duration)/listingData.test$duration) <= accuracyLevel, 1, 0)
out = as.data.frame(cbind(listingData.test$duration, listingData.test$censor, survMed, surv25th, surv75th, IQR, correctPred))
colnames(out) = c("duration", "censor", "survMed", "Q1", "Q2", "IQR","correctPred")
out[1:10,]
mean(out$correctPred, na.rm=TRUE)

#Function to calculate F score
calcF = function(beta, recallRate, precRate) {
  Fvalue = (1 + beta^2)*(precRate*recallRate)/(beta^2*precRate + recallRate)
  return(Fvalue)
}

#Accuracy Measures
hotPropThres = 30
hotPredicted = ifelse(out$survMed <= hotPropThres, 1, 0)
hotActual = ifelse(out$duration <= hotPropThres, 1, 0)

hotPropIndex = (1:nrow(out))[!is.na(hotActual) & hotActual == 1]
hotPropIndexFound = (1:nrow(out))[!is.na(hotPredicted) & hotPredicted == 1]

truePos = length((1:length(hotPropIndex))[hotPropIndex %in% hotPropIndexFound])
falsePos = length((1:length(hotPropIndexFound))[!(hotPropIndexFound %in% hotPropIndex)])
trueNeg = nrow(out) - length(hotPropIndex) - falsePos
falseNeg = length(hotPropIndex) - truePos

TPR = truePos/length(hotPropIndex)
FPR = falsePos/(nrow(out) - length(hotPropIndex))
precRate = truePos/(truePos + falsePos)

betaForFvalue = 1
Fvalue = ifelse(truePos == 0 & falsePos == 0, NA,
                calcF(betaForFvalue, truePos/length(hotPropIndex), truePos/(truePos + falsePos)))

