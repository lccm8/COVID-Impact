# Initialize environment and load packages
options(java.parameters = "-Xmx2048m")
options(stringsAsFactors = FALSE)
Sys.setlocale("LC_TIME","English_United States.1252")
options(scipen = 999)
rm(list=ls())    
library(plyr)    
library(dplyr)   
library(tidyr)   
library(purrr)  
library(magrittr)  
library(lubridate)   
library(readtext)
library(xlsx)      
library(openxlsx)
library(missForest)
library(xgboost)
library(plotly)
library(reshape2)
library(Hmisc)
library(vars)
library(urca)
library(xts)
require(TTR)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load and filter data
Data <- read.csv("owid-covid-data.csv")
StringencyData <- Data[!is.na(Data$stringency_index),]
Data <- Data[,-which(names(Data) == "stringency_index")]
StringencyData <- aggregate(StringencyData$stringency_index, by=list(StringencyData$location), FUN=mean)
colnames(StringencyData) <- c("location","stringency_index")
Data <- Data[which(Data$date == "2020-12-31"),]
Data <- Data[-which(Data$continent == ""),]
Data %<>% merge(StringencyData,by="location")

# Add IMF data

IMF <- openxlsx::readWorkbook("IMF.xlsx",sheet = 1)
Data <- merge(Data, IMF, by="location")
Data %>% names()

Features <- c("iso_code","location","total_cases_per_million","total_deaths_per_million","icu_patients_per_million",
              "hosp_patients_per_million","total_tests_per_thousand","positive_rate","total_vaccinations_per_hundred",
              "people_fully_vaccinated_per_hundred","population_density","median_age","gdp_per_capita",
              "extreme_poverty","cardiovasc_death_rate","stringency_index","diabetes_prevalence","female_smokers","male_smokers",
              "hospital_beds_per_thousand","life_expectancy","human_development_index","Fiscal.additional.subtotal",
              "Fiscal.health","Fiscal.non.health","Fiscal.accelerated.or.deferredrev","Liquidity.subtotal",
              "Liquidity.below.the.line","Liquidity.guarantees","Liquidity.quasi.fiscal")

#Data <- Data[,Features]

# Countries with less than 50% data
NoData <- apply(Data[,Features],1,function(x) sum(is.na(x))/length(Features))
Delete <- which(NoData > 0.5)
Data <- Data[-Delete,]

# Missing data (40% NA)
Missing <- apply(Data[,Features],2,function(x) sum(is.na(x))/nrow(Data))  
Delete <- which(Missing >= 0.4) # Delete those with more than 40% missing
Features <- Features[-Delete]
Data <- Data[,Features]

# Response variable
y <- openxlsx::readWorkbook("GDP growth.xlsx",sheet = 1)
colnames(y) <- c("location","y")
library(plotly)
library(rjson)

df <- read.csv("world_gdp_with_codes.csv")
colnames(df) <- c("location","GDP","code")
df <- merge(df,y,by="location")
df <- df[!is.na(df$y),]
fig <- plot_ly(df, type='choropleth', locations=df$code, z=df$y, text=df$location, colorscale="Purples",
               zmin = min(df$y),zmax = max(df$y))




Data <- merge(Data,y, by="location")



# Partition data
set.seed(123)
index <- sample(seq(nrow(Data)), round(nrow(Data)*0.8))
Train <- Data[index,-c(1,2)]
Test <- Data[-index,-c(1,2)]

# Imputation to check for correlation
impTrain <- missForest(as.matrix(Train))$ximp %>% as.data.frame()
FeatureComplete <- rbind(Test, impTrain)
impTest <- missForest(FeatureComplete)$ximp[1:nrow(Test), ]

# Correlation to y (no need for feature correlaion - xgboost)
cormat <- rcorr(as.matrix(impTrain))
Correlation <- melt(cormat$r)
Correlation <- Correlation[which(Correlation$Var1 == "y"),]
cor.pv <- melt(cormat$P)
Correlation <- merge(Correlation,cor.pv[which(cor.pv$Var1 == "y"),],by="Var2")
Correlation <- Correlation[,c(1,3,5)]
colnames(Correlation) <- c("Feature","Correlation","Pvalue")
Correlation <- Correlation[order(Correlation$Pvalue),]

# limits of correlation analysis: linear relationship only and also imputation to NAs

# Xgboost feature selection
GbModel <- xgboost(data = as.matrix(Train[,-c(ncol(Train))]), label = as.matrix(Train$y), nrounds = 5,
                   eta = 0.5, max_depth = 5)
Pred <- predict(GbModel,as.matrix(Test[-c(ncol(Test))]))
VarImp <- xgb.importance(feature_names = colnames(Train[,-ncol(Train)]), model = GbModel)

sqrt(mean((Test$y-Pred)^2))


# Compare features
FeaturesImp <- Correlation[-which(Correlation$Feature == "y"),]
FeaturesImp$NAs <- NA
FeaturesImp$VarImp <- NA
FeaturesImp$VarImpAboveMean <- NA

for(i in seq(nrow(FeaturesImp))) {
  FeaturesImp$NAs[i] <- Missing[which(names(Missing) == FeaturesImp$Feature[i])] %>% round(2)
  FeaturesImp$VarImp[i] <- VarImp$Gain[which(VarImp$Feature == FeaturesImp$Feature[i])] %>% round(2)
  FeaturesImp$VarImpAboveMean[i] <- FeaturesImp$VarImp[i] >= mean(VarImp$Gain)
}

# Clean those below average in xgboost or pval < 0.1 in cor.test
FeaturesImp <- FeaturesImp[which(FeaturesImp$Pvalue <= 0.1 | FeaturesImp$VarImpAboveMean),]
newTrain <- impTrain[,which(names(impTrain) %in% c(as.character(FeaturesImp$Feature),"y"))]
newTest <- impTest[,which(names(impTest) %in% c(as.character(FeaturesImp$Feature),"y"))]

# Correlation
source("corstars.R")
FeatCorrelation <- corstars(newTrain[,-ncol(newTrain)])

# PCA
PCA_feat <- c("median_age","extreme_poverty","life_expectancy","human_development_index",
              "gdp_per_capita")
PCA <- prcomp(newTrain[,which(names(newTrain) %in% PCA_feat)])
summary(PCA)
Demographics <- PCA$x[,1]
newTrain <- cbind(newTrain[,-which(names(newTrain) %in% PCA_feat)],Demographics) 
PCA_test <- predict(PCA, newdata = newTest[,which(names(newTest) %in% PCA_feat)])
Demographics <- PCA_test[,1]
newTest <- cbind(newTest[,-which(names(newTest) %in% PCA_feat)],Demographics) 
# COVID cases and deaths
PCA_feat <- c("total_cases_per_million","total_deaths_per_million")
PCA <- prcomp(newTrain[,which(names(newTrain) %in% PCA_feat)])
summary(PCA)
Cases_and_deaths <- PCA$x[,1]
newTrain <- cbind(newTrain[,-which(names(newTrain) %in% PCA_feat)],Cases_and_deaths)
PCA_test <- predict(PCA, newdata = newTest[,which(names(newTest) %in% PCA_feat)])
Cases_and_deaths <- PCA_test[,1]
newTest <- cbind(newTest[,-which(names(newTest) %in% PCA_feat)],Cases_and_deaths) 
# Stimulus
PCA_feat <- c("Liquidity.subtotal","Fiscal.additional.subtotal","Fiscal.non.health","Fiscal.health")
PCA <- prcomp(newTrain[,which(names(newTrain) %in% PCA_feat)])
summary(PCA)
Stimulus <- PCA$x[,1]
newTrain <- cbind(newTrain[,-which(names(newTrain) %in% PCA_feat)],Stimulus)
PCA_test <- predict(PCA, newdata = newTest[,which(names(newTest) %in% PCA_feat)])
Stimulus <- PCA_test[,1]
newTest <- cbind(newTest[,-which(names(newTest) %in% PCA_feat)],Stimulus)
# Smokers
PCA_feat <- c("female_smokers","male_smokers")
PCA <- prcomp(newTrain[,which(names(newTrain) %in% PCA_feat)])
summary(PCA)
Smokers <- PCA$x[,1]
newTrain <- cbind(newTrain[,-which(names(newTrain) %in% PCA_feat)],Smokers)
PCA_test <- predict(PCA, newdata = newTest[,which(names(newTest) %in% PCA_feat)])
Smokers <- PCA_test[,1]
newTest <- cbind(newTest[,-which(names(newTest) %in% PCA_feat)],Smokers)

# Fit linear model
LinearModel <- lm(y ~ ., data = newTrain)
summary(LinearModel)
Pred <- predict(LinearModel, newTest)
sqrt(mean((newTest$y-Pred)^2))


newTrain <- newTrain[,-which(names(newTrain) == "Fiscal.health")]  


## VAR VECM on vaccination


# Load and filter data
Data <- read.csv("owid-covid-data.csv")
Data <- Data[which(Data$location == "Israel"),]
names(Data)
Keep <- c("date","total_vaccinations","people_vaccinated","people_fully_vaccinated","new_vaccinations",
          "new_vaccinations_smoothed","total_vaccinations_per_hundred","people_vaccinated_per_hundred",
          "people_fully_vaccinated_per_hundred","stringency_index")
for(i in 2:nrow(Data)) {
  if(is.na(Data$stringency_index[i])) Data$stringency_index[i] <- Data$stringency_index[i-1]
}
stringency_index_smoothed <- TTR::SMA(Data$stringency_index,n=7)
Data$stringency_index <- stringency_index_smoothed
Data <- Data[!is.na(Data$total_vaccinations),Keep]
Date <- Data$date %>% as.Date() %>% as.POSIXct()
Data <- Data[,-1] %<>% as.xts(order.by = Date)
  
Cointegrated <- matrix(ncol = 2) %>% as.data.frame()
colnames(Cointegrated) <- c("Var1","Var2")
a2 <- 1/Data$stringency_index

t = 1
names(Data[,t])
set.seed(123)

a1 <- Data[,t]
# Treat NA
if(is.na(a1[1])) a1[1] <- 0
for(i in 2:nrow(Data)) {
 if(is.na(a1[i])) a1[i] <- a1[i-1]
}

TotalVaccination <- as.ts(a1)
InverseStringencyIndex <- as.ts(a2)
CompareTimeSeries <- ts.intersect(TotalVaccination,InverseStringencyIndex)
plot.ts(CompareTimeSeries)

VaccinationTransformed <- diff(as.numeric(log(a1)))
StringencyTransformed <- diff(as.numeric(log(a2)))

ccf(VaccinationTransformed,StringencyTransformed,lag.max = 10)

ccf(diff(as.numeric(a1)),diff(as.numeric(a2)),lag.max = 10)


# Select lag 
idx <- ts.intersect(diff(as.ts(a1)),diff(as.ts(a2)))
BestLag <- NULL
for(a in 1:10) {
   vModel <- VARselect(idx, lag.max=10, type="const")
   BestLag %<>% c(as.numeric(vModel$selection))
}
BestLag <- table(BestLag)
    
LagNames <- BestLag %>% names() %>% as.numeric()
    
ifelse(LagNames[as.numeric(which(BestLag == max(BestLag))[1])] == 1, BestLag <- LagNames[as.numeric(which(BestLag == max(BestLag))[2])],
       BestLag <- LagNames[as.numeric(which(BestLag == max(BestLag))[1])])

varX <- VAR(idx, p = BestLag, type = "const")
      
# Cointegration
      
# Johansen Test
# https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R/
      
JTest <- try(ca.jo(log(Cross.ts),type="trace", K=8, ecdet="none", spec="longrun"),silent = TRUE)
sObject <- summary(JTest)
sObject@teststat[2] >= sObject@cval[2,2]





