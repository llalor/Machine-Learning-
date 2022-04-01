rm(list = ls()) #cleaning the workspace

if (!require('randomForest')) install.packages('randomForest'); library('randomForest')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('groupdata2')) install.packages('groupdata2'); library('groupdata2')


# setwd("C:/Users/...") # set working directory
CensusData <- read.csv("Census.csv", header=TRUE)
attach(CensusData)
names(CensusData)
fix(CensusData)

# Clean and sort data
CensusData$workclass<-as.factor(CensusData$workclass)
CensusData$education<-as.factor(CensusData$education)
CensusData$maritalstatus<-as.factor(CensusData$maritalstatus)
CensusData$occupation<-as.factor(CensusData$occupation)
CensusData$relationship<-as.factor(CensusData$relationship)
CensusData$race<-as.factor(CensusData$race)
CensusData$sex<-as.factor(CensusData$sex)
CensusData$nativecountry<-as.factor(CensusData$nativecountry)
CensusData$over50k<-as.factor(CensusData$over50k)
head(CensusData)
str(CensusData)
CensusData[CensusData == " ?"] <- NA

# Randomly Downsample data

set.seed(1)
trainSmall = CensusData[sample(nrow(CensusData), "2000"), ]
fix(trainSmall)

# Split data into training and testing sets
train <- sample(nrow(trainSmall), 0.5*nrow(trainSmall), replace = FALSE)
TrainSet <- trainSmall[train,]
TestSet <- trainSmall[-train,]
summary(TrainSet)
str(TrainSet)
summary(TestSet)
str(TestSet)

## Random forest
#Training Set

fit_rf<-randomForest(over50k~.,
                     data=trainSmall,
                     importance=TRUE,
                     proximity=TRUE,
                     na.action=na.roughfix)
bag.census <- randomForest(over50k~., data=TrainSet, importance=TRUE,  na.action = na.roughfix)
bag.census

#Testing Set 
bag.census2 <- randomForest(over50k~., data=TestSet, ntree=500, mtry=12,
                            importance=TRUE, na.action = na.roughfix)
bag.census2

# Predicting on training set
predTrain <- predict(bag.census, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$over50k)  

# Predicting on Testing set
predTest <- predict(bag.census, TestSet, type = "class")
# Checking classification accuracy
table(predTest,TestSet$over50k)

# To check important variables
importance(bag.census)
varImpPlot(bag.census)
importance(bag.census2)        
varImpPlot(bag.census2) 

## logistic regression
glm.fit=glm(over50k~.,data=TrainSet, family=binomial, na.action = na.roughfix)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
glm.probs=predict(glm.fit,data=TestSet,type="response") #Predictions on Testing set

# set threshold 0.5
contrasts(TestSet$over50k)
glm.pred=rep("<=50k",1000)
glm.pred[glm.probs>.5]=">50k"
table(glm.pred,TestSet$over50k)




