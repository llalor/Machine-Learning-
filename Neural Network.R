rm(list = ls()) #cleaning the environment

if (!require('neuralnet')) install.packages('neuralnet'); library(neuralnet)
if (!require('caret')) install.packages('caret'); library(caret)
if (!require('e1071')) install.packages('e1071'); library(e1071)
if (!require('matrixStats')) install.packages('matrixStats'); library(matrixStats)
if (!require('boot')) install.packages('boot'); library(boot)
if (!require('plyr')) install.packages('plyr'); library(plyr)
setwd("~/R") #set working directory
data <- read.csv("cereals.csv")
fix(data)

##Defining Variables

calories = data$calories
protein = data$protein
fat = data$fat
sodium = data$sodium
fiber = data$fiber
rating = data$rating
data2 = cbind(calories, protein, fat,sodium, fiber, rating)
fix(data2)

## Scaling Variables
preprocess = preProcess(data, method = c("range"))
norm = predict(preprocess, data)
summary(norm)
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
fix(scaled)
head(scaled)

## Split data

## set the seed to make your partition reproducible
set.seed(1)
sample = sample.int(n = nrow(data), size = floor(.6*nrow(data)), replace = F)
train = data[sample, ]
test = data[-sample, ]

## Scaling Training Set 

trainpreprocess = preProcess(train, method = c("range"))
trainnorm = predict(trainpreprocess, train)
summary(trainnorm)

## Scaled Training Variables

traincalories = trainnorm$calories
trainprotein = trainnorm$protein
trainfat = trainnorm$fat
trainsodium = trainnorm$sodium
trainfiber = trainnorm$fiber
trainrating = trainnorm$rating
trainx = cbind(traincalories, trainprotein, trainfat, trainsodium, trainfiber)
trainscaled = data.frame(traincalories, trainprotein, trainfat, trainsodium, trainfiber,
                         trainrating)

## Scaling Testing Set

testpreprocess = preProcess(test, method = c("range"))
testnorm = predict(testpreprocess, test)
summary(testnorm)

## Scaled Testing Variables

testcalories = testnorm$calories
testprotein = testnorm$protein
testfat = testnorm$fat
testsodium = testnorm$sodium
testfiber = testnorm$fiber
testrating = testnorm$rating
testscaled = data.frame(testcalories,testprotein,testfat,testsodium,testfiber,testrating)

## Neural Network

set.seed(1)
n = neuralnet(trainrating ~ ., data=trainscaled, hidden=3,linear.output=FALSE)
plot(n)

## Prediciton

output = compute(n,testscaled[,-6])
probability = output$net.result

## Prediction Rescaled

predicttestNN = (probability * (max(data$rating) - min(data$rating))) + min(data$rating)
predictionNN = ifelse(predicttestNN>50,1,0)
actualNN = ifelse(test$rating>50,1,0)
actualNNfactor = as.factor(actualNN)
predictionNNfactor = as.factor(predictionNN)
confusionNN = confusionMatrix(actualNNfactor,predictionNNfactor)
confusionNN
MSENN = sum((predictionNN - actualNN)^2/nrow(test))
RMSENN = MSENN^0.5
RMSENN

## Plot of Prediction vs Actual

plot(test$rating, predicttestNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real
rating")
abline(0,1)
## K-Fold Cross Validation
# Initialize variables
set.seed(1)
k = 100
RMSE.NN = NULL
List = list( )

## Fit neural network model within nested for loop

for(j in 10:65){
  for (i in 1:k) {
    kdata = sample(1:nrow(data),j )
    
    trainNN = scaled[kdata,]
    testNN = scaled[-kdata,]
    datatest = data[-kdata,]
    
    NN = neuralnet(rating ~ ., trainNN, hidden = 3, linear.output= T)
    predicttestNN = compute(NN,testNN[,c(1:5)])
    predicttestNN = (predicttestNN$net.result*(max(data$rating)-
                                                 min(data$rating)))+min(data$rating)
    
    RMSE.NN [i]<- (sum((datatest$rating - predicttestNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}
Matrix.RMSE = do.call(cbind, List)
Matrix.RMSE

## Prepare boxplot
boxplot(Matrix.RMSE[,56], ylab = "RMSE", main = "RMSE BoxPlot (@ Traning Set =
65)")

## Variation of median RMSE 

medians = colMedians(Matrix.RMSE)
medians
X = seq(10,65)
plot (medians~X, type = "l", xlab = "Training Set Size", ylab = "Median RMSE",
      main = "Variation of RMSE w/ Respect to Training Set")

