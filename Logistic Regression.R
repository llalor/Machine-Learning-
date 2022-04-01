rm(list = ls()) #cleaning the workspace

if (!require('ISLR')) install.packages('ISLR'); library('ISLR')
if (!require('class')) install.packages('class'); library('class')

## Install packages and Library's

install.packages("survival");library("survival")
install.packages("pscl");library("pscl")

library(stargazer)

# setwd("C:/Users/...") # set working directory
Stock.Market <- read.csv("Assignment 1 Data.csv")
Stock.Market1 <- read.csv("Assignment 1 Data sorted 2.csv")#Upload dataset
names(Stock.Market)
fix(Stock.Market)
View(Stock.Market)
fix(Stock.Market1)
str(Stock.Market)
Stock.Market1$SPDirection<-as.factor(Stock.Market1$SPDirection)
str(Stock.Market1)
# is.factor(Stock.Market$SPDirection)

Variables <- data.frame(Stock.Market$SPDirection, Stock.Market$UMCSI, Stock.Market$VIX, Stock.Market$PMI)
cor(Variables) # Pairwise correlations

summary(Stock.Market)
summary(Stock.Market1)

glm.fit=glm(SPDirection~UMCSI+VIX+PMI, data=Stock.Market1, family=binomial) # Perform logistic regression
summary(glm.fit)#Output of logistic regression
coef(glm.fit)# Coefficients of regression
summary(glm.fit)$coef
anova(glm.fit,test="Chisq")

## R-Squared and Adjusted R^2
pR2(glm.fit)
R2=0.1544667
N=240
K=3
AdjR2=1-(1-R2)[(1-N)/(N-K-1)]
AdjR2

glm.probs=predict(glm.fit,type="response") # Predicting probabilities
contrasts(Stock.Market1$SPDirection)
glm.pred=rep("Down",239)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Stock.Market1$SPDirection)
mean(glm.pred==Stock.Market1$SPDirection)

## Train data

fix(Stock.Market1) # Fix first column name
variables1<-data.frame(Stock.Market1$Year, Stock.Market1$SPDirection, Stock.Market1$UMCSI, 
                       Stock.Market1$VIX, Stock.Market1$PMI)
train=(Stock.Market1$Year<2013) # Training data every data point between 2000-2013
Stock.Market.2013=Stock.Market1[!train,] # Test data set 
dim(Stock.Market.2013)
SPDirection.2013=Stock.Market$SPDirection[!train]
fix(Stock.Market.2013)

## Use results from training set to make predictions on test set

glm.fit2=glm(SPDirection~UMCSI+VIX+PMI, data=Stock.Market1, family="binomial", subset=train) 
glm.probs=predict(glm.fit2,Stock.Market.2013,type="response")

glm.pred=rep("Down",84)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, SPDirection.2013)




