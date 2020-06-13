#R Project - Supervised Learning
library(dplyr)
library(data.table)
library(zoo)

setwd("C:/Users/ofera/Studies/R/Project/src")
data = fread('../Data/train_data.csv')

nObs = nrow(data)
colsToRmv = data[ ,colSums(is.na(data)) >= nObs/4]
data[ ,which(colsToRmv == TRUE)] = NULL

numeriCol <- which(sapply(data,is.numeric))
data = sapply(data[,..numeriCol], na.aggregate)
data = data.table(data)

ntrain = 1000000
ntest = 3000

trainIdx = sample(1:nrow(data),ntrain,replace = FALSE)
testIdx = sample(1:nrow(data),ntest,replace = FALSE)

trainData = data[trainIdx, ]
testData = data[testIdx, ]

lm1 = lm(temp~.,data = trainData)
firstPredict = predict(lm1)
trainErr = MSE(firstPredict - trainData$temp)
print(trainErr)

testErr = MSE(predict(lm1, newdata = testData ) - testData$temp)
print(testErr)


MSE = function(x){
  mean(x^2)
}

