#R Project - Supervised Learning
library(data.table)
data = fread('../Data/train_data.csv')
data = na.omit(data)
ntrain = 10000
ntest = 3000
trainIdx = sample(1:nrow(data),ntrain,replace = FALSE)
testIdx = sample(1:nrow(data),ntest,replace = FALSE)
trainData = data[trainIdx,]
lm1 = lm(temp~.,data = trainData)
firstPredict = predict(lm1, newdata = na.omit(trainData))
MSE = function(x){
  mean(x^2)
}
error = MSE(firstPredict - trainData$temp[trainIdx])
head(firstPredict)
