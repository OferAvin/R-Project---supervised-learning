#R Project - Supervised Learning
library(dplyr)
library(data.table)
library(zoo)



#rm outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#MSE func
MSE = function(x){
  mean(x^2)
}

#Load data
setwd("C:/Users/ofera/Studies/R/Project/src")
data = fread('../Data/train_data.csv')

#turn to factor
noNumCol = which(sapply(data,is.numeric) == FALSE)
#data$inme = as.factor(data$inme)
data$city = as.factor(data$city)
data$prov = as.factor(data$prov)
data$yr = as.factor(data$yr)
data$mo = as.factor(data$mo)
data$hr = as.factor(data$hr)

#data = na.omit(data)
 
#remove to many na cols and unnecessary cols
nObs = nrow(data)
varToRmv = c("mdct", "date", "da","wsid","wsnm","inme")
data[ ,(varToRmv) := NULL]


data[data == 0] = NA
data$hr[is.na(data$hr)] = 0
colsToRmv = data[ ,colSums(is.na(data)) >= nObs/6]
data[ ,which(colsToRmv == TRUE)] = NULL



numeriCol <- which(sapply(data,is.numeric))
withOutL = as.data.table(lapply(data[,..numeriCol], remove_outliers))
rowsToRmv = which((rowSums(is.na(data)) >=6)==TRUE )
data = data[-rowsToRmv,]
rowsToRmv = which(is.na(data$temp)==TRUE)
data = data[-rowsToRmv,] 

#na omit
data  = na.omit(data)

#replace na by mean
#data[,numeriCol] = as.data.table(sapply(data[,..numeriCol], na.aggregate))



cities = (unique(as.factor(data$city)))
c1 = cities[1:52]
c2 = cities[53:102]

c1Obs = match(data$city,c1)
datac1 = (data[which(data$city %in% c1),])
datac2 = data[which(data$city %in% c2),]

datac1$city = droplevels(datac1$city)
datac2$city = droplevels(datac2$city)

#prepare train and test data
ntrain = 15000
ntest = 7000

#c1
trainIdxc1 = sample(1:nrow(datac1),ntrain,replace = FALSE)
testIdxc1 = sample(1:nrow(datac1),ntest,replace = FALSE)

trainDatac1 = datac1[trainIdxc1,  ]
y.testc1 = datac1$temp[testIdxc1]
testTDatac1 = datac1[testIdxc1, -"temp"]

#c2
trainIdxc2 = sample(1:nrow(datac2),ntrain,replace = FALSE)
testIdxc2 = sample(1:nrow(datac2),ntest,replace = FALSE)

trainDatac2 = datac2[trainIdxc2, ]
y.testc2 = datac2$temp[testIdxc2]
testTDatac2 = datac2[testIdxc2, -"temp"]

#no city
trainIdxc3 = sample(1:nrow(data),ntrain,replace = FALSE)
testIdxc3 = sample(1:nrow(data),ntest,replace = FALSE)

trainDatac3 = data[trainIdxc3, - "city" ]
y.testc3 = data$temp[testIdxc3]
testTDatac3 = data[testIdxc3, -"temp"]



library(caret)
library(randomForest)
#c1
minMSE1 = vector(length = 6)
rfModel = randomForest(temp~., data = trainDatac1, ntree=430,mtry = 13 ,proximity = TRUE)
minMSE1[i-9] = min(rfModel$mse)

MSE(predict(rfModel) - trainDatac1$temp)
MSE(predict(rfModel, newdata = testTDatac1) - y.testc1)

#c2
rfModel2 = randomForest(temp~., data = trainDatac2, ntree=460,mtry = 13,proximity = TRUE)
MSE(predict(rfModel2) - trainDatac2$temp)
MSE(predict(rfModel2, newdata = testTDatac2) - y.testc2)


#no city
rfModel3 = randomForest(temp~., data = trainDatac3,ntree=440 ,mtry = 13 ,proximity = TRUE)
MSE(predict(rfModel3) - trainDatac3$temp)
MSE(predict(rfModel3, newdata = testTDatac3) - y.testc3)



(levels(as.factor(testData$city)))
length(levels(as.factor(testNewC$city)))




##########_______________TEST______________#################
##########______TEST_____#################
testData = fread('../Data/test_for_prediction.csv')
nObs = nrow(testData)


testData$city = as.factor(testData$city)
testData$prov = as.factor(testData$prov)
testData$city = as.factor(testData$mo)
testData$prov = as.factor(testData$yr)
testData$city = as.factor(testData$hr)


varToRmv = c("mdct", "date", "da","wsid","wsnm","inme")
testData[testData == 0] = NA
testData$hr[is.na(testData$hr)] = 0

newC = which((testData$city %in% cities) == FALSE)

#replace na by mean
#numeriCol <- which(sapply(testData,is.numeric))
#testData[,numeriCol] = as.data.table(sapply(testData[,..numeriCol], na.aggregate))

#impute missing vals
library(Hmisc)
numeriCol <- which(sapply(testData,is.numeric))
for (i in 1:length(numeriCol)){
  testData[,numeriCol[i]] = with(testData, impute(testData[,numeriCol[i]], median))
  
  testData$impHmd = NULL
}

testc1 = (testData[which(testData$city %in% c1),])
testc2 = testData[which(testData$city %in% c2),]
testNewC = testData[newC,] 

testc1$city = droplevels(testc1$city)
testc2$city = droplevels(testc2$city)
testNewC$city = droplevels(testNewC$city) 


f = predict(rfModel, newdata = testc1 )
s = predict(rfModel2, newdata = testc2 )
t = predict(rfModel3, newdata = testNewC )


id = c(testc1$id, testc2$id, testNewC$id )
temp = c(f,s,t)

id = as.numeric(id)
 
toAssign = as.data.frame(id)
toAssign$temp = temp

toAssign = toAssign[order(toAssign$id) , ]

write.csv(toAssign, '../Data/toAssign2.csv', row.names=FALSE )

