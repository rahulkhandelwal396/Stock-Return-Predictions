Stocks = read.csv("StocksCluster.csv")
set.seed(144)
install.packages("caTools")
library(caTools)
Spl = sample.split(Stocks$PositiveDec, SplitRatio = 0.70)

StocksTrain = subset(Stocks, Spl == TRUE)
StocksTest = subset(Stocks, Spl == FALSE)

#Initial Logistic Regression Model
StocksModel = glm(ReturnDec ~ ., data = StocksTrain, family=binomial)
PredictTrain = predict(StocksModel, type="response")
table(Stocks$ReturnDec, PredictTrain > 0.5)
#0.571
PredictTest = predict(StocksModel, newdata=StocksTest, type="response")
table(Stocks$ReturnDec, PredictTest > 0.5)
#56.7

#Baseline Accuracy
table(StocksTest$ReturnDec)
#Use the most frequent outcome and calculate its percentage

LimitedTrain = StocksTrain
LimitedTrain$ReturnDec = NULL
LimitedTest = StocksTest
LimitedTest$ReturnDec = NULL

#Normalizing the data using caret package
install.packages("caret")
library(caret)
preproc = preProcess(LimitedTrain)
normTrain = predict(preproc, LimitedTrain)
normTest = predict(preproc, LimitedTest)

#Applying the clustering algorithm:
set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)

#Converting clusters into kcca object to use cluster-then-predict method
install.packages("flexclust")
library(flexclust)

Km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest)

StocksTrain1 = subset(StocksTrain, clusterTrain == 1)
StocksTrain2 = subset(StocksTrain, clusterTrain == 2)
StocksTrain3 = subset(StocksTrain, clusterTrain == 3)

StocksTest1 = subset(StocksTest, clusterTest == 1)
StocksTest2 = subset(StocksTest, clusterTest == 2)
StocksTest3 = subset(StocksTest, clusterTest == 3)

mean(StocksTrain1$ReturnDec)
mean(StocksTrain2$ReturnDec)
mean(StocksTrain3$ReturnDec)

StocksModel1 = glm(PositiveDec ~ ., data= StocksTrain1, family=binomial)
StocksModel2 = glm(PositiveDec ~ ., data= StocksTrain2 family=binomial)
StocksModel3 = glm(PositiveDec ~ ., data= StocksTrain3, family=binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, newdata=StocksTest1, type="response")
PredictTest2 = predict(StocksModel1, newdata=StocksTest2, type="response")
PredictTest3 = predict(StocksModel1, newdata=StocksTest3, type="response")

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
#0.62

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
#0.55

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
#0.646

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcome = c(StocksTest1$ReturnDec, StocksTest2$ReturnDec, StocksTest3$ReturnDec)
table(AllOutcomes, AllPredictions > 0.5)
