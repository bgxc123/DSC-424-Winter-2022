#read in files
hTrain = read.csv("housingTrain.csv")
hTest = read.csv("housingTest.csv")

#linear regression Training set
fitOLS = lm(MEDV ~ ., data=hTrain)
summary(fitOLS)

#RMSE
rmseOLSTrain = sqrt(mean(fitOLS$residuals^2))
rmseOLSTrain

#Predict Y values in test
predictOLS = predict(fitOLS,hTest)
rmseOLSTest = sqrt(mean(hTest$MEDV-predictOLS)^2)
rmseOLSTest
summary(hTrain)

#CV ridge regression
install.packages("glmnet")
install.packages("Rcpp")
library(glmnet)

#create matrices for setups
xTrain = as.matrix(hTrain[,-13])
yTrain = as.matrix(hTrain[,13])

xTest = as.matrix(hTest[,-13])
yTest = as.matrix(hTest[,13])

fitRidge = cv.glmnet(xTrain,yTrain, alpha=0, nfolds=7)
fitRidge$lambda.min
fitRidge$lambda.1se
fitRidge
plot(fitRidge)

#Ridge prediction
ridgePred = predict(fitRidge, xTrain, s="lambda.1se")
rmseRidgeTrain = sqrt(mean((ridgePred - yTrain)^2))
rmseRidgeTrain
rmseOLSTrain
ridgePredT = predict(fitRidge,xTest, s="lambda.1se")
rmseRidgeTest = sqrt(mean((ridgePredT - yTest)^2))
rmseRidgeTest
rmseOLSTest
#R^2 => Training Set
summary(olsFit)
fitRidge2 = glmnet(xTrain, yTrain, alpha=0, lambda=4.041337)
fitRidge2

#Lasso Regression
fitLasso = cv.glmnet(xTrain, yTrain, alpha=1, nfolds=7)
fitLasso$lambda.min
fitLasso$lambda.1se

plot(fitLasso)

pLassoTrain = predict(fitLasso, newx=xTrain, s="lambda.1se")
rmseLassoTrain = sqrt(mean((yTrain - pLassoTrain)^2))
rmseLassoTrain
rmseOLSTrain

pLassoTest = predict(fitLasso, newx=xTest, s="lambda.1se")
rmseLassoTest = sqrt(mean((yTest - pLassoTest)^2))
rmseLassoTest
rmseOLSTest

#R^2 => Training Set
summary(fitOLS)
fitRidge2 = glmnet(xTrain, yTrain, alpha=1, lambda=0.4233768)
fitRidge2

