#Read in test/train
iTrain = read.csv("insurTrain.csv")
iTest = read.csv("insurTest.csv")

head(iTrain)

#OLS regression
fitOLS = lm(newpol ~ . - zipcode - fairpol, data=iTrain)
summary(fitOLS)

#RMSE Train
rmseOLSTrain = sqrt(mean(fitOLS$residuals^2))
rmseOLSTrain

#plotting residuals and predicted values
plot(fitOLS$residuals)
predictedVals = predict(fitOLS, iTest)
plot(iTest$newpol - predictedVals)

#predict test set
predictOLSTest = predict(fitOLS, iTest)
rmseOLSTest = sqrt(mean(iTest$newpol-predictOLSTest)^2)
rmseOLSTest

#ridge regression
xTrain = as.matrix(iTrain[,-13])
yTrain = as.matrix(iTrain[,13])

xTest = as.matrix(iTest[,-13])
yTest = as.matrix(iTest[,13])
