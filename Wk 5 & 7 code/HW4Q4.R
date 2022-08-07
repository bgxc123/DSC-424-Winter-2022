library("MASS")

#read in test/train datasets
bondTrain = read.csv("BondRatingTrain.csv")
head(bondTrain)
bondTrain = bondTrain[3:13]
plot(bondTrain, col=bondTrain$CODERTG)
head(bondTrain)
bondTest = read.csv("BondRatingTest.csv")
head(bondTest)

#lda analysis
bond.lda = lda(CODERTG ~ ., data=bondTrain)
print(bond.lda)

#prediction on training set / confusion matrix
bond.lda.values = predict(bond.lda,bondTrain)
par(mar=c(1,1,1,1))
ldahist(data=bond.lda.values$x[, 1], g=bondTrain$CODERTG)
table(bondTrain$CODERTG, bond.lda.values$class)

#prediction on test set / confusion matrix
bond.lda.values = predict(bond.lda,bondTest)
ldahist(data=bond.lda.values$x[, 1], g=bondTest$CODERTG)
table(bondTest$CODERTG, bond.lda.values$class)
