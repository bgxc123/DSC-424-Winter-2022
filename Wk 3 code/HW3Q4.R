library(corrplot)
library(psych)
#a.) conduct a pca using cov matrix
df = read.csv("Census2.csv")
p = prcomp(df)
print(p)
summary(p)
plot(p)

#b.) transform median home value
df$MedianHomeVal = df$MedianHomeVal / 100000
head(df)

p2 = prcomp(df)
print(p2)
summary(p2)
plot(p2)

#c.) do any other fields need scaling?
summary(df)

#d.) correlation matrix pca
p3 = prcomp(df,scale=TRUE)
print(p3)
summary(p3)
plot(p3)
head(p$x)

#e.) check corr matrix at 95% conf level
dfCorrTest = corr.test(df[1:4], adjust="none")
round(dfCorrTest$p, 2)
