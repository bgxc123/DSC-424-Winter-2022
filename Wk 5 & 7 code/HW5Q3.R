df = read.csv("StoresAndAges.csv")
head(df)
library(ca)
dfFreq = df[2:5]
rownames(dfFreq) = df$X
dfFreq
#a)
mosaicplot(dfFreq, shade=T, main="")

#b)
c = ca(dfFreq)
summary(c)
plot(c)
