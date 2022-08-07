library(foreign) #allows us to read spss files
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)

df = read.csv("wiscsem.csv")
head(df)
df = df[3:13]
head(df)
summary(df)

#corrplot and initial PCA
cor.df = cor(df)
library(corrplot)
corrplot(cor.df, method="ellipse", order="AOE")



#corr.test
library(psych)
dfCorrTest = corr.test(df, adjust="none")
round(dfCorrTest$p, 2)

#varimax rotation
p = principal(df, nfactors = 3, rot="varimax")
print(p$loadings, cutoff=.4,sort=T)
source("PCA_Plot.R")
PCA_Plot_Psyc(p)
summary(p$scores)


#sort pc1 & pc2
s = data.frame(p$scores)
s[order(s$RC1), 1:2]
s[order(s$RC2), 1:2]

#to save space on word doc
head(s[order(s$RC1), 1:2])
tail(s[order(s$RC1), 1:2])

head(s[order(s$RC2), 1:2])
tail(s[order(s$RC2), 1:2])

#common factor analysis
fit = factanal(df, 3)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)
