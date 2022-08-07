df = read.csv("Survey.csv")
head(df)

#a
library(corrplot)
c = cor(df, method="pearson")
corrplot(c, method="ellipse")

c2 = cor(df,method="spearman")
corrplot(c2, method="ellipse")

c3 = cor(df, method="kendall")
corrplot(c3, method="ellipse")

#b
KMO(df)

#c
p = prcomp(c2, scale=T)
summary(p)
print(p)
plot(p)
abline(1,0, col="red")

#d
library(psych)
p2 = principal(c2, nfactors=4)
summary(p2)
print(p2$loadings, cutoff=.4)

#e
install.packages("lavaan")
library(lavaan)

dfOrd.model = 'usability =~ Qu1 + Qu2 + Qu3 + Qu5
            image =~ Vis3 + Im1 + Im2 + Im3
            transparency =~ Qu5 + Vis1 + Vis2
            test =~ Qu4 + Vis3'
fit = cfa(dfOrd.model, data=df)
summary(fit, fit.measures=TRUE)

#f
install.packages("polycor")
library(polycor)

dfOrd = df
#convert variables to ordinal
dfOrd$Qu1 = factor(dfOrd$Qu1, levels = c(1,2,3,4,5), ordered = T)
dfOrd$Qu2 = factor(dfOrd$Qu2, levels = c(1,2,3,4,5), ordered = T)
dfOrd$Qu3 = factor(dfOrd$Qu3, levels = c(1,2,3,4,5), ordered = T)
dfOrd$Qu4 = factor(dfOrd$Qu4, levels = c(1,2,3,4,5), ordered = T)
dfOrd$Qu5 = factor(dfOrd$Qu5, levels = c(1,2,3,4,5), ordered = T)

dfOrd$Vis1 = factor(dfOrd$Vis1, levels = c(1,2,3,4,5), ordered = T)
dfOrd$Vis2 = factor(dfOrd$Vis2, levels = c(1,2,3,4,5), ordered = T)
dfOrd$Vis3 = factor(dfOrd$Vis3, levels = c(1,2,3,4,5), ordered = T)

dfOrd$Im1 = factor(dfOrd$Im1, levels = c(1,2,3,4,5), ordered = T)
dfOrd$Im2 = factor(dfOrd$Im2, levels = c(1,2,3,4,5), ordered = T)
dfOrd$Im3 = factor(dfOrd$Im3, levels = c(1,2,3,4,5), ordered = T)

head(dfOrd)

h = hetcor(dfOrd)
hCor = h$correlations
hCor

#polychoric factor analysis
p3 = princomp(covmat = hCor, cor=T)
summary(p3)
plot(p3)
abline(1,0, col="red")

#4 factors to choose
p4 = principal(hCor, nfactors = 4)
summary(p4)
print(p4$loadings, cutoff=.4)

#build model, test for RMSEA and chi-squared
dfOrd.model = 'quality =~ Qu1 + Qu2 + Qu3 + Qu5 + Vis3
            image =~ Im1 + Im2 + Im3
            transparency =~ Qu5 + Vis1 + Vis2
            usability =~ Qu4 + Vis3'
fit2 = cfa(dfOrd.model, data=dfOrd)
summary(fit2, fit2.measures=TRUE)
