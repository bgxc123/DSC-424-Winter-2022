install.packages("corrplot")
install.packages("QuantPsyc")
install.packages("car")
install.packages("leaps")
install.packages("lm.beta")
install.packages("SciViews")


library(psych) #has a much better scatterplot function
library(corrplot) #A nice correlation matrix viz
library(car) # Misc statistical methods
library(QuantPsyc) #Misc statistical methods
library(leaps) #Gives us forward, backward, and stepwise
library(lm.beta) #Gives us standardized coefficients
library(SciViews) #for natural log transformation

data = read.csv("olympics.csv")

#Transforming medal/gender counts
data$Total.Medals = data$Gold.medals + data$Silver.medals + data$Bronze.medals
data$Per.Capita.GDP = data$X2011.GDP / data$X2010.population

head(data)
str(data)
summary(data)

#correlation plot
pairs.panels(data)

cor.data = cor(data)
corrplot(cor.data)

#female vs male - Total Medals
maleModel = lm(Total.Medals ~ X2011.GDP + X2010.population + Male.count, data=data)
summary(maleModel)
vif(maleModel)

femaleModel = lm(Total.Medals ~ X2011.GDP + X2010.population + Female.count, data=data)
summary(femaleModel)
vif(femaleModel)

#standardized coefficients
maleStdCoef = coef(lm.beta(maleModel))
maleStdCoef
femaleStdCoef = coef(lm.beta(femaleModel))
femaleStdCoef

#female vs male - Wealth
maleModel2 = lm(Per.Capita.GDP ~ Male.count, data=data)
summary(maleModel2)
vif(maleModel2)

femaleModel2 = lm(Per.Capita.GDP ~ Female.count, data=data)
summary(femaleModel2)
vif(femaleModel2)

#standardized coefficients
maleStdCoef2 = coef(lm.beta(maleModel2))
maleStdCoef2
femaleStdCoef2 = coef(lm.beta(femaleModel2))
femaleStdCoef2
