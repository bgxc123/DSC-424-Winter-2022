#LDA exploratory analysis on timeframe
df = read.csv("NFLDataFinal1999-2019.csv")
head(df)
m = lm(pick ~ . - combine60ydShuttle - combineHand, data = df_nums)
summary(m)
df_nums = df[c(4:6,11,14:20,22)]
df_numsTime = df[c(4:6,11,14:20,22,27)]
head(df_numsTime)
#make the year periods a factor class
yrs = factor(df$X7YearPeriods, levels = c("1999-2005","2006-2012","2013-2019"))
df_numsTime$X7YearPeriods = yrs
#scatterplots
plot(df_numsTime, col = df_numsTime$X7YearPeriods)

#lda on time periods
head(df_numsTime)
library("MASS")
years.lda = lda(X7YearPeriods ~ ., data=df_numsTime)
print(years.lda)
print(years.lda$scaling[order(years.lda$scaling[, 1]), ])
print(years.lda$scaling[order(years.lda$scaling[, 2]), ])

#prediction
years.lda.values = predict(years.lda)
ldahist(data=years.lda.values$x[, 1], g=df_numsTime$X7YearPeriods)
ldahist(data=years.lda.values$x[, 2], g=df_numsTime$X7YearPeriods)

years.lda.values$x
plot(years.lda.values$x[, 1], years.lda.values$x[, 2], col=df_numsTime$X7YearPeriods, pch=16)

#lda college conference & pick/combine data
df_college = df[c(4:6,11,14:20,22,26)]
head(df_college)
conferences = factor(df$CollegeConference, levels = c("Non Power 5","SEC","Pac 12","Big 12","ACC","Big 10"))
df_college$CollegeConference = conferences

#sample, train/test
s = sample(nrow(df_college), nrow(df_college) * .75)
collegeTrain = df_college[s, ]
collegeTest = df_college[-s, ]
head(collegeTrain)

#lda
college.lda = lda(CollegeConference ~ ., data=collegeTrain)
print(college.lda)

print(college.lda$scaling[order(college.lda$scaling[, 1]), ])
print(college.lda$scaling[order(college.lda$scaling[, 2]), ])

#predict / check separation w graphs
college.lda.values = predict(college.lda, collegeTest)
par(mar=c(1,1,1,1))
ldahist(data=college.lda.values$x[, 1], g=collegeTest$CollegeConference)
college.lda.values$x
plot(college.lda.values$x[, 1], college.lda.values$x[, 2], col=collegeTest$CollegeConference, pch=16)

#confusion matrix
table(collegeTest$CollegeConference, college.lda.values$class)

#lda Power 5 vs non power 5
df_p5 = df[c(4:6,11,14:20,22,28)]
head(df_p5)
conferences = factor(df$Power.5.vs.Non.Power.5, levels = c("Non Power 5","Power 5"))
df_p5$Power.5.vs.Non.Power.5 = conferences

#sample, train/test
s = sample(nrow(df_p5), nrow(df_p5) * .75)
p5Train = df_p5[s, ]
p5Test = df_p5[-s, ]
head(p5Train)

#lda
p5.lda = lda(Power.5.vs.Non.Power.5 ~ ., data=p5Train)
print(p5.lda)

print(p5.lda$scaling[order(p5.lda$scaling[, 1]), ])

#predict / check separation w graphs
p5.lda.values = predict(p5.lda, p5Test)
par(mar=c(1,1,1,1))
ldahist(data=p5.lda.values$x[, 1], g=p5Test$Power.5.vs.Non.Power.5)
p5.lda.values$x
plot(p5.lda.values$x[, 1], col=p5Test$Power.5.vs.Non.Power.5, pch=16)

#confusion matrix
table(p5Test$Power.5.vs.Non.Power.5, p5.lda.values$class)

#logistic regression
df_logit = df[c(4:6,11,14:20,22,29)]
s = sample(nrow(df_logit),nrow(df_logit)*.75)
logitTrain = df_logit[s,]
logitTest = df_logit[-s,]
head(logitTrain)
model = glm(Test ~ . , family=binomial(link = 'logit'),data = logitTrain)
summary(model)
library(car)
pred = predict(model, newdata = logitTest, type = 'response')
print(pred)
pred = ifelse(pred > .5, 1, 0)
table(pred, logitTest$Test)

#MDS Attempt
summary(df_nums)
snums = scale(df_nums)
summary(snums)
d = dist(snums)
library("MASS")
model = isoMDS(d) #crashes RStudio

#PCA / dimensionality reduction to process data
p = prcomp(snums)
summary(p) #8 PCs to hit 95% cumulative proportion of variance
print(p)

plot(p)
abline(1,0, col="red") #3 PCs pass the var = 1 criteria

plot(p$x[,1:2]) #looks to be 3 clusters with some outliers
snumsReduced = p$x[,1:3]
d = dist(snumsReduced)
fit = cmdscale(d,eig=TRUE, k=2)
fit
xMDS = fit$points[,1]
yMDS = fit$points[,2]
plot(xMDS, yMDS, xlab="Coordinate 1", ylab= "Coordinate 2", 
     main="MDS of Draft/Combine Data",pch=16, cex=.5)

#cluster validation
install.packages("factoextra")
library(factoextra)
fviz_nbclust(snumsReduced, kmeans, method = "wss")+
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method") #giving 6 clusters
fviz_nbclust(snumsReduced, kmeans, method = "silhouette")+
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Silhouette method") #giving us 6 clusters as well

snumsClustk = kmeans(snumsReduced, centers=6)
plot(xMDS, yMDS, xlab="Coordinate 1", ylab= "Coordinate 2", 
     main="MDS of Draft/Combine Data",pch=16, cex=.5, col=snumsClustk$cluster)
library(scatterplot3d)
scatterplot3d(p$x[,1:3], color=snumsClustk$cluster)
library(car)
install.packages("rgl")
library("rgl")

scatter3d(x = p$x[,1], y = p$x[,2], z = p$x[,3],
          groups = as.factor(snumsClustk$cluster), grid = FALSE, surface = FALSE)
