head(mtcars)
library(MASS)

#a.)
s = c("cyl","disp","hp","wt","carb")
A = as.data.frame(mtcars[,s])
head(A)

#b.) 
dim(A)
A = cbind(rep(1,nrow(A)),A)
names(A)[1] = "Count"
head(A)

#c.)
A = as.matrix(A)
head(A)

#d.)
Y = mtcars$mpg
ginv(t(A)%*%A) %*%t(A)%*%Y

#e.)
fit = lm(mpg ~ cyl + disp + hp + wt + carb, data=mtcars)
summary(fit)x
