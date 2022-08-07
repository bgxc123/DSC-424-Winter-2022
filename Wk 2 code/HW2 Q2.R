#importing inverse function ginv
library(MASS)
#creating/checking matrices
Z = matrix(c(1,1,1,1,2,-3,4,-1), ncol=2, byrow=F)
Z
Y = c(0,1,4,-3)
Y
#a.)
t(Z)

#b.)
t(Z) %*% Z

#c.)
ginv(t(Z)%*%Z)
#check without fraction multiplication
116* ginv(t(Z)%*%Z)

#d.)
t(Z)%*%Y

#e.)
ginv(t(Z)%*%Z)%*%t(Z)%*%Y

#f.)
det(t(Z)%*%Z)

#regression test
x = c(2,-3,4,-1)
y = c(0,1,4,-3)

df = data.frame(y,x)
df

fit = lm(y ~ x, data=df)
summary(fit)
