Z = matrix(c(1,4,1,3,1,2,1,-5), nrow=4, byrow=T)
Z
Y = t(Z)
Y %*% Z

#a.)
v = c(-1,1,3)
w = c(2,-1,1)

v %*% w

#b.)
-3 %*% w

#c.)
M = matrix(c(20,5,0,5,25,-10,0,10,5), nrow=3, byrow=T)
M
M %*% v

#d.)
N = matrix(c(-20,0,10,5,10,15,5,20,-5), nrow=3, byrow=T)
N
M + N

#e.)
M - N

#f.)
Z = matrix(c(1,4,1,3,1,2,1,-5), nrow=4, byrow=T)
t(Z)

#g.)
t(Z) %*% Z
