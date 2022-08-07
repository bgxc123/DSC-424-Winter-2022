df = read.table("Employment.txt",header=T, sep="\t")
#is data appropriate for scaling?
head(df)
summary(df)

#prcomp with scaling
p = prcomp(df[-1], scale = TRUE)
summary(p)
print(p)
plot(p)
abline(h=1, col="red")
head(p$x)



#parallel analysis
m = matrix(rnorm(9*26, 0, 1), ncol=9)
rDS = data.frame(m)
head(rDS)
pRand = prcomp(rDS,scale=T)
par(mfrow=c(1,2))
plot(pRand, ylim=c(0,3.5))
abline(1,0,col="red")

par(mfrow=c(1,1))
parallel_PFA = fa.parallel(df[-1], n.iter=1000)

#principal analysis
library(psych)
p2 = principal(df[,-1], nfactors=2, rot="varimax")
print(p2$loadings, cutoff=.4,sort=T)
p2$loadings
p2$values

#comparing low/high scores
s = p2$scores
PFA = data.frame(df[1],s)
PFA

#looking at the loadings matrix in e)

#1 more/less factor for PCA
p3 = principal(df[,-1], nfactors=3, rot="varimax")
print(p3$loadings, cutoff=.4,sort=T)

p4 = principal(df[,-1], nfactors=1, rot="varimax")
print(p4$loadings, cutoff=.4,sort=T)
