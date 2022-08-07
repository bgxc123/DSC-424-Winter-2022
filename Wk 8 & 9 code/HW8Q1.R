df = read.table("kellog.dat",header=TRUE, skip=2,row.names = 1)
head(df)

d = dist(df)
d

library("MASS")
model = isoMDS(d)
model
model$stress
model$points
x = model$points[,1]
y = model$points[,2]
plot(x,y, xlab = "Coordinate 1", ylab = "Coordinate 2", type="n")
text(x,y, labels = row.names(df), cex=.5)

#agglomerative hierarchical clustering
c = hclust(d)
plot(c, cex = 0.6, hang = -1)

#cutree
c = cutree(c, k = 4)
c
plot(model$points, col=c)

#interpret a dimension in MDS
print(model$points[order(model$points[, 1]), ])
print(model$points[order(model$points[, 2]), ])
