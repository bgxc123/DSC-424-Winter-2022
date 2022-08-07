df = read.csv("data_marsh_cleaned.csv")
head(df)
library("CCA")

water = df[2:6]
head(water)
soil = df[7:9]
head(soil)
ccdf = cc(soil,water)
ccdf
ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}
wilksSoil_Water = ccaWilks(soil, water, ccdf)
round(wilksSoil_Water[2,], 2)

c = matcor(soil, water)
img.matcor(c, type = 2)

c = cancor(soil, water)
c
