library(spatstat)
library(dbscan)

#1
data<-lansing
plot(split(data), pch = 19, cex = 0.5)
summary(data)
#dane zawieraja informacje na temat lokalizacji oraz gatunkow drzew

#2
data_s<-split(data)
blackoak<-data_s$blackoak
maple<-data_s$maple

plot(density(data_s$blackoak))
plot(blackoak_data, add=TRUE, pch=19)

plot(density(data_s$maple))
plot(data_s$maple, add=TRUE, pch=19)

#3
ppm(blackoak)
ppm(maple)

#4
ks.test(blackoak$x, pnorm)
ks.test(blackoak$y, pnorm)

ks.test(maple$x, pnorm)
ks.test(maple$y, pnorm)

#5
blackoak_ppm<-ppm(blackoak~x + y)
blackoak_ppm
class(blackoak_ppm)
plot(blackoak_ppm, pch=19)

#6
cdf.test(blackoak_ppm,"x")
cdf.test(blackoak_ppm,"y")

#7
set.seed(100)
proccess=rmh(blackoak_ppm)
ppm(proccess)

#8
diagnose.ppm(blackoak_ppm)
