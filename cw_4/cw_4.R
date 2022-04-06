#zad 1
library("MASS")

dim(cats)
head(cats)

plot( cats$Bwt, cats$Hwt, pch=19, xlab="Btw (kg)", ylab="Hwt (g)", main="Wykres 1")

#zad 2
x<-cats$Bwt-mean(cats$Bwt)
y<-cats$Hwt-mean(cats$Hwt)
n<-dim(cats)
cov_<-sum(x*y)/(n[1]-1)
cov_

#zad 3
cov(cats$Bwt,cats$Hwt)
cor(cats$Bwt,cats$Hwt)

#zad 4
#współczynnik korelacji dodatni

#zad 5
cor.test(cats$Bwt,cats$Hwt)

#zad 6
cats.mod<-lm(cats$Hwt ~ cats$Bwt, data = cats)
summary(cats.mod)

#zad 7
library("dplyr")
library("ggpubr")

ggscatter(cats, x = "Bwt", y = "Hwt", cor.coef = TRUE, cor.method = "pearson", 
          add = "reg.line", add.params = list(color = "black", fill = "lightgray" ),
          conf.int = TRUE  )

