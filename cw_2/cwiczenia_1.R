#zad 1
A=(1:20)
B=(20:1)
C=A+B
C

#zad 2
D=seq(0,100,by=11)
D

#zad 3
rivers
suma<-sum(rivers, na.rm = FALSE)
suma
srednia<-mean(rivers, na.rm = FALSE)
srednia
mediana<-median(rivers, na.rm = FALSE)
mediana
wariancja<-var(rivers)
wariancja
odch_stand<-sd(rivers, na.rm = FALSE)
odch_stand
min<-min(rivers, na.rm=FALSE)
max<-max(rivers, na.rm=FALSE)

vector<-c(suma, srednia, mediana, wariancja, odch_stand, min, max)
vector

#zad 4

getwd()

histogram <- hist(rivers, breaks=20)

#zad 5
dane <- read.table(file = "gravity.txt")
dane
names(dane)<-c("X", "grav_modeled", "grav_measured")
dane

library(ggplot2)
ggplot(dane) + geom_point(aes(x=X, y=grav_measured),size=2, shape=23, color="blue")+geom_line(aes(x=X, y=grav_modeled), size=2, color="red")
+xlabel(X)+ylabel(grav_measured)

#zad 6
funkcja_1 =  function(x, y)
{
  return(x**y)
}

r<-funkcja_1(2,3)

#zad 7
vec_1 <- c("imie", "plec", "kierunek", "rok")
vec_2 <- c( "Kasia", "K", "GF", "3")
vec_3 <- c( "Ewa", "K", "GIN", "1")
vec_4 <- c( "Jan", "M", "INF", "2")
vec_5 <- c( "Piotr", "M", "GF", "4")

data_frame(vec_1, vec_2, vec_3, vec_4, vec_5)

#zad 8
diamonds
summary(diamonds)
colnames(diamonds)
rownames(diamonds)
length(diamonds)
dim(diamonds)

#zad 9
filter(diamonds, diamonds$carat > 0.8)

#zad 10
arrange(diamonds, carat)
