library("ggplot2")
library("spatstat")

#zad 1
#points<-runifpoint(n=200, win=owin(c(0,1), c(0,2)))
x<-runif(n=200, min=0, max=1)
y<-runif(n=200, min=0, max=2)
points_1<-data.frame(x,y)

#zad 2
ggplot(data=points_1,aes(x,y))+geom_point()+ggtitle('points_1')

#zad 3
ggplot(data=points_1,aes(x,y))+geom_point()+coord_fixed()+ggtitle('points_2')

#zad 4
r_squared<-runif(n=300, min=0, max=100)
angle<-runif(300, min=0, max=2*pi)
x_2<-sqrt(r_squared)*cos(angle)
y_2<-sqrt(r_squared)*sin(angle)
points_2<-data.frame(x_2,y_2)
colnames(points_2)<-c("x","y")

#zad 5
plot(disc(radius=10))
points(points_2$x,points_2$y)

#zad 6
planar_point_pattern<-ppp(points_2$x,points_2$y,window = disc(radius=10))
quadrat_t<-quadrat.test(planar_point_pattern)
quadrat_t #wartosc p > 0.05 zatem nie mozna odrzucic hipotezy zerowej
plot(quadrat_t)

#zad 7
rpoispp(lambda=500/area(disc(radius=10)),win=disc(radius=10))
#501 punktow