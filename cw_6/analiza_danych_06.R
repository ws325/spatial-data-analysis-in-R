library("spatstat")
#zad 1
dane<-murchison
dane

#zad 2
plot(dane)
#plot(dane$gold)
#plot(dane$faults, add=TRUE)
#plot(dane$greenstone, add=TRUE)

#opis
# gold - proces punktowy wystepowania zloz zlota
# faults - rozmieszczenie uskokow
# greenstone - obszary wystepowania wychodni zielencow

class(dane$gold) # point pattern dataset
class(dane$faults) # line segment pattern
class(dane$greenstone) # observation window

#zad 3
est_lambda<-summary(dane$gold)
est_lambda

#zad 4
plot(dane$gold)

#zad 5
plot(dane$gold, main="quadrat count")
plot(quadratcount(dane$gold, 5, 5), add=TRUE)

#zad 6
q_t<-quadrat.test(dane$gold, 5, 5)
plot(q_t, main="quadrat test")
#p<0.05 zatem mozemy odrzucic hipoteze zerowa

#zad 7
dane_density<-density(dane$gold)
dane_density
#plot(distmap(dane$gold))
plot(dane_density)

#zad 8
contour(dane_density)
persp(dane_density)

#zad 9
plot(dane$gold)
plot(dane$faults, add=TRUE)

#zad 10
dist<-distmap(dane$faults)
plot(dist)

#zad 12
plot(rhohat(dane$gold,dist), main="density graph")

#zad 13
dist_f<-distfun(dane$faults)

#zad 14
dist_f(400000,7000000)

#zad 15
plot(dist_f)

