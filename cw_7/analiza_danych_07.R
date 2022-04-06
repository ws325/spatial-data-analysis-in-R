library("ggplot2")
library("spatstat")

#zad 1

#a
p_poi<-rpoispp(1, lmax=NULL, win=disc(10))
plot(p_pois)
# lamda=1

#b
p_reg<-rStrauss(1, gamma = 0.4, W=owin(disc(10)))
plot(p_reg)
?rStrauss
# beta=1, gamma=0.4
# gamma<=1

#wartosci jakie mogą przyjmowac argumenty tej funkcji:
#beta	- liczba pozytywna
#gamma - liczba od 0 do 1 wlacznie
#R - liczba nieujemna 
#W - obiekt klasy "owin"
#expand	- wartosc logiczna
#nsim	- liczba 
#drop	- wartość logiczna

#c
p_clu<-rThomas(kappa = 0.1, scale = 1, mu=7, win = disc(10))
# kappa = 0.1, scale = 1, mu = 7
# prodces ten polega na generowaniu punktow rodzicow, a nastepnie w okreslonym obszarze
# od nich, generowaniu punktow potomnych, co pozwala na tworzenie zbiorow o rozkladzie pogrupowanym

plot(p_poi, main = "Rozklad niezalezny")
plot(p_reg, main = "Rozklad regularny")
plot(p_clu, main = "Rozklad pogrupowany")

#zad 2
hist(nndist(p_pois))
hist(nndist(p_reg))
hist(nndist(p_clu))

#zad 3
g_poi<-Gest(p_poi)
g_reg<-Gest(p_reg)
g_clu<-Gest(p_clu)

#zad 4
plot(g_poi)
plot(g_reg)
plot(g_clu)
# krzywe na wykr. przedstawiaja prawdopodobienstwo ze punkt ma najblizszego sasiada w zaleznosci od r

#zad 5
plot(Kest(p_poi, correction="border"), .~r)
plot(Kest(p_reg, correction="border"), .~r)
plot(Kest(p_clu, correction="border"), .~r)

#zad 6
plot(distmap(p_poi))
plot(distmap(p_reg))
plot(distmap(p_clu))


     