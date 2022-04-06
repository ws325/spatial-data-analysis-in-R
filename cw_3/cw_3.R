
# a) hipoteza statystyczna - przypuszczenie dotyczące rozkładu populacji
#    hipoteza h0 to hipoteza którą aktuaknie sprawdzamy (decyzję podejmujemy na podstawie wyników próby losowej)
#    hipoteza h1 to hipoteza którą przyjmeimy po odrzuceniu hipotezy h0

# b) poziom istotności alfa - dopuszczalne ryzyko uznania prawidziwej hipotezy zerowej za fałszywą

# c) rozkład normalny - rozkład prawdopodobieństwa, którego wykres funkcji jest krzywą w krztałcie dzwonu

# d) dystrybuanta - funkcja jednoznacznie wyznaczająca rozkład prawdopodobieństwa

#zad. 1
vec_1 <- seq(-2,2,0.01)
plot(vec_1, dnorm(vec_1), xlab = "x", ylab = "Gestosc prawdopodobienstwa")

#zad. 2
plot(pnorm(vec_1), xlab = "x", ylab = "Dystrybuanta")

#zad. 3
library("sp")
data<-readRDS("ca_geo.rds", refhook = NULL)
class(data)
data_fra<-data.frame(data)
class(data_fra)

#zad. 4

data_fra["pH"]

#zakres wartości zmiennej
summary(data_fra$pH)

#srednia
mean(data_fra$pH, na.rm=TRUE)

#odchylenie standardowe
sd(data_fra$pH, na.rm=TRUE)

#rozstęp międzykwartylowy
IQR(data_fra$pH, na.rm=TRUE)

#zad. 5
histogram_1 <- hist(data_fra$pH, breaks=20)

#zad. 6
density(data_fra$pH, na.rm=TRUE)

#zad. 7
boxplot(data_fra$pH, data=data_fra, varwidth = TRUE)
title("Wykres dla zmiennej pH")

#zad. 8
stand<-scale(data_fra$pH)
histogram_2 <- hist(stand, breaks=20)

#zad. 9
shapiro.test(data_fra$pH)

#p<5%, zatem można odrzucić hipotezę zerową

