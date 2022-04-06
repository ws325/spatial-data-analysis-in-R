library(sp) 
library("readxl")
library(spatstat)
library(dbscan)
library(tidyverse)
library(ggplot2)
library(raster)

####----------------------------------------------------------
#ndadanie wczytanym danym ukladu wspolrzednych ETRS 1989 Poland CS2000 Zone 7 - EPSG:2178
data <- read_excel("zestaw8.xlsx", col_types = c("numeric", "numeric"))
head(data)
dim(data)
coord <- SpatialPoints(cbind(data$Long, data$Lat), proj4string = CRS("+proj=longlat"))
coordUTM <- spTransform(coord, CRS("+init=epsg:2178"))
dataUTM <- data.frame(coordUTM)
head(dataUTM)
colnames(dataUTM) <- c("Lon", "Lat")
write.csv2(dataUTM, "data_out.csv", row.names = F)

#wczytanie danych po zmianie ukladu wspolrzednych oraz osiedla.shp
data <- read.csv2('data_out.csv')
head(data)
data_dist <- shapefile("osiedla.shp")

#stworzenie mapy Krakowa z podzialem na dzielnice
cra_map <- ggplot() + geom_polygon(data=data_dist, aes(x = long, y = lat, group = group), 
                      show.legend = FALSE, color = "black", fill = "white")  + coord_fixed()
cra_map

cra_map + geom_point(data=data, aes(x=Lon, y=Lat), alpha=0.4, size = 0.6, colour="red") +
  ggtitle("Mapa wykroczen - Krakow")

####----------------------------------------------------------
# DBSCAN - opiera sie na dwoch parametrach wejsciowych:
#  - epsilon (Eps) – promien sąaiedztwa
#  - minPts – minimalna liczba obserwacji potrzebna by wybrana obserwacja została uznana za punkt centralny danej grupy
#                   (punkt centralny również jest liczony).

# Dzialanie algorytmu:
#  - dla kazdej obderwacji znajdujemy jej sasiadow
#  - kazda obserwacja ktora ma co najmniej MinPts sasiadow w odleglosci mniejszej niz epsilon to punkt centralny
#  - wszystkie obserwacje spelniajace warunki z powyzszego punktu laczone sa w grupe
#  - obserwacje ktore znajduja sie w odleglosci epsilon, a nie sa punktami centralnymi, zostaja przylaczone do istniejacych grup
#  - obserwacje, które naleza do grup, lecz w ich zasięgu epsilon nie znajduje się żadna nowa obserwacja, nazywane są obserwacjami granicznymi danej grupy
#  - wszystkie obserwacje, które nie zostały przyłączone do żadnej z grup, stają się obserwacjami odstającymi.

# Zalety:
#  - odporny na wplyw obserwacji odstających
#  - dobrze radzi sobie z grupami o niewypukłym ksztalcie
#  - szybkie dzialanie i relatywnie dobre wyniki
#  - daje możliwosc definiowania wielu miar odległosci

# Wady:
#  - nie daje możliwosci definiowania a priori liczby segmentow – liczba segmentww zalezy od liczby obserwacji i dobranych parametrow
#  - dobor odpowiednich parametrow bywa dosyc problematyczny – optymalizacja bywa dluga i uciazliwa

# dla wartosci: eps = 5, minPts = 15
dbscan_1 <- dbscan(data, eps=5, minPts = 15)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = dbscan_1$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda DBSCAN, eps=5, minPts=15") + 
  labs(x = "Long", y = "Lat")

# dla wartosci: eps = 50, minPts = 15
dbscan_2 <- dbscan(data, eps = 50, minPts = 15)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = dbscan_2$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda DBSCAN, eps=50, minPts=15") + 
  labs(x = "Long", y = "Lat")

# dla wartosci: eps=30, minPts = 5
dbscan_3 <- dbscan(data, eps=150, minPts = 15)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = dbscan_3$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda DBSCAN, eps=150, minPts=15") + 
  labs(x = "Long", y = "Lat")

# Wnioski:
# Wraz ze zwiekszaniem wartosci promienia(epsilon) rosnie ilosc pojawiajacych sie klastrow na mapie,
# szczegolnie w centrum Krakowa, poniewaz tam zageszczenie punktow jest najwieksze

####----------------------------------------------------------
# HDBSCAN
# Rozszerza algorytm DBSCAN, przekształcając go w hierarchiczny algorytm grupowania,
# a następnie wykorzystuje technikę wyodrebniania płaskiego klastrowania w oparciu o stabilnosc klastrow.
# Przyjmuje jeden argument minPts - minimalna liczbe probek 

# Zalety:
#  - lepszy niz dbscan dla danych o roznej gestosci
#  - szybszy niz dbscan

# Wady:
#  - skomplikowanie algorytmu

# dla wartosci: minPts = 5
hdbscan_1 <- hdbscan(data, minPts = 5)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = hdbscan_1$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda HDBSCAN, minPts=5") + 
  labs(x = "Long", y = "Lat")

# dla wartosci: minPts = 10
hdbscan_2 <- hdbscan(data, minPts = 30)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = hdbscan_2$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda HDBSCAN, minPts=30") + 
  labs(x = "Long", y = "Lat")

# dla wartosci: minPts = 30
hdbscan_3 <- hdbscan(data, minPts = 150)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = hdbscan_3$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda HDBSCAN, minPts=150") + 
  labs(x = "Long", y = "Lat")

# dla wartosci: minPts = 300
hdbscan_4 <- hdbscan(data, minPts = 300)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = hdbscan_4$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda HDBSCAN, minPts=300") + 
  labs(x = "Long", y = "Lat")

# Wnioski:
# Wraz ze zwiekszaniem wartosci minPts maleje liczba kalstrow, poniewaz dla wartosci minPts=5
# tworzy sie kilka klastrow rozlozonych sotunkowo rownomiernie wokol centrum Krakowa, natomiast im wartosc
# minPts jest wieksza tym klastry mniejsze klastry sa bardziej skupione w centrum.

####----------------------------------------------------------
# OPTICS
# Podstawa dzialania algorytmu jest podobna do DBSCAN, jednek usuwa on powazna wade dzialania tego algorytmu - nie ma 
# problemow z rozpoznawaniem klastrow o bardzo roznej gestosci, rownoczesnie znacznie zmniejszana jest wrazlowosc na paramery 
# epsilon oraz minPts, ktore rowniez wystepuja.

# Zalety:
#  - brak koniecznosci nadawania z gory wielkosci promienia
#  - nie wymaga parametrow gestosci

# Wady:
#  - algorytm zle radzi sobie z wielowymiarowymi danymi
#  - tworzy tylko porzadek klastrowy

# dla wartosci: minPts = 15, eps_cl = 15
out_1 <- optics(data, minPts = 15)
optics_1 <- extractDBSCAN(out_1, eps_cl = 15)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = optics_1$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda OPTICS, minPts=15, eps_cl=15") + 
  labs(x = "Long", y = "Lat")

# dla wartosci: minPts = 15, eps_cl = 40
out_2 <- optics(data, minPts = 15)
optics_2 <- extractDBSCAN(out_2, eps_cl = 40)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = optics_2$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda OPTICS, minPts=15, eps_cl=40") + 
  labs(x = "Long", y = "Lat")

# dla wartosci: minPts = 15, eps_cl = 100
out_3 <- optics(data, minPts = 15)
optics_3 <- extractDBSCAN(out_3, eps_cl = 100)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = optics_3$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda OPTICS, minPts=15, eps_cl=100") + 
  labs(x = "Long", y = "Lat")

# dla wartosci: minPts = 15, eps_cl = 550
out_4 <- optics(data, minPts = 15)
optics_4 <- extractDBSCAN(out_4, eps_cl = 550)
cra_map + geom_point(data=data, aes(x=Lon, y=Lat, color = optics_4$cluster), alpha=0.8, size = 0.9) +
  scale_colour_viridis_c(option = "mako", name = "Intensywnosc wykroczen") + ggtitle("metoda OPTICS, minPts=15, eps_cl=550") + 
  labs(x = "Long", y = "Lat")

# Wnioski:
# Zmienialem jedynie wartosc dla epsilon -  minPts = 15 (stale), liczba klastrow rozlozonych wokol centrum miasta rosla,
# najmniej klastrow, skupinych w scislym centrum bylo dla najmniejszej wartosci epsilon=15. Im wieksza wartosc epsilon tym wielkosc klastrow rosla.
# Najwieksze klastery - w centrum Krakowa oraz w polnocno-wschodzniej czesci


