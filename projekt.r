library(e1071)
koszty = c(449.6, 315.1, 447.2, 258.3, 391.3, 297.2, 295.3, 303.4, 312.3, 369.3, 385.8, 466, 302.1, 304.3, 359.5, 309.2, 371.7, 504.6, 414.5, 321.9, 386.2, 489, 514.6, 249.8, 184.7, 452.8, 513.2, 540.4, 458.1, 459.4, 259.5, 296.4, 397.1, 525.7, 331.3, 232.4, 222.4, 418.9, 265.5, 282.8, 543.6, 387.8, 343.1, 351.6, 440.9, 403.3, 370.2, 207.6, 223.3, 304.3, 283.9)
obroty = c(575.6, 250.8, 271.8, 337, 198.6, 361.7, 361.5, 447.2, 345, 455.9, 393.1, 368.3, 400, 449.7, 252.7, 437, 290.1, 297.7, 525.5, 411, 419.4, 530, 409.6, 474, 303.7, 246.8, 458.4, 480.3, 199.6, 461.9, 321.5, 305.7, 379.8, 493.2, 294, 322.9, 368.5, 262.4, 494.9, 446.7, 322.8, 475.8, 320, 252.6, 353.5, 303.3, 396.6, 255.6, 540.6)

kosztyDl <- length(koszty)
obrotyDl <- length(obroty)
#Zadanie 1
#analiza struktury rocznych kosztów
koszty <- sort(koszty)
obroty <- sort(obroty)

kosztySr <- mean(koszty)
obrotySr <- mean(obroty)

#dominant <- (koszt)
#dominanta <- (obroty)

# summary(koszty)
# summary(obroty)

print("WARTOSCI DLA SZEREGOW")

kosztyQ1 <- quantile(koszty, .25)
obrotyQ1 <- quantile(obroty, .25)

print(cat("Kwantyl pierwszy dla kosztow: ", kosztyQ1))
print(cat("Kwantyl pierwszy dla obrotow: ", obrotyQ1))

kosztyMedian <- quantile(koszty, .5)
obrotyMedian <- quantile(obroty, .5)

print(cat("Mediana dla kosztow: ", kosztyMedian))
print(cat("Mediana dla obrotow: ", obrotyMedian))

kosztyQ3 <- quantile(koszty, .75)
obrotyQ3 <- quantile(obroty, .75)

kosztyRostepCwiartkowy <- kosztyQ3 - kosztyQ1
obrotyRostepCwiartkowy <- obrotyQ3 - obrotyQ1

kosztyWariancja <- var(koszty)
obrotyWariancja <- var(obroty)

kosztyOdchylenie <- sd(koszty)
obrotyOdchylenie <- sd(obroty)

kosztyWspolczynnikZmiennosci <- kosztyOdchylenie / kosztySr
obrotyWspolczynnikZmiennosci <- obrotyOdchylenie / obrotySr

kosztyOdchylenieCwiartkowe <- kosztyRostepCwiartkowy / 2
obrotyOdchylenieCwiartkowe <- obrotyRostepCwiartkowy / 2

kosztyAsymetria <- moment(koszty, order = 3, center = TRUE)
obrotyAsymetria <- moment(obroty, order = 3, center = TRUE)

kosztyKoncentracja <- moment(koszty, order = 4, center = TRUE)
obrotyKoncentracja <- moment(obroty, order = 4, center = TRUE)

#szereg rozdzielczy

kosztyMax <- max(koszty)
obrotyMax <- max(obroty)

kosztyMin <- min(koszty)
obrotyMin <- min(obroty)

kosztyDif <- kosztyMax - kosztyMin
obrotyDif <- obrotyMax - obrotyMin

kosztySzerPrzed <- kosztyDif / ceiling(sqrt(kosztyDl))
obrotySzerPrzed <- obrotyDif / sqrt(obrotyDl)

kosztyPunkty <- seq(kosztyMin, kosztyMax + 0.1, by = kosztySzerPrzed) #0.1 dodane by uniknąć problemów
obrotyPunkty <- seq(obrotyMin, obrotyMax + 0.1, by = obrotySzerPrzed) #0.1 dodane by uniknąć problemów

kosztyPrzed <- cut(koszty, kosztyPunkty, right = FALSE, inlude.lowest = TRUE)
obrotyPrzed <- cut(obroty, obrotyPunkty, right = FALSE, inlude.lowest = TRUE)

kosztySzerRozdz <- table(kosztyPrzed)
obrotySzerRozdz <- table(obrotyPrzed)

kosztySzerRozdzSr <- mean(kosztySzerRozdz)
obrotySzerRozdzSr <- mean(obrotySzerRozdz) 

kosztySzerRozdzQ1 <- quantile(kosztySzerRozdz, .25)
obrotySzerRozdzQ1 <- quantile(obrotySzerRozdz, .25)

kosztySzerRozdzMedian <- quantile(kosztySzerRozdz, .5)
obrotySzerRozdzMedian <- quantile(obrotySzerRozdz, .5)

kosztySzerRozdzQ3 <- quantile(kosztySzerRozdz, .75)
obrotySzerRozdzQ3 <- quantile(obrotySzerRozdz, .75)

kosztySzerRozdzRostepCwiartkowy <- kosztySzerRozdzQ3 - kosztySzerRozdzQ1
obrotySzerRozdzRostepCwiartkowy <- obrotySzerRozdzQ3 - obrotySzerRozdzQ1

kosztySzerRozdzWariancja <- var(kosztySzerRozdz)
obrotySzerRozdzWariancja <- var(obrotySzerRozdz)

kosztySzerRozdzOdchylenie <- sd(kosztySzerRozdz)
obrotySzerRozdzOdchylenie <- sd(obrotySzerRozdz)

kosztySzerRozdzWspolczynnikZmiennosci <- kosztySzerRozdzOdchylenie / kosztySzerRozdzSr
obrotySzerRozdzWspolczynnikZmiennosci <- obrotySzerRozdzOdchylenie / obrotySzerRozdzSr

kosztySzerRozdzOdchylenieCwiartkowe <- kosztySzerRozdzRostepCwiartkowy / 2
obrotySzerRozdzOdchylenieCwiartkowe <- obrotySzerRozdzRostepCwiartkowy / 2

kosztySzerRozdzAsymetria <- moment(kosztySzerRozdz, order = 3, center = TRUE)
obrotySzerRozdzAsymetria <- moment(obrotySzerRozdz, order = 3, center = TRUE)
 
kosztySzerRozdzKoncentracja <- moment(kosztySzerRozdz, order = 4, center = TRUE)
obrotySzerRozdzKoncentracja <- moment(obrotySzerRozdz, order = 4, center = TRUE)

#histogramy rozkladow empirycznych



hist(koszty, breaks = kosztyPunkty, axes = F)
axis(2)
axis(1, at = seq(kosztyMin, kosztyMax, by = kosztySzerPrzed), labels = seq(kosztyMin, kosztyMax, by = kosztySzerPrzed))
hist(obrot, breaks = obrotyPunkty, axes = F)  
axis(2)
axis(1, at = seq(obrotyMin, obrotyMax, by = obrotySzerPrzed), labels = seq(obrotyMin, obrotyMax, by = obrotySzerPrzed))

#Zadanie 2
#Sprawdzenie czy roczne dzialalnosci koszty maja rozklad normalny

koszty = sort(koszty)
liczbaKosztow = length(koszty) #liczymy ile elementow maja koszty
k = 0.1241 # szukamy odpowiedzniego k z tab kolomogorwa lillieforsa
p = pnorm((koszty - mean(koszty)) / sd(koszty)) # rozklad normalny z koszty-mean(koszty))/sd(koszty)
# Statystyki testowe ktora sa miara rozbieznosci pomiedzy rozkladem empirycznym i hipotetycznym:
# Rozklad empiryczny to dystrybuanta rozkl
DKosztyPlus = max(seq(1:liczbaKosztow) / liczbaKosztow - p) #seq wektor bedacy ciagiem arytmetycznym
DKosztyMinus = max(p - (seq(1:liczbaKosztow) - 1) / liczbaKosztow)
dKoszty = max(DKosztyPlus, DKosztyMinus)
if (dKoszty < k) {
  cat("Koszty maja rozklad normalny\n")
} else {
  cat("Koszty nie maja rozkladu normalnego\n")
}
#obroty ale z ks.test
zmiennaChwilowa <- ks.test(obroty, "pnorm", mean(obroty), sd(obroty))
if (zmiennaChwilowa[2] > 0.05) {
  cat("Obroty maja rozkład normalny")
} else {
  cat("Obroty nie maja rozkładu normalny")
}



#obroty = sort(obroty); 

#liczbaObrotow = lengh(obroty) #liczymy ile elementow maja obroty
#
k = 0.1473; #  z tab kolomogorwa lillieforsa#p1=pnorm((obroty-mean(obroty))/sd(obroty))
#Dobrotyplus = max(seq(1:liczbaObrotow)/ l iczbaObrotow-p1 )#
#Dobrotyminus = min()

#Zadanie 3
#

kosztyLiczebosc = length(koszty)

kosztySuma = sum(koszty)


