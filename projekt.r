library(e1071)
# koszty = c(449.6, 315.1, 447.2, 258.3, 391.3, 297.2, 295.3, 303.4, 312.3, 369.3, 385.8, 466, 302.1, 304.3, 359.5, 309.2, 371.7, 504.6, 414.5, 321.9, 386.2, 489, 514.6, 249.8, 184.7, 452.8, 513.2, 540.4, 458.1, 459.4, 259.5, 296.4, 397.1, 525.7, 331.3, 232.4, 222.4, 418.9, 265.5, 282.8, 543.6, 387.8, 343.1, 351.6, 440.9, 403.3, 370.2, 207.6, 223.3, 304.3, 283.9)
# obroty = c(575.6, 250.8, 271.8, 337, 198.6, 361.7, 361.5, 447.2, 345, 455.9, 393.1, 368.3, 400, 449.7, 252.7, 437, 290.1, 297.7, 525.5, 411, 419.4, 530, 409.6, 474, 303.7, 246.8, 458.4, 480.3, 199.6, 461.9, 321.5, 305.7, 379.8, 493.2, 294, 322.9, 368.5, 262.4, 494.9, 446.7, 322.8, 475.8, 320, 252.6, 353.5, 303.3, 396.6, 255.6, 540.6)
source('functions.r')
dane <- read.csv("dane.csv", sep = ",", header = FALSE)
koszty <- as.vector(dane[[1]], mode = "numeric")
obroty <- as.vector(dane[[2]], mode = "numeric")
kosztySuma <- sum(koszty)

kosztyDl <- length(koszty)
obrotyDl <- length(obroty)

koszty <- sort(koszty)
obroty <- sort(obroty)

kosztySr <- mean(koszty)
obrotySr <- mean(obroty)

kosztyWariancja <- var(koszty)
obrotyWariancja <- var(obroty)

zadanie1 <- function() {

  kosztyDominanta <- dominanta(koszty)

  print("WARTOSCI DLA SZEREGOW")

  kosztyQ1 <- quantile(koszty, .25)
  obrotyQ1 <- quantile(obroty, .25)


  kosztyMedian <- quantile(koszty, .5)
  obrotyMedian <- quantile(obroty, .5)



  kosztyQ3 <- quantile(koszty, .75)
  obrotyQ3 <- quantile(obroty, .75)


  kosztyRostepCwiartkowy <- kosztyQ3 - kosztyQ1
  obrotyRostepCwiartkowy <- obrotyQ3 - obrotyQ1

  cat("Rozstep cwiartkowy dla kosztow: ", kosztyRostepCwiartkowy, "\n")
  cat("Rozstep cwiartkowy dla obrotow: ", obrotyRostepCwiartkowy, "\n")

  cat("Wariancja dla kosztow: ", kosztyWariancja, "\n")
  cat("Wariancja dla obrotow: ", obrotyWariancja, "\n")

  kosztyOdchylenie <- sd(koszty)
  obrotyOdchylenie <- sd(obroty)

  cat("Odchylenie standardowe dla kosztow: ", kosztyOdchylenie, "\n")
  cat("Odchylenie standardowe dla obrotow: ", obrotyOdchylenie, "\n")

  kosztyWspolczynnikZmiennosci <- kosztyOdchylenie / kosztySr
  obrotyWspolczynnikZmiennosci <- obrotyOdchylenie / obrotySr

  cat("Wspolczynnik zmiennosci dla kosztow: ", kosztyWspolczynnikZmiennosci, "\n")
  cat("Wspolczynnik zmiennosci dla obrotow: ", obrotyWspolczynnikZmiennosci, "\n")

  kosztyOdchylenieCwiartkowe <- kosztyRostepCwiartkowy / 2
  obrotyOdchylenieCwiartkowe <- obrotyRostepCwiartkowy / 2

  cat("Odchylenie cwiartkowe dla kosztow: ", kosztyOdchylenieCwiartkowe, "\n")
  cat("Odchylenie cwiartkowe dla obrotow: ", obrotyOdchylenieCwiartkowe, "\n")

  kosztyAsymetria <- moment(koszty, order = 3, center = TRUE)
  obrotyAsymetria <- moment(obroty, order = 3, center = TRUE)

  cat("Asymetria dla kosztow: ", kosztyAsymetria, "\n")
  cat("Asymetria dla obrotow: ", obrotyAsymetria, "\n")

  kosztyKoncentracja <- moment(koszty, order = 4, center = TRUE)
  obrotyKoncentracja <- moment(obroty, order = 4, center = TRUE)

  cat("Koncentracja dla kosztow: ", kosztyKoncentracja, "\n")
  cat("Koncentracja dla obrotow: ", obrotyKoncentracja, "\n")

  #szereg rozdzielczy

  kosztyMax <- max(koszty)
  obrotyMax <- max(obroty)

  kosztyMin <- min(koszty)
  obrotyMin <- min(obroty)

  kosztyDif <- kosztyMax - kosztyMin
  obrotyDif <- obrotyMax - obrotyMin

  kosztySzerPrzed <- kosztyDif / ceiling(sqrt(kosztyDl))
  obrotySzerPrzed <- obrotyDif / ceiling(sqrt(obrotyDl))

  kosztyPunkty <- seq(kosztyMin, kosztyMax + 0.1, by = kosztySzerPrzed) #0.1 dodane by uniknąć problemów
  obrotyPunkty <- seq(obrotyMin, obrotyMax + 0.1, by = obrotySzerPrzed) #0.1 dodane by uniknąć problemów

  kosztyPrzed <- cut(koszty, kosztyPunkty, right = FALSE, inlude.lowest = TRUE)
  obrotyPrzed <- cut(obroty, obrotyPunkty, right = FALSE, inlude.lowest = TRUE)

  kosztySzerRozdz <- table(kosztyPrzed)
  obrotySzerRozdz <- table(obrotyPrzed)

  print("PARAMETRY DLA SZEREGU ROZDZIELCZEGO")

  kosztySzerRozdzSr <- mean(kosztySzerRozdz)
  obrotySzerRozdzSr <- mean(obrotySzerRozdz)

  cat("Srednia dla kosztow: ", kosztySzerRozdzSr, "\n")
  cat("Srednia dla obrotow: ", obrotySzerRozdzSr, "\n")

  kosztySzerRozdzQ1 <- quantile(kosztySzerRozdz, .25)
  obrotySzerRozdzQ1 <- quantile(obrotySzerRozdz, .25)

  cat("Kwantyl pierwszy dla kosztow: ", kosztySzerRozdzQ1, "\n")
  cat("Kwantyl pierwszy dla obrotow: ", obrotySzerRozdzQ1, "\n")

  kosztySzerRozdzMedian <- quantile(kosztySzerRozdz, .5)
  obrotySzerRozdzMedian <- quantile(obrotySzerRozdz, .5)

  cat("Mediana dla kosztow: ", kosztySzerRozdzMedian, "\n")
  cat("Mediana dla obrotow: ", obrotySzerRozdzMedian, "\n")

  kosztySzerRozdzQ3 <- quantile(kosztySzerRozdz, .75)
  obrotySzerRozdzQ3 <- quantile(obrotySzerRozdz, .75)

  cat("Kwantyl trzeci dla kosztow: ", kosztySzerRozdzQ3, "\n")
  cat("Kwantyl trzeci dla obrotow: ", obrotySzerRozdzQ3, "\n")

  kosztySzerRozdzRostepCwiartkowy <- kosztySzerRozdzQ3 - kosztySzerRozdzQ1
  obrotySzerRozdzRostepCwiartkowy <- obrotySzerRozdzQ3 - obrotySzerRozdzQ1

  cat("Rozstep cwiartkowy dla kosztow: ", kosztySzerRozdzRostepCwiartkowy, "\n")
  cat("Rozstep cwiartkowy dla obrotow: ", obrotySzerRozdzRostepCwiartkowy, "\n")

  kosztySzerRozdzWariancja <- var(kosztySzerRozdz)
  obrotySzerRozdzWariancja <- var(obrotySzerRozdz)

  cat("Wariancja dla kosztow: ", kosztySzerRozdzWariancja, "\n")
  cat("Wariancja dla obrotow: ", obrotySzerRozdzWariancja, "\n")

  kosztySzerRozdzOdchylenie <- sd(kosztySzerRozdz)
  obrotySzerRozdzOdchylenie <- sd(obrotySzerRozdz)

  cat("Odchylenie standardowe dla kosztow: ", kosztySzerRozdzOdchylenie, "\n")
  cat("Odchylenie standardowe dla obrotow: ", obrotySzerRozdzOdchylenie, "\n")

  kosztySzerRozdzWspolczynnikZmiennosci <- kosztySzerRozdzOdchylenie / kosztySzerRozdzSr
  obrotySzerRozdzWspolczynnikZmiennosci <- obrotySzerRozdzOdchylenie / obrotySzerRozdzSr

  cat("Wspolczynnik zmiennosci dla kosztow: ", kosztySzerRozdzWspolczynnikZmiennosci, "\n")
  cat("Wspolczynnik zmiennosci dla obrotow: ", obrotySzerRozdzWspolczynnikZmiennosci, "\n")

  kosztySzerRozdzOdchylenieCwiartkowe <- kosztySzerRozdzRostepCwiartkowy / 2
  obrotySzerRozdzOdchylenieCwiartkowe <- obrotySzerRozdzRostepCwiartkowy / 2

  cat("Odchylenie Cwiartkowe dla kosztow: ", kosztySzerRozdzOdchylenieCwiartkowe, "\n")
  cat("Odchylenie Cwiartkowe dla obrotow: ", obrotySzerRozdzOdchylenieCwiartkowe, "\n")

  kosztySzerRozdzAsymetria <- moment(kosztySzerRozdz, order = 3, center = TRUE)
  obrotySzerRozdzAsymetria <- moment(obrotySzerRozdz, order = 3, center = TRUE)

  cat("Asymetria dla kosztow: ", kosztySzerRozdzAsymetria, "\n")
  cat("Asymetria dla obrotow: ", obrotySzerRozdzAsymetria, "\n")

  kosztySzerRozdzKoncentracja <- moment(kosztySzerRozdz, order = 4, center = TRUE)
  obrotySzerRozdzKoncentracja <- moment(obrotySzerRozdz, order = 4, center = TRUE)

  cat("Koncentracja dla kosztow: ", kosztySzerRozdzKoncentracja, "\n")
  cat("Koncentracja dla obrotow: ", obrotySzerRozdzKoncentracja, "\n")

  #histogramy rozkladow empirycznych

  hist(koszty, breaks = kosztyPunkty, axes = F)
  axis(2)
  axis(1, at = seq(kosztyMin, kosztyMax, by = kosztySzerPrzed), labels = seq(kosztyMin, kosztyMax, by = kosztySzerPrzed))
  hist(obroty, breaks = obrotyPunkty, axes = F)
  axis(2)
  axis(1, at = seq(obrotyMin, obrotyMax, by = obrotySzerPrzed), labels = seq(obrotyMin, obrotyMax, by = obrotySzerPrzed))
}

#Zadanie 2
#Sprawdzenie czy roczne dzialalnosci koszty maja rozklad normalny

zadanie2 <- function(dane) {
  k = 0
  dane <- sort(dane)
  daneD1 <- length(dane)
  if (daneD1 == 51) { k = 0.1241 }
  else { k = 0.1266 }
  # szukamy odpowiedzniego k z tab kolomogorwa lillieforsa
  p <- pnorm((dane - mean(dane)) / sd(dane)) # rozklad normalny z koszty-mean(koszty))/sd(koszty)
  # Statystyki testowe ktora sa miara rozbieznosci pomiedzy rozkladem empirycznym i hipotetycznym:
  # Rozklad empiryczny to dystrybuanta rozkl
  DanePlus <- max(seq(1:daneD1) / daneD1 - p) #seq wektor bedacy ciagiem arytmetycznym
  DaneMinus <- max(p - (seq(1:daneDl) - 1) / daneDl)
  d <- max(DanePlus, DaneMinus)
  if (d < k) {
    cat("Roczne koszty i obroty maja rozklad normalny\n")
  } else {
    cat("Roczne koszty i obroty nie maja rozkladu normalnego\n")
  }
}

zadanie3 <- function() {
  kosztySr = mean(koszty)
  kosztyOdchylStd = sd(koszty)

  #Wyznaczenie wspolczynnika t-Studenta

  kosztyWspolczynnikUfnosci = 0.95

  alfa = 1 - kosztyWspolczynnikUfnosci

  kosztyTStudent = qt(alfa, kosztyLiczebosc - 1, lower.tail = FALSE)

  #Obliczenie przedziału ufności


  kosztyDolnaGranica = kosztySr - kosztyTStudent * (kosztyOdchylStd / (sqrt(kosztyLiczebosc - 1)))
  kosztyGornaGranica = kosztySr + kosztyTStudent * (kosztyOdchylStd / (sqrt(kosztyLiczebosc - 1)))

  cat("Dolna granica przedzialu ufnosci: ", kosztyDolnaGranica, '\n')


  cat("Gorna granica przedzialu ufnosci: ", kosztyGornaGranica, '\n')

  #Obliczenie precyzji oszacowania

  kosztyPrecyzja = 0.5 * (kosztyGornaGranica - kosztyDolnaGranica) / kosztySr

  cat("Wzgledna precyzja oszacowania: ", kosztyPrecyzja, '\n')
}

zadanie4 <- function() {
  obrotyWariancja <- var(obroty)
  obrotyDlugosc <- length(obroty)
  obrotyOdchylenie <- sd(obroty)

  obrotyDolnaGranica <- obrotyDlugosc * obrotyWariancja / qchisq(0.975, obrotyDlugosc - 1)
  obrotyGornaGranica <- obrotyDlugosc * obrotyWariancja / qchisq(0.025, obrotyDlugosc - 1)

  precyzja <- (obrotyGornaGranica - obrotyDolnaGranica) * 100 / obrotyOdchylenie ** 2
  print("Precyzja: ")
  precyzja

  chikwadrat <- ((obrotyDlugosc - 1) * obrotyOdchylenie ** 2) / obrotyWariancja

  if (chikwadrat > obrotyDolnaGranica && chikwadrat < obrotyDolnaGranica) {
    print("mozna przyjac oszacowanie")
  } else {
    print("nie mozna przyjac oszacowania")
  }
}

test_f_snedecora <- function() {

  alfa <- 0.05;

  print("Test   F-Snedeco ra")
  print("H0: wariancje obu populacji sa rowne")
  print("H1: wariancje kosztow jest wieksza od wariancji obrotow")

  #obliczamy wartosc statystyki F
  statystyka_f = (var(koszty) * (length(koszty) / (length(koszty) - 1))) / (var(obroty) * (length(obroty) / (length(obroty) - 1)))

  #obliczamy kwantyle rozkladu F Snedecora
  #Gdy hipoteza jest prawdziwa to statystyka F ma rozkład F-Snedecora z (n-1) i (m-1) stopniami swobody
  kwartyle_f = qf(c(alfa / 2, 1 - alfa / 2), length(koszty) - 1, length(obroty) - 1)

  cat(" Wartosc obliczonej statystyki:", statystyka_f, '\n')
  cat(" Obszarem krytycznym jest przedzial:(0, ", kwartyle_f[1], ") u(", kwartyle_f[2], ", inf) . \n")

  #sprawdzamy czy statystyka nalezy do obszaru testowego

  if (statystyka_f > kwartyle_f[1] && statystyka_f < kwartyle_f[2]) {
    print("Wartość statystyki nie nalezy do obszaru krytycznego")
    print("Wiec nie ma podstwy do odrzucenia hipotezy H0 o rownosci wariancji kosztow i obrotow")
    return(TRUE)
  } else {
    print("Wartosc statystyki nalezy do obszaru krytycznego")
    print("Odrzucamy hipoteze H0")
    return(FALSE)
  }
}

test_t_studenta <- function() {

  print("Test t-Studenta")
  print("H0: Srednie obroty sa rowne kosztom")
  statystyka_t <- (kosztySr - obrotySr) / sqrt((kosztyDl * kosztyWariancja + obrotyDl * obrotyWariancja) * (1 / kosztyDl + 1 / obrotyDl) / (kosztyDl - 1 + obrotyDl - 1))
  kwartyl_t = qt(1 - alfa, df = kosztyDl + obrotyDl - 2)

  cat("Wartosc obliczonej statystyki: ", statystyka_t, " \n")
  cat("Obszar krytyczny: (", kwartyl_t, ", inf) \n")

  #sprawdzenie czy obliczona statystyka naleze do obszaru krytycznego
  if (statystyka_t > kwartyl_t) {
    print("Obliczona statystyka nalezy do obszaru krytyczne go")
  } else {
    print("Obliczona statystyka nie nalezy do obszaru krytycznego.")
    print("Brak podstaw do odrzucenia hipotezy zerowej H0")

  }
}

test_t_studenta_c_coxa <- function() {
  alfa = 0.05
  print("Test t-Studenta z korekta Cocharana-Coxa")
  print("H0: Srednie obroty rownaja sie kosztom.")
  print("H1: Srednie obroty sa wieksze niz srednie koszta.")

  wariancjaNieobciazonaKosztow = kosztyDl * kosztyWariancja / (kosztyDl - 1)
  wariancjaNieobciazonaObrotow = obrotyDl * obrotyWariancja / (obrotyDl - 1)

  statystyka_t = (mean(koszty) - mean(obroty)) / sqrt(kosztyWariancja / kosztyDl + obrotyWariancja / obrotyDl - 1)

  kwantyl_t = (kosztyWar * qt(1 - alfa, kosztyDl - 1) / kosztyDl + obrotyWariancja * qt(1 - alfa, kosztyDl - 1) / obrotyDl
             / (kosztyWariancja / kosztyDl) + obrotyWariancja / obrotyDl)

  #czy statystyka nalezy do obszaru krytycznego
  if (statystyka_t > kwartyl_t) {
    print("Obliczona statystyka naezy do obszaru krytycznego")
    print("odrzucamy hipoteze zerowa")

  } else {
    print("obliczona statystyka nie nalezy do obszaru krytycznego")
    print("brak podstaw do odrzucenia hipotezy H0")
  }
}

zadanie5 <- function() {

  #sprawdzenie czy wariancje kosztow i obrotow sa rowne
  if (test_f_snedecora()) {
    test_t_studenta()
  }
  else {
    #dodajemy do t studenta poprawke coharana coxa bo odkrylismy ze wariancje kosztow i obrotow sa rozne
    test_t_studenta_c_coxa()
  }
}
