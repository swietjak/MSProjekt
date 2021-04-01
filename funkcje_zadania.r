source('funkcje_zad1.r')
source('funkcje_zad5.r')

zadanie1 <- function() {
  print(obroty)
  kosztyDominanta <- dominanta(koszty)
  obrotyDominanta <- dominanta(obroty)

  cat("\n", format("WARTOSCI DLA SZEREGOW SZCZEGOLOWEGO", width = 40, justify = c("left")), format("KOSZTY", width = 20, justify = c("right")), format("OBROTY", width = 20, justify = c("right")), "\n")

  kosztyQ1 <- quantile(koszty, .25)
  obrotyQ1 <- quantile(obroty, .25)

  kosztyMedian <- quantile(koszty, .5)
  obrotyMedian <- quantile(obroty, .5)

  kosztyQ3 <- quantile(koszty, .75)
  obrotyQ3 <- quantile(obroty, .75)

  kosztyRostepCwiartkowy <- kosztyQ3 - kosztyQ1
  obrotyRostepCwiartkowy <- obrotyQ3 - obrotyQ1

  kosztyWspolczynnikZmiennosci <- kosztyOdchylenieSt / kosztySr
  obrotyWspolczynnikZmiennosci <- obrotyOdchylenieSt / obrotySr

  kosztyOdchylenieCwiartkowe <- kosztyRostepCwiartkowy / 2
  obrotyOdchylenieCwiartkowe <- obrotyRostepCwiartkowy / 2

  kosztyAsymetria <- skosnosc(koszty)
  obrotyAsymetria <- skosnosc(obroty)

  kosztyKoncentracja <- kurtoza(koszty)
  obrotyKoncentracja <- kurtoza(obroty)

  kosztyEksces <- kosztyKoncentracja - 3
  obrotyEksces <- obrotyKoncentracja - 3

  kosztyOdchPrzec <- odchyleniePrzecietne(koszty)
  obrotyOdchPrzec <- odchyleniePrzecietne(obroty)

  kosztyOdchPrzecOdMed <- odchyleniePrzecietneOdMediany(koszty)
  obrotyOdchPrzecOdMed <- odchyleniePrzecietneOdMediany(obroty)

  kosztyWariancjaNieobc <- wariancjaNieobc(koszty)
  obrotyWariancjaNieobc <- wariancjaNieobc(obroty)

  wypiszParametr(kosztySr, obrotySr, "Srednia")
  wypiszParametr(kosztyOdchylenieSt, obrotyOdchylenieSt, "Odchylenie standardowe")
  wypiszParametr(kosztyWariancja, obrotyWariancja, "Wariancja")
  wypiszParametr(kosztyWariancjaNieobc, obrotyWariancjaNieobc, "Wariancja nieobciazona")
  wypiszParametr(kosztyDominanta, obrotyDominanta, "Moda")
  wypiszParametr(kosztyQ1, obrotyQ1, "Kwantyl 1")
  wypiszParametr(kosztyMedian, obrotyMedian, "Mediana")
  wypiszParametr(kosztyQ3, obrotyQ3, "Kwantyl 3")
  wypiszParametr(kosztyRostepCwiartkowy, obrotyRostepCwiartkowy, "Rozstep cwiartkowy")
  wypiszParametr(kosztyWspolczynnikZmiennosci, obrotyWspolczynnikZmiennosci, "Wspolczynnik zmiennosci")
  wypiszParametr(kosztyOdchylenieCwiartkowe, obrotyOdchylenieCwiartkowe, "Odchylenie cwiartkowe")
  wypiszParametr(kosztyOdchPrzec, obrotyOdchPrzec, "Odchylenie przecietne")
  wypiszParametr(kosztyOdchPrzecOdMed, obrotyOdchPrzecOdMed, "Odchylenie przecietne od mediany")
  wypiszParametr(kosztyAsymetria, obrotyAsymetria, "Skosnosc")
  wypiszParametr(kosztyKoncentracja, obrotyKoncentracja, "Kurtoza")
  wypiszParametr(kosztyEksces, obrotyEksces, "Eksces")


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

  kosztySzerRozdzVec <- as.vector(kosztySzerRozdz, "numeric")
  obrotySzerRozdzVec <- as.vector(obrotySzerRozdz, "numeric")

  print("PARAMETRY DLA SZEREGU ROZDZIELCZEGO")
  cat("\n", format("WARTOSCI DLA SZEREGOW ROZDZIELCZYCH", width = 40, justify = c("left")), format("KOSZTY", width = 20, justify = c("right")), format("OBROTY", width = 20, justify = c("right")), "\n")

  kosztySzerRozdzSr <- sredniaPrzedzialowa(kosztyPunkty, kosztySzerRozdzVec)
  obrotySzerRozdzSr <- sredniaPrzedzialowa(obrotyPunkty, obrotySzerRozdzVec)

  kosztySzerRozdzQ1 <- kwartyl(kosztyPunkty, kosztySzerRozdzVec, kosztySzerPrzed, 1)
  obrotySzerRozdzQ1 <- kwartyl(obrotyPunkty, obrotySzerRozdzVec, obrotySzerPrzed, 1)

  kosztySzerRozdzMedian <- kwartyl(kosztyPunkty, kosztySzerRozdzVec, kosztySzerPrzed, 2)
  obrotySzerRozdzMedian <- kwartyl(obrotyPunkty, obrotySzerRozdzVec, obrotySzerPrzed, 2)

  kosztySzerRozdzQ3 <- kwartyl(kosztyPunkty, kosztySzerRozdzVec, kosztySzerPrzed, 3)
  obrotySzerRozdzQ3 <- kwartyl(obrotyPunkty, obrotySzerRozdzVec, obrotySzerPrzed, 3)

  kosztySzerRozdzRostepCwiartkowy <- kosztySzerRozdzQ3 - kosztySzerRozdzQ1
  obrotySzerRozdzRostepCwiartkowy <- obrotySzerRozdzQ3 - obrotySzerRozdzQ1

  kosztySzerRozdzWariancja <- wariancjaPrzedzial(kosztyPunkty, kosztySzerRozdzVec)
  obrotySzerRozdzWariancja <- wariancjaPrzedzial(obrotyPunkty, obrotySzerRozdzVec)

  kosztySzerRozdzOdchylenie <- sqrt(kosztySzerRozdzWariancja)
  obrotySzerRozdzOdchylenie <- sqrt(obrotySzerRozdzWariancja)

  kosztySzerRozdzOdchPrzec <- odchyleniePrzecietnePrzedzialowe(kosztyPunkty, kosztySzerRozdzVec)
  obrotySzerRozdzOdchPrzec <- odchyleniePrzecietnePrzedzialowe(obrotyPunkty, obrotySzerRozdzVec)

  kosztySzerRozdzWspolczynnikZmiennosci <- wspolczynnikZmiennosciPrzedzialowe(kosztyPunkty, kosztySzerRozdzVec)
  obrotySzerRozdzWspolczynnikZmiennosci <- wspolczynnikZmiennosciPrzedzialowe(obrotyPunkty, obrotySzerRozdzVec)

  kosztySzerRozdzOdchylenieCwiartkowe <- kosztySzerRozdzRostepCwiartkowy / 2
  obrotySzerRozdzOdchylenieCwiartkowe <- obrotySzerRozdzRostepCwiartkowy / 2

  kosztySzerRozdzAsymetria <- skosnoscPrzedzialowa(kosztyPunkty, kosztySzerRozdzVec)
  obrotySzerRozdzAsymetria <- skosnoscPrzedzialowa(obrotyPunkty, obrotySzerRozdzVec)


  kosztySzerRozdzKoncentracja <- kurtozaPrzedzialowa(kosztyPunkty, kosztySzerRozdzVec)
  obrotySzerRozdzKoncentracja <- kurtozaPrzedzialowa(obrotyPunkty, obrotySzerRozdzVec)

  kosztySzerRozdzEksces = kosztySzerRozdzKoncentracja - 3
  obrotySzerRozdzEksces = obrotySzerRozdzKoncentracja - 3

  kosztySzerRozdzDominanta = moda(kosztyPunkty, kosztySzerRozdzVec, kosztySzerPrzed)
  obrotySzerRozdzDominanta = moda(obrotyPunkty, obrotySzerRozdzVec, obrotySzerPrzed)

  kosztySzerRozdzOdchPrzecOdMed = odchyleniePrzecietnePrzedzialoweOdMediany(kosztyPunkty, kosztySzerRozdzVec, kosztySzerPrzed)
  obrotySzerRozdzOdchPrzecOdMed = odchyleniePrzecietnePrzedzialoweOdMediany(obrotyPunkty, obrotySzerRozdzVec, obrotySzerPrzed)

  wypiszParametr(kosztySzerRozdzSr, obrotySzerRozdzSr, "Srednia")
  wypiszParametr(kosztySzerRozdzOdchylenie, obrotySzerRozdzOdchylenie, "Odchylenie standardowe")
  wypiszParametr(kosztySzerRozdzWariancja, obrotySzerRozdzWariancja, "Wariancja")
  wypiszParametr(kosztySzerRozdzDominanta, obrotySzerRozdzDominanta, "Moda")
  wypiszParametr(kosztySzerRozdzQ1, obrotySzerRozdzQ1, "Kwartyl 1")
  wypiszParametr(kosztySzerRozdzMedian, obrotySzerRozdzMedian, "Mediana")
  wypiszParametr(kosztySzerRozdzQ3, obrotySzerRozdzQ3, "Kwartyl 3")
  wypiszParametr(kosztySzerRozdzRostepCwiartkowy, obrotySzerRozdzRostepCwiartkowy, "Rozstep cwiartkowy")
  wypiszParametr(kosztySzerRozdzWspolczynnikZmiennosci, obrotySzerRozdzWspolczynnikZmiennosci, "Wspolczynnik zmiennosci")
  wypiszParametr(kosztySzerRozdzOdchylenieCwiartkowe, obrotySzerRozdzOdchylenieCwiartkowe, "Odchylenie cwiartkowe")
  wypiszParametr(kosztySzerRozdzOdchPrzec, obrotySzerRozdzOdchPrzec, "Odchylenie przecietne")
  wypiszParametr(kosztySzerRozdzOdchPrzecOdMed, obrotySzerRozdzOdchPrzecOdMed, "Odchylenie przecietne od mediany")
  wypiszParametr(kosztySzerRozdzAsymetria, obrotySzerRozdzAsymetria, "Skosnosc")
  wypiszParametr(kosztySzerRozdzKoncentracja, obrotySzerRozdzKoncentracja, "Kurtoza")
  wypiszParametr(kosztySzerRozdzEksces, obrotySzerRozdzEksces, "Eksces")

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

zadanie2 <- function(dane, nazwa_zbioru) {
  k = 0
  d = 0
  p = 0
  daneD1 = 0
  DanePlus = 0
  DaneMinus = 0
  dane <- sort(dane)
  daneD1 <- length(dane)
  if (daneD1 == 51) { k = 0.1241 }
  if (daneD1 == 49) { k = 0.1266 }
  # szukamy odpowiedzniego k z tab kolomogorwa lillieforsa
  p <- pnorm((dane - mean(dane)) / sd(dane)) # rozklad normalny z koszty-mean(koszty))/sd(koszty)
  # Statystyki testowe ktora sa miara rozbieznosci pomiedzy rozkladem empirycznym i hipotetycznym:
  # Rozklad empiryczny to dystrybuanta rozkl
  DanePlus <- max(seq(1:daneD1) / daneD1 - p) #seq wektor bedacy ciagiem arytmetycznym
  DaneMinus <- max(p - (seq(1:daneD1) - 1) / daneD1)
  d <- max(DanePlus, DaneMinus)
  cat("Wartość statystyki testowej: ", d)
  cat("Wartość k: ", k)
  if (d < k) {
    cat("Roczne", nazwa_zbioru, "maja rozklad normalny\n")
  } else {
    cat("Roczne", nazwa_zbioru, "nie maja rozkladu normalnego\n")
  }
}

zadanie3 <- function() {
  kosztySr = mean(koszty)
  kosztyOdchylenieSt = sd(koszty)

  cat("Liczebnosc przedzialu: ", kosztyDl, "\n")
  cat("Suma wartosci:", kosztySuma, "\n")
  cat("Srednia: ", kosztySr, "\n")
  cat("Odchylenie standardowe: ", kosztyOdchylenieSt, "\n\n\n")

  #Wyznaczenie wspolczynnika t-Studenta

  kosztyWspolczynnikUfnosci = 0.95

  alfa = 1 - kosztyWspolczynnikUfnosci

  kosztyTStudent = qt(alfa, kosztyDl - 1, lower.tail = FALSE)

  #Wyznaczenie przedziału ufności

  kosztyDolnaGranica = kosztySr - kosztyTStudent * (kosztyOdchylenieSt / (sqrt(kosztyDl - 1)))
  kosztyGornaGranica = kosztySr + kosztyTStudent * (kosztyOdchylenieSt / (sqrt(kosztyDl - 1)))

  cat("Dolna granica przedzialu ufnosci: ", kosztyDolnaGranica, '\n')
  cat("Gorna granica przedzialu ufnosci: ", kosztyGornaGranica, '\n')

  #Obliczenie względnej precyzji oszacowania

  kosztyPrecyzja = 0.5 * (kosztyGornaGranica - kosztyDolnaGranica) / kosztySr

  cat("Wzgledna precyzja oszacowania: ", kosztyPrecyzja, '\n')

  #sprawdzenie, czy mamy podstawy do uogólnienia otrzymanego przedziału ufności na całą populację rocznych kosztów działalności?
  #Wzgledna prezycja wynosi około 6%, więc wynik można uogólnić dla całej populacji(z pewnymi zastrzeżeniami)
}

zadanie4 <- function() {

  cat("Wariancja:", obrotyWariancja, "\n")
  cat("Liczebnosc przedzialu: ", obrotyDl, "\n")

  #Wyznaczenie przedziału ufności
  obrotyDolnaGranica = obrotyDl * obrotyWariancja / qchisq(0.975, obrotyDl - 1)
  obrotyGornaGranica = obrotyDl * obrotyWariancja / qchisq(0.025, obrotyDl - 1)

  cat("Dolna granica przedzialu ufnosci: ", obrotyDolnaGranica, "\n")
  cat("Gorna granica przedzialu ufnosci: ", obrotyGornaGranica, "\n\n\n")

  #Wyznaczenie względnej prezycji oszacowania
  precyzja = (obrotyGornaGranica - obrotyDolnaGranica) * 100 / obrotyWariancja

  cat("Wzgledna precyzja oszacowania: ", precyzja, "\n")

  #Wzgledna precyzja wynosi około 88%, więc wyników tej próby nie można uogólniać dla całej populacji.

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