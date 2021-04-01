policzWystapienia <- function(y, x) {
  wystapienia <- y
  for (i in length(y)) {
    wyst <- 0
    for (j in length(x)) {
      if (y[i] == x[j]) {
        wyst <- wyst + 1
      }
    }
    wystapienia[i] <- wyst
  }
  return(wystapienia)
}

sredniaHarmoniczna <- function(x) {
  wynik = 1 / mean(1 / x)
  return(wynik)
}

sredniaGeometryczna <- function(x) {
  wynik = prod(x) ^ (1 / length(x))
  return(wynik)
}

dominanta <- function(x) {
  unikalne <- as.numeric(levels(factor(x)))
  p <- unikalne[which.max(policzWystapienia(unikalne, x))]
  return(p)
}

wariancjaNieobc <- function(x) {
  sr <- mean(x)
  suma <- sum((x - sr) ^ 2) / (length(x) - 1)
  return(suma)
}

odchyleniePrzecietne <- function(x) {
  sr <- mean(x)
  suma <- sum(abs(x - sr)) / length(x)
  return(suma)
}

odchyleniePrzecietneOdMediany <- function(x) {
  x <- sort(x)
  med <- quantile(x, 0.5)
  suma <- sum(abs(x - med)) / length(x)
  return(suma)
}

odchylenieCwiartkow <- function(x) {
  x <- sort(x)
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  roznica <- (Q1 - Q3) / 2
  return(roznica)
}

wspolczynnikZmiennosci <- function(x) {
  x <- sort(x)
  odch <- sqrt(var(x))
  sr <- mean(x)
  wsp <- odch / sr * 100
  return(wsp)
}

pozycyjnyWspolczynnik <- function(x) {
  odch <- odchyleniePrzecietne(x)
  sr <- mean(x)
  wsp <- odch / sr * 100
  return(wsp)
}

wypiszParametr <- function(kosz, obr, nazwa) {
  cat("\n", format(nazwa, width = 40), format(kosz, justify = c("centre"), width = 20), format(obr, justify = c("centre"), width = 20), "\n")
}

momentCentralny <- function(x, n) {
  dlugosc <- length(x)
  sr <- mean(x)
  wynik <- 0

  for (i in 1:dlugosc) {
    wynik <- wynik + (x[i] - sr) ** n
  }
  wynik <- wynik / dlugosc
  return(wynik)
}
skosnosc <- function(x) {
  licznik <- momentCentralny(x, 3)
  mianownik <- sqrt(var(x))
  mianowik = mianownik ** 3
  return(licznik / mianownik)
}

kurtoza <- function(x) {
  licznik <- momentCentralny(x, 4)
  mianownik <- sqrt(var(x) ** 4)
  return(licznik / mianownik)
}

eksces <- function(kurt) {
  return(kurt - 3)
}

sredniaPrzedzialowa <- function(granice, liczebnosc) {
  suma <- 0
  dlugosc <- sum(liczebnosc)
  for (i in 1:length(liczebnosc)) {
    x <- (granice[i] + granice[i + 1]) / 2
    suma <- suma + (x * liczebnosc[i])
  }
  return(suma / dlugosc)
}

kwartyl <- function(granice, liczebnosc, dlugoscPrzedzialu, rzad) {
  dlugosc <- sum(liczebnosc)
  dlugosc <- dlugosc * (rzad / 4)
  ktoryPrzedzial <- 1
  dlugoscDoMed <- liczebnosc[ktoryPrzedzial]
  while (dlugoscDoMed < dlugosc) {
    ktoryPrzedzial <- ktoryPrzedzial + 1
    dlugoscDoMed <- dlugoscDoMed + liczebnosc[ktoryPrzedzial]
  }
  wynik <- granice[ktoryPrzedzial] + (dlugoscPrzedzialu / liczebnosc[ktoryPrzedzial]) * (dlugosc - dlugoscDoMed) #bylo i SPRAWDZIC
  return(wynik)
}

moda <- function(granice, liczebnosc, dlugoscPrzedzialu) {
  liczebnoscModa = max(liczebnosc)
  ktoryPrzedzial <- 1
  while (liczebnosc[ktoryPrzedzial] != liczebnoscModa) {
    ktoryPrzedzial <- ktoryPrzedzial + 1
  }
  wynik <- granice[ktoryPrzedzial] + (liczebnoscModa - liczebnosc[ktoryPrzedzial - 1]) * dlugoscPrzedzialu / (2 * liczebnoscModa - liczebnosc[ktoryPrzedzial - 1] - liczebnosc[ktoryPrzedzial + 1]) #DODAŁEM * PO DLUGOSC PRZEDZIALU
  return(wynik)
}

momentCentralnyPrzedzialowy <- function(granice, liczebnosc, n) {
  sr <- sredniaPrzedzialowa(granice, liczebnosc)
  wynik <- 0
  dlugosc <- sum(liczebnosc)
  for (i in 1:length(liczebnosc)) {
    x <- (granice[i] + granice[i + 1]) / 2
    wynik <- wynik + (x - sr) ^ n
  }
  wynik <- wynik / dlugosc
  return(wynik)
}

wariancjaPrzedzial <- function(granice, liczebnosc) {
  return(momentCentralnyPrzedzialowy(granice, liczebnosc, 2))
}

odchyleniePrzecietneRozdz <- function(granice, liczebnosc, war) {
  suma <- 0
  dlugosc <- sum(liczebnosc)
  for (i in 1:length(liczebnosc)) {
    x <- (granice[i] + granice[i + 1]) / 2
    czynnikSumy <- abs(x - war)
    suma <- suma + (czynnikSumy * liczebnosc[i])
    dlugosc <- dlugosc + liczebnosc[i]
  }
  return(suma / dlugosc)
}

odchyleniePrzecietnePrzedzialowe <- function(granice, liczebnosc) {
  sr <- sredniaPrzedzialowa(granice, liczebnosc)
  return(odchyleniePrzecietneRozdz(granice, liczebnosc, sr))
}

odchyleniePrzecietnePrzedzialoweOdMediany <- function(granice, liczebnosc, rzad) {
  me <- kwartyl(granice, liczebnosc, rzad, 2)
  return(odchyleniePrzecietneRozdz(granice, liczebnosc, me))
}

odchylenieCwiartkowePrzedzialowe <- function(granice, liczebnosc, dlugoscPrzedzialu) {
  Q1 <- kwartyl(granice, liczebnosc, dlugoscPrzedzialu, 1)
  Q3 <- kwartyl(granice, liczebnosc, dlugoscPrzedzialu, 3)
  wynik <- (Q1 - Q3) / 2
  return(wynik)
}

wspolczynnikZmiennosciPrzedzialowe <- function(granice, liczebnosc) {
  odch <- sqrt(wariancjaPrzedzial(granice, liczebnosc))
  sr <- sredniaPrzedzialowa(granice, liczebnosc)
  return(odch * 100 / sr)
}


skosnoscPrzedzialowa <- function(granice, liczebnosc) {
  licznik <- momentCentralnyPrzedzialowy(granice, liczebnosc, 3)
  mianownik <- sqrt(wariancjaPrzedzial(granice, liczebnosc))
  mianowik = mianownik ^ 3
  return(licznik / mianownik)
}

kurtozaPrzedzialowa <- function(granice, liczebnosc) {
  licznik <- momentCentralnyPrzedzialowy(granice, liczebnosc, 4)
  mianownik <- wariancjaPrzedzial(granice, liczebnosc) ^ 2
  return(licznik / mianownik)
}