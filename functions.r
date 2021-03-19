# kwantyl <- function(x,) { }

dane <- read.csv("dane.csv", sep = ",", header = FALSE)
koszty <- as.vector(dane[[1]], mode = "numeric")
obroty <- as.vector(dane[[2]], mode = "numeric")

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
  wynik = prod(x) ^ (1 / lenght(x))
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
  suma <- sum(abs(x - sr)) / lenght(x)
  return(suma)
}

odchyleniePrzecietneOdMediany <- function(x) {
  x <- sort(x)
  med <- quantile(x, 0.5)
  suma <- sum(abs(x - med)) / lenght(x)
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
  cat("\n", nazwa, ":", format(kosz, obr, width = 20), "\n")
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
  licznik <- momentCentral
  ny(x, 3)
  mianownik <- sqrt(var(x))
  mianowik = mianownik ** 3
  return(licznik / mianownik)
}

kurtoza <- function(x) {
  licznik <- momentCentralny(x, 4)
  mianownik <- sqrt(var(x)) ** 4)
  return(licznik / mianownik)
}

eksces <- function(kurt) {
  return(kurt - 3)
}









