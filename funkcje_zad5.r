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

kosztyOdchylenieSt = sd(koszty)
obrotyOdchylenieSt = sd(obroty)

test_f_snedecora <- function() {

  alfa <- 0.05;
  cat("Test   F-Snedecora\n")
  cat("H0: wariancje obu populacji sa rowne\n")
  cat("H1: wariancje kosztow jest wieksza od wariancji obrotow\n")

  #obliczamy wartosc statystyki F
  statystyka_f = (kosztyWariancja * (kosztyDl / (kosztyDl - 1))) / (obrotyWariancja * (obrotyDl / (obrotyDl - 1)))

  #obliczamy kwantyle rozkladu F Snedecora
  #Gdy hipoteza jest prawdziwa to statystyka F ma rozkład F-Snedecora z (n-1) i (m-1) stopniami swobody
  kwartyle_f = qf(c(alfa / 2, 1 - alfa / 2), kosztyDl - 1, obrotyDl - 1)

  cat(" Wartosc obliczonej statystyki:", statystyka_f, '\n')
  cat(" Obszarem krytycznym jest przedzial:(0, ", kwartyle_f[1], ") u(", kwartyle_f[2], ", inf) . \n")

  #sprawdzamy czy statystyka nalezy do obszaru testowego

  if (statystyka_f > kwartyle_f[1] && statystyka_f < kwartyle_f[2]) {
    cat("Wartosc statystyki nie nalezy do obszaru krytycznego\n")
    cat("Wiec nie ma podstwy do odrzucenia hipotezy H0 o rownosci wariancji kosztow i obrotow\n")
    return(TRUE)
  } else {
    cat("Wartosc statystyki nalezy do obszaru krytycznego\n")
    cat("Odrzucamy hipoteze H0\n")
    return(FALSE)
  }
}

test_t_studenta <- function() {
  alfa = 0.05
  cat("Test t-Studenta\n")
  cat("H0: Srednie obroty sa rowne kosztom\n")
  statystyka_t <- (kosztySr - obrotySr) / sqrt((kosztyDl * kosztyWariancja + obrotyDl * obrotyWariancja) * (1 / kosztyDl + 1 / obrotyDl) / (kosztyDl + obrotyDl - 2))
  kwartyl_t = qt(1 - alfa, df = kosztyDl + obrotyDl - 2)

  cat("Wartosc obliczonej statystyki: ", statystyka_t, " \n")
  cat("Obszar krytyczny: (", kwartyl_t, ", inf) \n")

  #sprawdzenie czy obliczona statystyka naleze do obszaru krytycznego
  if (statystyka_t > kwartyl_t) {
    cat("Obliczona statystyka nalezy do obszaru krytyczne go\n")
  } else {
    cat("Obliczona statystyka nie nalezy do obszaru krytycznego.\n")
    cat("Brak podstaw do odrzucenia hipotezy zerowej H0\n")

  }
}

test_t_studenta_c_coxa <- function() {
  alfa = 0.05
  cat("Test t-Studenta z korekta Cocharana-Coxa\n")
  cat("H0: Srednie obroty rownaja sie kosztom.\n")
  cat("H1: Srednie obroty sa wieksze niz srednie koszta.\n")

  wariancjaNieobciazonaKosztow = kosztyDl * kosztyWariancja / (kosztyDl - 1)
  wariancjaNieobciazonaObrotow = obrotyDl * obrotyWariancja / (obrotyDl - 1)

  statystyka_t = (kosztySr - obrotySr) / sqrt(kosztyWariancja / kosztyDl + obrotyWariancja / obrotyDl - 1)

  kwantyl_t = (kosztyWar * qt(1 - alfa, kosztyDl - 1) / kosztyDl + obrotyWariancja * qt(1 - alfa, kosztyDl - 1) / obrotyDl
             / (kosztyWariancja / kosztyDl) + obrotyWariancja / obrotyDl)

  #czy statystyka nalezy do obszaru krytycznego
  if (statystyka_t > kwartyl_t) {
    cat("Obliczona statystyka naezy do obszaru krytycznego\n")
    cat("odrzucamy hipoteze zerowa\n")

  } else {
    cat("obliczona statystyka nie nalezy do obszaru krytycznego\n")
    cat("brak podstaw do odrzucenia hipotezy H0\n")
  }
}