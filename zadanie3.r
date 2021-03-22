install.packages("nortest")
library("nortest")
koszty = c(449.6, 315.1, 447.2, 258.3, 391.3, 297.2, 295.3, 303.4, 312.3, 369.3, 385.8, 466, 302.1, 304.3, 359.5, 309.2, 371.7, 504.6, 414.5, 321.9, 386.2, 489, 514.6, 249.8, 184.7, 452.8, 513.2, 540.4, 458.1, 459.4, 259.5, 296.4, 397.1, 525.7, 331.3, 232.4, 222.4, 418.9, 265.5, 282.8, 543.6, 387.8, 343.1, 351.6, 440.9, 403.3, 370.2, 207.6, 223.3, 304.3, 283.9)
obroty = c(575.6, 250.8, 271.8, 337, 198.6, 361.7, 361.5, 447.2, 345, 455.9, 393.1, 368.3, 400, 449.7, 252.7, 437, 290.1, 297.7, 525.5, 411, 419.4, 530, 409.6, 474, 303.7, 246.8, 458.4, 480.3, 199.6, 461.9, 321.5, 305.7, 379.8, 493.2, 294, 322.9, 368.5, 262.4, 494.9, 446.7, 322.8, 475.8, 320, 252.6, 353.5, 303.3, 396.6, 255.6, 540.6)
#Zadanie 3

zadanie3 <- function(koszty) {

  koszty = sort(koszty)
  kosztyLiczebosc = length(koszty)
  kosztySuma = sum(koszty)
  kosztySr = mean(koszty)
  kosztyOdchylStd = sd(koszty)

  cat("Liczebnosc przedzialu: ", kosztyLiczebosc, "\n")
  cat("Suma wartosci:", kosztySuma, "\n")
  cat("Srednia: ", kosztySr, "\n")
  cat("Odchylenie standardowe: ", kosztyOdchylStd, "\n\n\n")


  #Wyznaczenie wspolczynnika t-Studenta

  kosztyWspolczynnikUfnosci = 0.95

  alfa = 1 - kosztyWspolczynnikUfnosci

  kosztyTStudent = qt(alfa, kosztyLiczebosc - 1, lower.tail = FALSE)

  cat("Wspolczynnik t-Studenta ", kosztyTStudent, "\n\n\n")

  #Wyznaczenie przedziału ufności


  kosztyDolnaGranica = kosztySr - kosztyTStudent * (kosztyOdchylStd / (sqrt(kosztyLiczebosc - 1)))
  kosztyGornaGranica = kosztySr + kosztyTStudent * (kosztyOdchylStd / (sqrt(kosztyLiczebosc - 1)))


  cat("Dolna granica przedzialu ufnosci: ", kosztyDolnaGranica, "\n")
  cat("Gorna granica przedzialu ufnosci: ", kosztyGornaGranica, "\n\n\n")


  #Obliczenie względnej precyzji oszacowania

  kosztyPrecyzja = 0.5 * (kosztyGornaGranica - kosztyDolnaGranica) * 100 / kosztySr

  cat("Wzgledna precyzja oszacowania: ", kosztyPrecyzja, "\n")

  #sprawdzenie, czy mamy podstawy do uogólnienia otrzymanego przedziału ufności na całą populację rocznych kosztów działalności?
  #Wzgledna prezycja wynosi około 6%, więc wynik można uogólnić dla całej populacji(z pewnymi zastrzeżeniami)
}
