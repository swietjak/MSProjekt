obroty = c(575.6, 250.8, 271.8, 337, 198.6, 361.7, 361.5, 447.2, 345, 455.9, 393.1, 368.3, 400, 449.7, 252.7, 437, 290.1, 297.7, 525.5, 411, 419.4, 530, 409.6, 474, 303.7, 246.8, 458.4, 480.3, 199.6, 461.9, 321.5, 305.7, 379.8, 493.2, 294, 322.9, 368.5, 262.4, 494.9, 446.7, 322.8, 475.8, 320, 252.6, 353.5, 303.3, 396.6, 255.6, 540.6)

#zadanie 4
obrotyWariancja = var(obroty)
obrotyDlugosc = length(obroty)
obrotyOdchylenie = sd(obroty)

obrotyDolnaGranica = obrotyDlugosc * obrotyWariancja / qchisq(0.975, obrotyDlugosc - 1)
obrotyGornaGranica = obrotyDlugosc * obrotyWariancja / qchisq(0.025, obrotyDlugosc - 1)

precyzja = (obrotyGornaGranica - obrotyDolnaGranica) * 100 / obrotyOdchylenie ** 2
print("Precyzja: ")
precyzja

chikwadrat = ((obrotyDlugosc - 1) * obrotyOdchylenie ** 2) / obrotyWariancja

if (chikwadrat > obrotyDolnaGranica && chikwadrat < obrotyDolnaGranica) {
  print("mozna przyjac oszacowanie")
} else {
  print("nie mozna przyjac oszacowania")
}