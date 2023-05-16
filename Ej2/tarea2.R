ej1 = permutations(11,3)
num1 = nrow(ej1)

ej2 = combinations(5,3,c('a','b','c','d','e'))
num2 = nrow(ej2)

ej3 = permutations(39,25)
num3 = nrow(ej3)

ej4 = combinations(39,25)
num4 = nrow(ej4)

permu = factorial(39)/factorial(39-25)
combi = factorial(39)/(factorial(25)*factorial(39-25))

ej5 = permutations(50,50)
num4 = nrow(ej5)

prob1 = 1/factorial(50)
prob2 = 1/(factorial(50)-1)


# COSAS DE LA CLASE 12-05

dbinom(2, 10, 0.25)
dhyper(2, 5, 15, 10)

combinations(10, 3)
