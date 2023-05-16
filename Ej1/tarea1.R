setwd("U:")
# Desafio 1: Hacer 10 ejemplos de operaciones

op1 = 320 * 102
op2 = op1 / 10
op3 = log10(op2) + op1 - ( op1/300 )
op4 = op3 + sqrt(op2) + sin(op1)
op5 = round((op1 - op2 * 3/op4 - op3/( op1 / 2 )), 2)
op6 = 200 - 100 * 42 / cos(10)
op7 = op6 - op5
op8 = op6 + 4 * op7
op9 = op7 - 2/op8
op10 = 100 / ( cos(3) * sin(8) )

# Desafio 2: Ejemplos usando otros tipos de estructura de datos

csv = read.csv("U:/Codigo/R/prueba.csv", header=TRUE, sep=";")

sec_num = 1:30

generos = c("L", "G", "B", "T", "Q", "Q", "Q", "L", "G")
generos = factor(generos)
generos = factor(generos, levels=c("L", "G", "B", "T", "Q"), 
                 labels=c("Lesbian", "Gay", "Bisexual", "Transexual", "Queer"))

# Desafio 3: Piedra papel o tijeras

cachipum = function () {
  cat("1.- Piedra\n2.- Papel\n3.- Tijera")
  user = as.integer(readline(prompt = "Ingresa una opcion: "))
  ia = sample(1:3,1)
  print(paste("Elegiste:", switch(user, "Piedra", "Papel", "Tijera")))
  print(paste("La IA eligio:", switch(ia, "Piedra", "Papel", "Tijera")))
  if ( user == ia ) {
    print("Empate")
  }
  if ( (user == 1 & ia == 2) | (user == 2 & ia == 3) | (user == 3 & ia == 1) ) {
    print("Perdiste")
  }
  if ( (user == 2 & ia == 1) | (user == 3 & ia == 2) | (user == 1 & ia == 3) ) {
    print("Ganaste")
  }
}

# Desafio 4:Usar un dataset y Describir para que se usa y cada variable, usar plot y summary

example1 = datasets::beaver
data(beavers)
summary(example1)
plot(example1)

# Desafio 5: Documentar lo anterior