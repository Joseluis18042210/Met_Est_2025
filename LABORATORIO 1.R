# Gastos mensuales 
300 + 240 + 1527 + 400 + 1500 + 1833 
celular <- 300
celular
transportes <- 240
comestibles <- 1527
gimnasio <- 400
alquiler <- 1500
otros <- 1833
total <-celular+transportes+comestibles+gimnasio+alquiler+otros
semestre <- total*5
anual <- total*10

#valor absoluto
abs(10)
abs(4)

#raiz cuadrada
sqrt(9)

#logaritmo natural 
log(2)

2*9
4+5 # tambien se puede colocar un comentario

celular <- 300
Celular <-- 300
CELULAR <- 8000

celular+Celular
CELULAR-celular
help(abs)
help(mean)
?abs
?mean
help.search("absolute")


gastos <- c(celular, transportes, comestibles, gimnasio, alquiler, otros)
gastos 

barplot(gastos)

sort(gastos)
gastos_ord <- sort(gastos, decreasing = TRUE)
barplot(gastos_ord)
help("barplot")
barplot(gastos_ord, main = "gastos mensuales",
        names.arg = c("otros", "comestibles", "alquiler", "gimnasio", "celular", "transportes"))


































