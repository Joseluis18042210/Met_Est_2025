# Asignacion 3: Contraste de medidas 
# Jose Luis Alvarez Ocañas 
# Ingenieria forestal 
# 03/09/2025

#EL primer paso es encontrar la base de datos Iris aqui en R 
library(datasets)
data("iris")
summary(iris)
summary(iris$Petal.Length)
head(iris)
head(iris$Petal.Length)
# La funcion aggreagate sirve para realizar un resumen por especie.
aggregate(Petal.Length ~ Species, data =iris[iris$Species %in% c("versicolor", "virginica"),], summary)

#Pruebas de estadisticas 
#¿Hay diferencias notables entre las especies vesticolor y virginica?

#Datos
iris_sub <- subset (iris, Species %in% c("versicolor","virginica"))
versicolor <- iris_sub$Petal.Length[iris_sub$Species == "versicolor"]
virginica <- iris_sub$Petal.Length[iris_sub$Species == "virginica"]

#La funcion shapiro para calcular la normalidad 
shapiro.test(versicolor)
shapiro.test(virginica)

#Analizar la variancia y la homogenialidad
var.test(versicolor,virginica)

#Prueba de t 
t.test(versicolor,virginica,
       alternative = "two.sided",
       var.equal = FALSE,)

# COHENS 
if(!require(effsize)) install.packages("effsize")
library(effsize)
cohen.d(versicolor, virginica)

# BOXPLOT
boxplot(Petal.Length ~ Species, data = iris_sub,
        main = "comparacion de petal.length",
        xlab = "Especie",
        ylab = "petal.lenght",
        col = c("blue","green"))
