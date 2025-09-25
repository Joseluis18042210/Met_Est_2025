# Ejercicio de tagle codigo R
#Jose Luis ALvarez oca
# 25/09/2025

resp <- data.frame(
  Tiempo = c(12, 15, 17, 18, 20, 21, 22, 26),
  Edad = c(13,25,20,35,45,30,60,95)
)
resp

#Crear nuevas columnas con los rangos (1 a 8)
resp$Rango_tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <- rank(resp$Edad, ties.method = "first")

#ver resultado 
resp

#graficos
plot(resp$Tiempo, resp$Edad)
plot(resp$Rango_tiempo, resp$Rango_Edad)

resp$dif <-resp$Rango_tiempo - resp$Rango_Edad
resp$dif2 <- resp$dif^2
sum(resp$dif2)

cor.test(resp$Rango_tiempo, resp$Rango_Edad, method = "spearman")
cor.test(resp$Tiempo,resp$Edad, method = "spearman")

#Ejemplo Tau Kendall

tau <- data.frame(
  A = c(1,2,3,4,5,6),
  B = c(3,1,4,2,6,5)
)
cor.test(tau$A, tau$B, method = "kendall")
)

# Punto biserial

set.seed(123) # Para reproductiblidad 


#Numero de observaciones 
n <- 20

# Generar horas de estudio (entre 1 y 10)
Horas_estudio <- sample(1:10, n, replace = TRUE)
#Asignar probabilidad de aprobar en funcion de horas de estudio 
# A mas horas, mas alta la probabilidad 

Resultado <- sapply(Horas_estudio, function(horas) {
  ifelse(runif(1) < (horas / 10), "Aprobado", "Reprobado")
})
estudio <- data.frame(
  Estudiante = 1:n,
  Horas_estudio,
  Resultado
)
#crear variable dicotomica: 1 = aprobado, 0 = Reprobado
estudio$Resultado_bin <- ifelse(estudio$Resultado == "Aprobado", 1, 0)
head(estudio)

cor.test(estudio$Horas_estudio, estudio$Resultado_bin, method = "pearson")

mean_aprobados <- mean(estudio$Horas_estudio[estudio$Resultado == "Aprobado"])
mean_aprobados

mean_reprobados <-mean(estudio$Horas_estudio[estudio$Resultado == "Reprobado"])
mean_reprobados

sd(estudio$Horas_estudio)
N <- length(estudio$Estudiante)
n_aprobados <- length(estudio$Horas_estudio[estudio$Resultado == "Aprobado"])
n_reprobados <- length(estudio$Horas_estudio[estudio$Resultado == "Rerpobado"])

rpb <- ((mean_aprobados-mean_reprobados)/sd(estudio$Horas_estudio)) * sqrt(n_aprobados*n_reprobados/N^2)
rpb
