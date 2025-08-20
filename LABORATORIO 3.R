# Laboratorio Semana 3 
# 20/08/2025
# Jose luis Alvarez Ocañas 


# Importar Datos  -------------------------------------------------------

Temp <- read.csv("Data/temperatura (1).csv", header = T)

# #Graficar datos  --------------------------------------------------------

#Ingresar datos de manera manuaul

edad <- c(18,19,18,18,25,19,18,18,18,17,19,19,
          18,17,19,18,19,19) 
alumno <- seq(1,18,1)

info <-data.frame(alumno, edad)

info$Altura <- c(174,174,170,160,158,155,188,170,175,170,172,170,174,180,158,
                 161,188,164)

boxplot(info$Altura,
        # col sirve para ponerle un color 
        col = "yellow",
        # Main sirve para poner un titulo en el boxtplot
        main = "Clase 3 semestre") 

colores = c("green", "white","red")
boxplot(datos_meses,
        col = colores)


# Estadisticas descriptivas  ----------------------------------------------
