#Pruebas de t
#Caso de muestras independientes 
#JLAO
# 27/08/2025

#IMPORTAR DATOS DE INDICE DE CALIDAD

calidad <- read.csv("ejemplo_2 tagle datos.csv", header = T)

calidad$Tratamiento<- as.factor(calidad$Tratamiento)

colores <- c("yellow","skyblue")
boxplot(calidad$IE ~ calidad$Tratamiento,
        col= colores,
        xlab = "tratamientos",
        ylab = "indice calidad",
        ylim= c(0.4,1.2),
        main = "vivero Iturbide")

#Estadisticas descriptivas 
#tapply sirve para obtener un valor cuando hay dos grupos, osea media.

tapply(calidad$IE, calidad$Tratamiento,mean)
tapply(calidad$IE, calidad$Tratamiento, var)

#observamos que la varianza del grupo fert
#es 3 veces mas grande que el grupo de control (Ctrl)

# revisar el comportamiento de los datos
library("ggplot2")

ggplot(calidad, aes(x=IE, color = Tratamiento))+geom_density()
ggplot(calidad, aes(x=IE, color = Tratamiento))+geom_histogram()













