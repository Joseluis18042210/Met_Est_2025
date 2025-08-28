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
tapply(calidad$IE, calidad$Tratamiento, sd)

#observamos que la varianza del grupo fert
#es 3 veces mas grande que el grupo de control (Ctrl)

# revisar el comportamiento de los datos
library("ggplot2")

ggplot(calidad, aes(x=IE, color = Tratamiento))+geom_density()
ggplot(calidad, aes(x=IE, color = Tratamiento))+geom_histogram()

#separar los datos por tratamiento 

df_ctlr <- subset(calidad, Tratamiento == "Ctrl")
df_fert <- subset(calidad, Tratamiento != "Ctrl") 

#qqnorm sirve para revisar normalidad

par(mfrow = c(1,2))
qqnorm(df_ctlr$IE);qqline(df_ctlr$IE)
qqnorm(df_fert$IE);qqline(df_fert$IE)
par(mfrow = c(1,1))

#prueba de normalidad shapiro

shapiro.test(df_ctlr$IE)
shapiro.test(df_fert$IE)

#revisar homogenidad de varianzas 
var.test(df_ctlr$IE,df_fert$IE)
var.test(calidad$IE ~calidad$Tratamiento)

# APLICAR LA PRUEBA DE T, VARIANZAS IGUALES
# DOS colas = two.sided

t.test(calidad$IE ~ calidad$Tratamiento,
       var.equal = T,
       alternative = "two.sided")

cohens_efecto <- function(x,y){
  n1 <- length(x);n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y)
  sp <- sqrt(((n1 - 1)* s1^2 + (n2-1) * s2^2)/ (n1 +n2 -2))
  (mean(x)- mean(y))/sp
}

d_cal <- cohens_efecto(df_ctlr$IE,df_fert$IE) 







