# Actividad 7 (Asignacion 5)
#02/10/2025
#Jose luis alvarez Ocañas
# Instrucciones
#Para cada ejercicio * Examinar la relación que existe entre dos muestras mediante una correlación, *
#Explore los datos gráficamente y explique, * Establezca la Hipótesis nula y la Hipótesis alternativa,
#Aplique la prueba correspondiente, * Reporte los datos (indicar valor de r, grados de libertad y
#probabilidad, así como la significancia de la correlación)

#Datos: velocidad del arroyo (Speed) y abundancia de efímeras (Abundance).
# Ejercicio 1: Correlación entre velocidad y abundancia de efímeras
speed <- c(2, 3, 5, 9, 14, 24, 29, 34)
abundance <- c(6, 3, 5, 23, 16, 12, 48, 43)

# Gráfico de dispersión
plot(speed, abundance,
     main = "Relación entre velocidad del arroyo y abundancia de efímeras",
     xlab = "Velocidad del arroyo",
     ylab = "Abundancia de efímeras",
     pch = 19, col = "red")

# Calcular correlación de Pearson
cor.test(speed, abundance, method = "pearson")


#Datos: propiedades del suelo (pH, N, Densidad, P, Ca, Mg, K, Na, Conduc).
# Ejercicio 2: Correlaciones de propiedades del suelo
suelo <- data.frame(
  Gp = c("T0","T0","T0","T0","T1","T1","T1"),
  Block = c(1,2,3,4,1,2,3),
  pH = c(5.40,5.65,5.14,5.14,5.14,5.10,4.70),
  N = c(0.188,0.165,0.260,0.169,0.164,0.094,0.100),
  Dens = c(0.92,1.04,0.95,1.10,1.12,1.22,1.52),
  P = c(215,208,300,248,174,129,117),
  Ca = c(16.35,12.25,13.02,11.92,14.17,8.55,8.74),
  Mg = c(7.65,5.15,5.68,7.88,8.12,6.92,8.16),
  K = c(0.72,0.71,0.68,1.09,0.70,0.81,0.39),
  Na = c(1.14,0.94,0.60,1.01,2.17,2.67,3.32),
  Conduc = c(1.09,1.35,1.41,1.64,1.85,3.18,4.16)
)

# Matriz de correlaciones
cor_matrix <- cor(suelo[,3:11], method = "pearson")
cor_matrix

# Test de significancia para todas las correlaciones
library(Hmisc)
rcorr(as.matrix(suelo[,3:11]))

# Gráfico de correlaciones
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

#Recreación de los 4 conjuntos de datos y sus gráficos.
# Ejercicio 3: El cuarteto de Anscombe
x <- c(10,8,13,9,11,14,6,4,12,7,5)

y1 <- c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)
y2 <- c(9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.10,9.13,7.26,4.74)
y3 <- c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
x4 <- c(8,8,8,8,8,8,8,19,8,8,8)
y4 <- c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.50,5.56,7.91,6.80)

# Crear data frame
anscombe <- data.frame(
  x = c(x,x,x,x4),
  y = c(y1,y2,y3,y4),
  grupo = rep(c("I","II","III","IV"), each = 11)
)

# Gráficos de dispersión
library(ggplot2)
ggplot(anscombe, aes(x=x, y=y)) +
  geom_point(color="yellow", size=3) +
  geom_smooth(method="lm", se=FALSE, color="brown") +
  facet_wrap(~grupo) +
  labs(title="Cuarteto de Anscombe", x="x", y="y")
# Calcular correlaciones
tapply(1:nrow(anscombe), anscombe$grupo, function(i) cor(anscombe$x[i], anscombe$y[i]))


#E1Correlación velocidad vs. abundancia de efímeras
#r = 0.844
#p-value = 0.0084
#Conclusión: Existe una correlación positiva fuerte 
#y estadísticamente significativa entre la velocidad del arroyo y la abundancia de efímeras (p < 0.05).

#EJERCIO 2 
#Correlaciones entre propiedades del suelo:
#Ejemplo de pares relevantes:
#pH – N → r = 0.388, p = 0.389 (no significativa)
#pH – Dens → r = -0.774, p = 0.041 (significativa)
#pH – Na → r = -0.711, p = 0.073 (tendencia, pero no significativa)
#N – P → r = 0.941, p = 0.0016 (muy significativa)
#Dens – Conduc → r = 0.958, p = 0.0007 (muy significativa)
#Na – Conduc → r = 0.923, p = 0.003 (muy significativa)

#E3
#Resultados de las correlaciones de cada conjunto
#Grupo I → r = 0.816, p = 0.0022 (significativa)
#Grupo II → r = 0.816, p = 0.0022 (significativa)
#Grupo III → r = 0.816, p = 0.0022 (significativa)
#Grupo IV → r = 0.817, p = 0.0022 (significativa)
