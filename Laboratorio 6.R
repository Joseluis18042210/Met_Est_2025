##====================================================================
#correlacion
# Datos del geyser old faithful
# 24/09/2025
# =========================================


data("faithful")
plot(faithful$waiting, faithful$eruptions,
     xlab = "Tiempo de espera (min)",
     ylab = "Erupicion (min)",
     col = "skyblue",
     pch = 20)

# Funcion de correlacion de las dos variables 

shapiro.test(faithful$eruptions)
shapiro.test(faithful$waiting)

# Pearson solo se utiliza cuando tenemos datos normales
cor.test(faithful$waiting,faithful$eruptions,
         method = "pearson")
# Obtuvimos una correlacion de 0.9008 la cual es una correlacion muy alta.


#====================================================
#spearman se utiliza como contraparte para datos no normales

cor.test(faithful$waiting, faithful$eruptions,
         method = "spearman")
