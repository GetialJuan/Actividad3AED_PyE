#------------------------------------------------------#
#--         ACTIVIDAD_3_ANALISIS_EXPLORATORIO        --#
#-- Universidad del Valle: Escuela de Estadistica     --#
#-- Asignatura: Probabilidad y Estad�stica            --#
#-- Profesor: Ivan Mauricio Bermudez Vera             --#
#-- Estudiantes:                                      --#
#--   Juan Sebastian Getial Getial <2124644>          --#
#--   Mauricio Muñoz Gutierez <----------->          --#
#--   Brayan Andres Sanchez Lozano <------->          --#
#------------------------------------------------------#

#Se suben los datos
datos <- read.table("data_embutidos.txt", header=TRUE, dec=".")

#Espesificaciones del peso -> 220 ± 10 gr
#Limites
limite_superior = 220  + 10
limite_inferior = 220 - 10

#Valores maximos y minimos
max = max(datos$peso)
min = min(datos$peso)

#boxplot del peso

x11()
boxplot(datos$peso,
        ylab="gr",
        ylim=c(min-5,max+5),
        col="gray"
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#boxplot del peso segun operario
x11()
boxplot(datos$peso ~ datos$operario,
        ylab="gr",
        names=c("Op A", "Op B"),
        main="pesos segun op",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#boxplot del peso segun maquina
x11()
boxplot(datos$peso ~ datos$maquina,
        ylab="gr",
        names=c("M1", "M2"),
        main="pesos segun maquina",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#boxplot peso segun operario y maquina
library(dplyr)#Se debe instalar primero

opA <- dplyr::filter(datos, operario == "A") 
opB <- dplyr::filter(datos, operario == "B")


x11(); par(mfrow=c(1,2))
#boxplot opA segun maquina
boxplot(opA$peso ~ opA$maquina,
        ylab="gr",
        names=c("M1", "M2"),
        main="pesos segun opA y maquina",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#boxplot opB segun maquina
boxplot(opB$peso ~ opB$maquina,
        ylab="gr",
        names=c("M1", "M2"),
        main="pesos segun opB y maquina",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

##Viendo los dos ultimos boxplots se puede observar que la
##maquina 2 es la que parece ser la fuente del problema


