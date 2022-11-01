#------------------------------------------------------#
#--         ACTIVIDAD_3_ANALISIS_EXPLORATORIO        --#
#-- Universidad del Valle: Escuela de Estadistica    --#
#-- Asignatura: Probabilidad y Estadistica           --#
#-- Profesor: Ivan Mauricio Bermudez Vera            --#
#-- Estudiantes:                                     --#
#--   Juan Sebastian Getial Getial <2124644>         --#
#--   Mauricio Muñoz Gutierez <----------->          --#
#--   Brayan Andres Sanchez Lozano <2128974>         --#
#------------------------------------------------------#

#Se suben los datos
datos <- read.table("data_embutidos.txt", header=TRUE, dec=".")

#Especificaciones del peso -> 220 ± 10 gr
#Limites
limite_superior = 220  + 10
limite_inferior = 220 - 10

#Valores maximos y minimos
max = max(datos$peso)
min = min(datos$peso)

#box plot del peso
x11()
boxplot(datos$peso,
        ylab="gr",
        ylim=c(min-5,max+5),
        col="gray",
        main="Peso de los embutidos"
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#box plot del peso segun operario
x11()
boxplot(datos$peso ~ datos$operario,
        ylab="gr",
        names=c("Operario A", "Operario B"),
        main="Peso según operario",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#box plot del peso segun máquina
x11()
boxplot(datos$peso ~ datos$maquina,
        ylab="gr",
        names=c("Máquina 1", "Máquina 2"),
        main="Peso segun máquina",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#box plot peso segun operario y máquina
library(dplyr) #Se debe instalar primero

opA <- dplyr::filter(datos, operario == "A") 
opB <- dplyr::filter(datos, operario == "B")

#box plot operario A segun máquina
x11(); par(mfrow=c(1,2))
boxplot(opA$peso ~ opA$maquina,
        ylab="gr",
        names=c("Máquina 1", "Máquina 2"),
        main="Peso según operario A y máquina",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#box plot operario B segun máquina
boxplot(opB$peso ~ opB$maquina,
        ylab="gr",
        names=c("Máquina 1", "Máquina 2"),
        main="Peso según operario B y máquina",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior, limite_superior, 31.5),lty=2,col="Red")

#grafico con las cuatro cajas.
x11()
boxplot(datos$peso ~ datos$maquina * datos$operario,
        ylab="gr",
        xlab = "Operario - Maquina",
        names=c("A-1", "A-2", "B-1", "B-2"),
        main="Peso segun operario y maquina",
        ylim=c(min-5,max+5)
)
abline(h=c(limite_inferior,limite_superior,31.5),lty=2,col="Red")