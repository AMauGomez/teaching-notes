library(tidyverse)
library(mice)
library(lattice)
library(pan)
library(ggmice)
library(readxl)
library(naniar)
library(MASS)
library(psych)
library(dplyr)
library(GGally)
library(overdisp)
library(AER)
library(performance)
library("DHARMa")
library("glmmTMB")
library("pscl")
library("gamlss")
library(countimp)
library(caTools)
library(broom.mixed)
library(lattice)
library(readxl)
library(corrplot)
library("amap")


###Cargamos la matriz de distancias
###El paso más importante es definir la distancia que queremos utilizar##
datos<-read.csv("datos/matrixbrands.csv")
##CAMBIAMOS EL FORMATO##
rownames(datos)<-datos$X

datos<-datos[,2:21]

###Realizamos el escalamiento
esc_mul<-cmdscale(datos, k = 2)
esc_mul


###Graficamos

x11()
plot(esc_mul[,1], esc_mul[,2],
     type = "n")
text(esc_mul[,1], esc_mul[,2],
     labels = row.names(datos))



####Con medida canberra

datos_can<-dist(datos, method="canberra")
datos_can

##Realizamos el escalamiento
esc_mul_can<-cmdscale(datos_can, k = 2)
esc_mul_can

###Graficamos

x11()
plot(esc_mul_can[,1], esc_mul_can[,2])
text(esc_mul_can[,1], esc_mul_can[,2],
     labels = row.names(datos))



####Con medida Manhattan

datos_man<-dist(datos, method="manhattan")
datos_man

##Realizamos el escalamiento
esc_mul_man<-cmdscale(datos_man, k = 2)
esc_mul_man

###Graficamos

x11()
plot(esc_mul_man[,1], esc_mul_man[,2])
text(esc_mul_man[,1], esc_mul_man[,2],
     labels = row.names(datos))





