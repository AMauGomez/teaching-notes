library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(dplyr)

datos_AL<-read.table("datos/nacimientos_mujeres_CA.txt")
datos_AL<-datos_AL/1.45
datos_AL<-datos_AL[1:168,]
datos_AL<-as.data.frame(datos_AL)
datos_AL<-as.numeric(datos_AL$datos_AL)

datos_NY<-read.table("datos/new_york_births.txt")
datos_NY<-as.data.frame(datos_NY)
datos_NY<-datos_NY %>% rename(datos_NY=V1)
datos_NY<-as.numeric(datos_NY$datos_NY)

datos<-cbind(datos_AL, datos_NY)
datos<-as.data.frame(datos)
#write.csv(datos, "nacimientos_AL_NY.csv")



###Milk y pigs
file.choose()

pigs<-read.table("datos/pigs.txt")

pigs<-pigs[1:120,]

milk<-read.table("datos/milk.txt")
milk<-milk*100
milk<-milk[1:120,]

datos_1<-cbind(pigs, milk)
#write.csv(datos_1, "Production milk and pigs slaughtered in Manchester 1980 1990.csv")
