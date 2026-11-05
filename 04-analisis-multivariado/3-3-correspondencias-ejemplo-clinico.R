library(ca)
library(gplots)
library(graphics)
library(FactoMineR)#
library(factoextra)#
library(dplyr)
library(cabootcrs)#tablas contingencia



#Caragamos la matriz de datos obtenidos a la pregunta a pacientes post hospitalarios sobre con cual
#medicamento sintieron una mejoría en el dolor post operatorio, registrando tambien su tipo de operación.

datos<- matrix(c(35, 42, 80, 52, 
                      80, 37, 40, 10,
                     50, 60, 35, 97),nrow=3,byrow=T)
dimnames(datos)<- list(Tipo_de_Cirugía=c("Local","Abierta","Interna"),
                           Medicamento=c("Naproxeno", "Ibuprofeno", "Diclofenaco", "Paracetamol"))
addmargins(datos)
datos<-as.table(datos)

####Hay que verificar que exista relación entre las variables###
###Chi Cuadrada##


chisq.test(datos)

##Existen muchas otras pruebas de correlación##

#Fisher
#Mcnhemar
#Kendall
#Mathews
#Phi

###Tabla de Proporciones##
##freq Relativa##
prop.table(datos)

###Por columna

prop.table(datos, 2) 

###Graficamente##


x11()
mosaicplot((datos),shade=F)


##Como correlograma##

x11()
balloonplot(t(datos), main ="Cirugías y Medicamentos",
            label = FALSE, show.margins = FALSE)


