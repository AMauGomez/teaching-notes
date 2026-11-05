library(tidyverse)
library(mice)
library(lattice)
library(pan)
library(ggmice)
library(readxl)#xlsx, s
library(naniar)
library(MASS)
library(psych)###AD
library(dplyr)
library(GGally)
library(overdisp)
library(AER)
library(performance)
library("DHARMa")
library("glmmTMB")
library("pscl")
library("gamlss")
library(caTools)
library(psych)
library("xtable")
library(psych)#Sirve para el describir 
library(corrplot)#
library("andrews")#

#Cargamos los datos#
datos<-read.csv("datos/Niveles de Obesidad.csv")
#datos<-datos[,2:17]
datos<-na.omit(datos)

datos$Gender<-if_else(datos$Gender=="Male", 1, 0)
variables<-colnames(datos)
variables_num<-variables[c(2,3,4,7,8,11,13,14)]
variables_num<-as.data.frame(variables_num)
a<-datos[,2:4]
b<-datos[,7:8]
c<-datos[,11]
d<-datos[,13:14]
data_num<-cbind.data.frame(a,b,c,d)


#Analsis descriptivo#
descrip_1<-describeBy(datos$Gender, datos$NObeyesdad, mat=TRUE)#psych
descrip_1<-as.data.frame(descrip_1)
descrip_1

summary(datos)

x11()
plot_0<-ggpairs(datos,columns=5:8, aes(color=datos$NObeyesdad,alpha=0.5),
                upper= list(continuous= wrap("cor",size=2.5)))
plot_0

##Comparación de medias entre variables (deben estar en la misma unidad o estandarizados)##
media_1<-boxplot(datos$Weight, datos$Age, datos$Height)
media_1


#estadísticas descriptivas##
descrip<-describeBy(datos$NCP, datos$NObeyesdad, mat=TRUE)
descrip<-as.data.frame(descrip)
descrip


variables_1<-variables[c(2,4,6,10,14)]

plot_1<-ggpairs(datos,columns=5:8, aes(color=datos$NObeyesdad,alpha=0.5),
        upper= list(continuous= wrap("cor",size=2.5)))

plot_1



plot_2<-ggpairs(datos,columns=8:11, aes(color=datos$NObeyesdad,alpha=0.5),
                upper= list(continuous= wrap("cor",size=2.5)))
plot_2


##Curvas de Andrews#

andrews_1<-andrews(data_num, clr=5)

andrews_1


x11()
plot(datos$Height, datos$Weight)

plot(density(datos$Height))


#####Pruebas de Normalidad####

library(MVN)

mardia(data_num)
