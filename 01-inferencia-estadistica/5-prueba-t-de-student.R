###Cargamos las librerias a utilizar
library(tidyverse)
library(readxl)
library(GGally)
library(ggplot2)
#Cargamos los datos
datos<-read_xlsx("datos/wine.xlsx")
datos$Class<-factor(datos$Class)
str(datos)
#Creamos subconjuntos dependiendo del vinedo de procedencia
vinedo_1.1<-subset(datos, datos$Class==1.1)
vinedo_1.2<-subset(datos, datos$Class==1.2)
vinedo_1<- merge(x = vinedo_1.1, y = vinedo_1.2, all = TRUE)
vinedo_2<-subset(datos, datos$Class==2)
vinedo_3<-subset(datos, datos$Class==3)

#Para el primer viñedo se usaron dos tipos de uvas (verdes y moradas)
#se hizo un muestreo aletorio simple en los vinos elabrorados con uvas de diferente color
#Se seleccionaron 30 vinos (las 30 primeras observaciones)elaborados con uva verde (1) y 30 vinos
#elaborados con uvas moradas (2)
#¿Existe una diferencia significativa en las propiedades de los vinos dependiendo
# el color de la uva?

#Visualizamos los datos de las variables a estudiar "Alcohol", "Ash", "Magnesium" 
#"Proanthocyanins" y "Proline"

variables<-colnames(vinedo_1)
variables_1<-variables[c(2,4,6,10,14)]
variables_1
x11()
ggpairs(vinedo_1,columns=variables_1,aes(color=Class,alpha=0.5),
        upper= list(continuous= wrap("cor",size=2.5)))


#¿Que podemos deducir de la grafica?


#Hacemos una prueba t de medias que toma en cuenta el tamaño de las muestra  (datos pareados)y que la varianza 
#en cada grupo es la misma y que provienen de distribuciones Normales
#Primera obtendremos la media de todo el viñedo 1 y esa será nuestra 
# hipotesis, es decir, estamos suponiendo que el color de la uva
#No influye en las caracteristicas del vino del viñedo 1

mu_1<-mean(vinedo_1$Proanthocyanins)
mu_1

#Por lo cual nuestra hipotesis nula es que la media mu_1
t_1<-t.test(vinedo_1.1$Proanthocyanins, mu=mu_1)
t_1

t_2<- t.test(vinedo_1.2$Proanthocyanins, mu=mu_1)
t_2


#Queremos analizar si existe diferencia entre los valores obtenidos para los diferentes 
#viñedos en las variables: Alcohol, Ash, Magnesium, non flavo, protoan, pronilne.

