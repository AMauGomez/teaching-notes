library(tidyverse)
library(readxl)
library(GGally)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(stats)
library(FactoMineR)
library(factoextra)
library(psych)
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(stats)
library(FactoMineR)
library(factoextra)
library(psych)
library(ggcorrplot)
library(ggplot2)
library(GGally)
library(GPArotation)
library(qgraph)
library(corrplot)
library(regclass)
library(tidyverse)
library(readxl)
library(GGally)
library(ggplot2)
library(car)
library(readxl)
library(readr)
library(pls)
library(FactoMineR)
library(stats)
library(factoextra)
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(stats)
library(FactoMineR)
library(factoextra)
library(psych)
library(Metrics)
datos<-read.csv("datos/CarPrice_Assignment.csv")
datos<-datos[,10:22]



###Stepwise Regression##

###stepwise regression

lm_vacio<-lm(price~1, datos)
summary(lm_vacio)



lm_completo<-lm(price~., datos)
summary(lm_completo)


##Fordward

lm_forward<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "forward", trace=T)
summary(lm_forward)

AIC(lm_forward)

##Backward

lm_backward<-step(lm_completo, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "backward", trace=F)
summary(lm_backward)
AIC(lm_backward)

###Both


lm_stepwise<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "both", trace=F)
summary(lm_stepwise)


####Predicciones con Modelos Lineales####

#Dividir los datos en datos de entrenamiento y en datos de prueba
#Normalmente 80-20 o 70-30

data.samples <- sample(1:nrow(datos), nrow(datos) * 0.7, replace = FALSE)
training <- datos[data.samples, ]
prueba <- datos[-data.samples, ]


####Entrenamos el modelo####

datos<-training

lm_vacio<-lm(price~1, datos)
summary(lm_vacio)



lm_completo<-lm(price~., datos)
summary(lm_completo)


##Fordward

lm_forward<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "forward", trace=F)
summary(lm_forward)

AIC(lm_forward)


rmse(datos, predicted = (lm_forward))

##Backward

lm_backward<-step(lm_completo, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "backward", trace=F)
summary(lm_backward)
AIC(lm_backward)


acf(lm_backward$df.residual)

bptest(lm_backward)

durbinWatsonTest(lm_backward)
shapiro.test(lm_backward$residuals)

bgtest(lm_backward)

acf(lm_forward$df.residual)

bptest(lm_forward)

shapiro.test(lm_backward$residuals)

durbinWatsonTest(lm_forward)


bgtest(lm_forward)
###Both


lm_stepwise<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "both", trace=F)
summary(lm_stepwise)



###cual es el mejor modelo?


##Hacemos las predicciones###
pred<-predict(lm_backward, prueba)

pred

prueba<-cbind(prueba,pred)


####Calculo de los errores##


##RMSE

rmse(prueba$price, prueba$pred)

###

##MAE##

mae(prueba$price, prueba$pred)



####Modelo hecho en clase##

lm_2<-lm(price ~ enginesize+ peakrpm + stroke + 
           compressionratio + carwidth + horsepower, data = training)
summary(lm_2)


pred2<-predict(lm_2, prueba)

pred2

prueba<-cbind(prueba,pred2)


##RMSE

rmse(prueba$price, prueba$pred2)


##MAE##

mae(prueba$price, prueba$pred2)








###Varaibles Dummy##

datos<-read.csv("datos/CarPrice_Assignment.csv")


 ##Las variable dummy o  variables ficticias nos ayudan a poder incorporar variable categoricas
##al análisis de regresión Lineal ##

##Al incluir una variable categorica en la regersión se incluyen todas las categorías de la variable
##aunque no todas las categorias sean significativas##

##Existe una libreria que crea las variables dummy de forma rapida

library(fastDummies)

###Quitamos la marca porque son muchas categorias##

datos<-datos[,2:22]

datos<-dummy_cols(datos)

###Seleccionamos las variables que nos interesen sin duplicar la variable original y las ficticias## 

datos_1<-datos[,8:50]

##Ahora ya es posible ingresar las variables categoricas como caractricsticas que#
#se tienen o no se tienen en cierta variable#3


###¿Obtendremos mejores resultados?##
#Dividimos la muestra para entrenar y testear#

data.samples <- sample(1:nrow(datos_1), nrow(datos_1) * 0.7, replace = FALSE)
training <- datos_1[data.samples, ]
prueba <- datos_1[-data.samples, ]


####Entrenamos el modelo####

datos_1<-training

lm_vacio<-lm(price~1, datos_1)
summary(lm_vacio)


lm_completo<-lm(price~., datos_1)
summary(lm_completo)


##Fordward

lm_forward<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "forward", trace=F)
summary(lm_forward)

AIC(lm_forward)

##Backward

lm_backward<-step(lm_completo, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "backward", trace=F)
summary(lm_backward)
AIC(lm_backward)

###Both


lm_stepwise<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "both", trace=F)
summary(lm_stepwise)

AIC(lm_stepwise)
AIC(lm_backward)
AIC(lm_forward)


###HACEMOS LAS PREDICCIONES#3

pred<-predict(lm_forward, prueba)

pred

prueba<-cbind(prueba,pred)


####Calculo de los errores##


##RMSE

###Sum

rmse(prueba$price, prueba$pred)


##MAE##

mae(prueba$price, prueba$pred)


##DISTANCIA DE COOCK

head(cooks.distance(lm_forward))


plot(cooks.distance(lm_forward))







