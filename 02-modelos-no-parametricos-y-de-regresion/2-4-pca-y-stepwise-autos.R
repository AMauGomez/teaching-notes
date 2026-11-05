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

datos<-read.csv("datos/CarPrice_Assignment.csv")

str(datos)

datos<-datos[, 9:22]

lm_1<-lm(price~datos$horsepower+datos$enginesize+datos$peakrpm, data=datos)

summary(lm_1)

confint(lm_1)

lm_2<-lm(price~datos$horsepower+datos$enginesize+datos$boreratio, data=datos)

summary(lm_2)

confint(lm_2)

##Creamos el PCA

PCA_centrado=prcomp(datos, center=TRUE, scale=TRUE)
PCA_centrado
summary(PCA_centrado)


x11()
fviz_screeplot(PCA_centrado, main="PCA centrado y reescalado",  addlabels = TRUE)



x11()
fviz_pca_var(PCA_centrado,col.var="contrib",
             gradient.cols = c("green", "blue"),
             repel = TRUE)



x11()
fviz_pca_ind(PCA_centrado, col.ind = "cos2", 
             gradient.cols = c("gray", "purple", "red"),
             repel = TRUE)



###Creamos el  modelo a ojo

LM_CLASE<-lm(price~ datos$carlength+ datos$curbweight+datos$horsepower, data=datos)
summary(LM_CLASE)

x11()
plot(LM_CLASE)

pairs(datos)
LM_CLASE_1<-lm(price~ datos$fueltype+datos$enginesize+datos$carwidth+datos$highwaympg+datos$citympg, data=datos)
summary(LM_CLASE_1)



###Creamos el modelo con las componentes

modelo_1 <- pcr(datos$price ~ ., 
                data = datos, scale=TRUE, validation="CV", ncomp=2)
summary(modelo_1)





###stepwise regression

lm_vacio<-lm(price~1, datos)
summary(lm_vacio)

datos<-datos[,9:22]

lm_completo<-lm(price~., datos)
summary(lm_completo)


##Fordward

lm_forward<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "forward")
summary(lm_forward)


confint(lm_forward)

##Backward

lm_backward<-step(lm_completo, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "backward")
summary(lm_backward)



AIC(lm_forward)
AIC(lm_backward)

BIC(lm_forward)
BIC(lm_backward)


summary(lm_forward)
confint(lm_forward)

###Both


lm_stepwise<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "both")
summary(lm_stepwise)

lm_2<-lm(price ~ enginesize+ peakrpm + stroke + 
           compressionratio + carwidth + horsepower, data = datos)
summary(lm_2)


VIF(lm_2)



a<-predict(lm_forward)
datos<-cbind(datos,a)
cbind(a, datos)





####Ascombe###




















