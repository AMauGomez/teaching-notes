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

datos<-read_xlsx("datos/PCOS_data_without_infertility clean.xlsx", sheet = 2)
datos$`AMH(ng/mL)`<-as.numeric(datos$`AMH(ng/mL)`)
datos<-na.omit(datos)

lm_vacio<-lm(datos$`Age (yrs)`~1, datos)
summary(lm_vacio)


lm_completo<-lm(datos$`Age (yrs)`~., datos)
summary(lm_completo)


##FORWARD##

lm_forward<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "forward")
summary(lm_forward)


### Backward

lm_backward<-step(lm_completo, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "backward")
summary(lm_backward)


##both

lm_stepwise<-step(lm_vacio, scope = list(lower=lm_vacio, upper=lm_completo ), direction = "both")


summary(lm_stepwise)

summary(lm_forward)

summary(lm_backward)

##STEPWEISE BOTH##

modelo_lm_stepwise<-lm(datos$`Age (yrs)` ~ `Marraige Status (Yrs)` + `Follicle No. (R)` + 
                         `Hair loss(Y/N)` + `Height(Cm)` + `BP _Diastolic (mmHg)` + 
                         `RBS(mg/dl)` + `AMH(ng/mL)` + 
                         `BP _Systolic (mmHg)` + `Reg.Exercise(Y/N)` + BMI + `Weight (Kg)`, 
                       data = datos)

summary(modelo_lm_stepwise)
vif(modelo_lm_stepwise)
AIC(modelo_lm_stepwise)

##Notamos que otra ya no fue significante##
##Volvemos a sacar la variable no sig#

modelo_lm_stepwise_1<-lm(datos$`Age (yrs)` ~ `Marraige Status (Yrs)` + `Follicle No. (R)` + 
                         `Hair loss(Y/N)` + `Height(Cm)` + `BP _Diastolic (mmHg)` + 
                         `RBS(mg/dl)` + `AMH(ng/mL)` + `Reg.Exercise(Y/N)` + BMI + `Weight (Kg)`, 
                       data = datos)

summary(modelo_lm_stepwise_1)

vif(modelo_lm_stepwise_1)
AIC(modelo_lm_stepwise_1)
##Volvio a sacar una variable significantiva##3

modelo_lm_stepwise_2<-lm(datos$`Age (yrs)` ~ `Marraige Status (Yrs)` + `Follicle No. (R)` + 
                           `Hair loss(Y/N)` + `BP _Diastolic (mmHg)` + 
                           `RBS(mg/dl)` + `AMH(ng/mL)` + BMI + `Weight (Kg)`, 
                         data = datos)

summary(modelo_lm_stepwise_2)

vif(modelo_lm_stepwise_2)
AIC(modelo_lm_stepwise_2)

# de condición


######
#Suponiendo que nos quedamos con los resultados de los stepwise##


summary(lm_forward)

summary(lm_backward)
summary(modelo_lm_stepwise_2)
###AIC##

AIC(lm_forward)
AIC(lm_backward)
AIC(modelo_lm_stepwise_2)

##VIF##

vif(lm_forward)
vif(lm_backward)
vif(modelo_lm_stepwise_2)



##Quitando las variables con multicolinealidad#


modelo_lm_stepwise_3<-lm(datos$`Age (yrs)` ~ `Marraige Status (Yrs)` + `Follicle No. (R)` + 
                           `Hair loss(Y/N)` + `BP _Diastolic (mmHg)` + 
                           `RBS(mg/dl)` + `AMH(ng/mL)` + BMI + `Weight (Kg)`, 
                         data = datos)

summary(modelo_lm_stepwise_3)

vif(modelo_lm_stepwise_3)
AIC(modelo_lm_stepwise_3)


#####REGRESION CON COMPONENTES PRINCIPALES##

##PCA##
#Haciendo el PCA centrado y escalado obtenemos

PCA_centrado<-prcomp(datos, center=TRUE, scale=TRUE)

#Primero revisaremos los eigenvalues resultantes del PCA para ver la proporción de varianza explicada por cada componente y decidir con que número de componentes nos quedamos, utilizaremos la regla de Kaiser, es decir, eigenvalues mayores a 1, además nuscamos una varianza acumulada mayor al 90%.

eigenvalues<-get_eigenvalue(PCA_centrado)
eigenvalues


##regla del codo
fviz_screeplot(PCA_centrado, main="Gráfico de Componentes", addlabels=TRUE)


##% de varianza explicada ##
summary(PCA_centrado)

###Regresión###

modelo_1 <- pcr(datos$`Age (yrs)` ~ ., 
                data = datos, scale=TRUE, validation="CV")
summary(modelo_1)


#RMSEP
#RMSECV
#RMSEC


##RMSEP##
X11()
validationplot(modelo_1)

##MSEP#
X11()
validationplot(modelo_1, val.type="MSEP")

##R{2}
X11()
validationplot(modelo_1, val.type="R2")

###MODELO DE LAS VARIABLES YA SELECCIONADAS##

modelo_2<- pcr(datos$`Age (yrs)` ~ `Marraige Status (Yrs)` + `Follicle No. (R)` + 
                  `Hair loss(Y/N)` + `BP _Diastolic (mmHg)` + 
                  `RBS(mg/dl)` + `AMH(ng/mL)` + BMI + `Weight (Kg)`, 
                data = datos, scale=TRUE, validation="CV")
summary(modelo_2)


##RMSEP##
X11()
validationplot(modelo_2)

##MSEP#
X11()
validationplot(modelo_2, val.type="MSEP")

##R{2}
X11()
validationplot(modelo_2, val.type="R2")

