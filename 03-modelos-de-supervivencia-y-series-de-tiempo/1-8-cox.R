library(flexsurv)
library(survival)
library(tidyverse)
library(ggplot2)
library(ggsurvfit)
library(survminer)
library(dplyr)
library(ranger)
library(ggfortify)


##Cargamos los datos
datos<-read.csv("datos/Breast Cancer.csv")


datos<-na.omit(datos)

#Nos interesa saber el tiempo de supervivencia en meses

tiempo<-datos$Overall.Survival..Months.

tiempo<-as.numeric(tiempo)

delta<-datos$Patient.s.Vital.Status

delta<-ifelse(delta=="Died of Disease",1,0)


datos_1<-cbind(tiempo, delta)
datos_1<-as.data.frame(datos_1)

###Kaplan-Meier

##Ahora usando el lado del tumor más grande
data<-Surv(tiempo, delta)

km_Lado<-survfit(data~ datos$Primary.Tumor.Laterality, data = datos, type = "kaplan-meier")
summary(km_Lado)

x11()
ggplot2::autoplot(km_Lado)


###Tipo de Cirugía

km_ciru<-survfit(data~ datos$Type.of.Breast.Surgery, data = datos, type = "kaplan-meier")
summary(km_ciru)

x11()
ggplot2::autoplot(km_ciru)


########################log Rank######################################33

##Log rank test para tipo de celularidad

data=cbind(data, datos$Cellularity)
data<-as.data.frame(data)

data<-na.omit(data)
survdiff(Surv(tiempo, delta) ~ datos$Cellularity, data = data)

#sÍ TIENE SENTIDO

##Para la variable histoligias

data=cbind(data, datos$Tumor.Other.Histologic.Subtype)
data<-as.data.frame(data)

data<-na.omit(data)
survdiff(Surv(tiempo, delta) ~ datos$Tumor.Other.Histologic.Subtype, data = data)


##NO TIENE SENTIDO COMPARAR

##Para la variable tamaño del tumor

data=cbind(data, datos$Tumor.Size)
data<-as.data.frame(data)

data<-na.omit(data)
survdiff(Surv(tiempo, delta) ~ datos$Tumor.Size, data = data)

###3No tiene sentido


######################################################################

#########Cox proportional models
cox_1<-coxph(Surv(tiempo, delta) ~  as.factor(datos$HER2.Status) + datos$Tumor.Size+ datos$Lymph.nodes.examined.positive, data = datos)

summary(cox_1)


##########Verificación de supuestos#######

######Proporcionalidad#####

riesgos<-cox.zph(cox_1)
riesgos

##H_0:la tasa de riesgo de un individuo es relativamente constante en el tiempo


##De forma grafica
x11()
ggcoxzph(riesgos)


########Para identificar No linearidad entre Y y las covariables##############3
###Martingala

x11()
ggcoxfunctional(Surv(tiempo, delta) ~  as.factor(datos$HER2.Status) + datos$Tumor.Size+ datos$Lymph.nodes.examined.positive, data = datos)

##Valores cercanos a 1 == Murio muy pronto
##VALORES NEGATIVOS GRANDES== VIVIÓ MUCHO XD

##Queremos no Ver patrones


##Para identificar Outliers
##Devianza

x11()
ggcoxdiagnostics(cox_1, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

##VALORES POSTIVOS== murio muy rapido
##VALORES NEGATIVOS==VIVIÓ mucho

##betas estimadas

x11()
ggcoxdiagnostics(cox_1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())


##Si algunas observaciones se alejan mucho esos datos son influyentes


x11()
ggcoxdiagnostics(cox_1, type = "dfbetas", ox.scale = "observation.id", title = "Residuales dfbetas", 
                 subtitle = "dfbetas vs id", caption = "Análisis residual VS Betas")







##############Conviertiendo moldelos Cox a  Modelos Parametricos ##############

#Con la función fexsurvreg es posible ajustar modelos parametricos una vez que
#verificamos que nuestros datos se ajustan a un dist especifica


mod_exp<-flexsurvreg(Surv(tiempo, delta) ~ datos$Age.at.Diagnosis+ datos$Tumor.Size  + datos$Lymph.nodes.examined.positive, dist = "exp", data = datos)
mod_exp

mod_wei<-flexsurvreg(Surv(tiempo, delta) ~ datos$Age.at.Diagnosis+ datos$Tumor.Size  + datos$Lymph.nodes.examined.positive, dist = "weibull", data = datos)
mod_wei


mod_gom<-flexsurvreg(Surv(tiempo, delta) ~ datos$Age.at.Diagnosis+ datos$Tumor.Size  + datos$Lymph.nodes.examined.positive, dist = "gompertz", data = datos)
mod_gom






