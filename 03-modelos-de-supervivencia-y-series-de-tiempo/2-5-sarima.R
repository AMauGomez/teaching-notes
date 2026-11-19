library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(quantmod)
library(caTools)
library(Metrics)
library(tidyverse)
library(zoo)


###Jhonson


data("JohnsonJohnson")

datos<-JohnsonJohnson

plot(JohnsonJohnson, main = "Johnson & Johnson", col = "purple")



###
ggtsdisplay(datos)


##ACF
ggAcf(datos, lag=101)

##PACF
ggPacf(datos, lag=101)

###Como es multiplicativa siempre es bueno aplicar log

datos<-log(datos)
plot(decompose(datos,  type = "multiplicative"))
plot(decompose(datos,  type = "additive"))

###Necesito diferenciar?
adf.test(datos)


##Sí, hay que diferenciar

datos_dif<-diff(datos)

##Estacionaria?

adf.test(datos_dif)

###Revisamos los parametros del ARMA
ggtsdisplay(datos_dif)

###Podemos notar un ARIMA(,,)

checkresiduals(datos_dif)

#h_0:Los datos (o residuos) se distribuyen de forma independiente.


##Falta la componente estacional
nsdiffs(datos)

###Diferenciamos con lag, porque ya es estacionaria


datos_dif_4<-diff(datos_dif, lag = 4)

##Revisamos de nuevo
ggtsdisplay(datos_dif_4)
ggtsdisplay(datos_dif_4)

checkresiduals(datos_dif_4)

###Podemos notar un ARIMA(,,)


mod_1<- Arima(datos, order=c(3,1,0), 
                       seasonal=c(1,1,1))
checkresiduals(mod_1)


##Auto arima

auto.arima(datos, trace=T)

mod_2<-Arima(datos, 
             order=c(2,0,0), 
             seasonal=c(1,1,0))
checkresiduals(mod_2)


##Revisamos el rmse (obvio ustedes con su test sample)


pred_1<-tail(mod_1$fitted)

pred_2<-tail(mod_2$fitted)

actual<-tail(datos)
actual

cbind(actual, pred_1, pred_2)
##RMSE

mod_1_rmse<-rmse(actual, pred_1)

mod_1_rmse
mod_2_rmse<-rmse(actual, pred_2)

mod_2_rmse

AIC(mod_1)
AIC(mod_2)

##Aqui lo haré con los forecast

for_1<-forecast(mod_1, 10)
for_2<-forecast(mod_2, 10)

cbind(tail(for_1$fitted), tail(for_2$fitted))




#########ARCH  sp500###############
library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(quantmod)
library("rugarch")
library(FinTS)

######
getSymbols("^GSPC", src= "yahoo",from= "1995-06-01",to= "2008-06-30",
           periodicity= "monthly")
base <-GSPC[,4] #parasolousarlacolumnadelos preciosdecierrea
sp <-ts(base, start=1995,frequency= 365) #creamoselobjetodeseriesdetiempo

###
ggtsdisplay(sp)


##ACF
ggAcf(sp, lag=101)

##PACF
ggPacf(sp, lag=101)


##Estacionariedad
adf.test(log(sp))


##Sí, hay que diferenciar

datos_dif<-diff(sp)

##Estacionaria?

adf.test(datos_dif)


checkresiduals(datos_dif)


##Revisamos si el modelo ARCH es buena idea
ArchTest(sp, lags = 1, demean = TRUE)


###Realizamos el modelo arch


spec<-ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,0)),
                 distribution.model = "norm")


fit<-ugarchfit(spec, data=sp)
fit

pred<-ugarchforecast(fit, data=sp)
pred
plot(pred, which = 1)  # which evita el menú interactivo

