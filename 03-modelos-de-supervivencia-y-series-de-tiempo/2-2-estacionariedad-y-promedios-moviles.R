library(tseries)
library(forecast)
library(ggfortify)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

births
nacimientos <- ts(births, frequency=12, start=c(1946,1))

nacimientos<-log(nacimientos)

x11()
plot.ts(nacimientos)


##Podemos descomponer nuestra serie en sus componentes de interes

x11()
plot(decompose(nacimientos))


##Para detectar estacionalidad podemos hacer uso del periodograma
#o de la funcion acf

x11()
acf(nacimientos,lag.max=150) 

###Adf Test

adf.test(nacimientos,k=1)

##PP Test

PP.test(nacimientos)



###KPSS test


kpss.test(nacimientos, null="Trend")


##Base Airpasseneger


datos<-AirPassengers

x11()
plot.ts(datos)


adf.test(datos,k=1)

##PP Test

PP.test(datos)


###KPSS test

kpss.test(datos, null="Trend")


###Acciones de CEMEX

library(quantmod)

getSymbols("NVDA", src= "yahoo",from= "2018-01-01",to= "2025-06-30",
           periodicity= "monthly")
base <-NVDA[,6] #parasolousarlacolumnadelos preciosdecierreajustados
cemex <-ts(base, start=2001,frequency= 12) #creamoselobjetodeseriesdetiempo

x11()
plot.ts(cemex)


x11()
plot(decompose(cemex))



##3Estacionaria??

adf.test(cemex)

##
dif_cem<-diff(cemex)



adf.test(dif_cem)


x11()
plot(dif_cem)



x11()
autoplot(ggAcf(dif_cem, lag.max = 50))


##PACF decaemiento exponencial o se ve como sen(x)
## despues del lag p-q

x11()
autoplot(ggPacf(dif_cem, lag.max = 50))





##En ocasiones aplicar transformaciones a los datos puede ayudar a reducir la tendencia y esatcionalidad

x11()
plot.ts(log(cemex))

##Pruebas de estacionariedad


adf.test(cemex,k=1)

##PP Test

PP.test(cemex)


###KPSS test


kpss.test(cemex, null="Trend")

x11()
plot(decompose(cemex))
     


##Ya que no es estacioanria habria que aplicar algun metodo para intentar hacerla estacionaria

##Moving Average

cem_10<-cemex[1:10,]
cem_10<-as.vector(cem_10)

cem_10
time<-c(1,2,3,4,5,6,7,8,9,10)

cemex_10<-cbind(time, cem_10)

cemex_10<-as.data.frame(cemex_10)

cemex_10

##Creamos una variable para las predicciones (forecast)

cemex_10$predicciones=NA

cemex_10

cemex_10<-rbind(cemex_10, c(11,NA,NA))

cemex_10<-as.data.frame(cemex_10)


cemex_10

##Definimos el proceso de promedios moviles para a=3

for(i in 1:8){
  a=i+2
  cemex_10$predicciones[i+3]=mean(cemex_10$cem_10[i:a])
}

cemex_10

##De forma general para cualquier a

cemex_10$predicciones=NA

cemex_10

##Con a=6
k=6

A=nrow(cemex_10)-k


for(i in 1:8){
  a=i+(k-1)
  cemex_10$predicciones[i+k]=mean(cemex_10$cem_10[i:a])
}

cemex_10

##Residuales o errores

cemex_10$errores=cemex_10$cem_10-cemex_10$predicciones
cemex_10$errorescuadrados=(cemex_10$cem_10-cemex_10$predicciones)^2

cemex_10


library(tidyverse)
library(TSA)
library(zoo)
library(forecast)


getSymbols("CX", src= "yahoo",from= "2001-01-01",to= "2025-06-30",
           periodicity= "monthly")
base <-CX[,6] #parasolousarlacolumnadelos preciosdecierreajustados
cemex <-ts(base, start=2001,frequency= 12) #creamoselobjetodeseriesdetiempo

Acf(cemex, lag=50)

ma_anter = stats::filter(cemex, filter = rep(1/12, 12), method = 'convolution', sides = 1)
ma_centrado = stats::filter(cemex, filter = rep(1/12, 12), method = 'convolution', sides = 2)

x11()
plot(cemex, main = 'CEMEX MA(12)')
lines(ma_anter, lty = 1, lwd = 1, col = 'blue')
lines(ma_centrado, lty = 2, lwd = 1, col = 'red')
legend("topleft", legend = c('ma_anter = 1', 'ma_centrado = 2'), lty = c(1, 3))

##Evaluación del modelo

tail(ma_anter, 20)
tail(ma_centrado, 20)
tail(cemex, 25)


##Mejorando el modelo

Acf(cemex, lag=50)
Acf(cemex, lag=250)


ma_anter_36 = stats::filter(cemex, filter = rep(1/36, 36), method = 'convolution', sides = 1)
ma_centrado_36 = stats::filter(cemex, filter = rep(1/36, 36), method = 'convolution', sides = 2)

x11()
plot(cemex, main = 'CEMEX MA(36)')
lines(ma_anter_36, lty = 1, lwd = 1, col = 'blue')
lines(ma_centrado_36, lty = 2, lwd = 1, col = 'red')
legend("topleft", legend = c('ma_anter_36 = 1', 'ma_centrado_36 = 2'), lty = c(1, 3))

##Evaluación del modelo

tail(ma_anter_36, 60)
tail(ma_centrado_36, 60)
tail(cemex, 25)

##Diferenciación

difer<- diff(cemex, differences=1)

x11()
plot(difer)

PP.test(difer)
adf.test(difer,k=1)
