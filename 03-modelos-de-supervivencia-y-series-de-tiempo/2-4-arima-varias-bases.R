library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(quantmod)
library(caTools)
library(Metrics)
library(tidyverse)

###Rio Nilo
data("Nile")

datos<-Nile

x11()
ggtsdisplay(datos)

##ACF
x11()

ggAcf(datos, lag=101)

##PACF

x11()
ggPacf(datos, lag=101)


datos<-log(datos)

###Necesito diferenciar?

adf.test(log(datos))


#| #Model         | ACF                                              | PACF pattern                                     | Interpretation           |
# | ------------- | ------------------------------------------------| ----------------------- -------------------------| ------------------------ |
# | **AR(p)**     | Decaimiento gradual expo y/o forma sinudal      | Correlaciones significativas hasta el lag  *p*   | (PACF) se corta el lag hasta el p→ orden p |
# | **MA(q)**     | Correlaciones significativas hasta el lag *q*   | Decaimiento gradual expo y/o forma sinudal            | (ACF) se corta el lag q → orden q  |
# | **ARMA(p,q)** | Se observan decaimineto gradualmente           |  Se observan decaimiento gradualmente            | Mixed process            |



##Ajustamos el modelo

fit_RN <- arima(datos, order = c(1,0,0))
summary(fit_RN)
checkresiduals(fit_RN)


fit_RN_1 <- arima(datos, order = c(1,0,1))
summary(fit_RN_1)
x11()
checkresiduals(fit_RN_1)

###Graficamos para ver si nuestra serie se parece

x11()
autoplot(datos, series = "Serie original") +
  autolayer(fitted(fit_RN), series = "ARIMA ajustado", linetype = "dashed", color = "red") +
  autolayer(fitted(fit_RN_1), series = "ARIMA ajustado", linetype = "dashed", color = "blue") +
  ggtitle("Serie original vs. Serie Ajustada ARIMA") +
  ylab("Valor") + xlab("Tiempo") +
  theme_minimal()

###Checar

###Lago Huron

data("LakeHuron")

datos<-LakeHuron

x11()
plot(datos, main = "Lake Huron Water Levels", col = "darkblue")

###
x11()
ggtsdisplay(datos)



##ACF
x11()

ggAcf(datos, lag=101)

##PACF

x11()
ggPacf(datos, lag=101)



###Necesito diferenciar?

adf.test(datos)



###
datos_dif<-diff(datos)

x11()
ggtsdisplay(datos_dif)


adf.test(datos_dif)

##Ajustamos el modelo

fit_LH <- arima(datos_dif, order = c(2,0,1))
summary(fit_LH)
checkresiduals(fit_LH)


###Graficamos para ver si nuestra serie se parece

x11()
autoplot(datos_dif, series = "Serie original") +
  autolayer(fitted(fit_LH), series = "ARIMA ajustado", linetype = "dashed", color = "red") +
  ggtitle("Serie original vs. Serie Ajustada ARIMA") +
  ylab("Valor") + xlab("Tiempo") +
  theme_minimal()





###Jhonson


data("JohnsonJohnson")

datos<-JohnsonJohnson

plot(JohnsonJohnson, main = "Johnson & Johnson", col = "purple")



###
x11()
ggtsdisplay(datos)



##ACF
x11()

ggAcf(datos, lag=101)

##PACF

x11()
ggPacf(datos, lag=101)



###Necesito diferenciar?

adf.test(datos)



###Realmente necesito diferenciar?


adf.test(log(datos))



x11()
ggtsdisplay(log(datos))


x11()
plot(decompose(log(datos)))

datos_dif_2<-diff(datos, lag = 2)

adf.test(datos_dif_2)

x11()
ggtsdisplay(datos_dif_2)

checkresiduals(datos_dif_2)




##Ajustamos el modelo

fit_JJ <- arima(datos_dif_2, order = c(3,0,1))
summary(fit_JJ)
checkresiduals(fit_JJ)


fit_JJ_1 <- arima(log(datos), order = c(0,0,1))
summary(fit_JJ_1)
checkresiduals(fit_JJ_1)

###Graficamos para ver si nuestra serie se parece

x11()
autoplot(log(datos), series = "Serie original") +
  autolayer(fitted(fit_JJ_1), series = "ARIMA ajustado", linetype = "dashed", color = "blue")+
  autolayer(fitted(fit_JJ), series = "ARIMA ajustado", linetype = "dashed", color = "red") +
  ggtitle("Serie original vs. Serie Ajustada ARIMA") +
  ylab("Valor") + xlab("Tiempo") +
  theme_minimal()





