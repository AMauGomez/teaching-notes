library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(quantmod)
library(caTools)
library(Metrics)
library(tidyverse)



###Modelo AR(p) o ARIMA(p,0,0)
##Determinandom el grado de p

##ACF decaemiento exponencial o se ve como sen(x)

##PACF corta en el lag p (el primero en salirse) y que despues ya no se salga ninguno(caso perfecto)

arima_AR_3<-arima.sim(list(order = c(3, 0, 0),ar = c(.93, -0.75, .48)),n = 200)

##ACF
x11()
autoplot(ggAcf(arima_AR_3, lag.max = 100))

#PACF
x11()
autoplot(ggPacf(arima_AR_3, lag.max = 50))



###Modelo MA(q) o ARIMA(0,0,q)
##Determinandom el grado de q

arima_MA_4<-arima.sim(list(order = c(0, 0, 4), ma = c(.23, -0.35, .12, -.81)),n = 200)

##ACF corta en el lag q y ya no se sale ninguno (caso perfecto)
##ACF
x11()
autoplot(ggAcf(arima_MA_4, lag.max = 50))


##PACF decaemiento exponencial o se ve como sen(x)

#PACF
x11()
autoplot(ggPacf(arima_MA_4, lag.max = 50))


######################3

###Modelo ARMA(p,q)
##Determinandom el grado de p y q

arima_2_3<-arima.sim(list(order = c(2, 0, 3), ar= c(.54, -.57), ma = c(.86, -0.45, -.67)),n = 200)

mod_0<-Arima(datos, c(3,0,6))

##ACF decaemiento exponencial o se ve como sen(x)
## despues del lag q-p

x11()
autoplot(ggAcf(arima_2_3, lag.max = 50))


##PACF decaemiento exponencial o se ve como sen(x)
## despues del lag p-q

x11()
autoplot(ggPacf(arima_2_3, lag.max = 50))



##Es dificil observar en los arima cuales son los lag por lo cual se optimiza el aic
##Tambien se puede iterar sobre algunos lags que creamos adecuados

arma_1<-auto.arima(arima_2_3, start.p = 0, max.p = 5, start.q = 0, max.q = 6, trace=1)

arma_1

##No siempre hace las estimaciones exactas



##Ejemplo

######
getSymbols("KO", src= "yahoo",from= "1995-06-01",to= "2025-06-30",
           periodicity= "monthly")
base <-KO[,6] #parasolousarlacolumnadelos preciosdecierreajustados
coca<-ts(base, start=1990,frequency= 12) #creamoselobjetodeseriesdetiempo

##Separamos los datos 70-30 0 80-20


dim(coca)

# Create the training and test sets
train_data <- coca[1:300,]
test_data <- coca[301:361]

train_data<-ts(train_data)



##Plots

x11()


ggtsdisplay(train_data)

##ACF
x11()

ggAcf(train_data, lag=100)

##PACF

x11()
ggPacf(train_data, lag=100)


##Estacionariedad

adf.test(train_data)

##H_0:No esatcionaria
#H_a:estacionaria


##Se rechaza h_0 si pv<alfa=.05


##Difereciada

train_data_dif<-diff(train_data)


x11()
ggtsdisplay(train_data_dif)


##ACF
x11()

ggAcf(train_data_dif, lag=100)

##PACF

x11()
ggPacf(train_data_dif, lag=100)


##Estacionariedad

adf.test(train_data_dif)

##ARIMA buscamos un modelo inicial

arima_dif<-auto.arima(train_data_dif, start.p = 0, max.p = 3, start.q = 0, max.q = 18, trace=1)



x11()
autoplot(forecast(arima_dif))

##Resi

x11()

checkresiduals(arima_dif)

#Ljung-Box test

#H_0:Los residuales son independientes i.e. es un ruido blanco

##H_a Existe correlacion entre residuales


x11()
tsdiag(arima_dif)

##Forecast


mod<-Arima(train_data, c(0,1,2))
summary(mod)

mod_1<-Arima(train_data,c(1,1,2))
summary(mod_1)


pred<-forecast(mod, 61)
pred<-as.data.frame(pred)
pr<-pred[,1]


compara<-cbind(pr, test_data)
compara<-as.data.frame(compara)
compara<-compara %>% mutate(rmse=sqrt(mean((compara$test_data- compara$pr)^2)))


pred_1<-forecast(mod_1, 61)
pred_1<-as.data.frame(pred_1)
pr_1<-pred_1[,1]


compara_1<-cbind(pr_1, test_data)
compara_1<-as.data.frame(compara_1)
compara_1<-compara_1 %>% mutate(rmse=sqrt(mean((compara_1$test_data- compara_1$pr)^2)))



##Evaluacion 

rmse(test_data, pred[,1])

mae(test_data, pred[,1])

###Ploteamos el forecast

x11()
autoplot(forecast(mod))

##Tiene sentido###


##Proponemo algo###


mod_1<-Arima(train_data, c(4,1,2))
summary(mod_1)


x11()
autoplot(forecast(mod_1))



##########T2######
set.seed(123)  # Para reproducibilidad

# iteraciones
n <- 200
t <- 1:n
#s_t
s_t <- numeric(n)
s_t[1:100] <- 0
s_t[101:200] <- 10 * exp(-(t[101:200] - 100) / 20) * cos(2 * pi * t[101:200] / 4)

# RB  w_t ~ N(0,1)
w_t <- rnorm(n, mean = 0, sd = 1)

# x_t
x_t <- s_t + w_t

# Graficar
x11()
plot(t, x_t, type = "l", col = "blue", lwd = 2,
     main = "Serie simulada x_t = s_t + w_t",
     xlab = "t", ylab = "x_t")
legend("topright", legend = c("x_t"), col = c("blue"),
       lty = c(1,2), lwd = 2)

####################SARIMA###################33






