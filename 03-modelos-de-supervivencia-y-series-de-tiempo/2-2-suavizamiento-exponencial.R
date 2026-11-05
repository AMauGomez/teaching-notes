library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)

##Algunas bases de datos
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
nacimientos <- ts(births, frequency=12, start=c(1946,1))

##Ejemplo de Suavizamiento Exponencial

library(quantmod)

getSymbols("CX", src= "yahoo",from= "2002-01-01",to= "2025-06-30",
           periodicity= "monthly")
base <-CX[,6] #parasolousarlacolumnadelos preciosdecierreajustados
cemex <-ts(base, start=2001,frequency= 12) #creamoselobjetodeseriesdetiempo

x11()
plot.ts(cemex)


##Diferenciando
plot(diff(cemex))


x11()
acf(cemex)
diff.cemex<-diff(cemex)
#Prueba de estacionariedad
adf.test(diff.cemex,k=1)

adf.test(cemex)

Acf((diff.cemex))

Pacf(cemex, lag.max = 55)

#Suavizamiento Exponencial

##Primer Grado

cemex_ses<-ses(cemex, h=12) #H es el numero de predicciones

cemex_ses$model

##
diff.cemex<-diff(cemex)

ses(diff.cemex, h=12)

x11()
plot(ses(diff.cemex, h=12))
##Revisamos las predicciones

head(cemex_ses$fitted)

head(cemex)


tail(cemex_ses$fitted)

tail(cemex)



x11()
plot(cemex_ses)
lines(cemex, lty = 1, lwd = 1, col = 'blue')
lines(cemex_ses$fitted, lty = 2, lwd = 1, col = 'red')
legend("topleft", legend = c('Real', 'SES'), lty = c(1, 3))


###Segundo Grado: Método Holt

cemex_holt<-HoltWinters(cemex, gamma =F)


cemex_holt


##Revisamos las predicciones

head(cemex_holt$fitted)

head(cemex)


tail(cemex_holt$fitted)

tail(cemex)

##Evaluacion

library(Metrics)

x11()
autoplot(cemex_holt)



###hacemos las predicciones

holt_pred <- predict(cemex_holt, n.ahead = 6, prediction.interval = TRUE, level = 0.95)

x11()
plot(cemex_holt, holt_pred)


getSymbols("CX", src= "yahoo",from= "2002-01-01",to= "2016-06-30",
           periodicity= "monthly")
base <-CX[,6] #parasolousarlacolumnadelos preciosdecierreajustados
cemex <-ts(base, start=2001,frequency= 12) #creamoselobjetodeseriesdetiempo



###Método Holt-Winters

cemex_holt_w<-HoltWinters(cemex, alpha = 1, beta = 0.1)

cemex_holt_w


holt_pred_w<- predict(cemex_holt_w, n.ahead = 12, prediction.interval = TRUE, level = 0.95)

x11()
autoplot(holt_pred_w)

x11()
plot(cemex_holt_w, holt_pred_w)

x11()
plot.ts(cemex)


####Yule walker

ar_model <- ar(cemex, method = "yule-walker")


x11()
pacf(cemex)


x11()
acf(cemex, lag.max = 155)
# View the estimated AR coefficients
print(ar_model$ar)

# View the estimated white noise variance
print(ar_model$var.pred)

# View the selected model order
print(ar_model$order)