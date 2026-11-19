library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(quantmod)

######
getSymbols("^GSPC", src= "yahoo",from= "1995-06-01",to= "2008-06-30",
           periodicity= "monthly")
base <-GSPC[,6] #parasolousarlacolumnadelos preciosdecierreajustados
sp <-ts(base, start=1995,frequency= 12) #creamoselobjetodeseriesdetiempo


##Plots
plot.ts(sp)
plot(decompose(sp))


##Estacionariedad

adf.test(sp)

##Correlaciones
acf(sp)
pacf(sp, lag.max = 55)


####Yule walker

ar_model <- ar(sp, order.max =5 , method = "yw", aic = FALSE)




# View the estimated AR coefficients
print(ar_model$ar)

# View the estimated white noise variance
print(ar_model$var.pred)

# View the selected model order
print(ar_model$order)




######TAREA 1###

datos<-UKgas
uk<-ts(datos, start=1960, frequency=4)


plot(uk)
data<-lynx
linces<-ts(data)

