library(forecast)##Pred)
library(tseries)##Objeto ts
library(ggplot2)
library(ggfortify)
library(quantmod)#jalar datos de la bolsa
library(caTools)
library(Metrics)
library(tidyverse)

##Ruido Blanco

w = rnorm(500,0,1)
abline(y=0)
x11()
plot.ts(w, main="Ruido Blanco")


###Ruido blanco con sigma 
##Moving Average
v = filter(w, sides=2, filter=rep(1/3,3))
plot.ts(v, ylim=c(-3,3), main="Promedio Moviles")


##Caminata
set.seed(123)
w = rnorm(1000)
x = cumsum(w) 
x11()
plot.ts(x, ylim=c(-5,55), main="Caminata Aleatoria", ylab='')
lines(x, col=4); abline(h=0, col=4, lty=2)

###Ar(1)

ar_1<-arima.sim(list(order = c(1, 0, 0), ar=c(.8)),n = 200)

x11()
autoplot(ar_1)



ar_2<-arima.sim(list(order = c(1, 0, 0), ar=c(1)),n = 200)

x11()
autoplot(ar_2)


####Ar1 

set.seed(123) 
n <- 1000
x <- numeric(n)
x[1] <- 0 
phi1 <- .99
for (i in 2:n) {
  x[i] <- phi1 * x[i-1] + rnorm(1, mean = 0, sd = 1) # Add random noise
}


###A time series

Ar_1_1<- ts(x)

x11()
plot(Ar_1_1, main = "AR(1)",
     xlab = "Time", ylab = "Value", col = "blue")




x11()
ggtsdisplay(Ar_1_1)


####Ar1 raiz unica

set.seed(1233) 
n <- 1000
y <- numeric(n)
y[1] <- 0 
phi1 <- 1
for (i in 2:n) {
  y[i] <- phi1 * y[i-1] + rnorm(1, mean = 0, sd = 1) # Add random noise
}

Ar_1_2<- ts(y)

x11()
plot(Ar_1_2, main = "AR(1.1)",
     xlab = "Time", ylab = "Value", col = "blue")


x11()
ggtsdisplay(Ar_1_2)



