library(tidyverse)
library(goftest)
library(nortest)
library(ggplot2)
library(psych)

set.seed(123)
n <- 40               
x <- rpois(n,lambda = 4 )

#Creamos la funcion empirica
emp_func <- ecdf(x)


#Graficamos
x11()
plot(emp_func, 
     main = "CDF Empririca vs.  Normal CDF Teorica",
     xlab = "Valor (x)", 
     ylab = "Probabiliad acumulada",
     lwd = 2,               
     col = "black",         
     verticals = TRUE,      
     do.points = TRUE,      
     pch = 19)              

#Añadimos la dist normal
curve(pnorm(x, mean=0, sd=1), 
      add = TRUE,           
      col = "red",          
      lwd = 3,              
      lty = 2)              






#####KS TEST#3

data(faithful)
datos<-faithful


plot(faithful$eruptions)

describe(faithful$eruptions)


###Queremos probar Normalidad 

ecdf_erup <- ecdf(datos$eruptions)

#Graficamos

x11()
plot(ecdf_erup, 
     main = "CDF Empririca vs.  Normal CDF Teorica",
     xlab = "Valor (x)", 
     ylab = "Probabiliad acumulada",
     lwd = 2,               
     col = "black",         
     verticals = TRUE,      
     do.points = TRUE,      
     pch = 19)              

#Añadimos la dist normal
curve(pnorm(x, mean=0, sd=1), 
      add = TRUE,           
      col = "red",          
      lwd = 3,              
      lty = 2)              



####Para comparar dos distribuciones
limites_x <- range(c(datos$eruptions, datos$waiting))


x11()
plot(ecdf(datos$eruptions), 
     col = "blue", 
     main = "Distribuciones Empiricas",
     xlab = "Valor (x)", 
     ylab = "Probabilidad Acumulada",
     verticals = TRUE, 
     do.points = FALSE,
     xlim = limites_x,
     lwd = 2)

plot(ecdf(datos$waiting), 
     col = "red", 
     add = TRUE,        
     verticals = TRUE, 
     do.points = FALSE,
     lwd = 2)


x11()

ggplot(datos, aes(eruptions)) + stat_ecdf(geom = "step")


###Simulacion

library(tseries)
n_1<-150

x <- rt(n_1, 4)

x11()
plot(ecdf(x), 
     main = "CDF Empririca vs.  Normal CDF Teorica",
     xlab = "Valor (x)", 
     ylab = "Probabiliad acumulada",
     lwd = 2,               
     col = "black",         
     verticals = TRUE,      
     do.points = TRUE,      
     pch = 19)     



ks_test<-ks.test(x, pnorm)

ks_test




sw_test <- shapiro.test(x)
sw_test


library(psych)
describe(x)

lillie_test <- lillie.test(x)
lillie_test

# Anderson-Darling (sensible a las colas)
ad_test <- ad.test(x)
ad_test
# D) Jarque-Bera (Asimetría y Kurtosis)
jb_test <- jarque.bera.test(x)
jb_test


##Graficamos

x11()

par(mfrow = c(1, 2)) 

# Gráfico 1: QQ-Plot 
qqnorm(x, main = "QQ-Plot", pch = 19, col = "blue")
qqline(x, col = "red", lwd = 2)
legend("topleft", legend = "Desviación en extremos", bty="n", text.col="red")

# Gráfico 2: ECDF vs Teórica 
plot(ecdf(x), main = " KS-Lilliefors)", 
     col = "blue", verticals = TRUE, do.points = FALSE, lwd = 2,
     xlab = "Valor", ylab = "Fn(x)")

curve(pnorm(x, mean = mean(x), sd = sd(x)), add = TRUE, col = "red", lty = 2, lwd = 2)


legend("bottomright", legend = c("Datos (t-student)", "Normal Teórica"), 
       col = c("blue", "red"), lty = 1:2, lwd = 2)




cor(faithful$eruptions, faithful$waiting, method = "kendall")


x11()
plot(faithful$eruptions, faithful$waiting)


