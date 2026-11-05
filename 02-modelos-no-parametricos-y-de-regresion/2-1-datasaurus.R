library(datasauRus)
library(ggplot2)
library(lmtest)
library(stats)
library(FactoMineR)

data("datasaurus_dozen_wide")
datos <- datasaurus_dozen_wide

##AWAY
datos_away<-datos[,1:2]

summary(datos_away)


fit <- lm(datos_away$away_y~datos_away$away_x , data = datos_away)

summary(fit)

# Obtener valores observados (y), estimados (y_hat) y la media
y <- datos_away$away_y
y_hat <- predict(fit)
y_mean <- mean(datos_away$away_y)


ss_tot <- sum((y - y_mean)^2)
ss_tot

ss_res <- sum((y - y_hat)^2)
ss_res

ss_reg <- sum((y_hat - y_mean)^2)
ss_reg
print(y_res)

x11()

plot(density(y_res))

###NORMALIDAD##

ks.test(y_res, "pnorm")

shapiro.test(y_res)


##Normalidad#


##Linealidad


##Homocedaticidad

bptest()
dwtest()
pacf()
hqtest()


###Covarianza cero entre errores

x11()
pacf(y_res)
x11()
acf(y_res)












# Calcular R cuadrada
r_cuadrada_manual <- ss_reg / ss_tot

r_cuadrada_manual



summary(fit)



##DINOS

datos_dinos<-datos[,7:8]

fit_1<- lm(datos_dinos$dino_y~datos_dinos$dino_x , data = datos_dinos)

# Obtener valores observados (y), estimados (y_hat) y la media
y <- datos_dinos$dino_y
y_hat <- datos_dinos<-datos[, 7:8]

predict(fit_1)
y_mean <- mean(y)

ss_tot <- sum((y - y_mean)^2)
ss_tot

ss_res <- sum((y - y_hat)^2)
ss_res

ss_reg <- sum((y_hat - y_mean)^2)
ss_reg

# Calcular R cuadrada
r_cuadrada_manual <- ss_reg / ss_tot



print(r_cuadrada_manual)

summary(fit_1)


summary(datos_away)

summary(datos_dinos)


var(datos_away$away_y)
var(datos_dinos$dino_y)





##stars##

datos_starts<-datos[,19:20]

fit_2<- lm(datos_starts$star_y~datos_starts$star_x , data = datos_starts)

# Obtener valores observados (y), estimados (y_hat) y la media
y <- datos_starts$star_y
y_hat <- predict(fit_2)
y_mean <- mean(y)

ss_tot <- sum((y - y_mean)^2)
ss_res <- sum((y - y_hat)^2)
ss_reg <- sum((y_hat - y_mean)^2)

# Calcular R cuadrada
r_cuadrada_manual <- ss_reg / ss_tot



print(r_cuadrada_manual)

summary(fit_1)





















data("datasaurus_dozen")
datos_1 <- datasaurus_dozen

x11()
ggplot(datos_1, aes(x = x, y = y)) +
  geom_point(aes(color = dataset)) + 
  facet_wrap(~dataset, ncol = 3) +   
  theme_minimal() +
  theme(legend.position = "none") 


plot(datos_away$away_x, datos_away$away_y)


