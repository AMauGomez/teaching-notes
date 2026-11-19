library(forecast)
data("nottem")
plot(nottem)

#Separamos los datos en subconjuntos
train <- window(nottem, end = c(1936, 12))   # hasta 1936 inclusive
test  <- window(nottem, start = c(1937, 1))  # de 1937 en adelante

#Suavizamiento exponencial
s_exp<- ses(train, h= 36)
plot(s_exp)
lines(test, col= "darkblue")




#Metodo holt
m_holt<- holt(train, h= 36)
plot(m_holt)
lines(test, col="purple")

#Holt-Winters
m_hw<- forecast(HoltWinters(train), h= 36)
plot(m_hw)
lines(test, col= "red")
summary(m_hw)





