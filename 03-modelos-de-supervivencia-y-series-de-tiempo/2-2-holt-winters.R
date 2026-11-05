library(forecast)
data("nottem")
plot(nottem)

#Separamos los datos en subconjuntos
train <- window(nottem, end = c(1936, 12))   # hasta 1936 inclusive
test  <- window(nottem, start = c(1937, 1))  # de 1937 en adelante

#Suavizamiento exponencial

x11()
s_exp<- ses(train, h= 36)
plot(s_exp)
lines(test, col= "darkblue")




#Metodo holt

x11()
m_holt<- holt(train, h= 36)
plot(m_holt)
lines(test, col="purple")

#Holt-Winters
x11()
m_hw<- forecast(HoltWinters(train), h= 36)
plot(m_hw)
lines(test, col= "red")
sumary(m_hw)





