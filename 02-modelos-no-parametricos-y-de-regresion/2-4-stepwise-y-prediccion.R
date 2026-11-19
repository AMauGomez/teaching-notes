# =============================================================================
# 2.4 Selección de modelos y predicción con datos de prueba (precio de autos)
# Objetivo: elegir variables con forward, backward y stepwise en un conjunto de
#           entrenamiento, revisar supuestos y medir el error de predicción en
#           datos que el modelo no vio (RMSE y MAE).
# Datos:    "Car Price Prediction" (Kaggle), datos/CarPrice_Assignment.csv
# =============================================================================

library(lmtest)   # bptest(), bgtest(), dwtest()

datos <- read.csv("datos/CarPrice_Assignment.csv")
numericas <- c("wheelbase", "carlength", "carwidth", "carheight", "curbweight",
               "enginesize", "boreratio", "stroke", "compressionratio",
               "horsepower", "peakrpm", "citympg", "highwaympg")
datos <- datos[, c(numericas, "price")]

# --- 1. Entrenamiento (70%) y prueba (30%) --------------------------------------
set.seed(2024)
i <- sample(seq_len(nrow(datos)), round(0.7 * nrow(datos)))
entrena <- datos[i, ]
prueba <- datos[-i, ]

# --- 2. Selección de variables solo con los datos de entrenamiento -------------
vacio <- lm(price ~ 1, data = entrena)
completo <- lm(price ~ ., data = entrena)
modelos <- list(
  forward  = step(vacio, scope = list(lower = vacio, upper = completo), direction = "forward", trace = 0),
  backward = step(completo, scope = list(lower = vacio, upper = completo), direction = "backward", trace = 0),
  stepwise = step(vacio, scope = list(lower = vacio, upper = completo), direction = "both", trace = 0),
  en_clase = lm(price ~ enginesize + peakrpm + stroke + compressionratio + carwidth + horsepower, data = entrena))
lapply(modelos, formula)

# --- 3. Supuestos del modelo backward -----------------------------------------
m <- modelos$backward
shapiro.test(residuals(m))  # normalidad
bptest(m)                   # varianza constante
dwtest(m)                   # autocorrelación de orden 1
bgtest(m, order = 4)        # autocorrelación de orden mayor
acf(residuals(m), main = "ACF de los residuos (backward)")

# --- 4. Error de predicción en los datos de prueba ------------------------------
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae <- function(y, yhat) mean(abs(y - yhat))
comparacion <- data.frame(
  modelo = names(modelos),
  variables = sapply(modelos, function(x) length(coef(x)) - 1),
  AIC_entrenamiento = sapply(modelos, AIC),
  RMSE_prueba = sapply(modelos, function(x) rmse(prueba$price, predict(x, prueba))),
  MAE_prueba = sapply(modelos, function(x) mae(prueba$price, predict(x, prueba))))
print(comparacion, digits = 4, row.names = FALSE)
# El mejor AIC en entrenamiento no siempre es el menor error en datos nuevos.
