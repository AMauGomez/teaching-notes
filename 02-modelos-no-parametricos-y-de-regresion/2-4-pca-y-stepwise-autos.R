# =============================================================================
# 2.4 Selección de modelos: regresión por pasos contra regresión sobre
#     componentes principales (precio de autos)
# Objetivo: cuando las variables explicativas están muy correlacionadas,
#           comparar dos salidas: elegir variables (stepwise) o resumirlas en
#           componentes principales y hacer la regresión sobre ellos (PCR).
# Datos:    "Car Price Prediction" (Kaggle), datos/CarPrice_Assignment.csv
# =============================================================================

library(factoextra)  # gráficas de PCA
library(pls)         # pcr()
library(car)         # vif()

datos <- read.csv("datos/CarPrice_Assignment.csv")
numericas <- c("wheelbase", "carlength", "carwidth", "carheight", "curbweight",
               "enginesize", "boreratio", "stroke", "compressionratio",
               "horsepower", "peakrpm", "citympg", "highwaympg")
datos <- datos[, c(numericas, "price")]

# --- 1. El problema: variables explicativas muy correlacionadas ---------------
completo <- lm(price ~ ., data = datos)
sort(round(vif(completo), 1), decreasing = TRUE)   # VIF > 10: multicolinealidad grave

# --- 2. Componentes principales de las explicativas ---------------------------
pca <- prcomp(datos[, numericas], center = TRUE, scale. = TRUE)
summary(pca)
fviz_screeplot(pca, addlabels = TRUE, main = "Varianza explicada por componente")
fviz_pca_var(pca, col.var = "contrib", gradient.cols = c("green", "blue"), repel = TRUE)

# --- 3. Regresión sobre componentes principales con validación cruzada --------
set.seed(2024)
modelo_pcr <- pcr(price ~ ., data = datos, scale = TRUE, validation = "CV")
summary(modelo_pcr)
validationplot(modelo_pcr, val.type = "RMSEP", main = "Error de validación cruzada por número de componentes")

# --- 4. Regresión por pasos (AIC) ----------------------------------------------
vacio <- lm(price ~ 1, data = datos)
stepwise <- step(vacio, scope = list(lower = vacio, upper = completo), direction = "both", trace = 0)
summary(stepwise)
sort(round(vif(stepwise), 1), decreasing = TRUE)

# --- 5. Comparación -------------------------------------------------------------
# Error de validación cruzada (10 grupos) del modelo stepwise, para comparar con PCR
set.seed(2024)
grupo <- sample(rep(1:10, length.out = nrow(datos)))
err <- sapply(1:10, function(k) {
  m <- lm(formula(stepwise), data = datos[grupo != k, ])
  sqrt(mean((datos$price[grupo == k] - predict(m, datos[grupo == k, ]))^2))
})
cat("RMSE de validación cruzada, stepwise:", round(mean(err)), "\n")
# Stepwise conserva variables interpretables; PCR usa todas, pero combinadas en
# componentes que ya no corresponden a una sola característica del auto.
