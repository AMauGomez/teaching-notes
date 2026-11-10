# =============================================================================
# 2.2 Análisis descriptivo y representación gráfica de datos multivariados
# Objetivo: explorar un conjunto multivariado antes de modelar: resúmenes por
#           grupo, matrices de dispersión, curvas de Andrews y una primera
#           revisión de normalidad multivariada (Mardia).
# Datos:    "Niveles de Obesidad" (Kaggle Playground Series S4E2, versión
#           sintética de la base de obesidad de México, Perú y Colombia).
#           Archivo esperado: datos/Niveles de Obesidad.csv
# =============================================================================

library(dplyr)
library(GGally)
library(psych)     # describeBy() y mardia()
library(andrews)   # curvas de Andrews

datos <- read.csv("datos/Niveles de Obesidad.csv") |> na.omit()
datos$NObeyesdad <- factor(datos$NObeyesdad)

# Variables numéricas (por nombre, no por posición, para que no se rompa si
# cambia el orden de las columnas)
vars_num <- c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE")
data_num <- datos[, vars_num]

# Con ~20 mil filas las gráficas se saturan: usamos una muestra aleatoria
set.seed(2023)
muestra <- datos[sample(nrow(datos), 1000), ]

# --- 1. Resúmenes numéricos --------------------------------------------------
summary(data_num)

# Peso y número de comidas principales por nivel de obesidad
describeBy(datos$Weight, datos$NObeyesdad, mat = TRUE, digits = 2)
describeBy(datos$NCP,    datos$NObeyesdad, mat = TRUE, digits = 2)

# Proporción de hombres y mujeres en cada nivel
prop.table(table(datos$NObeyesdad, datos$Gender), margin = 1) |> round(2)

# --- 2. Diagramas de caja ----------------------------------------------------
# Para comparar variables entre sí deben estar en la misma escala: estandarizamos
boxplot(scale(data_num), las = 2, main = "Variables numéricas estandarizadas")
boxplot(Weight ~ NObeyesdad, data = datos, las = 2, cex.axis = 0.7,
        xlab = "", ylab = "Peso (kg)", main = "Peso por nivel de obesidad")

# --- 3. Matrices de dispersión por grupo -------------------------------------
ggpairs(muestra, columns = c("Age", "Height", "Weight"),
        aes(color = NObeyesdad, alpha = 0.5),
        upper = list(continuous = wrap("cor", size = 2.5)))

ggpairs(muestra, columns = c("FCVC", "NCP", "CH2O", "FAF", "TUE"),
        aes(color = NObeyesdad, alpha = 0.5),
        upper = list(continuous = wrap("cor", size = 2.5)))

plot(muestra$Height, muestra$Weight, col = muestra$NObeyesdad, pch = 19,
     xlab = "Estatura (m)", ylab = "Peso (kg)")
plot(density(datos$Height), main = "Densidad de la estatura")

# --- 4. Curvas de Andrews ----------------------------------------------------
# Cada observación p-dimensional se convierte en la curva
#   f(t) = x1/sqrt(2) + x2 sin(t) + x3 cos(t) + x4 sin(2t) + ...
# Observaciones parecidas dan curvas parecidas. El orden de las variables
# importa: las primeras pesan más en la forma de la curva.
set.seed(1)
para_andrews <- datos[sample(nrow(datos), 200), c(vars_num, "NObeyesdad")]
andrews(para_andrews, clr = ncol(para_andrews), ymax = 3,
        main = "Curvas de Andrews (200 personas, color = nivel de obesidad)")

# --- 5. Normalidad multivariada (Mardia) -------------------------------------
# Con n grande cualquier desviación pequeña resulta significativa; por eso se
# acompaña siempre de la gráfica de distancias de Mahalanobis contra chi-cuadrada.
mardia(muestra[, vars_num], plot = TRUE)
