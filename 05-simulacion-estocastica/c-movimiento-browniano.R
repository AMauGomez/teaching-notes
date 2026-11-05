library(ggplot2)
library(dplyr)

#Simulación Óptima O(n) usando incrementos independientes
SimularBrowniano_Cumsum <- function(t, n) {
  deltaT <- t/n
  Z <- rnorm(n)
  # Suma acumulada de los incrementos gaussianos (O(n))
  Bt <- c(0, cumsum(sqrt(deltaT) * Z))
  return(Bt)
}

#Simulación O(n^3) usando Normal Multivariada 
SimularBrowniano_Cholesky <- function(t, n) {
  tiempos <- seq(t/n, t, length.out = n)
  
  # matriz de covarianzas Sigma_ij = min(t_i, t_j)
  Sigma <- outer(tiempos, tiempos, pmin) 
  
  # Descomposición de Cholesky (O(n^3))
  L <- t(chol(Sigma)) 
  
  # Transformación del vector normal estándar
  Z <- rnorm(n)
  Bt <- c(0, L %*% Z)
  
  return(Bt)
}

# --- Comparación de tiempos ---
cat("Iniciando prueba de rendimiento computacional...\n")
n_prueba <- c(500, 1000, 2000, 4000)

for (n in n_prueba) {
  cat("\nSimulando n =", n, "puntos...\n")
  
  # Tiempo de Suma Acumulada O(n)
  t_cumsum <- system.time(SimularBrowniano_Cumsum(1, n))["elapsed"]
  
  # Tiempo de Cholesky O(n^3)
  t_chol <- system.time(SimularBrowniano_Cholesky(1, n))["elapsed"]
  
  cat("-> Método lineal (Cumsum):", t_cumsum, "segundos\n")
  cat("-> Método Cholesky (O(n^3)):", t_chol, "segundos\n")
}
cat("CONCLUSIÓN: Al duplicar 'n', Cholesky multiplica su tiempo casi por 8.\n\n")




# ==============================================================================
# Variación Total vs Variación Cuadrática
# ==============================================================================
set.seed(31415)
t_max <- 10
n_particiones <- 2^(2:11)

# Trauectoria Fina
Bt_fino <- SimularBrowniano_Cumsum(t_max, max(n_particiones))

VarTotal <- numeric(length(n_particiones))
VarCuadratica <- numeric(length(n_particiones))

for(i in 1:length(n_particiones)){
  # Extraer los índices para la partición anidada
  step <- max(n_particiones) / n_particiones[i]
  ind <- seq(1, max(n_particiones) + 1, by = step)
  
  # Calcular incrementos de la trayectoria
  dB <- diff(Bt_fino[ind])
  
  # Variación Total (Suma de valores absolutos)
  VarTotal[i] <- sum(abs(dB))
  
  # Variación Cuadrática (Suma de cuadrados)
  VarCuadratica[i] <- sum(dB^2)
}

df_variaciones <- data.frame(n = n_particiones, Total = VarTotal, Cuadratica = VarCuadratica)

# Gráfica 1: Variación Total crece hacia infinito
plot_TV <- ggplot(df_variaciones, aes(x = n, y = Total)) +
  geom_line(color = "red") + geom_point() +
  theme_minimal() +
  labs(title = "Variación Total", x = "n (Tamaño de partición)", y = "Variación Total")

# Gráfica 2: Variación Cuadrática converge a 't'
plot_QV <- ggplot(df_variaciones, aes(x = n, y = Cuadratica)) +
  geom_line(color = "blue") + geom_point() +
  geom_hline(yintercept = t_max, linetype = 'dashed') + # Asintota en y = t
  theme_minimal() +
  labs(title = "Variación Cuadrática", x = "n (Tamaño de partición)", y = "Variación Cuadrática")

print(plot_TV)
print(plot_QV)











# ==============================================================================
# OBJETIVO 3: El Puente Browniano X(t) = B(t) - t*B(1)
# ==============================================================================

# Simulamos un Browniano estándar en [0,1]
t_puente <- 1
n_puente <- 1000
tiempos <- seq(0, t_puente, length.out = n_puente + 1)

# Se usa el método rápido
B_t <- SimularBrowniano_Cumsum(t_puente, n_puente)

# Transformación al Puente Browniano condicionando B(1) = 0
X_t <- B_t - tiempos * B_t[n_puente + 1]

df_puente <- data.frame(Tiempo = tiempos, Browniano = B_t, Puente = X_t)

plot_puente <- ggplot(df_puente, aes(x = Tiempo)) +
  geom_line(aes(y = Browniano, color = "Browniano Original"), alpha = 0.6) +
  geom_line(aes(y = Puente, color = "Puente Browniano (Condicionado)"), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Browniano Original" = "gray", "Puente Browniano (Condicionado)" = "darkgreen")) +
  theme_minimal() +
  labs(title = "Construcción del Puente Browniano", x = "Tiempo", y = "Valor del Proceso", color = "Proceso") +
  theme(legend.position = "bottom")

print(plot_puente)