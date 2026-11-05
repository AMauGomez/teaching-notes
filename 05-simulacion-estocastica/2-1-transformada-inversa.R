################################################################################
###
# Generación de Variables Aleatorias: Método de la Transformada Inversa
# 1. Distribución Exponencial (Exp(lambda))
# 2. Distribución Beta(a, 1)
################################################################################

# 
library(ggplot2) #plots
library(ggthemes) #estilo
library(dplyr) #pipes
library(DescTools) #mometos y estadoisticas
library(microbenchmark) #tiempo de computo

set.seed(314159)

# ==============================================================================
# 1. DISTRIBUCIÓN EXPONENCIAL: Exp(lambda)
# Con Inversa analítica
# ==============================================================================

# Usando un for
exp_loop <- function(n, lambda = 1) {
  if (lambda <= 0) stop("lambda debe ser estrictamente positivo")
  if (n <= 0) stop("n debe ser un entero positivo")
  
  res <- numeric(n)
  for (i in 1:n) {
    u <- runif(1)
    res[i] <- -log(u) / lambda
  }
  return(data.frame(sample = res))
}

# usando vectorización 
exp_vec <- function(n, lambda = 1) {
  if (lambda <= 0) stop("lambda debe ser estrictamente positivo")
  if (n <= 0) stop("n debe ser un entero positivo")
  
  u <- runif(n)
  res <- -log(u) / lambda
  return(data.frame(sample = res))
}





# --- Simulacion de una Exp(2) con una muestra de 5000 valores---
n_exp <- 5000
lambda_val <- 2

datos_exp <- exp_vec(n = n_exp, lambda = lambda_val)

cat("\n=======================================================\n")
cat("          RESUMEN: DISTRIBUCIÓN EXPONENCIAL            \n")
cat("=======================================================\n")
summary(datos_exp$sample)
cat("Media Teórica:", 1 / lambda_val, " | Media Muestral:", mean(datos_exp$sample), "\n")
cat("Varianza Teórica:", 1 / (lambda_val^2), " | Varianza Muestral:", var(datos_exp$sample), "\n")
cat("Sesgo (Skewness):", Skew(datos_exp$sample), " (Teórico: 2)\n")
cat("Curtosis:", Kurt(datos_exp$sample), " (Teórico: 6)\n")

# Gráfico de densidad muestral vs teórica

x11()
ggplot(datos_exp, aes(x = sample)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, color = "black", fill = "green", alpha = 0.75) +
  stat_function(fun = function(x) dexp(x, rate = lambda_val), color = "steelblue", linewidth = 1.2) +
  labs(
    title = paste0("Exponencial: Muestra vs Densidad Teórica (lambda = ", lambda_val, ")"),
    x = "x",
    y = "Densidad"
  ) +
  theme_minimal()

# Gráfico Q-Q
x11()
ggplot(datos_exp, aes(sample = sample)) +
  stat_qq(distribution = qexp, dparams = list(rate = lambda_val), color = "green", alpha = 0.6) +
  stat_qq_line(distribution = qexp, dparams = list(rate = lambda_val), color = "black", linetype = "dashed") +
  labs(
    title = "Q-Q Plot: Distribución Exponencial",
    x = "Cuantiles Teóricos",
    y = "Cuantiles Muestrales"
  ) +
  theme_minimal()

# Prueba de bondad de ajuste Kolmogorov-Smirnov
ks_exp <- ks.test(datos_exp$sample, "pexp", rate = lambda_val)
print(ks_exp)


# Benchmark comparativo de la Exponencial con n=10,000
cat("\n--- Benchmark Exponencial (n = 10,000, 500 repeticiones) ---\n")
bm_exp <- microbenchmark(
  For_Loop    = exp_loop(10000, lambda = lambda_val),
  Vectorizado = exp_vec(10000, lambda = lambda_val),
  Nativo_R    = rexp(10000, rate = lambda_val),
  times = 500
)
print(bm_exp)

x1()
autoplot(bm_exp) +
  labs(title = "Comparación de Tiempos de Ejecución: Exponencial(lambda)") +
  theme_minimal()

























# ==============================================================================
# 2. DISTRIBUCIÓN BETA: Beta(a, 1)
# Densidad: f(x) = a * x^(a - 1),  x en [0, 1]
# FDA: F(x) = x^a
# Inversa analítica: X = U^(1 / a)
# ==============================================================================

# 2.1 Implementación con ciclo for
beta_loop <- function(n, a = 2) {
  if (a <= 0) stop("El parámetro 'a' debe ser estrictamente positivo")
  if (n <= 0) stop("n debe ser un entero positivo")
  
  res <- numeric(n)
  for (i in 1:n) {
    u <- runif(1)
    res[i] <- u^(1 / a)
  }
  return(data.frame(sample = res))
}

# 2.2 Implementación vectorizada
beta_vec <- function(n, a = 2) {
  if (a <= 0) stop("El parámetro 'a' debe ser estrictamente positivo")
  if (n <= 0) stop("n debe ser un entero positivo")
  
  u <- runif(n)
  res <- u^(1 / a)
  return(data.frame(sample = res))
}

# --- Análisis y validación de Beta(a, 1) ---
n_beta <- 5000
a_val <- 3 # Parámetro de forma a = 3, b = 1

datos_beta <- beta_vec(n = n_beta, a = a_val)

# Parámetros teóricos de Beta(a, 1):
# E[X] = a / (a + 1)
# Var(X) = a / ((a + 1)^2 * (a + 2))
media_teorica_beta <- a_val / (a_val + 1)
var_teorica_beta   <- a_val / (((a_val + 1)^2) * (a_val + 2))

cat("\n=======================================================\n")
cat("            RESUMEN: DISTRIBUCIÓN BETA(a, 1)           \n")
cat("=======================================================\n")
summary(datos_beta$sample)
cat("Media Teórica:", media_teorica_beta, " | Media Muestral:", mean(datos_beta$sample), "\n")
cat("Varianza Teórica:", var_teorica_beta, " | Varianza Muestral:", var(datos_beta$sample), "\n")
cat("Sesgo (Skewness):", Skew(datos_beta$sample), "\n")
cat("Curtosis:", Kurt(datos_beta$sample), "\n")

# Gráfico de densidad empírica vs teórica
ggplot(datos_beta, aes(x = sample)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black", fill = "darkred", alpha = 0.75) +
  stat_function(fun = function(x) dbeta(x, shape1 = a_val, shape2 = 1), color = "steelblue", linewidth = 1.2) +
  labs(
    title = paste0("Beta(a = ", a_val, ", 1): Muestra vs Densidad Teórica"),
    x = "x",
    y = "Densidad"
  ) +
  theme_minimal()

# Gráfico Q-Q
ggplot(datos_beta, aes(sample = sample)) +
  stat_qq(distribution = qbeta, dparams = list(shape1 = a_val, shape2 = 1), color = "darkred", alpha = 0.6) +
  stat_qq_line(distribution = qbeta, dparams = list(shape1 = a_val, shape2 = 1), color = "black", linetype = "dashed") +
  labs(
    title = paste0("Q-Q Plot: Distribución Beta(a = ", a_val, ", 1)"),
    x = "Cuantiles Teóricos",
    y = "Cuantiles Muestrales"
  ) +
  theme_minimal()

# Prueba de bondad de ajuste Kolmogorov-Smirnov
ks_beta <- ks.test(datos_beta$sample, "pbeta", shape1 = a_val, shape2 = 1)
print(ks_beta)

# Benchmark comparativo de la Beta(a, 1)
cat("\n--- Benchmark Beta(a, 1) (n = 10,000, 500 repeticiones) ---\n")
bm_beta <- microbenchmark(
  For_Loop    = beta_loop(10000, a = a_val),
  Vectorizado = beta_vec(10000, a = a_val),
  Nativo_R    = rbeta(10000, shape1 = a_val, shape2 = 1),
  times = 500
)
print(bm_beta)

# Visualización comparativa del rendimiento
autoplot(bm_beta) +
  labs(title = "Comparación de Tiempos de Ejecución: Beta(a, 1)") +
  theme_minimal()