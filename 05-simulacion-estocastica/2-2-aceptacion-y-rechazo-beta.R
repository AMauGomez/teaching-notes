################################################################################
###
# Simulación Estocástica: Método de Aceptación y Rechazo
###
################################################################################

library(ggplot2)
library(ggthemes)
library(dplyr)
library(DescTools)
library(microbenchmark)

set.seed(314159)

# Implementación Loop
beta_ar_loop <- function(n, a, b) {
  if (a < 1 || b < 1) {
    stop("Se requiere a >= 1 y b >= 1.")
  }
  if (n <= 0 || !is.numeric(n)) {
    stop("El tamaño de muestra n debe ser un entero positivo.")
  }
  
  # Caso base degenerado: Uniforme(0, 1)
  if (a == 1 && b == 1) {
    return(list(sample = runif(n), trials = n, acceptance_rate = 1.0))
  }
  
  # Caso frontera: a = 1, b > 1 (Moda en 0)
  if (a == 1 && b > 1) {
    x_mode <- 0
    max_kernel <- 1
  } 
  # Caso frontera: a > 1, b = 1 (Moda en 1)
  else if (a > 1 && b == 1) {
    x_mode <- 1
    max_kernel <- 1
  } 
  # Caso interior acampanado: a > 1 y b > 1
  else {
    x_mode <- (a - 1) / (a + b - 2)
    max_kernel <- (x_mode^(a - 1)) * ((1 - x_mode)^(b - 1))
  }
  
  res <- numeric(n)
  accepted <- 0
  total_trials <- 0
  
  while (accepted < n) {
    total_trials <- total_trials + 1
    
    # Y ~ Unif(0, 1)
    y <- runif(1)
    # U ~ Unif(0, 1)
    u <- runif(1)
    
    # Razon de aceptacion con nucleo no normalizado
    alpha <- ((y^(a - 1)) * ((1 - y)^(b - 1))) / max_kernel
    
    if (u <= alpha) {
      accepted <- accepted + 1
      res[accepted] <- y
    }
  }
  
  return(list(
    sample = res,
    trials = total_trials,
    acceptance_rate = n / total_trials
  ))
}




# Implementación Vectorizada 
beta_ar_vec <- function(n, a, b) {
  if (a < 1 || b < 1) {
    stop("Se requiere a >= 1 y b >= 1.")
  }
  if (n <= 0) stop("n debe ser un entero positivo.")
  
  if (a == 1 && b == 1) {
    return(list(sample = runif(n), trials = n, acceptance_rate = 1.0))
  }
  
  # Cálculo analitico de la moda
  if (a == 1 && b > 1) {
    x_mode <- 0
    c_const <- b
  } else if (a > 1 && b == 1) {
    x_mode <- 1
    c_const <- a
  } else {
    x_mode <- (a - 1) / (a + b - 2)
    c_const <- dbeta(x_mode, shape1 = a, shape2 = b)
  }
  
  max_kernel <- (x_mode^(a - 1)) * ((1 - x_mode)^(b - 1))
  
  res <- numeric(0)
  total_trials <- 0
  
  #  E[N] = n * c, con un margen de seguridad del 15%
  batch_size <- ceiling(n * c_const * 1.15)
  
  while (length(res) < n) {
    total_trials <- total_trials + batch_size
    
    y <- runif(batch_size)
    u <- runif(batch_size)
    
    alpha <- ((y^(a - 1)) * ((1 - y)^(b - 1))) / max_kernel
    accepted_vals <- y[u <= alpha]
    
    res <- c(res, accepted_vals)
  }
  
  return(list(
    sample = res[1:n],
    trials = total_trials,
    acceptance_rate = n / total_trials
  ))
}




# ==============================================================================
# Validacion
# ==============================================================================

n_sim   <- 10000
a_param <- 4
b_param <- 3

sim_out <- beta_ar_vec(n = n_sim, a = a_param, b = b_param)
muestra <- sim_out$sample



# Momentos teóricos de Beta(a, b)
media_teorica <- a_param / (a_param + b_param)
var_teorica   <- (a_param * b_param) / (((a_param + b_param)^2) * (a_param + b_param + 1))
x_mode        <- (a_param - 1) / (a_param + b_param - 2)
c_teorica     <- dbeta(x_mode, shape1 = a_param, shape2 = b_param)

cat("\n=======================================================\n")
cat("      DIAGNÓSTICO ESTADÍSTICO DE ACEPTACIÓN Y RECHAZO  \n")
cat("=======================================================\n")
cat("Moda Analítica (x*):", round(x_mode, 5), "\n")
cat("Constante Mayorante (c):", round(c_teorica, 5), "\n")
cat("Tasa de Aceptación Teórica (1/c):", round(1 / c_teorica, 5), "\n")
cat("Tasa de Aceptación Empírica:", round(sim_out$acceptance_rate, 5), "\n")
cat("Intentos Totales:", sim_out$trials, "\n\n")

cat("Media Teórica:", round(media_teorica, 5), "| Muestral:", round(mean(muestra), 5), "\n")
cat("Varianza Teórica:", round(var_teorica, 5), "| Muestral:", round(var(muestra), 5), "\n")
cat("Sesgo Muestral:", round(Skew(muestra), 5), "\n")
cat("Curtosis Muestral:", round(Kurt(muestra), 5), "\n")


# Muestreo 
n_puntos <- 2500
y_p <- runif(n_puntos)
u_p <- runif(n_puntos)
max_k <- (x_mode^(a_param - 1)) * ((1 - x_mode)^(b_param - 1))
ratio <- ((y_p^(a_param - 1)) * ((1 - y_p)^(b_param - 1))) / max_k

df_bidimensional <- data.frame(
  x = y_p,
  y = u_p * c_teorica, # rectángulo de altura c
  Estado = ifelse(u_p <= ratio, "Aceptado", "Rechazado")
)

x11()
ggplot() +
  geom_point(data = df_bidimensional, aes(x = x, y = y, color = Estado), alpha = 0.5, size = 1.2) +
  stat_function(fun = function(x) dbeta(x, a_param, b_param), color = "black", linewidth = 1.2) +
  geom_hline(yintercept = c_teorica, color = "darkblue", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("Aceptado" = "darkgreen", "Rechazado" = "firebrick")) +
  labs(
    title = paste0("Mecanismo de Envolvente: Beta(", a_param, ", ", b_param, ")"),
    subtitle = "Curva: Densidad f(x) | Línea azul: Envolvente c * g(x)",
    x = "x (Propuesta Instrumental)",
    y = "Altura de Evaluación"
  ) +
  theme_minimal()

# Histograma Muestral vs Densidad Teórica
x11()
ggplot(data.frame(x = muestra), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, color = "black", fill = "darkred", alpha = 0.75) +
  stat_function(fun = function(x) dbeta(x, a_param, b_param), color = "steelblue", linewidth = 1.2) +
  labs(
    title = paste0("Densidad Muestral vs Teórica Beta(", a_param, ", ", b_param, ")"),
    x = "x",
    y = "Densidad"
  ) +
  theme_minimal()

# Gráfico Q-Q

x11()
ggplot(data.frame(sample = muestra), aes(sample = sample)) +
  stat_qq(distribution = qbeta, dparams = list(shape1 = a_param, shape2 = b_param), color = "darkred", alpha = 0.6) +
  stat_qq_line(distribution = qbeta, dparams = list(shape1 = a_param, shape2 = b_param), color = "black", linetype = "dashed") +
  labs(
    title = "Q-Q Plot: Muestra Simulada vs Cuantiles Teóricos",
    x = "Cuantiles Teóricos",
    y = "Cuantiles Muestrales"
  ) +
  theme_minimal()

# KS test ===========================================================================
ks_res <- ks.test(muestra, "pbeta", shape1 = a_param, shape2 = b_param)
print(ks_res)



# ==============================================================================
# Costo Computacional
# ==============================================================================
cat("\n--- Evaluación de Rendimiento Computacional (n = 5,000) ---\n")
bm_out <- microbenchmark(
  While_Loop  = beta_ar_loop(5000, a = a_param, b = b_param)$sample,
  Vectorizado = beta_ar_vec(5000, a = a_param, b = b_param)$sample,
  Nativo_R    = rbeta(5000, shape1 = a_param, shape2 = b_param),
  times = 100
)
x11()
autoplot(bm_out)
