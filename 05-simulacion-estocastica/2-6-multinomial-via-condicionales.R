################################################################################
# Simulación Estocástica: Método de Composición (Factorización Condicional)
################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(microbenchmark)

set.seed(314159)

# ==============================================================================
# 1. GENERADOR MULTINOMIAL TRIVARIADO (K = 3) USANDO rbinom()
# ==============================================================================
# rbinom acepta vectores en "size". Esto permite simular
# directamente la condicional X2 | X1 para las n_sim simulaciones sin usar un bucle for.

sim_multinomial_3_native <- function(n_sim, n_trials, p_vec) {
  # Asegurar parametros
  if (length(p_vec) != 3) stop("p_vec debe tener longitud 3.")
  if (abs(sum(p_vec) - 1) > 1e-8) stop("Las probabilidades deben sumar 1.")
  if (any(p_vec < 0)) stop("Las probabilidades deben ser no negativas.")
  
  p1 <- p_vec[1]
  p2 <- p_vec[2]
  p3 <- p_vec[3]
  
  # Marginal no condicionada X1 ~ Binomial(n, p1)
  x1 <- rbinom(n = n_sim, size = n_trials, prob = p1)
  
  # Condicional X2 | X1 = x1 ~ Binomial(n - x1, p2 / (1 - p1))
  # R vectoriza automáticamente sobre el vector 'size = n_trials - x1'
  prob_cond_2 <- p2 / (1 - p1)
  x2 <- rbinom(n = n_sim, size = n_trials - x1, prob = prob_cond_2)
  
  #Asignación determinista del remanente X3
  x3 <- n_trials - x1 - x2
  
  return(data.frame(X1 = x1, X2 = x2, X3 = x3))
}

# ==============================================================================
# 2. GENERADOR MULTINOMIAL GENERALIZADO (K >= 2) USANDO rbinom()
# ==============================================================================
# El bucle solo itera sobre las K categorías, manteniendo n_sim vectorizado.

sim_multinomial_native <- function(n_sim, n_trials, p_vec) {
  k <- length(p_vec)
  if (abs(sum(p_vec) - 1) > 1e-8) stop("Las probabilidades deben sumar 1.")
  if (any(p_vec < 0)) stop("Las probabilidades deben ser no negativas.")
  
  # Matriz para almacenar los conteos
  res_mat <- matrix(0L, nrow = n_sim, ncol = k)
  colnames(res_mat) <- paste0("X", 1:k)
  
  n_disponible <- rep(n_trials, n_sim) # Vector de ensayos libres por simulación
  p_acumulado  <- 0                    # Probabilidad acumulada consumida
  
  # Cascada de Binomiales condicionales para las primeras K - 1 componentes
  for (j in 1:(k - 1)) {
    prob_cond <- p_vec[j] / (1 - p_acumulado)
    
    # Simulación vectorizada: size cambia por cada fila según n_disponible
    res_mat[, j] <- rbinom(n = n_sim, size = n_disponible, prob = prob_cond)
    
    # Actualización de estados
    n_disponible <- n_disponible - res_mat[, j]
    p_acumulado  <- p_acumulado + p_vec[j]
  }
  
  # La última categoría absorbe los ensayos restantes
  res_mat[, k] <- n_disponible
  
  return(as.data.frame(res_mat))
}

# ==============================================================================
# 3. VALIDACIÓN ESTADÍSTICA (Momentos Teóricos vs Empíricos)
# ==============================================================================
n_simulaciones <- 20000
n_ensayos      <- 25
probabilidades <- c(0.20, 0.50, 0.30)

muestra <- sim_multinomial_3_native(
  n_sim    = n_simulaciones, 
  n_trials = n_ensayos, 
  p_vec    = probabilidades
)

cat("\n=======================================================\n")
cat("   VALIDACIÓN ANALÍTICA: MULTINOMIAL VÍA rbinom()      \n")
cat("=======================================================\n")

# A. Esperanza Matemática: E[Xi] = n * pi
cat("--- MEDIAS MARGINALES ---\n")
for (i in 1:3) {
  esp_teor <- n_ensayos * probabilidades[i]
  esp_emp  <- mean(muestra[[paste0("X", i)]])
  cat(sprintf("X%d -> Teórica: %6.3f | Muestral: %6.3f\n", i, esp_teor, esp_emp))
}

# B. Varianzas Marginales: Var(Xi) = n * pi * (1 - pi)
cat("\n--- VARIANZAS MARGINALES ---\n")
for (i in 1:3) {
  var_teor <- n_ensayos * probabilidades[i] * (1 - probabilidades[i])
  var_emp  <- var(muestra[[paste0("X", i)]])
  cat(sprintf("X%d -> Teórica: %6.3f | Muestral: %6.3f\n", i, var_teor, var_emp))
}

# C. Matriz de Covarianzas Cruzadas: Cov(Xi, Xj) = -n * pi * pj
cat("\n--- COVARIANZAS CRUZADAS (Restricción de Suma) ---\n")
cov_emp <- cov(muestra)
pares <- list(c(1, 2), c(1, 3), c(2, 3))

for (par in pares) {
  i <- par[1]; j <- par[2]
  cov_teor <- -n_ensayos * probabilidades[i] * probabilidades[j]
  cov_val  <- cov_emp[paste0("X", i), paste0("X", j)]
  cat(sprintf("Cov(X%d, X%d) -> Teórica: %6.3f | Muestral: %6.3f\n", i, j, cov_teor, cov_val))
}

# D. Verificación del Soporte: sum(X_i) == n
cat("\n¿Todas las sumas por fila son idénticas a n?:", 
    all(rowSums(muestra) == n_ensayos), "\n")

# ==============================================================================
# 4. DIAGNÓSTICO GRÁFICO: FRECUENCIAS EMPÍRICAS VS PROBABILIDAD TEÓRICA
# ==============================================================================
df_teorico <- bind_rows(lapply(1:3, function(k) {
  data.frame(
    Categoria = paste0("X", k),
    Conteo    = 0:n_ensayos,
    Prob      = dbinom(0:n_ensayos, size = n_ensayos, prob = probabilidades[k])
  )
}))

df_empirico <- muestra %>%
  pivot_longer(cols = everything(), names_to = "Categoria", values_to = "Conteo") %>%
  count(Categoria, Conteo) %>%
  group_by(Categoria) %>%
  mutate(Prob_Empirica = n / n_simulaciones)

p_comp <- ggplot() +
  geom_col(data = df_empirico, aes(x = Conteo, y = Prob_Empirica, fill = Categoria),
           alpha = 0.65, color = "black", width = 0.7) +
  geom_point(data = df_teorico, aes(x = Conteo, y = Prob), 
             color = "black", size = 1.8) +
  geom_line(data = df_teorico, aes(x = Conteo, y = Prob, group = Categoria), 
            color = "black", linetype = "dashed", linewidth = 0.6) +
  facet_wrap(~ Categoria, scales = "free_y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = paste0("Simulación Multinomial(n = ", n_ensayos, ", p = c(0.2, 0.5, 0.3))"),
    subtitle = "Barras: Densidad empírica condicional | Puntos y líneas: Masa teórica Binomial(n, p_k)",
    x = "Número de Éxitos",
    y = "Probabilidad"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

x11()
print(p_comp)

# ==============================================================================
# 5. BENCHMARK COMPUTACIONAL
# ==============================================================================
# Comparación del algoritmo por composición frente a la función nativa rmultinom() de R
cat("\n--- Evaluación de Rendimiento (n_sim = 50,000) ---\n")
bm <- microbenchmark(
  Condicional_3    = sim_multinomial_3_native(50000, n_ensayos, probabilidades),
  Condicional_Gen  = sim_multinomial_native(50000, n_ensayos, probabilidades),
  Nativo_rmultinom = t(rmultinom(50000, size = n_ensayos, prob = probabilidades)),
  times = 50
)
x11()
print(autoplot(bm))
