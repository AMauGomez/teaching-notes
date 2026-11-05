set.seed(42)
M <- 20000

# --- 1. Teoria rapida  ---
P <- matrix(c(1,   0,   0,   0,
              0.5, 0,   0.5, 0,
              0,   0.5, 0,   0.5,
              0,   0,   0,   1), nrow = 4, byrow = TRUE)

# paso n=2 para X0=1
P2_teorica <- (P %*% P)[2, ]

# Matriz fundamental (I - Q)^(-1) para sacar el tiempo medio
Q <- P[2:3, 2:3]
tiempo_teorico <- sum(solve(diag(2) - Q)[1, ]) 

tiempo_teorico


# --- Pasos esperados antes de la primera llegada ---
# correr con while hasta tocar {0, 3} y cuenta pasos
simular_llegada <- function() {
  pos <- 1
  pasos <- 0
  while (pos > 0 && pos < 3) {
    pos <- pos + if (runif(1) <= 0.5) 1 else -1
    pasos <- pasos + 1
  }
  return(pasos)
}

tiempos_mc <- replicate(M, simular_llegada())

cat("Tiempo medio teorico:", tiempo_teorico, "\n")
cat("Tiempo medio Monte Carlo:", mean(tiempos_mc), "\n\n")


# --- Probabilidades de transicion a n = 2 pasos ---
# corre con for fijo de 2 pasos y revisa donde cayo
simular_paso2 <- function() {
  pos <- 1
  for (t in 1:2) {
    if (pos == 0 || pos == 3) break
    pos <- pos + if (runif(1) <= 0.5) 1 else -1
  }
  return(pos)
}

estados_n2 <- replicate(M, simular_paso2())
proba_mc <- table(factor(estados_n2, levels = 0:3)) / M

cat("Probas teoricas P^2:", P2_teorica, "\n")
cat("Probas Monte Carlo :", as.numeric(proba_mc), "\n")

