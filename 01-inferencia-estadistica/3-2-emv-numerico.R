################################################################################
# Inferencia Estadística · Estimación por máxima verosimilitud en R
# Aaron Mauricio Gómez Jiménez · Ayudantía, Facultad de Ciencias, UNAM
#
# 1. EMV de la exponencial: fórmula cerrada vs optimización numérica
# 2. Errores estándar con la información de Fisher observada (Hessiana)
# 3. Un caso sin fórmula cerrada: la gamma (dos parámetros)
# 4. Simulación: sesgo del EMV, cota de Cramér-Rao y cobertura del intervalo de Wald
################################################################################

set.seed(2026)

# ---- 1. Exponencial: f(x | lambda) = lambda * exp(-lambda * x) ---------------
n <- 30
lambda_real <- 2
x <- rexp(n, rate = lambda_real)

# Log-verosimilitud: l(lambda) = n log(lambda) - lambda * sum(x)
loglik_exp <- function(lambda, x) length(x) * log(lambda) - lambda * sum(x)

# Fórmula cerrada: derivar e igualar a cero da lambda_hat = 1 / media(x)
lambda_cerrada <- 1 / mean(x)

# Numérico: optim() minimiza, así que usamos menos la log-verosimilitud.
# Optimizamos en log(lambda) para no salir del espacio parametral (lambda > 0).
negll <- function(log_lambda, x) -loglik_exp(exp(log_lambda), x)
opt <- optim(par = 0, fn = negll, x = x, method = "BFGS", hessian = TRUE)
lambda_numerico <- exp(opt$par)

cat(sprintf("EMV cerrado: %.5f   EMV numérico: %.5f\n", lambda_cerrada, lambda_numerico))

# ---- 2. Error estándar: información de Fisher observada ----------------------
# La Hessiana de -l en log(lambda) da la varianza de log(lambda_hat); por el
# método delta, Var(lambda_hat) ~ lambda_hat^2 * Var(log lambda_hat).
se_log <- sqrt(1 / opt$hessian[1, 1])
se_lambda <- lambda_numerico * se_log
# Comparación con la teoría: I(lambda) = n / lambda^2, así que SE = lambda / sqrt(n)
cat(sprintf("SE numérico: %.5f   SE teórico (lambda_hat / sqrt(n)): %.5f\n",
            se_lambda, lambda_cerrada / sqrt(n)))

# Gráfica de la log-verosimilitud con el máximo
grid <- seq(0.5, 4, length.out = 300)
plot(grid, sapply(grid, loglik_exp, x = x), type = "l", lwd = 2,
     xlab = expression(lambda), ylab = "log-verosimilitud",
     main = "Log-verosimilitud de la exponencial")
abline(v = lambda_cerrada, lty = 2, col = "red")

# ---- 3. Gamma(alpha, beta): no hay fórmula cerrada para alpha ---------------
y <- rgamma(200, shape = 3, rate = 1.5)
negll_gamma <- function(theta, y) {
  alpha <- exp(theta[1]); beta <- exp(theta[2])           # ambos positivos
  -sum(dgamma(y, shape = alpha, rate = beta, log = TRUE))
}
opt_g <- optim(c(0, 0), negll_gamma, y = y, method = "BFGS", hessian = TRUE)
theta_hat <- exp(opt_g$par)
se_g <- theta_hat * sqrt(diag(solve(opt_g$hessian)))      # método delta
print(data.frame(parametro = c("alpha", "beta"), real = c(3, 1.5),
                 emv = round(theta_hat, 4), se = round(se_g, 4)))
# Comparación con un paquete: MASS::fitdistr(y, "gamma") debe dar lo mismo

# ---- 4. Simulación: ¿qué tan bueno es el EMV de la exponencial? -------------
# El EMV 1/media es sesgado: E[lambda_hat] = n lambda / (n - 1).
# La cota de Cramér-Rao para estimadores insesgados es lambda^2 / n.
simular <- function(n, lambda, B = 10000) {
  est <- replicate(B, 1 / mean(rexp(n, lambda)))
  se  <- est / sqrt(n)
  c(n = n,
    media_emv = mean(est),
    sesgo = mean(est) - lambda,
    sesgo_teorico = lambda / (n - 1),
    var_emv = var(est),
    cota_cr = lambda^2 / n,
    cobertura_wald = mean(abs(est - lambda) <= 1.96 * se))
}
resultados <- t(sapply(c(5, 10, 30, 100, 500), simular, lambda = lambda_real))
print(round(resultados, 4))
# Lectura: el sesgo coincide con lambda/(n-1) y desaparece cuando n crece;
# con n pequeña la varianza del EMV supera la cota de Cramér-Rao, y se acerca
# a ella cuando n crece (eficiencia asintótica). En este modelo la cobertura del
# intervalo de Wald queda cerca de 95% incluso con n = 5; ejercicio: repetir la
# simulación para otro parámetro (por ejemplo, la media 1/lambda) y comparar.
