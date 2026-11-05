"""
Inferencia Estadística · Estimación por máxima verosimilitud en Python
Aaron Mauricio Gómez Jiménez · Ayudantía, Facultad de Ciencias, UNAM

Mismo contenido que 3-2-emv-numerico.R:
1. EMV de la exponencial: fórmula cerrada vs optimización numérica
2. Error estándar con la información de Fisher observada
3. La gamma (sin fórmula cerrada)
4. Simulación: sesgo, cota de Cramér-Rao y cobertura del intervalo de Wald
"""
import sys

import numpy as np
from scipy import optimize, stats

sys.stdout.reconfigure(encoding="utf-8")
rng = np.random.default_rng(2026)

# ---- 1. Exponencial ------------------------------------------------------------
n, lambda_real = 30, 2.0
x = rng.exponential(scale=1 / lambda_real, size=n)


def loglik_exp(lam, x):
    return len(x) * np.log(lam) - lam * np.sum(x)


lambda_cerrada = 1 / x.mean()

# Optimizamos en log(lambda) para respetar lambda > 0
res = optimize.minimize(lambda t: -loglik_exp(np.exp(t[0]), x), x0=[0.0], method="BFGS")
lambda_numerico = np.exp(res.x[0])
print(f"EMV cerrado: {lambda_cerrada:.5f}   EMV numérico: {lambda_numerico:.5f}")

# ---- 2. Error estándar -----------------------------------------------------------
# BFGS devuelve una aproximación de la inversa de la Hessiana: la varianza de log(lambda_hat)
se_lambda = lambda_numerico * np.sqrt(res.hess_inv[0, 0])
print(f"SE numérico: {se_lambda:.5f}   SE teórico: {lambda_cerrada / np.sqrt(n):.5f}")

# ---- 3. Gamma ----------------------------------------------------------------
y = rng.gamma(shape=3.0, scale=1 / 1.5, size=200)


def negll_gamma(theta, y):
    alpha, beta = np.exp(theta)
    return -np.sum(stats.gamma.logpdf(y, a=alpha, scale=1 / beta))


res_g = optimize.minimize(negll_gamma, x0=[0.0, 0.0], args=(y,), method="BFGS")
alpha_hat, beta_hat = np.exp(res_g.x)
print(f"Gamma: alpha_hat = {alpha_hat:.4f} (real 3), beta_hat = {beta_hat:.4f} (real 1.5)")
# Comparación con scipy: stats.gamma.fit(y, floc=0) devuelve (alpha, 0, 1/beta)


# ---- 4. Simulación -----------------------------------------------------------
def simular(n, lam, B=10_000):
    est = 1 / rng.exponential(scale=1 / lam, size=(B, n)).mean(axis=1)
    se = est / np.sqrt(n)
    return {
        "n": n,
        "media_emv": est.mean(),
        "sesgo": est.mean() - lam,
        "sesgo_teorico": lam / (n - 1),
        "var_emv": est.var(ddof=1),
        "cota_cr": lam**2 / n,
        "cobertura_wald": np.mean(np.abs(est - lam) <= 1.96 * se),
    }


print(f"{'n':>5} {'sesgo':>9} {'teórico':>9} {'var':>9} {'cota CR':>9} {'cobertura':>10}")
for m in (5, 10, 30, 100, 500):
    r = simular(m, lambda_real)
    print(f"{r['n']:>5} {r['sesgo']:>9.4f} {r['sesgo_teorico']:>9.4f} "
          f"{r['var_emv']:>9.4f} {r['cota_cr']:>9.4f} {r['cobertura_wald']:>10.3f}")
