# Simulación Estocástica

Código de mis ayudantías y material complementario, ordenado según el temario oficial de la Facultad de Ciencias, UNAM (clave 0873).

## 1. Introducción a la simulación

| Tema | Código |
|---|---|
| 1.2–1.3 Generadores congruenciales y pruebas para números pseudoaleatorios | *pendiente* |

## 2. Métodos para generar variables

| Tema | Código |
|---|---|
| 2.1 Transformada inversa | [`2-1-transformada-inversa.R`](2-1-transformada-inversa.R) |
| 2.2 Aceptación y rechazo | [`2-2-aceptacion-y-rechazo-beta.R`](2-2-aceptacion-y-rechazo-beta.R) |
| 2.3–2.4 Cociente de uniformes, Box-Müller y Marsaglia | *pendiente* |
| 2.5–2.6 Variables discretas y multivariadas (multinomial vía condicionales) | [`2-6-multinomial-via-condicionales.R`](2-6-multinomial-via-condicionales.R) |

## 3. Integración Monte Carlo

| Tema | Código |
|---|---|
| 3.1–3.3 Monte Carlo y reducción de varianza (antitéticas, control, importancia) | *pendiente* |

## 4. Cadenas de Markov Monte Carlo

| Tema | Código |
|---|---|
| 4.2–4.3 Muestreo de Gibbs y Metropolis-Hastings | *pendiente* |

## 5. Algoritmos para datos faltantes

| Tema | Código |
|---|---|
| 5.3–5.4 Algoritmo EM y data augmentation | *pendiente* |

## 6. Remuestreo

| Tema | Código |
|---|---|
| 6.1–6.3 Bootstrap y jackknife | *pendiente* |

## Temas complementarios: procesos estocásticos

| Tema | Código |
|---|---|
| Ruina del jugador | [`c-ruina-del-jugador.R`](c-ruina-del-jugador.R) |
| Movimiento browniano: complejidad computacional | [`c-movimiento-browniano.R`](c-movimiento-browniano.R) |
| Movimiento browniano: complejidad y variaciones | [`c-movimiento-browniano-variaciones.Rmd`](c-movimiento-browniano-variaciones.Rmd) |
| Ecuaciones diferenciales estocásticas con datos de Bitcoin | [`c-ede-bitcoin.Rmd`](c-ede-bitcoin.Rmd) |

## Datos

Los scripts leen sus datos desde una carpeta `datos/`. Las bases de terceros no se incluyen en el repositorio; se descargan buscándolas por su nombre:

- Los ejemplos usan números simulados; el de ecuaciones diferenciales estocásticas descarga precios de Bitcoin de Yahoo Finance con `quantmod`.
