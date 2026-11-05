# Modelos de Supervivencia y de Series de Tiempo

Código de mis ayudantías y material complementario, ordenado según el temario oficial de la Facultad de Ciencias, UNAM.

## 1. Análisis de supervivencia

| Tema | Código |
|---|---|
| 1.2–1.4 Censura, truncamiento y funciones de supervivencia y riesgo | *pendiente* |
| 1.5 Kaplan-Meier · 1.6 modelos paramétricos · 1.7 prueba log-rank | [`1-5-kaplan-meier.R`](1-5-kaplan-meier.R) |
| 1.5 Tabla de vida y estimador de Nelson-Aalen | *pendiente* |
| 1.8 Modelo de riesgos proporcionales de Cox y verificación del supuesto | [`1-8-cox.R`](1-8-cox.R) |
| Aplicación completa: cáncer de mama | [`1-9-aplicacion-cancer-de-mama.R`](1-9-aplicacion-cancer-de-mama.R) |

## 2. Series de tiempo

| Tema | Código |
|---|---|
| 2.1 Series como procesos estocásticos: caminata aleatoria | [`2-1-caminata-aleatoria.R`](2-1-caminata-aleatoria.R) |
| Preparación de bases de series de tiempo | [`2-1-preparacion-de-bases.R`](2-1-preparacion-de-bases.R) |
| 2.2 Tendencia, estacionalidad y estacionariedad | [`2-2-pruebas-de-estacionariedad.R`](2-2-pruebas-de-estacionariedad.R) · [`2-2-estacionariedad-y-promedios-moviles.R`](2-2-estacionariedad-y-promedios-moviles.R) |
| 2.2 Suavizamiento exponencial y Holt-Winters | [`2-2-suavizamiento-exponencial.R`](2-2-suavizamiento-exponencial.R) · [`2-2-holt-winters.R`](2-2-holt-winters.R) |
| 2.3.1 Modelos autorregresivos | [`2-3-autorregresivo-tesla.R`](2-3-autorregresivo-tesla.R) |
| 2.3.2–2.3.3 Promedios móviles y ARMA: simulación e identificación con ACF/PACF | *pendiente* |
| 2.4–2.5 Construcción de modelos ARIMA y pronóstico | [`2-4-arima.R`](2-4-arima.R) · [`2-4-arima-varias-bases.R`](2-4-arima-varias-bases.R) |
| Extensión: modelos estacionales SARIMA | [`2-5-sarima.R`](2-5-sarima.R) |

## Datos

Los scripts leen sus datos desde una carpeta `datos/`. Las bases de terceros no se incluyen en el repositorio; se descargan buscándolas por su nombre:

- `Breast Cancer.csv`: base pública de Kaggle
- `milk.txt`, `pigs.txt`, `new_york_births.txt`, `nacimientos_mujeres_CA.txt`: series clásicas de libros de texto; otras se descargan con `quantmod` (Yahoo Finance) o desde la URL indicada en el código
