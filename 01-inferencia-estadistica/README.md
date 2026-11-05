# Inferencia Estadística

Clave 1541, Facultad de Ciencias, UNAM.

## El curso como un caso de estudio

Doy esta materia con **un solo caso de principio a fin**: una pregunta planteada el primer día, *"¿en hora pico atropellan a alguien cada media hora en la CDMX?"*, que cada unidad del temario ayuda a responder con datos reales del C5. El temario sugiere que al final del curso se haga un análisis completo de un conjunto de datos; aquí ese análisis es el hilo de todo el curso.

El caso está en su propio repositorio: **[cdmx-pedestrian-crashes](https://github.com/AMauGomez/cdmx-pedestrian-crashes)** ([versión para leer en línea](https://amaugomez.github.io/cdmx-pedestrian-crashes/)).

| Unidad del temario | Capítulo del caso |
|---|---|
| 1. Introducción y distribuciones muestrales (bajo normalidad, estadísticas de orden) | [1. Distribuciones muestrales](https://amaugomez.github.io/cdmx-pedestrian-crashes/chapters/1-distribuciones-muestrales.html) |
| 2. Análisis exploratorio de datos | [2. Análisis exploratorio](https://amaugomez.github.io/cdmx-pedestrian-crashes/chapters/2-analisis-exploratorio.html) |
| 3. Estimación puntual | [3. Estimación puntual](https://amaugomez.github.io/cdmx-pedestrian-crashes/chapters/3-estimacion-puntual.html) |
| 4. Estimación por intervalos | [4. Intervalos de confianza](https://amaugomez.github.io/cdmx-pedestrian-crashes/chapters/4-intervalos-de-confianza.html) |
| 5. Pruebas de hipótesis | [5. Pruebas de hipótesis](https://amaugomez.github.io/cdmx-pedestrian-crashes/chapters/5-pruebas-de-hipotesis.html) |
| Más allá del curso | [6. Modelos de frecuencia (GLM)](https://amaugomez.github.io/cdmx-pedestrian-crashes/chapters/6-modelos-de-frecuencia.html) |

## Material complementario en esta carpeta

| Tema | Código |
|---|---|
| 2.1–2.4 Análisis exploratorio con otro conjunto de datos (vinos) | [`2-analisis-exploratorio-vinos.Rmd`](2-analisis-exploratorio-vinos.Rmd) |
| 3.2.2 Máxima verosimilitud con solución numérica (el temario pide ejercicios con distribuciones de colas pesadas) | [`3-2-emv-numerico.R`](3-2-emv-numerico.R) · [`3-2-emv-numerico.py`](3-2-emv-numerico.py) |
| Pruebas t para medias (vinos) | [`5-prueba-t-de-student.R`](5-prueba-t-de-student.R) · [`5-pruebas-t-vinos.Rmd`](5-pruebas-t-vinos.Rmd) |

Los scripts de vinos leen `datos/wine.xlsx`: el conjunto *Wine* del UCI Machine Learning Repository (178 vinos de tres cultivares, CC BY 4.0), que no se incluye en el repositorio.
