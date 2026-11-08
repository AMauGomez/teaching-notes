# Modelos No Paramétricos y de Regresión

Código de mis ayudantías y material complementario, ordenado según el temario oficial de la Facultad de Ciencias, UNAM (clave 1639).

## El curso como un caso de estudio

Además del código de cada tema, la materia tiene **un caso de principio a fin**: *¿cómo se fija la tasa de una hipoteca y los brókers cobran más, a igual riesgo?*, con 46,190 hipotecas reales de 2024 (Freddie Mac). Cada capítulo recorre una unidad del temario con los mismos datos: **[mortgage-pricing-regression](https://github.com/AMauGomez/mortgage-pricing-regression)** ([versión para leer en línea](https://amaugomez.github.io/mortgage-pricing-regression/)).

| Unidad del temario | Capítulo del caso |
|---|---|
| 1. Pruebas no paramétricas (binomiales, McNemar, Cox y Stuart, rangos, bondad de ajuste, contingencia) | [1. Pruebas no paramétricas](https://amaugomez.github.io/mortgage-pricing-regression/chapters/1-pruebas-no-parametricas.html) |
| 2.1–2.2 Regresión lineal simple (incluye prueba F de carencia de ajuste y forma matricial) | [2. Regresión lineal simple](https://amaugomez.github.io/mortgage-pricing-regression/chapters/2-regresion-lineal-simple.html) |
| 2.3–2.4 Regresión múltiple y selección de modelos | [3. Regresión múltiple](https://amaugomez.github.io/mortgage-pricing-regression/chapters/3-regresion-multiple.html) |
| 2.5–2.6 ANOVA y ANCOVA | [4. ANOVA y ANCOVA](https://amaugomez.github.io/mortgage-pricing-regression/chapters/4-anova-y-ancova.html) |
| Más allá del curso: MANOVA y MANCOVA | [5. MANOVA y MANCOVA](https://amaugomez.github.io/mortgage-pricing-regression/chapters/5-manova-y-mancova.html) |

## 1. Pruebas no paramétricas

| Tema | Código |
|---|---|
| Repaso: pruebas de hipótesis | [`1-0-repaso-pruebas-de-hipotesis.Rmd`](1-0-repaso-pruebas-de-hipotesis.Rmd) |
| 1.1 Pruebas binomiales (proporciones, cuantiles, signos) | [`1-1-pruebas-binomiales.Rmd`](1-1-pruebas-binomiales.Rmd) |
| 1.1.4–1.1.5 McNemar y Cox-Stuart | en el caso: [capítulo 1](https://amaugomez.github.io/mortgage-pricing-regression/chapters/1-pruebas-no-parametricas.html) |
| 1.2 Pruebas de rango: Mann-Whitney y Wilcoxon | [`1-2-wilcoxon-mann-whitney.Rmd`](1-2-wilcoxon-mann-whitney.Rmd) |
| 1.2 Pruebas de rango: Friedman, Spearman y Kendall | [`1-2-friedman-spearman-kendall.Rmd`](1-2-friedman-spearman-kendall.Rmd) |
| 1.2 Prueba de rachas | [`1-2-prueba-de-rachas.Rmd`](1-2-prueba-de-rachas.Rmd) |
| 1.2.2–1.2.3 Kruskal-Wallis y varianza de más de dos poblaciones | en el caso: [capítulo 1](https://amaugomez.github.io/mortgage-pricing-regression/chapters/1-pruebas-no-parametricas.html) |
| 1.3 Bondad de ajuste: Kolmogorov-Smirnov | [`1-3-kolmogorov-smirnov.Rmd`](1-3-kolmogorov-smirnov.Rmd) |
| 1.3 Bondad de ajuste: Lilliefors y Anderson-Darling | [`1-3-bondad-de-ajuste-lilliefors.R`](1-3-bondad-de-ajuste-lilliefors.R) |
| 1.3.1 Ji cuadrada de bondad de ajuste | en el caso: [capítulo 1](https://amaugomez.github.io/mortgage-pricing-regression/chapters/1-pruebas-no-parametricas.html) |
| 1.4 Tablas de contingencia: prueba exacta de Fisher | [`1-4-prueba-exacta-de-fisher.Rmd`](1-4-prueba-exacta-de-fisher.Rmd) |
| 1.4.1–1.4.3 Independencia, proporciones y mediana | en el caso: [capítulo 1](https://amaugomez.github.io/mortgage-pricing-regression/chapters/1-pruebas-no-parametricas.html) |

## 2. Análisis de regresión y varianza

| Tema | Código |
|---|---|
| 2.1 Panorama: por qué graficar antes de ajustar (Datasaurus) | [`2-1-datasaurus.R`](2-1-datasaurus.R) |
| 2.1 Preparación de bases para regresión | [`2-1-preparacion-de-bases.Rmd`](2-1-preparacion-de-bases.Rmd) |
| 2.2 Regresión lineal simple desde cero: mínimos cuadrados, intervalos, predicción y falta de ajuste | en el caso: [capítulo 2](https://amaugomez.github.io/mortgage-pricing-regression/chapters/2-regresion-lineal-simple.html) |
| 2.2.13 Análisis de residuales y supuestos (continuación de los datos de vinos) | [`2-2-regresion-y-supuestos-vinos.Rmd`](2-2-regresion-y-supuestos-vinos.Rmd) |
| 2.2.12 Transformaciones: regresión polinomial | [`2-2-regresion-polinomial.Rmd`](2-2-regresion-polinomial.Rmd) |
| 2.3 Regresión lineal múltiple | [`2-3-regresion-multiple-autos.Rmd`](2-3-regresion-multiple-autos.Rmd) · [`2-3-regresion-seguros.Rmd`](2-3-regresion-seguros.Rmd) |
| 2.3.8 Diagnósticos: observaciones influyentes y distancia de Cook | [`2-3-distancia-de-cook.Rmd`](2-3-distancia-de-cook.Rmd) |
| 2.3.12 Multicolinealidad y sus efectos | [`2-3-multicolinealidad.Rmd`](2-3-multicolinealidad.Rmd) |
| 2.4 Selección de modelos | [`2-4-stepwise-bienes-raices.Rmd`](2-4-stepwise-bienes-raices.Rmd) · [`2-4-stepwise-y-prediccion.R`](2-4-stepwise-y-prediccion.R) · [`2-4-pca-y-stepwise-autos.R`](2-4-pca-y-stepwise-autos.R) · [`2-4-regresion-ovario-poliquistico.R`](2-4-regresion-ovario-poliquistico.R) |
| 2.5 Análisis de varianza (ANOVA) | en el caso: [capítulo 4](https://amaugomez.github.io/mortgage-pricing-regression/chapters/4-anova-y-ancova.html) |
| 2.6 Análisis de covarianza (ANCOVA) | en el caso: [capítulo 4](https://amaugomez.github.io/mortgage-pricing-regression/chapters/4-anova-y-ancova.html) |

## Datos

Los scripts leen sus datos desde una carpeta `datos/`. Las bases de terceros no se incluyen en el repositorio; se descargan buscándolas por su nombre:

- `wine.xlsx`: conjunto *Wine* del UCI Machine Learning Repository (CC BY 4.0)
- `CarPrice_Assignment.csv`, `insurance.csv`, `Real estate.csv`, `PCOS_data_without_infertility*.xlsx`, `binomial-data.csv`, `seguros_*.csv`: bases públicas de Kaggle
