# Análisis Multivariado

Código de mis ayudantías y material complementario, ordenado según el temario oficial de la Facultad de Ciencias, UNAM (clave 1815).

## 1. Álgebra de matrices y distribuciones multivariadas

| Tema | Código |
|---|---|
| 1.1 Descomposiciones espectral y en valores singulares · 1.4 normal multivariada | *pendiente* |
| 1.5 T² de Hotelling | [`1-5-t2-de-hotelling.R`](1-5-t2-de-hotelling.R) |

## 2. Análisis descriptivo de datos multivariados

| Tema | Código |
|---|---|
| 2.2 Representación gráfica (incluye curvas de Andrews) | [`2-2-analisis-descriptivo.R`](2-2-analisis-descriptivo.R) |

## 3. Técnicas de reducción de dimensión

| Tema | Código |
|---|---|
| 3.1 Componentes principales | [`3-1-pca-geopolitica.Rmd`](3-1-pca-geopolitica.Rmd) · [`3-1-pca-estres-estudiantes.R`](3-1-pca-estres-estudiantes.R) |
| 3.2 Análisis de factores | [`3-2-analisis-factorial.Rmd`](3-2-analisis-factorial.Rmd) |
| 3.3 Análisis de correspondencias | [`3-3-analisis-de-correspondencias.Rmd`](3-3-analisis-de-correspondencias.Rmd) · [`3-3-correspondencias-ejemplo-clinico.R`](3-3-correspondencias-ejemplo-clinico.R) |
| 3.4 Escalamiento multidimensional | [`3-4-escalamiento-multidimensional.R`](3-4-escalamiento-multidimensional.R) · [`3-4-escalamiento-multidimensional-2.R`](3-4-escalamiento-multidimensional-2.R) · [`3-4-tsne-pca-mds.R`](3-4-tsne-pca-mds.R) |

## 4. Métodos de clasificación

| Tema | Código |
|---|---|
| 4.1 Análisis de conglomerados (jerárquicos y k-medias) | [`4-1-conglomerados-ruspini.Rmd`](4-1-conglomerados-ruspini.Rmd) · [`4-1-conglomerados-autos.R`](4-1-conglomerados-autos.R) |
| 4.1.4 Conglomerados basados en modelos | *pendiente* |
| 4.2 Análisis discriminante lineal y cuadrático | [`4-2-discriminante-sp500.Rmd`](4-2-discriminante-sp500.Rmd) |

## Temas complementarios

| Tema | Código |
|---|---|
| Máquinas de soporte vectorial | [`c-svm.Rmd`](c-svm.Rmd) |
| k vecinos más cercanos | [`c-knn.Rmd`](c-knn.Rmd) |
| Imputación múltiple con MICE | [`c-imputacion-mice.Rmd`](c-imputacion-mice.Rmd) |

## Datos

Cada script indica en su encabezado el tema, el objetivo y la fuente de sus datos. Los scripts leen desde una carpeta `datos/`; las bases de terceros no se incluyen en el repositorio y se descargan buscándolas por su nombre:

- `ile_2019_2021.csv`: Interrupción legal del embarazo, Portal de Datos Abiertos de la CDMX
- `Wholesale customers data.csv`: UCI Machine Learning Repository
- `CarPrice_Assignment.csv`, `Medidas_Corporales.csv`, `Niveles de Obesidad.csv`, `StressLevelDataset.csv`, `geopol.csv`, `matrixbrands.csv`, `survey lung cancer.csv` y otras: bases públicas de Kaggle
- El discriminante usa `Smarket` del paquete `ISLR2`, y la T² de Hotelling usa `palmerpenguins` y `swiss`; no requieren descarga.

Todos los scripts se corrieron completos en R 4.5 (noviembre de 2026).
