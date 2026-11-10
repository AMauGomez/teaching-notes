# =============================================================================
# 3.4 Comparación de técnicas de reducción de dimensión: PCA, MDS,
#     correspondencias múltiples (MCA) y t-SNE sobre los mismos datos
# Objetivo: ver qué conserva cada técnica. PCA y MDS clásico con distancia
#           euclidiana dan el mismo mapa (salvo signo); MCA trabaja con
#           variables categóricas; t-SNE conserva vecindades locales y sirve
#           para visualizar, no para medir distancias entre grupos.
# Datos:    "Niveles de Obesidad" (Kaggle Playground Series S4E2).
#           Archivo esperado: datos/Niveles de Obesidad.csv
# =============================================================================

library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(Rtsne)

datos_1 <- read.csv("datos/Niveles de Obesidad.csv") |> na.omit()

# Muestra aleatoria de 500 personas (las primeras filas no son representativas
# y t-SNE es costoso con miles de puntos)
set.seed(2023)
datos_1 <- datos_1[sample(nrow(datos_1), 500), ]
nivel <- factor(datos_1$NObeyesdad)

# Matriz numérica: continuas + binarias codificadas 0/1 (sin id ni la etiqueta)
si_no <- function(x) as.integer(x == "yes")
datos <- datos_1 |>
  transmute(Hombre = as.integer(Gender == "Male"), Age, Height, Weight,
            Historia_familiar = si_no(family_history_with_overweight),
            FAVC = si_no(FAVC), FCVC, NCP, SMOKE = si_no(SMOKE), CH2O,
            SCC = si_no(SCC), FAF, TUE)

# --- 1. Componentes principales ----------------------------------------------
PCA_centrado <- prcomp(datos, center = TRUE, scale. = TRUE)
summary(PCA_centrado)

fviz_screeplot(PCA_centrado, addlabels = TRUE, main = "PCA centrado y reescalado")
fviz_pca_var(PCA_centrado, col.var = "contrib",
             gradient.cols = c("green", "blue"), repel = TRUE)
fviz_pca_ind(PCA_centrado, geom = "point", habillage = nivel, addEllipses = FALSE,
             title = "Individuos en los dos primeros componentes")
fviz_pca_biplot(PCA_centrado, geom.ind = "point", col.ind = "gray60",
                col.var = "blue", repel = TRUE)

# --- 2. MDS clásico con tres distancias --------------------------------------
z <- scale(datos)
graficar_mds <- function(d, titulo) {
  m <- cmdscale(d, k = 2)
  plot(m, col = nivel, pch = 19, cex = 0.6, xlab = "Coordenada 1",
       ylab = "Coordenada 2", main = titulo)
  invisible(m)
}
m_eucl <- graficar_mds(dist(z), "MDS euclidiano (variables estandarizadas)")

# Con distancia euclidiana sobre datos estandarizados, MDS reproduce el PCA:
# las correlaciones absolutas entre coordenadas deben ser 1
round(abs(diag(cor(m_eucl, PCA_centrado$x[, 1:2]))), 4)

graficar_mds(dist(z, method = "manhattan"), "MDS con distancia Manhattan")
graficar_mds(dist(datos, method = "canberra"), "MDS con distancia Canberra")

# --- 3. Análisis de correspondencias múltiples --------------------------------
# Discretizamos las continuas para trabajar solo con variables categóricas
cat_df <- datos_1 |>
  mutate(Height = Height * 100) |>
  transmute(
    Gender, family_history_with_overweight, FAVC, CAEC, SMOKE, SCC, CALC, MTRANS,
    Edad = cut(Age, c(0, 18, 30, 40, 50, 100),
               labels = c("<=18", "19-30", "31-40", "41-50", "50+"), include.lowest = TRUE),
    Estatura = cut(Height, c(0, 160, 170, 180, 250),
                   labels = c("<160", "160-170", "170-180", "180+"), include.lowest = TRUE),
    Peso = cut(Weight, c(0, 60, 80, 100, 120, 200),
               labels = c("<60", "60-80", "80-100", "100-120", "120+"), include.lowest = TRUE),
    Verduras = cut(FCVC, c(0, 1.5, 2.5, 3), labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE),
    Agua = cut(CH2O, c(0, 1.5, 2.5, 3), labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE),
    Actividad = cut(FAF, c(0, 0.5, 1.5, 3), labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE)
  ) |>
  mutate(across(everything(), as.factor))

modelo_MCA <- MCA(cat_df, ncp = 5, graph = FALSE)
fviz_screeplot(modelo_MCA, addlabels = TRUE)
fviz_mca_var(modelo_MCA, col.var = "cos2",
             gradient.cols = c("green", "orange", "red"), repel = TRUE,
             ggtheme = theme_minimal())
fviz_mca_ind(modelo_MCA, geom = "point", habillage = nivel,
             ggtheme = theme_minimal())

# --- 4. t-SNE ----------------------------------------------------------------
# Es estocástico: fijamos la semilla. Lo corremos con dos perplejidades para ver
# que la forma del mapa depende de ese parámetro. Las distancias entre grupos
# lejanos en un mapa t-SNE no se interpretan.
tsne_mapa <- function(perplejidad) {
  set.seed(42)
  ts <- Rtsne(z, perplexity = perplejidad, theta = 0.5, max_iter = 1500,
              check_duplicates = FALSE)
  ggplot(data.frame(x = ts$Y[, 1], y = ts$Y[, 2], nivel), aes(x, y, color = nivel)) +
    geom_point(size = 1.5) +
    theme_minimal() +
    labs(x = "", y = "", color = "Nivel de obesidad",
         title = paste("t-SNE, perplejidad =", perplejidad))
}
tsne_mapa(10)
tsne_mapa(40)
