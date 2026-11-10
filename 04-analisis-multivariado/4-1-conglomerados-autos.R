# =============================================================================
# 4.1 Análisis de conglomerados: jerárquicos (DIANA y AGNES) y k-medias
# Objetivo: agrupar 205 modelos de autos por sus características técnicas,
#           comparar métodos de enlace y elegir el número de grupos con la
#           silueta.
# Datos:    "Car Price Prediction" (Kaggle, CarPrice_Assignment.csv): 205
#           autos con dimensiones, motor, rendimiento y precio.
# =============================================================================

library(cluster)
library(factoextra)

datos_1 <- read.csv("datos/CarPrice_Assignment.csv")

# Solo variables numéricas técnicas (sin el identificador, sin el riesgo
# asignado por la aseguradora y sin el precio, que usamos después para describir)
vars <- c("wheelbase", "carlength", "carwidth", "carheight", "curbweight",
          "enginesize", "boreratio", "stroke", "compressionratio",
          "horsepower", "peakrpm", "citympg", "highwaympg")
datos <- datos_1[, vars]
rownames(datos) <- make.unique(datos_1$CarName)
datos_z <- scale(datos)

# --- 1. ¿Hay estructura de grupos? Estadística de Hopkins ---------------------
# Valores cercanos a 0.5 indican datos sin estructura; cercanos a 1, agrupables
set.seed(1)
get_clust_tendency(datos_z, n = 50, graph = FALSE)$hopkins_stat

# --- 2. Jerárquico divisivo: DIANA -------------------------------------------
dian <- diana(datos_z, metric = "euclidean")
dian$dc   # coeficiente divisivo

fviz_dend(dian, k = 4, cex = 0.4, color_labels_by_k = TRUE, rect = TRUE,
          main = "DIANA, distancia euclidiana, 4 grupos")
grp_diana <- cutree(as.hclust(dian), k = 4)
table(grp_diana)
fviz_cluster(list(data = datos_z, cluster = grp_diana), geom = "point",
             main = "DIANA en los dos primeros componentes")

# --- 3. Jerárquico aglomerativo: AGNES con tres enlaces -----------------------
# single: encadena y tiende a un grupo enorme más observaciones sueltas
# complete: grupos más compactos; ward: minimiza la varianza dentro de grupos
enlaces <- c("single", "complete", "ward")
agnes_mod <- lapply(setNames(enlaces, enlaces),
                    function(m) agnes(datos_z, metric = "euclidean", method = m))

sapply(agnes_mod, function(a) round(a$ac, 3))                   # coeficiente aglomerativo
sapply(agnes_mod, function(a) table(cutree(as.hclust(a), k = 4)))  # tamaños con k = 4

for (m in enlaces) {
  print(fviz_dend(agnes_mod[[m]], k = 4, cex = 0.4, color_labels_by_k = TRUE,
                  rect = TRUE, main = paste("AGNES, enlace", m)))
}

# --- 4. k-medias -------------------------------------------------------------
# Número de grupos por silueta promedio
fviz_nbclust(datos_z, kmeans, method = "silhouette", nstart = 30)

set.seed(2023)
k_mean <- eclust(datos_z, "kmeans", k = 4, nstart = 30, graph = FALSE)
k_mean$size
fviz_cluster(k_mean, geom = "point", ellipse.type = "euclid",
             ggtheme = theme_minimal())
fviz_silhouette(k_mean, palette = "jco")

# --- 5. Interpretación: perfil de cada grupo ---------------------------------
# Medias de algunas variables y del precio (que no entró al agrupamiento)
perfil <- aggregate(cbind(datos[, c("enginesize", "horsepower", "curbweight", "citympg")],
                          price = datos_1$price),
                    by = list(grupo = k_mean$cluster), FUN = mean)
round(perfil)

# Concordancia entre Ward y k-medias
table(Ward = cutree(as.hclust(agnes_mod$ward), k = 4), kmedias = k_mean$cluster)
