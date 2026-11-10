# =============================================================================
# 3.1 Componentes principales y 3.2 análisis de factores: dos ejemplos
# Objetivo: (1) revisar los supuestos de una regresión y luego resumir con PCA
#           un cuestionario de estrés en estudiantes; (2) construir un índice
#           único de "influencia digital" con PCA y compararlo con un análisis
#           factorial por máxima verosimilitud.
# Datos:    datos/StressLevelDataset.csv ("Student Stress Factors", Kaggle) y
#           datos/datos_influencers.csv (base de práctica de la clase: 500
#           creadores de contenido con métricas de audiencia e ingresos).
# =============================================================================

library(lmtest)      # dwtest(), bgtest(), bptest()
library(FactoMineR)  # PCA()
library(factoextra)  # gráficas de PCA
library(psych)       # describe(), KMO(), fa(), scree()
library(GGally)      # ggpairs()
library(qgraph)      # qgraph.loadings()

# ============================ EJEMPLO 1 ======================================
datos <- read.csv("datos/StressLevelDataset.csv")

# --- Regresión de la ansiedad sobre el resto del cuestionario -----------------
lm_1 <- lm(anxiety_level ~ ., data = datos)
summary(lm_1)

# Supuestos sobre los residuos
mean(residuals(lm_1))         # cero por construcción (hay intercepto)
dwtest(lm_1)                  # autocorrelación de orden 1 (Durbin-Watson)
bgtest(lm_1)                  # autocorrelación de orden mayor (Breusch-Godfrey)
bptest(lm_1)                  # heterocedasticidad (Breusch-Pagan)
shapiro.test(residuals(lm_1)) # normalidad
qqnorm(residuals(lm_1)); qqline(residuals(lm_1), col = "blue")

# --- PCA ---------------------------------------------------------------------
# Las variables tienen rangos distintos: hay que estandarizar
describe(datos)

PCA_centrado <- prcomp(datos, center = TRUE, scale. = TRUE)
summary(PCA_centrado)   # proporción de varianza por componente

fviz_screeplot(PCA_centrado, main = "PCA centrado y reescalado", addlabels = TRUE)
fviz_pca_var(PCA_centrado, col.var = "contrib",
             gradient.cols = c("green", "blue"), repel = TRUE)
fviz_pca_ind(PCA_centrado, geom = "point", col.ind = "cos2",
             gradient.cols = c("gray", "purple", "red"))
fviz_pca_biplot(PCA_centrado, geom.ind = "point", col.ind = "gray60",
                col.var = "blue", repel = TRUE)

# ============================ EJEMPLO 2 ======================================
# Meta: un "Índice de Influencia Digital" (un solo número) para rankear creadores
datos_influencers <- read.csv("datos/datos_influencers.csv")

# Ranking ingenuo, solo por seguidores
head(datos_influencers[order(datos_influencers$Seguidores, decreasing = TRUE),
                       c("Nombre", "Seguidores", "Ganancias_Ads")], 10)

datos_numericos <- datos_influencers[, -1]   # quitamos el nombre
ggpairs(datos_numericos)

# --- PCA con FactoMineR -------------------------------------------------------
PCA_inf <- PCA(datos_numericos, scale.unit = TRUE, graph = FALSE)
PCA_inf$eig        # valores propios y varianza explicada
PCA_inf$var$cor    # correlación de cada variable con cada componente

fviz_pca_var(PCA_inf, col.var = "cos2",
             gradient.cols = c("#FF0000", "#E7B800", "#00AFBB"),
             repel = TRUE, title = "PCA (correlaciones de Pearson)")

# Índice 0-100 con el primer componente. El signo de un componente es
# arbitrario: lo orientamos para que más seguidores implique más índice.
cp1 <- PCA_inf$ind$coord[, 1]
if (cor(cp1, datos_numericos$Seguidores) < 0) cp1 <- -cp1
datos_influencers$Indice <- round(100 * (cp1 - min(cp1)) / diff(range(cp1)), 1)
head(datos_influencers[order(-datos_influencers$Indice), c("Nombre", "Seguidores", "Indice")], 10)

# --- Análisis factorial -------------------------------------------------------
R <- cor(datos_numericos)

KMO(R)   # adecuación muestral: valores > 0.7 indican que tiene sentido factorizar

autovalores <- eigen(R)
round(autovalores$values / sum(autovalores$values) * 100, 1)   # % de varianza
scree(R)

# Dos factores por máxima verosimilitud con rotación varimax
af_ver <- fa(R, nfactors = 2, n.obs = nrow(datos_numericos),
             rotate = "varimax", fm = "ml")
af_ver

plot(af_ver)
biplot(af_ver$score.cor, loadings(af_ver), cex = c(0.7, 0.8), col = c("grey", "blue"),
       xlab = "Factor 1", ylab = "Factor 2",
       main = "Análisis factorial: máxima verosimilitud y varimax")
fa.diagram(af_ver, main = "Análisis factorial por máxima verosimilitud y varimax")
qgraph.loadings(af_ver$loadings)
