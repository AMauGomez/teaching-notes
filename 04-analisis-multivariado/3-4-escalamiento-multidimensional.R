# =============================================================================
# 3.4 Escalamiento multidimensional clásico (MDS) a partir de una matriz de
#     distancias
# Objetivo: representar en el plano 20 marcas a partir de las distancias entre
#           ellas, y ver cómo cambia el mapa al cambiar la medida de distancia.
# Datos:    datos/matrixbrands.csv: matriz simétrica 20 x 20 de distancias
#           entre marcas (diagonal cero).
# =============================================================================

datos <- read.csv("datos/matrixbrands.csv", check.names = FALSE)
rownames(datos) <- datos[[1]]
D <- as.matrix(datos[, -1])

# Comprobamos que de verdad sea una matriz de distancias
stopifnot(isSymmetric(unname(D), tol = 1e-6), all(diag(D) == 0))

# --- 1. MDS clásico sobre las distancias originales --------------------------
# cmdscale() doble-centra D^2 y toma los dos primeros vectores propios
esc_mul <- cmdscale(as.dist(D), k = 2, eig = TRUE)

# Bondad de ajuste: proporción de los valores propios que capturan las dos
# dimensiones (si hay valores propios negativos, D no es euclidiana)
round(esc_mul$GOF, 3)
sum(esc_mul$eig < -1e-8)   # número de valores propios negativos

graficar_mds <- function(coords, titulo) {
  plot(coords, type = "n", xlab = "Coordenada 1", ylab = "Coordenada 2",
       main = titulo)
  text(coords, labels = rownames(coords), cex = 0.8)
  abline(h = 0, v = 0, lty = 3, col = "gray")
}
graficar_mds(esc_mul$points, "MDS clásico: distancias originales")

# --- 2. Otras distancias -----------------------------------------------------
# Aquí cada marca se describe por su vector de distancias a las 20 marcas
# (su "perfil") y medimos qué tan distintos son esos perfiles con Canberra y
# Manhattan. Es otra pregunta: dos marcas quedan cerca si están a distancias
# parecidas del resto.
for (metodo in c("canberra", "manhattan")) {
  d_perfil <- dist(D, method = metodo)
  graficar_mds(cmdscale(d_perfil, k = 2),
               paste("MDS con distancia", metodo, "entre perfiles"))
}
