# =============================================================================
# 3.4 Escalamiento multidimensional (MDS) con distintas métricas
# Objetivo: comparar los mapas de MDS que producen las distancias euclidiana,
#           Manhattan y Canberra sobre los mismos clientes, y ver por qué hay
#           que estandarizar antes de medir distancias.
# Datos:    "Wholesale customers" (UCI Machine Learning Repository): gasto
#           anual de 440 clientes de un distribuidor mayorista en Portugal en
#           seis categorías de producto. Archivo: datos/Wholesale customers data.csv
# =============================================================================

library(ggplot2)

datos_completos <- read.csv("datos/Wholesale customers data.csv")
categorias <- c("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicassen")

set.seed(123)
gastos <- datos_completos[sample(nrow(datos_completos), 150), categorias]
rownames(gastos) <- paste0("C_", seq_len(nrow(gastos)))

# Para colorear los mapas agrupamos a los clientes por terciles de su gasto total
gasto_total <- rowSums(gastos)
nivel_gasto <- cut(gasto_total,
                   breaks = quantile(gasto_total, c(0, 1/3, 2/3, 1)),
                   labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE)
table(nivel_gasto)

# Funciones auxiliares: MDS clásico en 2 dimensiones como data frame y gráfica
mds_df <- function(d, metrica) {
  m <- cmdscale(d, k = 2)
  data.frame(Dim1 = m[, 1], Dim2 = m[, 2], Metrica = metrica, Nivel = nivel_gasto)
}

graficar <- function(df, titulo) {
  ggplot(df, aes(Dim1, Dim2, color = Nivel)) +
    geom_point(alpha = 0.8, size = 2.2) +
    scale_color_manual(values = c(Bajo = "#a63603", Medio = "#31a354", Alto = "#08519c")) +
    facet_wrap(~Metrica, scales = "free", ncol = 3) +
    theme_bw() +
    labs(title = titulo, x = "Coordenada 1", y = "Coordenada 2",
         color = "Gasto total (tercil)")
}

# --- 1. Datos en sus unidades originales -------------------------------------
# La categoría con más varianza (Fresh) domina las distancias euclidiana y Manhattan
round(apply(gastos, 2, sd))

crudo <- rbind(mds_df(dist(gastos, "euclidean"), "Euclidiana"),
               mds_df(dist(gastos, "manhattan"), "Manhattan"),
               mds_df(dist(gastos, "canberra"),  "Canberra"))
graficar(crudo, "MDS sobre el gasto en unidades monetarias")

# --- 2. Datos estandarizados -------------------------------------------------
# Canberra ya es invariante a la escala de cada variable, pero exige valores
# no negativos; por eso se aplica a los datos originales y no a los estandarizados.
z <- scale(gastos)
estandarizado <- rbind(mds_df(dist(z, "euclidean"), "Euclidiana"),
                       mds_df(dist(z, "manhattan"), "Manhattan"),
                       mds_df(dist(gastos, "canberra"), "Canberra (datos originales)"))
graficar(estandarizado, "MDS con variables estandarizadas")

# --- 3. ¿Qué tan bien representa el plano a las distancias? ------------------
gof <- sapply(list(Euclidiana = dist(z), Manhattan = dist(z, "manhattan"),
                   Canberra = dist(gastos, "canberra")),
              function(d) cmdscale(d, k = 2, eig = TRUE)$GOF[2])
round(gof, 3)
