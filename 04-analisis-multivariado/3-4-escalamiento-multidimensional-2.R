##MDS COSTCO##
library(magrittr)
library(vegan)
library(ggplot2)
library(gridExtra)

library(magrittr)
library(vegan)
library(ggplot2)

# cargar los datos
datos_completos <- read.csv("datos/Wholesale customers data.csv")
set.seed(123) # Semilla para reproducibilidad
gastos <- datos_completos[sample(nrow(datos_completos), 150), 3:8] 
rownames(gastos) <- paste0("C_", 1:150)

# 2. Crear los "Clústeres de Ingreso" asignando Alcaldías
# Calculamos el gasto total de cada cliente
gasto_total <- rowSums(gastos)

# Definimos los cortes (terciles) para separar en 3 clases
cortes <- quantile(gasto_total, probs = c(0.33, 0.66))

# Asignación aleatoria condicionada al nivel de gasto
gastos$Alcaldia <- sapply(gasto_total, function(x) {
  if(x > cortes[2]) {
    sample(c("Miguel Hidalgo", "Benito Juarez"), 1) # Alto
  } else if(x > cortes[1]) {
    sample(c("Coyoacan", "Cuauhtemoc"), 1)          # Medio
  } else {
    sample(c("Iztapalapa", "GAM"), 1)               # Popular
  }
})

# Ordenar los niveles para que la leyenda del gráfico tenga sentido lógico
gastos$Alcaldia <- factor(gastos$Alcaldia, 
                          levels = c("Miguel Hidalgo", "Benito Juarez", 
                                     "Coyoacan", "Cuauhtemoc", 
                                     "Iztapalapa", "GAM"))

# Extraemos solo las variables numéricas estandarizadas para el MDS
datos_num_scaled <- scale(gastos[, 1:6])
write.csv(datos_num_scaled, "datos_costco_escaled.csv")


# Calculamos las  Matrices de Distancia
dist_eucl <- dist(datos_num_scaled, method = "euclidean")
dist_manh <- dist(datos_num_scaled, method = "manhattan")
dist_canb <- dist(datos_num_scaled, method = "canberra")

# 
crear_df_mds <- function(dist_matrix, nombre_metrica) {
  mds <- cmdscale(dist_matrix, k = 2) %>% as.data.frame()
  colnames(mds) <- c("Dim1", "Dim2")
  mds$Metrica <- nombre_metrica
  mds$Alcaldia <- gastos$Alcaldia
  return(mds)
}

df_eucl <- crear_df_mds(dist_eucl, "1. Euclidiana ")
df_manh <- crear_df_mds(dist_manh, "2. Manhattan ")
df_canb <- crear_df_mds(dist_canb, "3. Canberra ")

# Unimos todo en un solo DataFrame
df_final <- rbind(df_eucl, df_manh, df_canb)


# Graficamos
x11()
ggplot(df_final, aes(x = Dim1, y = Dim2, color = Alcaldia)) +
  geom_point(alpha = 0.8, size = 2.5) +
  # Usamos paletas distintas para resaltar las "clases"
  scale_color_manual(values = c("Miguel Hidalgo" = "#08519c", "Benito Juarez" = "#3182bd",  
                                "Coyoacan" = "#006d2c", "Cuauhtemoc" = "#31a354",           
                                "Iztapalapa" = "#a63603", "GAM" = "#e6550d")) +            
  facet_wrap(~Metrica, scales = "free", ncol = 1) +
  theme_bw() +
  labs(title = "Comparativa MDS: Clientes Mayoristas en CDMX",
       x = "Coordenada 1", y = "Coordenada 2", color = "Alcaldía (Ingreso)") +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 11))





















# Usaremos solo una muestra aleatoria de 100 clientes para visualizarlo mejor
set.seed(42)

datos_completos <- read.csv("datos/Wholesale customers data.csv")
gastos <- datos_completos[sample(nrow(datos_completos), 100), 3:8] 
rownames(gastos) <- paste0("Cliente_", 1:100)

#Calculamos las  Matrices de Distancia
dist_eucl <- dist(gastos, method = "euclidean")

dist_manh <- dist(gastos, method = "manhattan")

dist_canb <- dist(gastos, method = "canberra")

#Escalamiento Multidimensional (MDS) 
mds_eucl <- as.data.frame(cmdscale(dist_eucl, k = 2))
mds_eucl$Metrica <- "Euclidiana"

mds_manh <- as.data.frame(cmdscale(dist_manh, k = 2))
mds_manh$Metrica <- "Manhattan"

mds_canb <- as.data.frame(cmdscale(dist_canb, k = 2))
mds_canb$Metrica <- "Canberra"

# Unimos resultados para graficar
df_mds <- rbind(mds_eucl, mds_manh, mds_canb)
colnames(df_mds)[1:2] <- c("Dim1", "Dim2")

# Graficamos

x11()
ggplot(df_mds, aes(x = Dim1, y = Dim2, color = Metrica)) +
  geom_point(alpha = 0.7, size = 2) +
  facet_wrap(~Metrica, scales = "free") +
  theme_bw() +
  labs(title = "MDS del Gasto de Clientes Mayoristas",
       x = "Coordenada 1", y = "Coordenada 2") +
  theme(legend.position = "none")








##########
datos_num_scaled<-read.csv("datos/datos_costco_escaled.csv")

datos_num_scaled<-datos_num_scaled[, 2:7]
# Calculamos las  Matrices de Distancia
dist_eucl <- dist(datos_num_scaled, method = "euclidean")
dist_manh <- dist(datos_num_scaled, method = "manhattan")
dist_canb <- dist(datos_num_scaled, method = "canberra")

# 
crear_df_mds <- function(dist_matrix, nombre_metrica) {
  mds <- cmdscale(dist_matrix, k = 2) %>% as.data.frame()
  colnames(mds) <- c("Dim1", "Dim2")
  mds$Metrica <- nombre_metrica
  mds$Alcaldia <- gastos$Alcaldia
  return(mds)
}

df_eucl <- crear_df_mds(dist_eucl, "1. Euclidiana ")
df_manh <- crear_df_mds(dist_manh, "2. Manhattan ")
df_canb <- crear_df_mds(dist_canb, "3. Canberra ")

# Unimos todo en un solo DataFrame
df_final <- rbind(df_eucl, df_manh, df_canb)

# Graficamos
ggplot(df_final, aes(x = Dim1, y = Dim2, color = Alcaldia)) +
  geom_point(alpha = 0.8, size = 2.5) +
  # Usamos paletas distintas para resaltar las "clases"
  scale_color_manual(values = c("Miguel Hidalgo" = "#08519c", "Benito Juarez" = "#3182bd",  
                                "Coyoacan" = "#006d2c", "Cuauhtemoc" = "#31a354",           
                                "Iztapalapa" = "#a63603", "GAM" = "#e6550d")) +            
  facet_wrap(~Metrica, scales = "free", ncol = 1) +
  theme_bw() +
  labs(title = "Comparativa MDS: Clientes Mayoristas en CDMX",
       x = "Coordenada 1", y = "Coordenada 2", color = "Alcaldía (Ingreso)") +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 11))

