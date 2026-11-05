library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(stats)
library(FactoMineR)
library(factoextra)
library(psych)
library(ggcorrplot)
library(cluster)
library(dendextend)
library(NbClust)
library(fpc)

datos_1<-read.csv("datos/CarPrice_Assignment.csv")
datos<-datos_1[,8:20]


###Estadística de Hopkings##

get=get_clust_tendency(datos, n=nrow(datos)-1, graph = FALSE)
get$hopkins_stat



###DIANA##

dian=diana(datos, metric ="euclidean", stand = TRUE)
dian$dc

  
x11()
fviz_dend(dian, 
          cex = 0.5,
          k_colors = c("blue", "red"),
          color_labels_by_k = TRUE, 
          rect = TRUE, 
          main="Dendograma Diana-Euclidiana"
          )
            
##Dendograma coloreado en 4 clusters##          

x11()
fviz_dend(dian,k=4,
 cex=0.5,
 k_colors=c("blue","red", "yellow", "cyan" ),
 color_labels_by_k=TRUE,
 rect=TRUE,
 main="Dendograma Diana"
 )



##Cortamos en 4 clusters##

grp = cutree(dian, k = 4)
x11()


##graficamos los clusters##
fviz_cluster(list(data = datos, cluster = grp))


##Observamos las cardinalidades de los grupos#3
table(grp)


###Si cortamos en 6 clusters##
x11()
fviz_dend(dian, k=6,
          cex = 0.5,
          k_colors = c("blue","red", "violet", "grey", "yellow", "cyan" ),
          color_labels_by_k = TRUE, 
          rect = TRUE, 
          main="Dendrogram of Diana"
)


##graficamos##

grp_1 = cutree(dian, k = 6)


x11()
fviz_cluster(list(data = datos, cluster = grp_1))


###Cardinalidad de los clsuters##

table(grp_1)



###Método Aglomerativo AGNES###


#Primero haremos un modelo AGNES con la metrica euclidiana y metodo single 
#(teoricamente clusters mas grandes), otra con metodo complete 
#(en teoria clusters mas compactos), y por ultimo uno con Ward 
#(fusiona clusters muy cercanos).#


agnes_single=agnes(datos, metric="euclidean", stand=TRUE, method="single")
agnes_complete=agnes(datos, metric="euclidean", stand=TRUE, method="complete")
agnes_ward=agnes(datos, metric="euclidean", stand=TRUE, method="ward")



### AGNES - Single ###


x11()
fviz_dend(agnes_single, k = 4, 
          cex = 0.5,
          k_colors = c("blue", "red"),
          color_labels_by_k = TRUE, 
          rect = TRUE, 
          main="Dendograma Agnes-Single"
)





grp_3 = cutree(agnes_single, k = 4)
x11()
fviz_cluster(list(data = datos, cluster = grp_3))



table(grp_3)


##Tendencia a agruparse##

agnes_single$ac



### AGNES- Complete ###

x11()
fviz_dend(agnes_complete, k = 4, 
          cex = 0.5,
          k_colors = c("blue", "red", "pink", "orange"),
          color_labels_by_k = TRUE, 
          rect = TRUE, 
          main="Dendograma Agnes-Complete"
)



grp_4 = cutree(agnes_complete, k = 4)

x11()
fviz_cluster(list(data = datos, cluster = grp_4))


table(grp_4)

agnes_complete$ac


### AGNES- Ward ###

fviz_dend(agnes_ward, k = 4, 
          cex = 0.5,
          k_colors = c("blue", "red", "pink", "orange"),
          color_labels_by_k = TRUE, 
          rect = TRUE, 
          main="Dendograma Agnes-Ward"
)



grp_5 = cutree(agnes_ward, k = 4)

x11()
fviz_cluster(list(data = datos, cluster = grp_5))


table(grp_5)



agnes_ward$ac





##Método K-Means##
  
##Hacemos el modelo de k-means, empezaremos haciendo 30 asignaciones iniciales 
#y con 4 particiones como lo habiamos inferido con los modelos anteriores


k_mean=eclust(datos, "kmeans", k = 4, nstart = 30, graph = FALSE)



x11()
fviz_cluster(k_mean, data = datos,
             palette = c("blue", "red", "orange", "green"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal()
)


k_mean$size




###Validación

fviz_silhouette(k_mean, palette = "jco")


##Optimo de clsuters##

x11()
fviz_nbclust(datos, kmeans, method='silhouette')
