library(tidyverse)
library(mice)
library(lattice)
library(pan)
library(ggmice)
library(readxl)
library(naniar)
library(MASS)
library(psych)
library(dplyr)
library(GGally)
library(overdisp)
library(AER)
library(performance)
library("DHARMa")
library("glmmTMB")
library("pscl")
library("gamlss")
library(countimp)
library(caTools)
library(broom.mixed)
library(lattice)
library(readxl)
library(corrplot)
library("amap")
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(stats)
library(FactoMineR)
library(factoextra)
library(psych)
library(ggcorrplot)
library(ggplot2)
library(GGally)
library(GPArotation)
library(qgraph)
library(corrplot)
library(cabootcrs)
library(Rtsne)


datos_1<-read.csv("datos/Niveles de Obesidad.csv")

###Tratamiento de los datos##

datos_1$Gender<-if_else(datos_1$Gender=="Male",1,0)
datos_1$family_history_with_overweight<-if_else(datos_1$family_history_with_overweight=="yes",1,0)
datos_1$FAVC<-if_else(datos_1$FAVC=="yes",1,0)
datos_1$SMOKE<-if_else(datos_1$SMOKE=="yes",1,0)
datos_1$SCC<-if_else(datos_1$SCC=="yes",1,0)

datos<-datos_1[1:200,1:13 ]

##PCA##

PCA_centrado=prcomp(datos, center=TRUE, scale=TRUE)
PCA_centrado

##Revisamos la proporción de varianza explicada por cada componente
summary(PCA_centrado)


##Creamos el digrama de codo
fviz_screeplot(PCA_centrado, main="PCA centrado y reescalado",  addlabels = TRUE)

##Visualizamos la dirección de las variables
x11()
fviz_pca_var(PCA_centrado,col.var="contrib",
             gradient.cols = c("green", "blue"),
             repel = TRUE)


#visulaizamos las observaciones en los componentes

x11()
fviz_pca_ind(PCA_centrado, col.ind = "cos2", 
             gradient.cols = c("gray", "purple", "red"),
             repel = TRUE)


##Visualizamods los datos y la direccion de las variables
x11()
fviz_pca_biplot(PCA_centrado, repel = TRUE,
                col.var = "blue", 
                col.ind = "green")


###Con MDS##


###Creamos la matriz a trabajar
###El paso más importante es definir la distancia que queremos utilizar##

###Realizamos el escalamiento
esc_mul<-cmdscale(dist(datos), k = 2)
esc_mul


###Graficamos

x11()
plot(esc_mul[,1], esc_mul[,2],
     type = "n")
text(esc_mul[,1], esc_mul[,2],
     labels = row.names(datos))



####Con medida canberra

datos_can<-dist(datos, method="canberra")
datos_can

##Realizamos el escalamiento
esc_mul_can<-cmdscale(datos_can, k = 2)
esc_mul_can

###Graficamos

x11()
plot(esc_mul_can[,1], esc_mul_can[,2])
text(esc_mul_can[,1], esc_mul_can[,2],
     labels = row.names(datos))



####Con medida Manhattan

datos_man<-dist(datos, method="manhattan")
datos_man

##Realizamos el escalamiento
esc_mul_man<-cmdscale(datos_man, k = 2)
esc_mul_man

###Graficamos

x11()
plot(esc_mul_man[,1], esc_mul_man[,2])
text(esc_mul_man[,1], esc_mul_man[,2],
     labels = row.names(datos))




####Analisis de Correspondencias###


datos<-read.csv("datos/Niveles de Obesidad.csv")
datos<-datos[1:200,]
datos<-datos%>%mutate(datos, Height=Height*100)
datos<-na.omit(datos)


##Creamos los cortes o discretización de los datos###

datos$Age_fac <- cut(datos$Age,
                     breaks = c(0, 18, 30, 40, 50, 60, 100),
                     labels = c('0-18', '19-30', '31-40', '41-50', '51-60', '60+'),
                     include.lowest = TRUE)

datos$Height_fac<- cut(datos$Height,
                       breaks = c(145, 160, 170, 180, 190, 200), 
                       labels = c("145-160", "160-170", "170-180", "180-190", "190-200"),
                       include.lowest = TRUE)

datos$Weight_fac <- cut(datos$Weight,
                        breaks = c(39, 60, 80, 100, 120, 166),
                        labels = c("39-60", "60-80", "80-100", "100-120", "120-166"),
                        include.lowest = TRUE)

# Creamos 3 niveles: bajo, medio y alto de la frecuencia en consumo de vegetales
datos$FCVC_fac <- cut(datos$FCVC,
                      breaks = c(0, 1, 2, 3), 
                      labels = c('Bajo', 'Medio', 'Alto'),
                      include.lowest = TRUE)

datos$NCP_fac <- cut(datos$NCP,
                     breaks = c(0, 1, 3, 4),
                     labels = c('Bajo', 'Medio', 'Alto'),
                     include.lowest = TRUE)

# Creamos 3 niveles: bajo, medio y alto de cosnumo de agua
datos$CH2O_fac <- cut(datos$CH2O,
                      breaks = c(0, 1.5, 2.5, 3), 
                      labels = c('Bajo', 'Medio', 'Alto'),
                      include.lowest = TRUE)

# Creamos 3 niveles: bajo, medio y alto de la frecuencia con la que hacen actividad física.
datos$FAF_fac <- cut(datos$FAF,
                     breaks = c(0, 0.5, 1.5, 3), 
                     labels = c('Low', 'Medium', 'High'),
                     include.lowest = TRUE)
# Creamos 3 niveles: bajo, medio y alto del tiempo promedio de uso de aparatos moviles 
datos$TUE_fac <- cut(datos$TUE,
                     breaks = c(0, 0.5, 1.5, 2), 
                     labels = c('Low', 'Medium', 'High'),
                     include.lowest = TRUE)

###Seleccionamos varaibles para el modelo##

datos_0<-datos[,1:5]
datos_10<-datos[,14:25]

datos_modelo <-cbind(datos_0, datos_10)


matriz_cont<-getBurt(datos_modelo)
head(matriz_cont)


###Creamos el analisis de correspondencias

modelo_MCA<-MCA(datos_modelo, ncp=5, graph = FALSE)



x11()
scree_plot<-fviz_screeplot(modelo_MCA, addlabels = TRUE, ylim = c(0, 20))
scree_plot




x11()
fviz_mca_var(modelo_MCA, col.var = "cos2",
             gradient.cols = c("green", "orange", "purple", "red"), 
             repel = TRUE, 
             ggtheme = theme_minimal())


x11()
fviz_mca_ind(modelo_MCA, col.ind = "cos2", 
             gradient.cols = c("green", "orange", "purple", "red"),
             repel = FALSE, 
             ggtheme = theme_minimal())

###T-SNE

###Es un proceso esticastico por lo que hay que usar semillas#
###Su principal uso no es la clusterización, sino la visualización de datos##

tsne = Rtsne(datos, perplexity=30, eta=800, theta=.5, max_iter=3000)

###Graficamos 
tsne= datos%>%
  bind_cols(x=tsne$Y[,1],y=tsne$Y[,2])

ggplot(data=tsne, aes(x=x,y=y,col=as.factor(NObeyesdad)))+
  geom_point()+
  theme_minimal()+
  labs(x='',y='',color = "Tipo de Obesidad")


tsne_1 = Rtsne(datos, perplexity=30, eta=800, theta=.5, max_iter=3000)

###Graficamos 
tsne_1= datos%>%
  bind_cols(x=tsne_1$Y[,1],y=tsne_1$Y[,2])

ggplot(data=tsne_1, aes(x=x,y=y,col=as.factor(NObeyesdad)))+
  geom_point()+
  theme_minimal()+
  labs(x='',y='',color = "Tipo de Obesidad")

