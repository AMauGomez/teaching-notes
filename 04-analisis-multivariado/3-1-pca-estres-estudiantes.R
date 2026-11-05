library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(stats)
library(FactoMineR)
library(factoextra)
library(psych)
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

##Cargamos los datos las respuestas obtenidas de la realización de cuestionarios a estudiantes. 

datos<-read.csv("datos/StressLevelDataset.csv")



lm_1<-lm(datos$anxiety_level~., data=datos)
lm_1$call
summary(lm_1)


mean(lm_1$residuals)

dwtest(lm_1)

bgtest(lm_1)


bptest(lm_1)
shapiro.test(lm_1$residuals)
x11()
qqnorm(lm_1$residuals)
qqline(lm_1$residuals, col="blue")



##Observamos los rangos de cadad variable##
describe(datos)


##Realizamos el PCA
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




#########################3EJEMPLO 2##############

datos_influencers<-read.csv("datos/datos_influencers.csv")

#Crear un 'Índice de Influencia Digital' único (un solo número del 0 al 100) para cada creador, 
#de modo que puedan rankearlos y saber cuanto pagarle a cada uno.


cat("\n=== TOP 10 CREADORES POR SEGUIDORES ===\n")
datos_ordenados <- datos_influencers[order(datos_influencers$Seguidores, decreasing = TRUE), ]
print(head(datos_ordenados[, c("Nombre", "Seguidores", "Ganancias_Ads")], 10))


##Solo numericas para hacer la correlacion

datos_numericos <- datos_influencers[, -1] 


x11()
ggpairs(datos_numericos)


#Realizamos el PCA
PCA_centrado=PCA(datos_numericos, scale.unit = TRUE, graph = F)
PCA_centrado

PCA_centrado$eig


PCA_centrado$var$cor


##Variables

grafico_pearson <- fviz_pca_var(PCA_centrado, 
                                col.var = "cos2", # Colorea según la calidad de representación
                                gradient.cols = c("#FF0000", "#E7B800", "#00AFBB"),
                                repel = TRUE, 
                                title = "PCA (Pearson)")
x11()
grafico_pearson






























###############Analisis Factorial#################

datos<-cor(datos_numericos)
##Agrupación de los datos#

KMO(datos)


###Eigen valores##

autovalores=eigen(datos)
varianzatotal=autovalores$values/sum(autovalores$values)*100
autovalores
varianzatotal


##Screeplot#
x11()
scree(datos)

###Usando 2 factores##

af_ver=fa(datos, nfactors = 2, n.obs=123,  rotate ="varimax", fm="mle" )
af_ver

##Visualizamos los datos como si fueran cuadrantes
x11()
plot(af_ver)


x11()
biplot(af_ver$score.cor, loadings(af_ver), cex=c(0.7,0.8), col = c("grey", "blue"), 
       xlab = "Factor 1", ylab = "Factor 2", 
       main = "Análisis Factorial Quartimax- MLE")



x11()
fa.diagram(af_ver, main="Analisis Factorial por Máxima Verosimilitud y Varimax")


##Hacemos la araña##
x11()
qgraph.loadings(af_ver$loadings)



