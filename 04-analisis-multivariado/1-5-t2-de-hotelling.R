# =============================================================================
# 1.5 Prueba T^2 de Hotelling para el vector de medias y normalidad multivariada
# Objetivo: probar H0: mu = mu0 para cinco mediciones a mano y con ICSNP,
#           construir intervalos simultáneos y revisar normalidad multivariada
#           (Mardia, Henze-Zirkler) con MVN. Incluye curvas de Andrews en ggplot.
# Datos:    ejemplo numérico de clase (11 sujetos, 5 mediciones de una
#           proteína), palmerpenguins::penguins y datasets::swiss.
# =============================================================================

library(ICSNP)           # HotellingsT2()
library(mvdalab)         # MVcis()
library(MVN)             # mvn()
library(ggplot2)
library(palmerpenguins)

#Se quiere probar que las mediciones de la proteina HLMG13 
##Se mantienen en los siguientes rangos usando la dosis adecuada

#H0:μ=(30,25,40,25,30)


###Mediciones del porcentaje de proteina HLMG13 

sujeto <- c(1,2,3,4,5,6,7,8,9,10,11)
y1 <- c(51,27,37,42,27,43,41,38,36,26,29)
y2 <- c(36,20,22,36,18,32,22,21,23,31,20)
y3 <- c(50,26,41,32,33,43,36,31,27,31,25)
y4 <- c(35,17,37,34,14,35,25,20,25,32,26)
y5 <- c(42,27,30,27,29,40,38,16,28,36,25)

# Creación del dataframe
datos_1 <- data.frame(sujeto,y1,y2,y3,y4,y5)


#H0:(μ1,μ2,μ3,μ4,μ5)′=(30,25,40,25,30)′

#Calculamos las media muestral

vector.medias <- apply(datos_1[,-1], MARGIN = 2, FUN = mean)
vector.medias


##Calculamos la varianza
S <- cov(datos_1[,-1])


##Calculamos el estadistico

# Medias de las hipótesis
mu <- c(30,25,40,25,30)
# Observaciones
n <- nrow(datos_1)
# parametros
p <- ncol(datos_1[,-1])

# Calculo
t2 <- as.numeric(n*(t((vector.medias-mu))%*%solve(S)%*%(vector.medias-mu)))
t2



### Valor crítico: T2 > (n-1)p/(n-p) * F_{p, n-p}(0.95) rechaza H0
((n*p-p)/(n-p))*qf(0.95,p,n-p)





HotellingsT2(datos_1[,-1],
             mu = c(30,25,40,25,30),
             test = "chi")


##Calculamos los intervalos de confianza

MVcis(data = datos_1[,-1], level = 0.95, include.zero = T)



###Normalidad




pingus<- na.omit(penguins[, 3:6])

#####UNIVARIADAS###
##SW=shapiro, Lillie=Lillierfors, SP=Shapiro Francia
#AD=Anderson


##Multivariadas###
##mardia, Henze- Zirkler, Royston., 

mardia<-mvn(data = pingus, 
    mvn_test = "mardia")


summary(mardia)

data("swiss")

swiss_hz <- mvn(data = swiss, 
                    mvn_test = "hz")


summary(swiss_hz)


swiss_1<-swiss[, 1:3]

mardia_swiss<-mvn(data = swiss_1, 
            mvn_test = "mardia")
summary(mardia_swiss)

andrewsCurves <- function(x, groups = NULL,
                          xlab = "", ylab = "", title = "",
                          legend.title = NULL) {
  
  x <- as.matrix(x)
  
  t <- seq(-pi, pi, length.out = 200)
  n <- nrow(x)
  p <- ncol(x)
  
  # Build Fourier basis
  f <- matrix(0, length(t), p)
  f[,1] <- 1/sqrt(2)
  
  for(i in 2:p){
    if(i %% 2 == 0){
      f[,i] <- sin((i/2)*t)
    } else {
      f[,i] <- cos(((i-1)/2)*t)
    }
  }
  
  # Vectorized transformation
  res <- x %*% t(f)
  
  # Long format
  df <- data.frame(
    t = rep(t, each = n),
    value = as.vector(res),
    id = rep(1:n, times = length(t))
  )
  
  if(!is.null(groups)){
    df$group <- rep(groups, times = length(t))
  }
  
  # Base plot
  if(!is.null(groups)){
    p <- ggplot(df, aes(t, value, group = id, colour = group)) +
      geom_line()
    
    # Apply legend title if provided
    if(!is.null(legend.title)){
      p <- p + labs(colour = legend.title)
    }
    
  } else {
    p <- ggplot(df, aes(t, value, group = id)) +
      geom_line() +
      theme(legend.position = "none")
  }
  
  p <- p +
    theme_minimal() +
    labs(x = xlab, y = ylab, title = title)
  
  return(p)
}


andrewsCurves(swiss_1)

