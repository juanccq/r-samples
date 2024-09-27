#--------------------------------------------------------------------------------
# Andrea Marin
# MODELAMIENTO Y VISUALIZACIÓN
# LO BASICO SOBRE LA VISUALIZACION
#--------------------------------------------------------------------------------

# se cargan algunas librerias
library(corrplot)
library(dplyr)
library(readxl)
library(tidyverse)

#conjunto de datos

library(help = "datasets")

data<- quakes

###########################  UTILIZANDO FUNCIONCES DE R PRESENTE UN INFORME CUANTITATIVO SOBRE
# 1. Caracterice rapidamente el conjunto de variables
# 2. Considera necesario hacer algun tipo de tratamiento
# 3. Realice un breve reporte numerico de la situaciòn general
# 4. Presente un resumen por estaciòn
# 5  Presente un resumen para todas las estaciones desde la 10 a 40  y de la 40 en adelante
################################################################################################
#1.
#2.
#3.
#4.
#5. data <- data %>%
#        mutate (est2=ifelse(stations %in% c(1:40),"S:1-40","S>40")) 

###########################  AHORA UTILIZANDO GRAFICAS INDAGUE SOBRE
# Distribuciones de latitud, longitud y profundidiad

ggplot(data, aes(x=lat)) + 
  geom_histogram()


data %>%
  ggplot( aes(x=lat)) +
  geom_density()

# ¿Hay diferencia en las densidades para latitid y longitud?


#   que otros graficos nos hablan de distribuciónes?
data %>%
  ggplot( aes( y=lat)) +
  geom_boxplot() 



# Cómo podemos ver lo que passa para ciertas estaciones
data %>%
  filter(stations %in% c(10:20)) %>%
  ggplot(aes( y=lat, x=factor(stations))) +
  geom_violin() 


# que relacion hay entre latitud y longitud  
data %>%
ggplot(aes(x=lat, y=long)) + 
  geom_point()


# que relacion hay entre latitud y magnitud
# como vemos las tres varaibles a la vez

data %>%
ggplot(aes(x=lat, y=long, size = depth)) +
  geom_point(alpha=0.7)

#
correlacion<-round(cor(data), 1)
corrplot(correlacion)


# que podemos decir sobre las estaciones
data %>%
ggplot( aes(x=stations)) + 
  geom_bar()


data %>%
  ggplot( aes(x=est2)) + 
  geom_bar()

# como hacemos una torta(pie-chart)  simple

valorp<-data.frame(table(data$stations))
pie(valorp$Freq)

valor<-data.frame(table(data$est2))
pie(valor$Freq)

valor%>%
ggplot(aes(x="",y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


# como se veria aplilada
valorp %>%
ggplot(aes(x="",y=Freq, fill=Var1)) + 
  geom_bar(position="stack", stat="identity")


#creemos muchas categorias
data <- data %>%
        mutate(est3=cut(stations,
                 breaks = c(0, 25, 45, 70,130)))

#repita el grafico anterior
valorp<-data.frame(table(data$est3))

valorp %>%*/*-
  ggplot(aes(x="",y=Freq, fill=Var1)) + 
  geom_bar(position="stack", stat="identity")


### Intentemos
# grafique el altitud maxima por estacion
# grafique la relacion entre la magnitud promedio y latitud maxima  por estacion
# grafique la distribuciòn de la longitud para cada estacion
