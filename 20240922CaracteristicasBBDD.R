#--------------------------------------------------------------------------------
# Andrea Marin
# MODELAMIENTO Y VISUALIZACIÓN
#--------------------------------------------------------------------------------

# se cargan algunas librerias
library(corrplot)
library(dplyr)
library(readxl)
library(tidyverse)


#Asignación de directorio y lectura de bbdd
setwd("C:/Users/Paola/Desktop/VIZ_CURSO/S4/Practica")
data<-read_excel('SupermarketTransactions_2.xlsx')
data <-data[,-17]

##################################  ANTES DE ENTRAR A TRABAJAR CON LA BBDD DEBEMOS HACER VERIFICACIONES SOBRE LAS VARAIBLES Y LA ##############
#################################   CONSISTENCIA DE LA BASE DE DATOS: NO DUPLICADOS, NA (INFORMACIÒN FALTANTE), ETC ###########################

# còmo esta compuesta la base
str(data)

# que tipo de datos hay en Gender
table(data$Gender)

# resumen principalmente para variables cuantitativas
summary(data)

# la unicidad: no exixtencia de valores duplicados
length(unique(data$Transaction))== nrow(data)   # si es falso es porque tiene registros duplicados y debe arreglar esa inconsitencia

duplic<-data$Transaction[duplicated(data$Transaction)]  #obtengo los registros duplicados

data[data$Transaction%in%duplic,]  # los filas del problema duplicado
data1<-data[!duplicated(data$Transaction),]  #elimino registros duplicados

# verificar la no existencia de informacion faltante (NA) : La solucion es imputar o eliminar los registros con informacion faltante
data2<-data1[complete.cases(data1),]

# Corrijo tipologia de variables, en este caso Children se toma como categorica
table(data$Children)
data3<-data2[!data2$Children %in% c("1000", "P"), ]
data3$Children<-as.numeric(data3$Children)   # de esta forma en data3 queda guardada mi data de forma correcta.

# EL ANTERIOR CÓDIGO SE PUEDE OPTIMIZAR, SIN EMBARGO, SE RECOMIENDA EN LAS PRIMERAS EXPLORACIONES IR ANALIZANDO LA BASE PASO A PASO
# E IR DOCUMENTANDO LAS DECISIONES QUE SE TOMAN.


# Supongamos que por conocimiento de experto se toma la decisión de trabajar con información completa, es decir eliminar todos aquellos
# registros con información faltante y ademas eliminar datos duplicados. Esto se puede hacer en una lìnea de còdigo:

datan <- data %>%
  distinct() %>%
  drop_na()
  
        #Para visualizacion de faltantes
        #library("naniar")
        #gg_miss_fct(x = data1, fct = Revenue)
        #gg_miss_case(data)

# ademans decidimos imputar los valores extraños en la variable  Children
datan <- datan %>%
                mutate(Children=ifelse(Children %in%c("1000", "P"), 0, Children)) %>%
                          mutate(Children=as.numeric(Children))



# otras ediciones

# Cambia el nombre de las variables
datan %>% 
  rename(ID_Trans = Transaction) 

# creamos una nueva columna a partir de varaibles ya existentes
datan <-datan %>%  mutate(Nueva= case_when(
    Gender == "F" & Children == 0 ~ "M0" ,
    Gender == "M" & Children == 1 ~ "F0" ,  
    .default = "other"))

#Discretización de columnas
datan <- datan %>% 
  mutate(reve_cat = cut(    # crear una columna nueva
    Revenue,         # columna numérica para hacer grupos
    breaks = c(0, 3, 5, 20), right = T, labels = F))



################################################## MEDIDAS DE RESUMEN DE TUS DATOS
# Otras formas de obtener resumenes de tus datos
library(skimr)
skim(datan)



        #OutVals = boxplot(data$Revenue)$out
        #print(OutVals)
        #psych::describe(data)

        # Graficos basicos del R base. Evidentemente hay que mejorarlos
        #hist(data$Revenue)
        #hist(data$Revenue, breaks = 20)

        #plot(data$Revenue, type= "p")
        #plot(density(data$Revenue), main='xxxxxxxxxxx')

        #median(na.omit((data$Revenue)))

# tabla agrupada
resumen<-datan %>%
 group_by(Gender, City) %>% 
  summarise (totalr = sum(Revenue))



############  TODO LO DE DEMAS LO ENCONTRAREMOS EN WEB
###########   LO IMPORTANTE ES ENTENDER MUY BIEN LA LOGICA DE NUESTROS DATOS Y HACERNOS LAS PREGUNTAS CORRECTAS SOBRE ELLOS





