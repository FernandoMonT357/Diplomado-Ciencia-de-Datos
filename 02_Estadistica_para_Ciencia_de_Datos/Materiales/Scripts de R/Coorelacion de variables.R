library(tidyverse)
ggplot(Cosecha_Jal, aes(x=Superficie, y=Cultivo)) +
  geom_boxplot()
#Eliminar pastos 
Cosecha_Jal %>%
  filter(Cultivo!="pastos") %>%
      ggplot(aes(x=Superficie, y=Cultivo)) +
        geom_boxplot()

Cosecha_Jal %>%
  filter(Cultivo %in% c("chile","jitomate")) %>%
  ggplot(aes(x=Superficie, y=Cultivo)) +
  geom_boxplot()
#==========Coorelación de variables

#Coovarianza 
#Coeficiente coorelacion 
#  Coeficiente de coorelación de pearson
#  indica si hay asociasion lineal entre variables
data("mtcars")
ggplot(PesoEstatura, aes(x=Peso,y=Estatura)) +
  geom_point()
#Coovarianza por defaul cov utiliza metodo de Pearson
cov(PesoEstatura$Peso,PesoEstatura$Estatura)#Calcular coovarianza
#Coeficiente de coorelacion de Pearson solo con numericas
cor(PesoEstatura$Peso,PesoEstatura$Estatura)
#Coeficiente de coorelacion para diferentes variables 
# Matriz de varianzas y coovarianzas
HV<-HEART %>%
  select(Age:Oldpeak) 
cov(HV)#Si es la relacion con la misma variables es varianza

#Matriz de coorelacion solo con
cor(HV)

# 1. Solo indica coerrelacion lineal 
# 2. Se ve afectado por los valores atipicos
# 3. Las variables deben ser cuantitativas

cov(precipitacion$Precipitacion,precipitacion$ppn)
cor(precipitacion$Precipitacion,precipitacion$ppn)
ggplot(precipitacion, aes(x=Precipitacion,y=ppn)) +
  geom_point()

cov(ventas$NumEmpleado,ventas$Ventas)
cor(ventas$NumEmpleado,ventas$Ventas)
ggplot(ventas, aes(x=NumEmpleado,y=Ventas)) +
  geom_point()

cov(faithful$eruptions,faithful$waiting)
cor(faithful$eruptions,faithful$waiting)
ggplot(faithful, aes(x=eruptions,y=waiting)) +
  geom_point()








