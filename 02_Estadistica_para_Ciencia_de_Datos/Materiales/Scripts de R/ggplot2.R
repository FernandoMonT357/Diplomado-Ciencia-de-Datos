##gggplot2 Graficas
library(tidyverse)
library(ggplot2)
#======================================
ggplot(NutritionStudy) + 
  geom_histogram(aes(x=Age), color= "red")#definir ejeXY
#Deinir intervalo  "bins"
ggplot(NutritionStudy) + 
  geom_histogram(aes(x=Age), color= "red", bins = 9)
#Cambiar color de relleco "Fill"
ggplot(NutritionStudy) + 
  geom_histogram(aes(x=Age), color= "red", bins = 9,
                 fill = "white")
#Poligono de frecuencia 
ggplot(NutritionStudy) + 
  geom_freqpoly(aes(x=Age) , color="blue", 
                bins=9)
#Unir poligono de frecuencia e histograma 
ggplot(NutritionStudy) + 
  geom_freqpoly(aes(x=Age) , color="blue", 
                bins=9) +
  geom_histogram(aes(x=Age), color= "red", bins = 10,
                 fill = "white")
#============
ggplot(NutritionStudy) + 
  geom_histogram(aes(x=Age), color= "red", bins = 10,
                 fill = "white") +
  geom_freqpoly(aes(x=Age) , color="blue", 
                bins=10)
#===============================================
ggplot(NutritionStudy, aes(Age)) +
  geom_histogram(color= "pink", bins = 10, fill = "white") +
  geom_freqpoly(color="blue", bins=10)
#====Grafica de datos categoricos
ggplot(NutritionStudy, aes(x=Smoke)) +
  geom_bar(fill="pink", color ="brown")

#==============0
ggplot(NutritionStudy, aes(x=Smoke, fill=VitaminUse)) +
  geom_bar( color ="brown")
 ###############
ggplot(NutritionStudy, aes(x=Smoke, fill=VitaminUse)) +
  geom_bar(position = "dodge")
table(NutritionStudy$Smoke, NutritionStudy$VitaminUse)
  
#====Grafica de cajas y bigotes

ggplot(NutritionStudy, aes(x=Age)) + 
  geom_boxplot()

ggplot(NutritionStudy, aes(y=Age)) + 
  geom_boxplot()
#Invertir ejes
ggplot(NutritionStudy, aes(x=Age)) + 
  geom_boxplot() + coord_flip()

ggplot(NutritionStudy, aes(x=Cholesterol)) + 
  geom_boxplot()

#Grafica de bigotes para dos bariables 

ggplot(NutritionStudy, aes(x=Cholesterol, y=VitaminUse)) + 
  geom_boxplot()

ggplot(NutritionStudy, aes(x=Cholesterol, y=PriorSmoke)) + 
  geom_boxplot()

ggplot(NutritionStudy, aes(x=Cholesterol, y= factor(PriorSmoke))) + 
  geom_boxplot()
#Grafica de dispercion 2 variables 

ggplot(NutritionStudy, aes(x=Calories, y=Fat)) +
  geom_point()
#Identificar los generos de los puntos 
ggplot(NutritionStudy, aes(x=Age, y=Cholesterol, color=Smoke)) +
  geom_point()
  
#==============================
ggplot(NutritionStudy, aes(x=Age, y=0)) + 
  geom_boxplot() + 
  geom_point()

#======== 
#Dispersa los puntos
ggplot(NutritionStudy, aes(x=Age, y=0)) + 
  geom_boxplot() + 
  geom_jitter(color="red", alpha=0.2)

#Dispersa los puntos
ggplot(NutritionStudy, aes(x=Age, y=Smoke, fill=Sex)) + 
  geom_boxplot() + 
  geom_jitter(color="red", alpha=0.2)

ggplot(NutritionStudy, aes(x=Age, y=Smoke, fill=Sex)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.3,aes(color=Sex))

#Colocar promedio de edad
promedad <- mean(NutritionStudy$Age)

ggplot(NutritionStudy, aes(x=Age, y=0)) + 
  geom_boxplot() +
  geom_point(aes(x=promedad))
#-------------------------------
ggplot(NutritionStudy, aes(x=Age, y=Cholesterol, color=Smoke)) +
  geom_point() +
  facet_wrap(~VitaminUse)#Crea paneles para la variable especificada


ggplot(NutritionStudy, aes(x=Age, y=Cholesterol, color=Smoke)) +
  geom_point() +
  facet_wrap(Sex~VitaminUse)

ggplot(NutritionStudy, aes(x=Age, y=Cholesterol, color=Smoke)) +
  geom_point() +
  facet_grid(Sex~VitaminUse)

#etiquetas

ggplot(NutritionStudy, aes(x=Age, y=Cholesterol, color=Smoke)) +
  geom_point() + 
  labs(title = "grafíco de nutricion",
       subtitle = "Edad vs Colesterol", 
       x = "Edad", y="Colesterol",
       color="Fumador") +
theme_linedraw()
#Asignacion 8

#====Grafica de datos categoricos
ggplot(NutritionStudy, aes(x=Smoke)) +
  geom_bar(fill="pink", color ="brown")

#==============0
ggplot(NutritionStudy, aes(x=Smoke, fill=VitaminUse)) +
  geom_bar( color ="brown")
###############
ggplot(NutritionStudy, aes(x=Smoke, fill=VitaminUse)) +
  geom_bar(position = "dodge")

ggplot(HEART, aes(x=AHD, fill=factor (Sex))) + 
  geom_bar() +
labs(title = "Grafico de barras",
     subtitle = "Efermedades del corazon", 
     x = "Enfermedad", y="Personas")
               
ggplot(HEART, aes(x = ChestPain, fill = AHD)) +
  geom_bar(position = "dodge")




ggplot(HEART, aes(x = MaxHR,y = AHD)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.3) +
  labs(title = "Grafico de caja",
       subtitle = "Efermedad del corazon VS frecuencia cardica maxima", 
       x = "Enfermedad del corazon", y="Frecuencia cardiaca maxima")



ggplot(HEART, aes(Age)) +
  geom_histogram(color= "pink", bins = 7, fill = "white")


ggplot(HEART, aes(Chol)) +
  geom_histogram(color= "pink", bins = 5, fill = "white") +
  geom_freqpoly(color="blue", bins=5)

ggplot(HEART, aes(x=RestBP, y=Chol, color=factor(Sex))) +
  geom_point()

ggplot(HEART, aes(x = RestBP, y = Chol, color = AHD)) +
  geom_point() +
  facet_wrap(~RestECG) +
  labs(
    title = "Presión arterial vs colesterol según electrocardiograma",
    x = "Presión arterial en reposo",
    y = "Colesterol"
  )


