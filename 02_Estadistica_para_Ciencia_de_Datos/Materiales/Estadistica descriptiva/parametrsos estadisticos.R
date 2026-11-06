data("mtcars")
mtcars
#Diccinario de datos 
summary(mtcars)
#Covertir bariiable cuantitativa en cualitativa 
mtcars$vs <- factor(mtcars$vs, labels = c("v", "s"))
mtcars$am <-factor(mtcars$am, labels = c("automatico", "manual"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$cyl <- factor(mtcars$cyl)
#Parametros estadisiticos 
mean(mtcars$mpg)#Promedio
median(mtcars$mpg)#Mediana
#moda
table(mtcars$cy)#Moda es 14
#Ordenar de mayot a menor
sort(table(mtcars$cy),decreasing = T)#Moda es 14

max(table(mtcars$cyl))

#Parametros de disperción
#Varianza: dispecion de los datos respecto al promedio
var(mtcars$mpg)#Solo muestra la dispercion de los datos 
#desviacion estandar 
sqrt(var(mtcars$mpg))
sd(mtcars$mpg)
#Coeficiente de variacion (CV)
(sd(mtcars$wt)/mean(mtcars$wt))*100
#Rengo intercuartilico se utiliza cuando de tiene valores atipicos 
IQR(mtcars$mpg)
###Paramtros de posición###
#Cuartiles 
quantile(mtcars$mpg)
#Dividir los cuatiles en diferentes partes se divide en 10 partes 
quantile(mtcars$mpg, prob= seq(0,1,by=0.01))

#BoxPlot
boxplot(mtcars$wt, horizontal = T)
quantile(mtcars$wt)
hist(mtcars$wt)
#Diespercion de los peso de acuerdo al tipo de motor
boxplot(mtcars$wt ~ mtcars$vs, horizontal = T)#alt+126~
#Diespercion de los peso de acuerdo al tipo de carburador
boxplot(mtcars$wt ~ mtcars$carb, horizontal = T)
#Asimetria y curtosis 
library(e1071)
#Asimetria A = 0 simetrica, A>0 asimetrica a la derecha(+) 
#A<0 asimetrica a la izquierda (-)
skewness((mtcars$wt))
#curtosis 
#K=0 Mesocurtica (simetrica)
# K >0 Leptocurtica (pocs disperciom)
#K<0 Plsticurtica
kurtosis(mtcars$wt)
#con cunatiles 
tapply(mtcars$wt, mtcars$vs, quantile)
#con promedio
tapply(mtcars$wt, mtcars$carb, sd)#Agrupar por variable 
#cualitativa

#Reto 6
#mtcars$vs <- factor(mtcars$vs, labels = c("v", "s"))
NutritionStudy$VitaminUse <-factor(NutritionStudy$VitaminUse, labels = c("Regularmente", "Ocasionalmente","No"))
NutritionStudy$Smoke <- factor(NutritionStudy$Smoke, labels = c("si", "No"))
summary(NutritionStudy)

mean(NutritionStudy$Fiber)

median((NutritionStudy$Fiber))

mean((NutritionStudy$Age))
sd(NutritionStudy$Age)

table(NutritionStudy$Smoke)


boxplot(NutritionStudy$Cholesterol ~ NutritionStudy$PriorSmoke, horizontal = T)

tapply(NutritionStudy$Calories, NutritionStudy$Sex, mean)
tapply(NutritionStudy$Calories, NutritionStudy$Sex, sd)

#Reto 2
mean(Cosecha_Jal$Superficie)
median(Cosecha_Jal$Superficie)
sd(Cosecha_Jal$Superficie)
var(Cosecha_Jal$Superficie)
#Cuartiles 
quantile(Cosecha_Jal$Superficie)
boxplot(Cosecha_Jal$Superficie, horizontal = T)


tapply(Cosecha_Jal$Superficie, Cosecha_Jal$Cultivo, mean)
tapply(Cosecha_Jal$Superficie, Cosecha_Jal$Cultivo, sd)


skewness(Cosecha_Jal$Superficie)
kurtosis(Cosecha_Jal$Superficie)

boxplot(Cosecha_Jal$Superficie ~ Cosecha_Jal$Cultivo, horizontal = T)

library(tidyverse)

x <- c(10, 20, 30, 40, 50)

mean(x)
