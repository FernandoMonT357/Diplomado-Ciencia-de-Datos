#Regresión lieneal
library(tidyverse)
ggplot(Sueldos, aes(x=AñosExp, y=Sueldo)) +
  geom_point() +
  geom_abline(intercept = 25792,slope = 9450, color="red")
#Modelo lineal sueldo=      B0     +   B1*         x         +e
#              sueldo = intercepto + pendiente independiente +e
lm(Sueldo ~ AñosExp, data = Sueldos )#utiliza minimos cuadrados

sul2años <- 25792 + 9540*2
43525-sul2años #error

ggplot(Sueldos, aes(x=AñosExp, y=Sueldo)) +
  geom_point() +
  geom_smooth()
#con intervalo de confianza
ggplot(Sueldos, aes(x=AñosExp, y=Sueldo)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(Sueldos, aes(x=AñosExp, y=Sueldo)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)


#Obtener más informacion del modelo
lmSueldo <-lm(Sueldo ~ AñosExp, data = Sueldos )
summary(lmSueldo)

#sueldo = 25792     +      9450 * años
#         sueldo        incremento
#          con            por
#   0 años experiencia    añoCall:
lm(formula = Sueldo ~ AñosExp, data = Sueldos)

#Reto 1

cov(precipitacion$Precipitacion, precipitacion$ppn)
cor(precipitacion$Precipitacion, precipitacion$ppn)

lmpre <- lm(formula = ppn~Precipitacion, data = precipitacion)
summary(lmpre)

ggplot(precipitacion, aes(x=Precipitacion, y=ppn)) +
  geom_point() +
  geom_smooth(method = "lm")

#Reto 2
#lm(formula = y ~ x, data = fuente de datos)

cov(ventas$Ventas, ventas$NumEmpleado)
cor(ventas$Ventas, ventas$NumEmpleado)

lmven <- lm(formula = Ventas ~ NumEmpleado, data = ventas)
summary(lmven)

ggplot(ventas, aes(x=NumEmpleado, y=Ventas)) +
  geom_point() +
  geom_abline(intercept=-0.58, slope= 0.41)

lmven$fitted.values#predicciones
lmven$coefficients
lmven$residuals#Valores residuales 
#El modelo es valido en el rango observado 
#si esta fuera del rango tiene insertidumbre.

#La primera es la columna
numEmp = data.frame(
  NumEmpleado = c(12,18,20)
)
predict(lmven,numEmp) #Aplicar el modelo a nuevos datos.
#calculo de residual de valores no observados 
plot(lmven)
#Prueva de normalidad realizar 









