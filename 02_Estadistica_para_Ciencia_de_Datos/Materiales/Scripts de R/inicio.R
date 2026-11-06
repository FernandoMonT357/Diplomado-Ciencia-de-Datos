#Calculo de frecuencias
#Frecuencia (F)

#Frecuancia Absoluta
F_abs <- table(MaizOloton$Longitud)

#Frecuencia Absoluta Acumulada
F_abs_Ac <-cumsum(F_abs)

#Frecuencia Relativa
F_relativa <- prop.table(F_abs) * 100
  
#Frecuencia Relativa Acumulada
F_relativa_Acu <- cumsum(F_relativa) 

# Tabla de contingencia
table(MaizOloton$Longitud,MaizOloton$Ancho)
prop.table(table(MaizOloton$Longitud,MaizOloton$Ancho))*100
#Columna: Ancho, Fila: Longitud

# Ejercicio 4
data("mtcars")
#Frecuancia Absoluta
table(mtcars$cyl)
#Frecuencia Absoluta Acumulada
cumsum(table(mtcars$cyl))
#Frecuencia Relativa
prop.table(table(mtcars$cyl)) * 100
#Frecuencia Relativa Acumulada
cumsum(prop.table(table(mtcars$cyl))) 
# Tabla de contingencia
table(mtcars$cyl,mtcars$am)
100*prop.table(table(mtcars$cyl,mtcars$am))






#Frecuancia Absoluta
f_absoluta <- table(mtcars$cyl)
#Frecuencia Relativa
prop.table(table(mtcars$carb)) * 100
#•	¿Cantidad de autos con transmisi´on autom´atica(am)?
table(mtcars$am)
prop.table(table(mtcars$am))*100
cumsum(table(mtcars$carb)) 

prop.table(table(mtcars$carb))*100

table(mtcars$am,mtcars$cyl)

prop.table(table(mtcars$am,mtcars$cyl)) * 100

table(HEART$AHD)


prop.table(table(HEART$Sex)) * 100


table(HEART$Sex)


prop.table(table(HEART$AHD)) * 100

cumsum(table(HEART$Age))

table(HEART$AHD, HEART$Sex)

prop.table(table(HEART$AHD, HEART$Sex))*100

#Tablas de frecuencia para datos agrupados
#fi = F absoluta
#Fi = F absoluta acumulada
# hi = F relativa
# Hi = F relativa acumulada
# minimi 5 intervalis maximo 3 intervalos 
min(HEART$Age)

#Parametros estaditicos

peso <- c(63,52,78,49,71,62,68,52)
#media
mean(peso)
#Mediana
median(peso)
#moda
table(peso)
as.numeric(names(table(peso))[which.max(table(peso))])
#Varianza 
var(peso)
#Desviación estándar
sd(peso)
#Coeficiente de variación
(sd(peso)/mean(peso))*100




#01/10/2026 Tabla de frecuencia para datos agrupados 
table(Durazno$Perimetro)
intervalos = 3
#rango
rango <- max(Durazno$Perimetro)-min(Durazno$Perimetro)
amplitud <- rango/intervalos
minimo <- min(Durazno$Perimetro)
#Limites
limites <- minimo+amplitud*(0:intervalos)
#Categorizar los valores en clases o intervalos
# se debe de cerrar el primer intervalo
intervalos <- cut(Durazno$Perimetro,breaks = limites, include.lowest = T)
table(intervalos)
cumsum(table(intervalos))
prop.table(table(intervalos)) * 100
cumsum(prop.table(table(intervalos))) 

#hitograma 
hist(Durazno$Perimetro, breaks = limites, include.lowest = T)
#Reglas para definir intervalos 
nclass.Sturges(Durazno$Perimetro)
nclass.scott(Durazno$Perimetro)
nclass.FD(Durazno$Perimetro)
# con rangos definidos 
#Pequeños 15.2-16.6
#Grandes 16.6-17.3
limites <- c(15.2,16.6,17.3)


#Reto 1
intervalo = 5
min = min(Durazno$Peso)
max = max(Durazno$Peso)
rango <- max-min
amplitud <- rango/intervalo
limites <- min+amplitud*(0:intervalo)
intervalos <- cut(Durazno$Peso,breaks = limites, include.lowest = T)
hist(Durazno$Peso, breaks = limites, include.lowest = T)
table(intervalos)
prop.table(table(intervalos)) * 100

cumsum(prop.table(table(intervalos))) *100


intervalo <- nclass.Sturges(Invernadero$`Extremo B`)
min <- min(Invernadero$`Extremo B`)
max <- max(Invernadero$`Extremo B`)
rango <- max - min
# Amplitud
amplitud <- rango / intervalo

# Límites de los intervalos
limites <- min + amplitud * (0:intervalo)

# Crear intervalos
intervalos <- cut(Invernadero$`Extremo B`,breaks = limites, include.lowest = TRUE)

frecuencia <- table(intervalos)
frecuencia_acum <- cumsum(frecuencia)
frecuencia_rel <- prop.table(frecuencia)
frecuencia_rel_acum <- cumsum(frecuencia_rel)*100
hist(Invernadero$`Extremo B`,breaks = limites,include.lowest = TRUE)
#Reto 

limites <- c(0, 68, 72, 80)

clasificacion <- cut(Manzanas$EjeLongitudinal, breaks = limites,include.lowest = TRUE)
table(clasificacion)
prop.table(table(clasificacion)) * 100
cumsum(prop.table(table(clasificacion)))*100


peso <- c(63,52,78,49,71,62,68,52)
#media
mean(peso)
#Mediana
median(peso)
#moda
table(peso)
as.numeric(names(table(peso))[which.max(table(peso))])
#Varianza 
var(peso)
#Desviación estándar
sd(peso)
#Coeficiente de variación
(sd(peso)/mean(peso))*100



