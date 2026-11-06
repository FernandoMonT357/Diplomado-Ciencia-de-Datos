#Manejo de datos con diplyr
library(tidyverse)
data("mtcars")
#Tuberias en diplyr permiten definir el flujo de operaciones 
n <- rnorm(1000)#numeros aleatorios
abs(n)#Valor absoluto
sort(n)#Ordena de menor a mayor
matrix(sort(abs(n)), ncol = 2)
#tuberia %>%
n %>%
  abs %>%
   sort %>%
     matrix(ncol = 2)
#mtcars funcion filter
mtcars%>%
  filter(hp>200 & carb == 4 & mpg <11)
#diferente a
mtcars %>%
  filter( gear != 5)
# o (|)
mtcars %>%
  filter(hp > 200 | am ==2)
#Seleccionar variables
mtcars %>%
  select(hp, wt,am, carb) %>%
    filter(hp >200 & carb == 4)
#####
mtcars %>%
  filter(hp >200 & carb == 4) %>%
    select(hp, wt, am)
#ordenar datos arrange de menor a mayor desc de mayor a menor
mtcars %>%
  filter(hp >200 & carb == 4) %>%
    select(hp, wt, am, gear) %>%
      arrange(desc(hp))

mtcars %>%
  filter(hp >200 & carb == 4) %>%
  select(hp, wt, am, gear) %>%
  arrange(desc(hp), desc(wt))
#mutate crear columnas nuevas 
mtcars %>%
  mutate(wtkg = 0.45 * wt)
#mutate(nombre de la columna = 0.45 * wt)
mtcars %>%
  summarise(promedio = mean(mpg), mean(hp), sd(mpg), sd(hp))
#group_by agrupa las variables 
mtcars %>% 
  group_by(vs) %>%
    summarise(n(), promedio = mean(mpg), sd(mpg))
#Agrupas 2 variables 
mtcars %>% 
  group_by(vs, cyl, gear) %>%
    summarise(n(), promedio = mean(mpg), sd(mpg))
#========================================
#Trabajo con datos faltantes
alumnos

#Nombre     edad  Peso
#<chr>     <dbl> <dbl>
# 1 Juan         25    70
# 2 Maria        30    60
# 3 Pedro        NA    55
# 4 Guadalupe    35    NA
mean(alumnos$edad)
mean(alumnos$edad, na.rm = T)#Ignora los NA
table(alumnos$edad)
table(alumnos$edad, useNA = "always")#muestra los NA
na.omit(alumnos$edad)#Quita los valores ausentes 
mean(na.omit(alumnos$edad))
drop_na(alumnos)#quita los NA
#Con valores NA se pueden sustituir por promedio o mediana 
#promedio cundo no tenga valores extremos 
#para variables categoricas se puede crear una nueva
promedad <- mean(alumnos$edad, na.rm = T)
replace_na(alumnos$edad, promedad)
#Cambiar en el data frame
alumnos$edad <- replace_na(alumnos$edad, promedad)
alumnos
mean(alumnos$edad)
###Asignacion 7

df <- Frijol %>%
  filter(Modalidad == "Temporal" & NomMunicipio == "Acatzingo") %>%
     select(NomEstado, NomMunicipio, Modalidad,Variedad)


df1 <- Frijol %>%
  filter(SuperficieSembrada > 20000 & NomEstado == "Sinaloa") %>%
    select(NomEstado, SuperficieSembrada,Variedad, AñoAgricola,Modalidad)


Frijol %>%
  filter(Variedad =="Frijol bayo") %>%
  summarise(maximo = max(SuperficieCosechada), minimo =min(SuperficieCosechada))

Frijol %>%
  filter(SuperficieSembrada != SuperficieCosechada) %>%
     select(SuperficieSembrada, SuperficieCosechada,Variedad)

mtcars %>% 
  group_by(vs) %>%
  summarise(n(), promedio = mean(mpg), sd(mpg))

df2 <- Frijol %>%
       group_by(NomEstado) %>%
         summarise(promedio = mean(SuperficieSembrada))

Frijol %>%
  group_by(Variedad) %>%
  summarise(maximo = max (SuperficieCosechada),promedio = mean(SuperficieCosechada))
#mutate crear columnas nuevas 


Frijol <- Frijol %>%
  mutate(Rendimiento_obtenido = ProducciónObtenida/SuperficieCosechada)
  
  
Frijol %>%
  Frijol(SuperficieConSanidadVegetal == "NA")%>%
   table(useNA = "always")


media <- mean(Frijol$SuperficieConSanidadVegetal,na.rm = T)

Frijol$SuperficieConSanidadVegetal <- replace_na(Frijol$SuperficieConSanidadVegetal, media)

Frijol$SuperficieConSanidadVegetal
table(Frijol$SuperficieConSanidadVegetal, useNA = "always")

