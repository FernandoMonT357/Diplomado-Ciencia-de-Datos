#Regresión lineal multiple
library(tidyverse)
#n_insectos = B0+B1*tem+B2*hume+e
lminsectos<-lm(N_insectos ~ Temperatura + Humedad, data = insectos)
#N_insectos = 18.272 +1.69*Temperatura + 1.62*Humedad
#El intercepto es una medida de de ajuste del modelo
summary(lminsectos)
lminsectos$residuals
#graficos 3D
library(scatterplot3d)
g3<-scatterplot3d(x=insectos$Humedad,
              y=insectos$Temperatura,
              z=insectos$N_insectos,
              type = "h")

#g3$plane3d(lminsectos)

#Reto

lmboston <-lm(medv ~ crim + lstat, data = Boston)

g3<-scatterplot3d(x=Boston$crim,
                  y=Boston$lstat,
                  z=Boston$medv,
                  type = "h")
g3$plane3d(lmboston)

summary(lmboston)

NuevasOb <- data_frame()

nuvasobs <- data.frame(crim=c(0.2,0.15),
                      lstat=c(0.5,0.3))

predict(lmboston, nuvasobs)




