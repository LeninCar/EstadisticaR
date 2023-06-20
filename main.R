
#Importar Base de datos

base = read.table("WHO.csv", header = TRUE, sep = ",", dec = ".")
library(dplyr)
library(ggplot2)


#varaibles 
year_1 = 2015
year_2 = 2016
year_3 = 2017

years = c(year_1, year_2,year_3)

colores_1 = c("#CD4F39", "steelblue3", "tan3")
colores_2 = c("#EE9A00", "seagreen1", "slateblue1")
colores_3 = c("#FFD700", "#FFC0CB", "#8B7D6B")
colores_4 = c("red", "turquoise3", "wheat1")


############Parte 1 - visualizacion de datos


#base filtrada por year_1 y variables seleccionadas

datos_idh_year_1 <- base %>%
  select( Country , Year , Status , HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Year == year_1 )

#Histogramas de variables HDI, GPD, life.expectancy, Schooling

hist(datos_idh_year_1$HDI, breaks = 20, col = "skyblue", xlab = "IDH", ylab = "Frecuencia", main = "Distribución del IDH")
hist(datos_idh_year_1$GDP, breaks = 20, col = "#FFE4C4", xlab = "IDH", ylab = "Frecuencia", main = "Distribución del PIB")
hist(datos_idh_year_1$Life.expectancy, breaks = 20, col = "#FF7F50", xlab = "IDH", ylab = "Frecuencia", main = "Distribución de la Esperanza de Vida")
hist(datos_idh_year_1$Schooling, breaks = 20, col = "#FFFFF0", xlab = "IDH", ylab = "Frecuencia", main = "Distribución de la Escolaridad")

#Resumen: medias, medianas, cuartiles de todas las varaibles
summary(datos_idh_year_1)

#Desviacion estandar de varaibles HDI, GPD, life.expectancy, Schooling

desviacion_estandar_year_1_hdi <- sd(datos_idh_year_1$HDI, na.rm = TRUE)
desviacion_estandar_year_1_gdp <- sd(datos_idh_year_1$GDP, na.rm = TRUE)
desviacion_estandar_year_1_life <- sd(datos_idh_year_1$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_1_schooling <- sd(datos_idh_year_1$Schooling, na.rm = TRUE)

#coeficientes de variacion year1

coef_var_year_1_hdi <- desviacion_estandar_year_1_hdi / media_year_1_idh * 100
coef_var_year_1_gdp <- desviacion_estandar_year_1_gdp / media_year_1_gdp * 100
coef_var_year_1_life <- desviacion_estandar_year_1_life / media_year_1_esperanza * 100
coef_var_year_1_schooling <- desviacion_estandar_year_1_schooling / media_year_1_escolaridad * 100

plot(datos_idh_year_1$Schooling, datos_idh_year_1$HDI, xlab = "Escolaridad", ylab = "IDH", main = "Relación entre Escolaridad e IDH", pch=18, col="grey")
plot(datos_idh_year_1$GDP,  datos_idh_year_1$HDI, xlab = "PIB", ylab = "IDH", main = "Relación entre PIB e IDH", pch=18, col="grey")
plot(datos_idh_year_1$Life.expectancy,  datos_idh_year_1$HDI, xlab = "Esperanza", ylab = "IDH", main = "Relación entre Esperanza de vida e IDH", pch=18, col="grey")

boxplot(datos_idh_years$GDP)

####################year_2##########################

datos_idh_year_2 <- base%>%select( Country , Year , Status , HDI , Schooling , Life.expectancy , GDP) %>%filter(Year == year_2)

#Histogramas de variables HDI, GPD, life.expectancy, Schooling

hist(datos_idh_year_2$HDI, breaks = 20, col = "skyblue", xlab = "IDH", ylab = "Frecuencia", main = "Distribución del IDH")
hist(datos_idh_year_2$GDP, breaks = 20, col = "#FFE4C4", xlab = "GPD", ylab = "Frecuencia", main = "Distribución del PIB")
hist(datos_idh_year_2$Life.expectancy, breaks = 20, col = "#FF7F50", xlab = "Esperanza de vida", ylab = "Frecuencia", main = "Distribución de la Esperanza de Vida")
hist(datos_idh_year_2$Schooling, breaks = 20, col = "#FFFFF0", xlab = "Escolaridad", ylab = "Frecuencia", main = "Distribución de la Escolaridad")

#Resumen: medias, medianas, cuartiles de todas las varaibles
summary(datos_idh_year_2)

#Desviacion estandar de varaibles HDI, GPD, life.expectancy, Schooling

desviacion_estandar_year_2_hdi <- sd(datos_idh_year_2$HDI, na.rm = TRUE)
desviacion_estandar_year_2_gdp <- sd(datos_idh_year_2$GDP, na.rm = TRUE)
desviacion_estandar_year_2_life <- sd(datos_idh_year_2$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_2_schooling <- sd(datos_idh_year_2$Schooling, na.rm = TRUE)


#coeficientes de variacion year2

coef_var_year_2_hdi <- desviacion_estandar_year_2_hdi / media_year_2_idh * 100
coef_var_year_2_gdp <- desviacion_estandar_year_2_gdp / media_year_2_gdp * 100
coef_var_year_2_life <- desviacion_estandar_year_2_life / media_year_2_esperanza * 100
coef_var_year_2_schooling <- desviacion_estandar_year_2_schooling / media_year_2_escolaridad * 100

plot(datos_idh_year_2$Schooling, datos_idh_year_2$HDI, xlab = "Escolaridad", ylab = "IDH", main = "Relación entre Escolaridad e IDH", pch=18, col="grey")
plot(datos_idh_year_2$GDP, datos_idh_year_2$HDI, xlab = "PIB", ylab = "IDH", main = "Relación entre PIB e IDH", pch=18, col="grey")
plot(datos_idh_year_2$Life.expectancy, datos_idh_year_2$HDI, xlab = "Esperanza", ylab = "IDH", main = "Relación entre Esperanza de vida e IDH", pch=18, col="grey")

#############################

####################year_3##########################

datos_idh_year_3 <- base %>%
  select( Country , Year , Status , HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Year == year_3)

#Histogramas de variables HDI, GPD, life.expectancy, Schooling

hist(datos_idh_year_3$HDI, breaks = 20, col = "skyblue", xlab = "IDH", ylab = "Frecuencia", main = "Distribución del IDH")
hist(datos_idh_year_3$GDP, breaks = 20, col = "#FFE4C4", xlab = "IDH", ylab = "Frecuencia", main = "Distribución del PIB")
hist(datos_idh_year_3$Life.expectancy, breaks = 20, col = "#FF7F50", xlab = "IDH", ylab = "Frecuencia", main = "Distribución de la Esperanza de Vida")
hist(datos_idh_year_3$Schooling, breaks = 20, col = "#FFFFF0", xlab = "IDH", ylab = "Frecuencia", main = "Distribución de la Escolaridad")


#Resumen: medias, medianas, cuartiles de todas las varaibles
summary(datos_idh_year_3)

#Desviacion estandar de varaibles HDI, GPD, life.expectancy, Schooling

desviacion_estandar_year_3_hdi <- sd(datos_idh_year_3$HDI, na.rm = TRUE)
desviacion_estandar_year_3_gdp <- sd(datos_idh_year_3$GDP, na.rm = TRUE)
desviacion_estandar_year_3_life <- sd(datos_idh_year_3$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_3_schooling <- sd(datos_idh_year_3$Schooling, na.rm = TRUE)

#coeficientes de variacion year3

coef_var_year_3_hdi <- desviacion_estandar_year_3_hdi / media_year_3_idh * 100
coef_var_year_3_gdp <- desviacion_estandar_year_3_gdp / media_year_3_gdp * 100
coef_var_year_3_life <- desviacion_estandar_year_3_life / media_year_3_esperanza * 100
coef_var_year_3_schooling <- desviacion_estandar_year_3_schooling / media_year_3_escolaridad * 100


plot(datos_idh_year_3$Schooling, xlab = "Escolaridad", ylab = "IDH", main = "Relación entre Escolaridad e IDH", pch=18, col="grey")
plot(datos_idh_year_3$HDI, xlab = "PIB", ylab = "IDH", main = "Relación entre PIB e IDH", pch=18, col="grey")
plot(datos_idh_year_3$GDP, xlab = "Esperanza", ylab = "IDH", main = "Relación entre Esperanza de vida e IDH", pch=18, col="grey")

#######################################################################

paste(" La media (promedio) del IDH en el year_1 es" , mean(datos_idh_year_1$GDP, na.rm = TRUE))
paste(" La media (promedio) del IDH en el year_2 es" , mean(datos_idh_year_2$GDP, na.rm = TRUE))
paste(" La media (promedio) del IDH en el year_3 es" , mean(datos_idh_year_3$GDP, na.rm = TRUE))

################################################################################ Parte 2 - Comparar valores entre años

summary(datos_idh_year_1)
summary(datos_idh_year_2)
summary(datos_idh_year_3)


######comparar medias para ver crecimiento a mediad que pasan los 3 años

datos_idh_years <- base %>%
  select( Country , Year , Status , HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Year == year_1 | Year == year_2 | Year == year_3)

media_year_1_idh <- mean(datos_idh_year_1$HDI, na.rm = TRUE)
media_year_2_idh <- mean(datos_idh_year_2$HDI, na.rm = TRUE)
media_year_3_idh <- mean(datos_idh_year_3$HDI, na.rm = TRUE)

media_year_1_escolaridad <- mean(datos_idh_year_1$Schooling, na.rm = TRUE)
media_year_2_escolaridad <- mean(datos_idh_year_2$Schooling, na.rm = TRUE)
media_year_3_escolaridad <- mean(datos_idh_year_3$Schooling, na.rm = TRUE)

media_year_1_gdp <- mean(datos_idh_year_1$GDP, na.rm = TRUE)
media_year_2_gdp <- mean(datos_idh_year_2$GDP, na.rm = TRUE)
media_year_3_gdp <- mean(datos_idh_year_3$GDP, na.rm = TRUE)

media_year_1_esperanza <- mean(datos_idh_year_1$Life.expectancy, na.rm = TRUE)
media_year_2_esperanza <- mean(datos_idh_year_2$Life.expectancy, na.rm = TRUE)
media_year_3_esperanza <- mean(datos_idh_year_3$Life.expectancy, na.rm = TRUE)

media_grafica_idh = c(media_year_1_idh, media_year_2_idh, media_year_3_idh)

media_grafica_gdp = c(media_year_1_gdp, media_year_2_gdp, media_year_3_gdp)

media_grafica_escolaridad = c(media_year_1_escolaridad, media_year_2_escolaridad, media_year_3_escolaridad)

media_grafica_esperanza = c(media_year_1_esperanza, media_year_2_esperanza, media_year_3_esperanza)

#####graficos de barras para ilustrar cambio


barplot(media_grafica_idh, ylim=c(0.68,0.71),names.arg = years,ylab="IDH",
        col="#BBFFFF",main="Comparacion IDH en años")

barplot(media_grafica_gdp, ylim=c(12000,13800) ,names.arg = years,ylab="GPD",
        col="palegreen1",main="Comparacion GPD en años")

barplot(media_grafica_escolaridad, ylim=c(7.5,9),names.arg = years,ylab="GPD",
        col="#FF4500",main="Comparacion Escolaridad en años")

barplot(media_grafica_esperanza, ylim=c(70,75),names.arg = years,ylab="GPD",
        col="#FFB6C1",main="Comparacion Esperanza de vida en años")



#mediana_year_1 <- median(datos_idh_year_1$HDI, na.rm = TRUE)
#desviacion_estandar_year_1 <- sd(datos_idh_year_1$HDI, na.rm = TRUE)
#rango_year_1 <- range(datos_idh_year_1$HDI, na.rm = TRUE)
#print(media_year_1)
#matriz_cor <- cor(datos_idh_year_1$HDI, datos_idh_year_1$GDP, use = "complete.obs")


#################Comparacion PIB respecto a las otras variables que lo componen

par(xpd = TRUE)
plot(datos_idh_years$GDP,  xlab = "#Paises", ylab = "PIB", main = "Dispersion del PIB", col = colores_1, pch=18)
legend("topleft", legend = years, fill = colores_1, bty = "n")
plot(datos_idh_years$Schooling,  xlab = "#Paises", ylab = "Escolaridad (años)", main = "Dispersion de la Escolaridad", col = colores_2, pch=15)
legend(x = 0.001, legend = years, fill = colores_2, bty = "n")
plot(datos_idh_years$Life.expectancy,  xlab = "#Paises", ylab = "Esperanza de vida (años)", main = "Dispersion de la Esperanza de vida", col = colores_3, pch=17)
legend("topleft", legend = years, fill = colores_3, bty = "n")
plot(datos_idh_years$HDI,  xlab = "#Paises", ylab = "IDH", main = "Dispersion del IDH", col = colores_4, pch=15)
legend("bottomright", legend = years, fill = colores_4, bty = "n")

######################################


plot(density(datos_idh_years$GDP,na.rm = TRUE ), col = "blue", lwd = 2,lty = "dashed", xlab = "Valores", ylab = "Densidad", main = "Gráfico de Densidad del PIB")
plot(density(datos_idh_years$HDI,na.rm = TRUE ), col = "#EE9A49", lwd = 2,lty = "dotted", xlab = "Valores", ylab = "Densidad", main = "Gráfico de Densidad del IDH")
plot(density(datos_idh_years$Schooling,na.rm = TRUE ),  col = "#9370DB", lwd = 2, lty = "dotdash", xlab = "Valores", ylab = "Densidad", main = "Gráfico de Densidad de la Escolaridad")
plot(density(datos_idh_years$Life.expectancy,na.rm = TRUE ), col = "darkolivegreen2", lwd = 2 , lty = "longdash",xlab = "Valores", ylab = "Densidad", main = "Gráfico de Densidad de la Esperanza de vida")



boxplot(datos_idh_year_1$Schooling, datos_idh_year_2$Schooling, datos_idh_year_3$Schooling, ylim=c(0,18), names = years, col = colores_1,  ylab = "Escolaridad", main = "Comparacion de Escolaridad respecto al año")
boxplot(datos_idh_year_1$GDP, datos_idh_year_2$GDP, datos_idh_year_3$GDP,  names = years, col = colores_2, ylab = "PIB", main = "Comparacion del PIB respecto al año")
boxplot(datos_idh_year_1$HDI, datos_idh_year_2$HDI, datos_idh_year_3$HDI, names = years, col = colores_3, ylab = "IDH", main = "Comparacion del IDH respecto al año")
boxplot(datos_idh_year_1$Life.expectancy, datos_idh_year_2$Life.expectancy, datos_idh_year_3$Life.expectancy, names = years, col = colores_4, ylab = "Esperanza de vida", main = "Comparacion de la Esperanza de vida respecto al año")


##################Parte 3 - Comparar valores entre desarrollo de pasies
########################################## Year1
datos_idh_year_1_developed <- datos_idh_year_1 %>%
  select( Status, Country , Year, HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Status == 'Developed')

media_year_1_idh_developed <- mean(datos_idh_year_1_developed$HDI, na.rm = TRUE)
media_year_1_gdp_developed <- mean(datos_idh_year_1_developed$GDP, na.rm = TRUE)
media_year_1_life_developed <- mean(datos_idh_year_1_developed$Life.expectancy, na.rm = TRUE)
media_year_1_schooling_developed <- mean(datos_idh_year_1_developed$Schooling, na.rm = TRUE)

summary(datos_idh_year_1_developed)

desviacion_estandar_year_1_hdi_developed <- sd(datos_idh_year_1_developed$HDI, na.rm = TRUE)
desviacion_estandar_year_1_gdp_developed <- sd(datos_idh_year_1_developed$GDP, na.rm = TRUE)
desviacion_estandar_year_1_life_developed <- sd(datos_idh_year_1_developed$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_1_schooling_developed <- sd(datos_idh_year_1_developed$Schooling, na.rm = TRUE)

coef_var_year_1_hdi_developed <- desviacion_estandar_year_1_hdi_developed / media_year_1_idh_developed * 100
coef_var_year_1_gdp_developed <- desviacion_estandar_year_1_gdp_developed / media_year_1_gdp_developed * 100
coef_var_year_1_life_developed <- desviacion_estandar_year_1_life_developed / media_year_1_life_developed * 100
coef_var_year_1_schooling_developed <- desviacion_estandar_year_1_schooling_developed / media_year_1_schooling_developed * 100

datos_idh_year_1_developing <- datos_idh_year_1 %>%
  select( Status, Country , Year, HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Status == 'Developing')

media_year_1_idh_developing <- mean(datos_idh_year_1_developing$HDI, na.rm = TRUE)
media_year_1_gdp_developing <- mean(datos_idh_year_1_developing$GDP, na.rm = TRUE)
media_year_1_life_developing <- mean(datos_idh_year_1_developing$Life.expectancy, na.rm = TRUE)
media_year_1_schooling_developing <- mean(datos_idh_year_1_developing$Schooling, na.rm = TRUE)

summary(datos_idh_year_1_developing)

desviacion_estandar_year_1_hdi_developing <- sd(datos_idh_year_1_developing$HDI, na.rm = TRUE)
desviacion_estandar_year_1_gdp_developing <- sd(datos_idh_year_1_developing$GDP, na.rm = TRUE)
desviacion_estandar_year_1_life_developing <- sd(datos_idh_year_1_developing$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_1_schooling_developing <- sd(datos_idh_year_1_developing$Schooling, na.rm = TRUE)

coef_var_year_1_hdi_developing <- desviacion_estandar_year_1_hdi_developing / media_year_1_idh_developing * 100
coef_var_year_1_gdp_developing <- desviacion_estandar_year_1_gdp_developing / media_year_1_gdp_developing * 100
coef_var_year_1_life_developing <- desviacion_estandar_year_1_life_developing / media_year_1_life_developing * 100
coef_var_year_1_schooling_developing <- desviacion_estandar_year_1_schooling_developing / media_year_1_schooling_developing * 100
########################################## Year2

datos_idh_year_2_developed <- datos_idh_year_2 %>%
  select( Status, Country , Year, HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Status == 'Developed')

media_year_2_idh_developed <- mean(datos_idh_year_2_developed$HDI, na.rm = TRUE)
media_year_2_gdp_developed <- mean(datos_idh_year_2_developed$GDP, na.rm = TRUE)
media_year_2_life_developed <- mean(datos_idh_year_2_developed$Life.expectancy, na.rm = TRUE)
media_year_2_schooling_developed <- mean(datos_idh_year_2_developed$Schooling, na.rm = TRUE)

summary(datos_idh_year_2_developed)

desviacion_estandar_year_2_hdi_developed <- sd(datos_idh_year_2_developed$HDI, na.rm = TRUE)
desviacion_estandar_year_2_gdp_developed <- sd(datos_idh_year_2_developed$GDP, na.rm = TRUE)
desviacion_estandar_year_2_life_developed <- sd(datos_idh_year_2_developed$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_2_schooling_developed <- sd(datos_idh_year_2_developed$Schooling, na.rm = TRUE)

coef_var_year_2_hdi_developed <- desviacion_estandar_year_2_hdi_developed / media_year_2_idh_developed * 100
coef_var_year_2_gdp_developed <- desviacion_estandar_year_2_gdp_developed / media_year_2_gdp_developed * 100
coef_var_year_2_life_developed <- desviacion_estandar_year_2_life_developed / media_year_2_life_developed * 100
coef_var_year_2_schooling_developed <- desviacion_estandar_year_2_schooling_developed / media_year_2_schooling_developed * 100

datos_idh_year_2_developing <- datos_idh_year_2 %>%
  select( Status, Country , Year, HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Status == 'Developing')

media_year_2_idh_developing <- mean(datos_idh_year_2_developing$HDI, na.rm = TRUE)
media_year_2_gdp_developing <- mean(datos_idh_year_2_developing$GDP, na.rm = TRUE)
media_year_2_life_developing <- mean(datos_idh_year_2_developing$Life.expectancy, na.rm = TRUE)
media_year_2_schooling_developing <- mean(datos_idh_year_2_developing$Schooling, na.rm = TRUE)

summary(datos_idh_year_2_developing)

desviacion_estandar_year_2_hdi_developing <- sd(datos_idh_year_2_developing$HDI, na.rm = TRUE)
desviacion_estandar_year_2_gdp_developing <- sd(datos_idh_year_2_developing$GDP, na.rm = TRUE)
desviacion_estandar_year_2_life_developing <- sd(datos_idh_year_2_developing$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_2_schooling_developing <- sd(datos_idh_year_2_developing$Schooling, na.rm = TRUE)

coef_var_year_2_hdi_developing <- desviacion_estandar_year_2_hdi_developing / media_year_2_idh_developing * 100
coef_var_year_2_gdp_developing <- desviacion_estandar_year_2_gdp_developing / media_year_2_gdp_developing * 100
coef_var_year_2_life_developing <- desviacion_estandar_year_2_life_developing / media_year_2_life_developing * 100
coef_var_year_2_schooling_developing <- desviacion_estandar_year_2_schooling_developing / media_year_2_schooling_developing * 100


########################################### Year3

datos_idh_year_3_developed <- datos_idh_year_3 %>%
  select( Status, Country , Year, HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Status == 'Developed')

media_year_3_idh_developed <- mean(datos_idh_year_3_developed$HDI, na.rm = TRUE)
media_year_3_gdp_developed <- mean(datos_idh_year_3_developed$GDP, na.rm = TRUE)
media_year_3_life_developed <- mean(datos_idh_year_3_developed$Life.expectancy, na.rm = TRUE)
media_year_3_schooling_developed <- mean(datos_idh_year_3_developed$Schooling, na.rm = TRUE)

summary(datos_idh_year_3_developed)

desviacion_estandar_year_3_hdi_developed <- sd(datos_idh_year_3_developed$HDI, na.rm = TRUE)
desviacion_estandar_year_3_gdp_developed <- sd(datos_idh_year_3_developed$GDP, na.rm = TRUE)
desviacion_estandar_year_3_life_developed <- sd(datos_idh_year_3_developed$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_3_schooling_developed <- sd(datos_idh_year_3_developed$Schooling, na.rm = TRUE)

coef_var_year_3_hdi_developed <- desviacion_estandar_year_3_hdi_developed / media_year_3_idh_developed * 100
coef_var_year_3_gdp_developed <- desviacion_estandar_year_3_gdp_developed / media_year_3_gdp_developed * 100
coef_var_year_3_life_developed <- desviacion_estandar_year_3_life_developed / media_year_3_life_developed * 100
coef_var_year_3_schooling_developed <- desviacion_estandar_year_3_schooling_developed / media_year_3_schooling_developed * 100

datos_idh_year_3_developing <- datos_idh_year_3 %>%
  select( Status, Country , Year, HDI , Schooling , Life.expectancy , GDP) %>%
  filter(Status == 'Developing')

media_year_3_idh_developing <- mean(datos_idh_year_3_developing$HDI, na.rm = TRUE)
media_year_3_gdp_developing <- mean(datos_idh_year_3_developing$GDP, na.rm = TRUE)
media_year_3_life_developing <- mean(datos_idh_year_3_developing$Life.expectancy, na.rm = TRUE)
media_year_3_schooling_developing <- mean(datos_idh_year_3_developing$Schooling, na.rm = TRUE)

summary(datos_idh_year_3_developing)

desviacion_estandar_year_3_hdi_developing <- sd(datos_idh_year_3_developing$HDI, na.rm = TRUE)
desviacion_estandar_year_3_gdp_developing <- sd(datos_idh_year_3_developing$GDP, na.rm = TRUE)
desviacion_estandar_year_3_life_developing <- sd(datos_idh_year_3_developing$Life.expectancy, na.rm = TRUE)
desviacion_estandar_year_3_schooling_developing <- sd(datos_idh_year_3_developing$Schooling, na.rm = TRUE)

coef_var_year_3_hdi_developing <- desviacion_estandar_year_3_hdi_developing / media_year_3_idh_developing * 100
coef_var_year_3_gdp_developing <- desviacion_estandar_year_3_gdp_developing / media_year_3_gdp_developing * 100
coef_var_year_3_life_developing <- desviacion_estandar_year_3_life_developing / media_year_3_life_developing * 100
coef_var_year_3_schooling_developing <- desviacion_estandar_year_3_schooling_developing / media_year_3_schooling_developing * 100

#################################################################################


############################Parte 4


####################Diagrama de barras dividivo en años con respecto al status

ggplot(datos_idh_years,aes(x=Year,y=HDI, fill=Status)) +
  geom_col(position = "dodge")+labs(x = "Year", y = "IDH", fill="Status")

ggplot(datos_idh_years,aes(x=Year,y=GDP, fill=Status)) +
  geom_col(position = "dodge")+labs(x = "Year", y = "PIB", fill="Status")

ggplot(datos_idh_years,aes(x=Year,y=Life.expectancy, fill=Status)) +
  geom_col(position = "dodge")+labs(x = "Year", y = "Esperanza de vida", fill="Status")

ggplot(datos_idh_years,aes(x=Year,y=Schooling, fill=Status)) +
  geom_col(position = "dodge")+labs(x = "Year", y = "Escolaridad", fill="Status")





                                                                                                                                              