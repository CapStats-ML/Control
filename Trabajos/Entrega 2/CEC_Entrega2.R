################################################################################
# Script Entrega 2: Control Estadistico de Calidad
# Autores: 
# - Cesar Augusto Prieto Sarmiento
# - Cristian Camilo Prieto Zambrano
# - Daniel Santiago Guzman 
# Fecha de Creación: 03/09/2024
# Ultima Fecha de Mod: 03/09/2024
# Descripción: Se plantea el desarrollo de los puntos del segundo trabajo de la asignatura
#              Control Estadistico de Calidad del Pregrado en Estadistica de la Universidad 
#              Nacional de Colombia sede Bogotá.
# Versión: 1
################################################################################

# EJERCICIO 1 ----
# Diseñar una carta Seis-Sigma para el monitoreo en línea de la proporción de no conformidad
# p = 0.1 usando muestras de tamaño variable, los cuales se generan en cada instante de
# monitoreo de una variable uniforme discreta en el intervalo [200, 400] con incrementos
# de 50 observaciones. El diseño debe realizarse para el caso: 

# a) Utilizando el esquema estandarizado.

# El esquema estandarizado nos da una carta que esta centrada en 0 y tiene limites 
# de control en -3 y 3, por lo que, para el caso de la proporción de no conformidad
# no podemos ver un ejemplo claro de los limites si no lo ajustamos a un ejemplo. 

n <- seq(200, 400, 50)   # Tamaños de muestra posibles
p0 <- 0.1                # Proporción nominal de no conformidad

m = 30                   # Cantidad de muestras a generar
set.seed(22)             # Fijar semilla para replicabilidad

# Generar tamaños de muestra aleatorios
tamaños = sample(x = n, size = m, replace = TRUE)

# Generar muestras de una distribución binomial
muestras = rbinom(n = m, size = tamaños, prob = p0)

# Calcular proporciones de no conformidad
proporciones = muestras / tamaños

UCL <- 3  # Límite de control superior
LCL <- -3 # Límite de control inferior


StanCC <- function(n, P, p0){
  Zi <- (P - p0) / sqrt(p0 * (1 - p0) / n)
  return(Zi)
}

# Calcular los valores Zi para cada muestra
Zi_values <- StanCC(tamaños, proporciones, p0)


par(mar = c(5.1, 5.1, 4.1, 2.1), bg = '#fffbf7') # Configurar márgenes y fondo
plot(Zi_values, type = 'o', col = '#423f32', pch = 20, cex = 1.5, ylim = c(LCL, UCL), family = "sans", bty = "L",
     xlab = 'Tiempo de medición', ylab = 'Valor de Zi para \n Prop. No Conformidad',  font.lab = 2, cex.lab = 0.8,
     font = 3, font.main = 4, main = 'Carta de control Z con límites variables', cex.axis = 0.7, col.main = 'black',
     sub = 'Proporción de no conformidad p = 0.1', font.sub = 2, cex.sub = 0.8, fg = '#423f32', col.sub = '#ff6f61')

# Líneas de guía
abline(h = seq(-3, 3, 0.5), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(1, m, 5), col = '#d6c6b8', lty = 'dotted')

# Líneas de control superior e inferior
abline(h = UCL, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
abline(h = LCL, col = '#ff6f61', lty = 'dashed', lwd = 1.8)

# Línea central (valor nominal de Z = 0)
abline(h = 0, col = '#6b8e8e', lwd = 1.2)

# Leyenda
legend(x = 25, y = -1.5 , legend = c('UCL & LCL', 'Obs', 'LC'), title = 'Lineas',
       col = c('#ff6f61', '#423f32', '#6b8e8e'), lty = c('dashed', 'solid', 'solid'), 
       lwd = c(1.8, 1.8, 1.8), cex = 0.8, bty = 'n', bg = 'white', text.font = 2,
       text.col = '#423f32')

# EJERCICIO 4 ----

# Kittlitz (1999) presenta datos sobre homicidios en Waco (Texas, EU) para los años 
# 1980-1989. La tabla que se presenta a continuación muestra las fechas de los homicidios 
# en 1989 y el número de días entre cada homicidio.

# a) Establézcase un esquema adecuado de monitoreo para la situación descrita.

# - Esquema de monitoreo adecuado: Carta de control de Shewhart para medias y rangos
# - Justificación: La carta de control de Shewhart para medias y rangos es adecuada para
#   monitorear la variabilidad de un proceso a través del tiempo. En este caso, se busca
#   monitorear la variabilidad de los días entre homicidios en Waco (Texas, EU) para el año
#   1989. La carta de control de medias y rangos es adecuada para este propósito, ya que
#   permite monitorear la variabilidad de los datos a través del tiempo y detectar posibles
#   cambios en la variabilidad del proceso.

# Importacion de los datos

Periodo1 <- read.csv("~/REPOS GIT/Control/Trabajos/Entrega 2/Periodo1.txt", 
                     colClasses = c("character", "numeric", "numeric"), 
                     col.names = c("Mes", "Dias", "DaysBetween"))
Periodo2 <- read.csv("~/REPOS GIT/Control/Trabajos/Entrega 2/Periodo2.txt", 
                     colClasses = c("character", "numeric", "numeric"),
                     col.names = c("Mes", "Dias", "DaysBetween"))

# Quitar NA's 

drop_na <- function(df){
  df <- df[complete.cases(df), ]
  return(df)
}

sum(is.na(Periodo1$Dias))
sum(is.na(Periodo1$DaysBetween))

Periodo1 <- drop_na(Periodo1)

sum(is.na(Periodo2$Dias))
sum(is.na(Periodo2$DaysBetween))

Periodo2 <- drop_na(Periodo2)

# Prueba de normalidad con qqplot

# Grafica 1
# Configurar márgenes y fondo
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')

# Crear el QQ plot con estilos personalizados
qqnorm(Periodo1$DaysBetween, main = "QQ Plot Periodo 1", col = '#423f32',  pch = 20, cex = 1.5, 
       family = "sans", bty = "L", xlab = "Cuantiles teóricos", ylab = "Cuantiles de los datos",
       font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4, col.main = 'black', cex.axis = 0.7)

# Añadir línea de referencia con estilo personalizado
qqline(Periodo1$DaysBetween, col = '#6b8e8e', lwd = 1.2)

# Añadir líneas de guía
abline(h = seq(0, 35, 5), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(-3, 3, 0.5), col = '#d6c6b8', lty = 'dotted')

# Añadir leyenda
legend(x = 1, y = 5, legend = c('Tendencia', 'Observaciones'), col = c('#6b8e8e', '#423f32'), 
       lty = c('solid', 'none'), pch = c(NA, 20), lwd = c(1.2, NA), cex = 0.8, bty = 'n', 
       bg = 'white', text.font = 2, text.col = '#423f32')


# Grafica 2
# Configurar márgenes y fondo
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')

# Crear el QQ plot con estilos personalizados
qqnorm(Periodo2$DaysBetween, main = "QQ Plot Periodo 1", col = '#423f32',  pch = 20, cex = 1.5, 
       family = "sans", bty = "L", xlab = "Cuantiles teóricos", ylab = "Cuantiles de los datos",
       font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4, col.main = 'black', cex.axis = 0.7)

# Añadir línea de referencia con estilo personalizado
qqline(Periodo2$DaysBetween, col = '#6b8e8e', lwd = 1.2)

# Añadir líneas de guía
abline(h = seq(0, 55, 5), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(-3, 3, 0.5), col = '#d6c6b8', lty = 'dotted')

# Añadir leyenda
legend(x = 1, y = 5, legend = c('Tendencia', 'Observaciones'), col = c('#6b8e8e', '#423f32'), 
       lty = c('solid', 'none'), pch = c(NA, 20), lwd = c(1.2, NA), cex = 0.8, bty = 'n', 
       bg = 'white', text.font = 2, text.col = '#423f32')


# Calculos necesarios para construir la carta R
# Podemos entender la variables DaysBetween como el rango de días entre homicidios
# Con lo cual no tenemos que calcular R para cada observacion sino R_barra y encontrar 
# los respectivos valores de  D4 y D3 para los limites de control de la carta R

# Calcular el rango promedio

RP1barra <- mean(Periodo1$DaysBetween)
RP2barra <- mean(Periodo2$DaysBetween)

# Calcular los límites de control para la carta R

























# Crear las cartas de control para las medias y los rangos

par(mfrow = c(2, 1), mar = c(5.1, 5.1, 4.1, 2.1), bg = '#fffbf7') # Configurar márgenes y fondo

# Carta de control para las medias
plot(Periodo1$DaysBetween, type = 'o', col = '#423f32', pch = 20, cex = 1.5, ylim = c(LCL_Xbarra, UCL_Xbarra), 
     family = "sans", bty = "L", xlab = 'Tiempo de medición', ylab = 'Días entre homicidios',  
     font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4, main = 'Carta de control para las medias', 
     cex.axis = 0.7, col.main = 'black', sub = 'Periodo 1', font.sub = 2, cex.sub = 0.8, fg = '#423f32', 
     col.sub = '#ff6f61')

# Líneas de guía
abline(h = seq(LCL_Xbarra, UCL_Xbarra, 5), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(1, nrow(Periodo1), 5), col = '#d6c6b8', lty = 'dotted')

# Líneas de control superior e inferior
abline(h = UCL_Xbarra, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
abline(h = LCL_Xbarra, col = '#ff6f61', lty = 'dashed', lwd = 1.8)

# Línea central (valor nominal de la media)
abline(h = medias1, col = '#6b8e8e', lwd = 1.2)

# Leyenda
legend(x = 25, y = 30, legend = c('UCL & LCL', 'Obs', 'LC'), title = 'Lineas',
       col = c('#ff6f61', '#423f32', '#6b8e8e'), lty = c('dashed', 'solid', 'solid'), 
       lwd = c(1.8, 1.8, 1.8), cex = 0.8, bty = 'n', bg = 'white', text.font = 2,
       text.col = '#423f32')


# b) Transfórmense los datos usando la raíz 0.25 y diséñese una carta de control 
#    apropiada para la situación.







