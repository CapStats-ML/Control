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

# Comprobemos 4 con corrimientos de 0.025 hacia arriba y hacia abajo

P0_i <- seq(0.025, 0.2, 0.025)

# Generamos una muestra para cada P0_i 

Muestra <- matrix(0, ncol = length(P0_i), nrow = m)

for (i in 1:length(P0_i)){
  Muestra[, i] <- rbinom(n = m, size = tamaños, prob = P0_i[i])
  Muestra[, i] <- Muestra[, i] / tamaños
}

# Generamos la Matriz de Muestras estandarisadas dependiendo solo teniendo en cuenta p0

Zi_values <- matrix(0, ncol = length(P0_i), nrow = m)

for (i in 1:length(P0_i)){
  Zi_values[, i] <- StanCC(tamaños, Muestra[, i], p0)
}

# Graficando las cartas de control 

par(mfrow = c(2, 2), mar = c(5.1, 5.1, 4.1, 2.1), bg = '#fffbf7') # Configurar márgenes y fondo

for (i in 1:length(P0_i)){
  plot(Zi_values[, i], type = 'o', col = '#423f32', pch = 20, cex = 1.5, ylim = c( min(Zi_values[, i]- 3, -3) , max(Zi_values[, i] + 1, 3)), family = "sans", bty = "L",
       xlab = 'Tiempo de medición', ylab = 'Valor de Zi para \n Prop. No Conformidad',  font.lab = 2, cex.lab = 0.8,
       font = 3, font.main = 4, main = paste('Carta de control Z con límites variables para p =', P0_i[i]), cex.axis = 0.7, col.main = 'black',
       sub = paste('Proporción de no conformidad p =', P0_i[i]), font.sub = 2, cex.sub = 0.8, fg = '#423f32', col.sub = '#ff6f61')
  
  # Líneas de guía
  abline(h = seq(-3, 3, 0.5), col = '#d6c6b8', lty = 'dotted')
  abline(v = seq(1, m, 5), col = '#d6c6b8', lty = 'dotted')
  
  # Líneas de control superior e inferior
  abline(h = UCL, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
  abline(h = LCL, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
  
  # Línea central (valor nominal de Z = 0)
  abline(h = 0, col = '#6b8e8e', lwd = 1.2)
}

RunLenghtpVar = function(n = seq(200,400,50), p = 0.1, SimularPorVez = 20000, 
                         L = 3, m = 50000, corrimiento = 0.1){
  UCL = L        # Límites superiores
  LCL = -L        # Límites inferiores
  RL = c()
  pb = txtProgressBar(min = 0, max = m, style = 3)
  longitud = 1
  while(length(RL) < m){
    tamaños = sample(x = n, size = SimularPorVez,   # Se generan 100 muestras de los 
                     replace = T)                   # posibles tamaños sin reemplazo
    muestras = rbinom(n = SimularPorVez, size = tamaños, 
                      prob = corrimiento)
    proporcion = muestras/tamaños
    proporciones = StanCC(tamaños, proporcion, p)
    for (i in 1:SimularPorVez){
      if (proporciones[i] < UCL & proporciones[i] > LCL){
        longitud = longitud + 1
      } else {
        RL = c(RL, longitud)
        longitud = 1
        setTxtProgressBar(pb, value = length(RL))
      }
    }
  }
  return(RL)
}

corrimientos = seq(0.025, 0.2, by = 0.025)
ARL = numeric(length(corrimientos))

for (i in 1:length(ARL)){
  ARL[i] = mean(RunLenghtpVar(corrimiento = corrimientos[i]))
}


# Para obtener un ARL de 374 toca mirar p = 0.5 con corrimientos al rededor de este 

StanCC <- function(n, P, p0){
  Zi <- (P - p0) / sqrt(p0 * (1 - p0) / n)
  return(Zi)
}

RunLenghtpVar = function(n = seq(200,400,50), p = 0.5, SimularPorVez = 25000, 
                         L = 3, m = 50000, corrimiento = 0.1){
  # Límites de control estandarizados
  UCL = L
  LCL = -L
  RL = c()
  pb = txtProgressBar(min = 0, max = m, style = 3)
  longitud = 1
  
  while(length(RL) < m){
    # Generación de tamaños de muestra y proporciones
    tamaños = sample(x = n, size = SimularPorVez, replace = TRUE)
    muestras = rbinom(n = SimularPorVez, size = tamaños, prob = corrimiento)
    proporcion = muestras / tamaños
    
    # Estandarización
    proporciones = StanCC(tamaños, proporcion, p)
    
    # Evaluación de las proporciones estandarizadas con respecto a los límites
    for (i in 1:SimularPorVez){
      if (proporciones[i] < UCL & proporciones[i] > LCL){
        longitud = longitud + 1
      } else {
        RL = c(RL, longitud)
        longitud = 1
        setTxtProgressBar(pb, value = length(RL))
      }
    }
  }
  return(RL)
}

# Simulación con diferentes corrimientos
corrimientos = seq(0.4, 0.6, by = 0.025)
ARL = numeric(length(corrimientos))

set.seed(123)
for (i in 1:length(corrimientos)){
  RL = RunLenghtpVar(corrimiento = corrimientos[i])  # Simula el Run Length
  ARL[i] = mean(RL)  # Calcula el ARL como el promedio de los RL simulados
}

print(ARL)



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

Periodo1 <- read.csv("~/REPOS GIT/Control/Entregas/Entrega 2/Periodo1.txt", 
                     colClasses = c("character", "numeric", "numeric"), 
                     col.names = c("Mes", "Dias", "DaysBetween"))

# Quitar NA's 
drop_na <- function(df){
  df <- df[complete.cases(df), ]
  return(df)
}

sum(is.na(Periodo1$Dias))
sum(is.na(Periodo1$DaysBetween))


### Carta C para el numero de homicidios por mes ----

NH <- numeric(12)

# Agrupar y contar por mes el número de homicidios

names <- c("Jan", "Feb", "March", "April", "May", "June",
           "July", "Aug", "Sep", "Oct", "Nov", "Dec")

for (i in 1:12){
  NH[i] <- sum(Periodo1$Mes == names[i])
}

C_barra <- sum(NH)/length(NH)

UCL <- C_barra + 3*sqrt(C_barra)
CL <- C_barra
LCL <- C_barra - 3*sqrt(C_barra)

# Gráfico de la carta C

par(mfrow = c(1,1), mar = c(5.1, 5.1, 4.1, 2.1), bg = '#fffbf7') # Configurar márgenes y fondo

plot(NH, type = 'o', col = '#423f32', pch = 20, cex = 1.5, ylim = c(0, UCL + 1), family = "sans", bty = "L",
     xlab = 'Mes', ylab = 'Número de homicidios',  font.lab = 2, cex.lab = 0.8,
     font = 3, font.main = 4, main = 'Carta de control C', cex.axis = 0.7, col.main = 'black',
     sub = 'Número de homicidios por mes', font.sub = 2, cex.sub = 0.8, fg = '#423f32', col.sub = '#ff6f61')

# Líneas de guía

abline(h = seq(0, UCL, 1), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(1, 12, 1), col = '#d6c6b8', lty = 'dotted')

# Líneas de control superior e inferior

abline(h = UCL, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
abline(h = CL, col = '#6b8e8e', lty = 'dashed', lwd = 1.8)
abline(h = 0, col = '#ff6f61', lty = 'dashed', lwd = 1.8)

# Leyenda 

legend(x = 9, y = 7 , legend = c('UCL & LCL', 'Obs', 'LC'), title = 'Lineas',
       col = c('#ff6f61', '#423f32', '#6b8e8e'), lty = c('dashed', 'solid', 'solid'), 
       lwd = c(1.8, 1.8, 1.8), cex = 0.7, bty = 'n', bg = 'white', text.font = 2,
       text.col = '#423f32')

# Carta con limites exponenciales  --------

#Con estos datos podemos hacer una carta con límites exponenciales y
#una carta con límites poisson agrupando por meses


#Carta con límites de probabilidad exponenciales

#Empezamos por estimar p
p_hat <- 1/(mean(as.numeric(Periodo1[,3]), na.rm = TRUE))

#De momento podemos tomar que los fechas siguen una distribución exp
#de parametro p_hat, seguímos por establecer límites de control.


#Límite inferior: la cola inferior de la dist. exp. que acomula 0.0027/2

LCL_1 <- qexp(0.00135, rate = p_hat)

#Límite superior: la cola superior que acumula el 0.0027/2

UCL_1 <- qexp(0.00135, rate = p_hat, lower.tail = FALSE)

CL <- qexp(0.5, rate = p_hat)

franja_m2 <- qexp(0.022, rate = p_hat)
franja_m1 <- qexp(0.15, rate = p_hat)

franja_p1 <- qexp(0.84, rate = p_hat)
franja_p2 <- qexp(0.97, rate = p_hat)



# Ahora para graficar la carta 

par(mar = c(5.1, 5.1, 4.1, 2.1), bg = '#fffbf7') # Configurar márgenes y fondo

plot(Periodo1$DaysBetween, type = 'o', col = '#423f32', pch = 20, cex = 1.5, ylim = c(0, UCL_1 + 1), family = "sans", bty = "L",
     xlab = 'Observación', ylab = 'Días entre homicidios',  font.lab = 2, cex.lab = 0.8,
     font = 3, font.main = 4, main = 'Carta de control con límites exponenciales', cex.axis = 0.7, col.main = 'black',
     sub = 'Días entre homicidios', font.sub = 2, cex.sub = 0.8, fg = '#423f32', col.sub = '#ff6f61')

# Líneas de guía

abline(h = seq(0, UCL_1, 5), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(0, 30, 5), col = '#d6c6b8', lty = 'dotted')

# Líneas de control superior e inferior

abline(h = UCL_1, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
abline(h = franja_p2, col = '#FAC30E', lty = 'dashed', lwd = 1.8)
abline(h = franja_p1, col = '#FAEB0E', lty = 'dashed', lwd = 1.8)

abline(h = LCL_1, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
abline(h = franja_m2, col = '#FAC30E', lty = 'dashed', lwd = 1.8)
abline(h = franja_m1, col = '#FAEB0E', lty = 'dashed', lwd = 1.8)

# Línea central (valor nominal de Z = 0)

abline(h = CL, col = '#6b8e8e', lwd = 1.8)

# Leyenda

legend(x = 20, y = 75 , legend = c('UCL & LCL', 'Obs', 'LC'), title = 'Lineas',
       col = c('#ff6f61', '#423f32', '#6b8e8e'), lty = c('dashed', 'solid', 'solid'), 
       lwd = c(1.8, 1.8, 1.8), cex = 0.7, bty = 'n', bg = 'white', text.font = 2,
       text.col = '#423f32')


# Problema: La dist. exp. no es simétrica, así que las reglas de sensibilidad
# no funcionan de la misma forma, hallemos los equivalentes a +- 1 y 2 sigma

ARL_1 <- 1/0.0027

# Ahora veamoslo con una construccion con la Poisson


# b) Transfórmense los datos usando la  x = y^(0.25) y diséñese una carta de control 
#    apropiada para la situación.


# Prueba de normalidad con qqplot

# Grafica QQplot
# Configurar márgenes y fondo
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')

# Crear el QQ plot con estilos personalizados
qqnorm(Periodo1$DaysBetween, main = "QQ Plot tiempo entre Asesinatos", col = '#423f32',  pch = 20, cex = 1.5, 
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


# Transformar los datos ----

Trans <- Periodo1$DaysBetween^(0.25)

# Grafica QQplot
# Configurar márgenes y fondo
par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')

# Crear el QQ plot con estilos personalizados
qqnorm(Trans, main = "QQ Plot Periodo Transformado", col = '#423f32',  pch = 20, cex = 1.5, 
       family = "sans", bty = "L", xlab = "Cuantiles teóricos", ylab = "Cuantiles de los datos",
       font.lab = 2, cex.lab = 0.8, font = 3, font.main = 4, col.main = 'black', cex.axis = 0.7)

# Añadir línea de referencia con estilo personalizado
qqline(Trans, col = '#6b8e8e', lwd = 1.2)

# Añadir líneas de guía
abline(h = seq(0, 3.5, 0.25), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(-3, 3, 0.5), col = '#d6c6b8', lty = 'dotted')

# Añadir leyenda
legend(x = 1, y = 1.25, legend = c('Tendencia', 'Observaciones'), col = c('#6b8e8e', '#423f32'), 
       lty = c('solid', 'none'), pch = c(NA, 20), lwd = c(1.2, NA), cex = 0.8, bty = 'n', 
       bg = 'white', text.font = 2, text.col = '#423f32')

# Calcular la media y la desviación estándar

media <- mean(Trans, na.rm = TRUE)
sd <- sd(Trans, na.rm = TRUE)

# Límites de Control
UCL <- media + 3 * sd
LCL <- media - 3 * sd

# Grafiar la carta de control

par(mar = c(5.1, 5.1, 4.1, 2.1), bg = '#fffbf7') # Configurar márgenes y fondo

plot(Trans, type = 'o', col = '#423f32', pch = 20, cex = 1.5, ylim = c(LCL, UCL), family = "sans", bty = "L",
     xlab = 'Observacion Temporal', ylab = 'Tiempo entre Ass \n Tranformadas',  font.lab = 2, cex.lab = 0.8,
     font = 3, font.main = 4, main = 'Carta de control Obs. Transformadas', cex.axis = 0.7, col.main = 'black',
     sub = 'Tiempo entre asesinatos', font.sub = 2, cex.sub = 0.8, fg = '#423f32', col.sub = '#ff6f61')

# Líneas de guía
abline(h = seq(-3, 3, 0.5), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(0, 30, 2.5), col = '#d6c6b8', lty = 'dotted')

# Líneas de control superior e inferior
abline(h = UCL, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
abline(h = LCL, col = '#ff6f61', lty = 'dashed', lwd = 1.8)

# Línea central (valor nominal de Z = 0)
abline(h = media, col = '#6b8e8e', lwd = 1.2)

# Leyenda
legend(x = 20, y = 1 , legend = c('UCL & LCL', 'Obs', 'LC'), title = 'Lineas',
       col = c('#ff6f61', '#423f32', '#6b8e8e'), lty = c('dashed', 'solid', 'solid'), 
       lwd = c(1.8, 1.8, 1.8), cex = 0.8, bty = 'n', bg = 'white', text.font = 2,
       text.col = '#423f32')

# Carta de Rango Movil

k <- 2
rango_movil <- sapply(1:(length(Trans) - k + 1), function(i) max(Trans[i:(i + k - 1)]) - min(Trans[i:(i + k - 1)]))

# Media y desviación estándar de los rangos móviles
media_rango <- mean(rango_movil, na.rm = TRUE)
sd_rango <- sd(rango_movil, na.rm = TRUE)

# Límites de Control para la carta de rango móvil
UCL_rango <- media_rango + 3 * sd_rango
LCL_rango <- media_rango - 3 * sd_rango

# Graficar la carta de rango móvil

par(mar = c(5.1, 5.1, 4.1, 2.1), bg = '#fffbf7') # Configurar márgenes y fondo

plot(rango_movil, type = 'o', col = '#423f32', pch = 20, cex = 1.5, ylim = c(LCL_rango - 1, UCL_rango + 1), family = "sans", bty = "L",
     xlab = 'Observacion Temporal', ylab = 'Rango del tiempo\nEntre Asesinatos',  font.lab = 2, cex.lab = 0.8,
     font = 3, font.main = 4, main = 'Carta de control MR', cex.axis = 0.7, col.main = 'black',
     sub = 'Tiempo entre asesinatos', font.sub = 2, cex.sub = 0.8, fg = '#423f32', col.sub = '#ff6f61')

# Líneas de guía
abline(h = seq(-3, 3, 0.25), col = '#d6c6b8', lty = 'dotted')
abline(v = seq(0, 30, 2.5), col = '#d6c6b8', lty = 'dotted')

# Líneas de control superior e inferior
abline(h = UCL_rango, col = '#ff6f61', lty = 'dashed', lwd = 1.8)
abline(h = LCL_rango, col = '#ff6f61', lty = 'dashed', lwd = 1.8)

# Línea central (valor nominal de Z = 0)
abline(h = media_rango, col = '#6b8e8e', lwd = 1.2)

# Leyenda
legend(x = 20, y = -0.5 , legend = c('UCL & LCL', 'Obs', 'LC'), title = 'Lineas',
       col = c('#ff6f61', '#423f32', '#6b8e8e'), lty = c('dashed', 'solid', 'solid'), 
       lwd = c(1.8, 1.8, 1.8), cex = 0.8, bty = 'n', bg = 'white', text.font = 2,
       text.col = '#423f32')


