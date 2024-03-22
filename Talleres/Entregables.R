################################################################################
# Script ACTIVIDAD EN CLASE - CONTROL ESTADISTICO DE CALIDAD
# Autor: Cesar Augusto Prieto Sarmiento - Cristian Camilo Prieto Zambrano
# Fecha de Creación: 13/03/2021
# Descripción: Se realizaron los ejercicios solicitados por el profesor
# Versión: 1.1
################################################################################

# LIBRERIAS USADAS
library(gridExtra)
library(readxl)
library(ggplot2)
library(nortest)
library(sqldf)
library(qcr)

setwd("~/DOCUMENTOS PERSONAJES/CESAR/Control/Talleres")

#################################################################################
################################ CODIGO PUNTO 1 #################################


Datos6A <- read_excel("Datos6A.xlsx")

# Definir el nivel de confianza
alpha <- 0.05  # Supongamos un nivel de confianza del 95%

calcular_Sbarra <- function(ni, si) {
  m <- length(ni)
  numerador <- sum((ni - 1) * si^2)
  denominador <- sum(ni) - m
  Sbarra <- sqrt(numerador / denominador)
  return(Sbarra)
}

# Calcular Sbarra
Sbarra <- calcular_Sbarra(Datos6A$N, Datos6A$DESV)

# Mostrar el valor de Sbarra
cat("Sbarra calculado:", Sbarra, "\n")

# Calcular LS y LI para cada fila
LS <- (Sbarra / Datos6A$C4) * sqrt(qchisq(1 - alpha/2, Datos6A$N - 1) / (Datos6A$N - 1))
LI <- (Sbarra / Datos6A$C4) * sqrt(qchisq(alpha/2, Datos6A$N - 1) / (Datos6A$N - 1))

# Agregar las columnas de LS y LI a la tabla original
Datos6A$LS <- LS
Datos6A$LI <- LI

Datos6A

# Crear una columna para identificar puntos fuera de los límites de control
Datos6A$MUESTRA <- ifelse(Datos6A$DESV > Datos6A$LS | Datos6A$DESV < Datos6A$LI, "FUERA","DENTRO")

# Graficar la carta de control con los puntos coloreados según estén dentro o fuera de los límites
ggplot(data = Datos6A, aes(x = seq_along(DESV), y = DESV)) +
  geom_point(aes(color = MUESTRA)) +  # Colorear puntos según estén dentro o fuera de límites
  geom_line() +
  geom_line(aes(y = LS), linetype = "dashed", color = "red") +
  geom_line(aes(y = LI), linetype = "dashed", color = "red") +
  labs(title = "Carta de Control para Sbarra Sin Estandarizar", x = "Secuencia de Muestreo", y = "Desviación Estándar") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("black", "red"))  # Definir colores para dentro y fuera de límites

#####

# Calcular la media y la desviación estándar de la desviación estándar
media_DESV <- mean(Datos6A$DESV, na.rm = TRUE)
desv_std_DESV <- sd(Datos6A$DESV, na.rm = TRUE)

# Estandarizar la desviación estándar
Datos6A$DESV_est1 <- (Datos6A$DESV - Sbarra)/((Sbarra/Datos6A$C4)*sqrt(1 - (Datos6A$C4)^2))

Datos6A$DESV_est2 <- (sqrt(Datos6A$N)*((Datos6A$DESV - Sbarra)/(Sbarra/Datos6A$C4)))

# Calcular la media de las desviaciones estándar estandarizadas
media_DESV_est1 <- calcular_Sbarra(Datos6A$N, Datos6A$DESV_est1)

media_DESV_est2 <- calcular_Sbarra(Datos6A$N, Datos6A$DESV_est2)

# Calcular los límites de control utilizando la metodología dada
LS1 <- (media_DESV_est1 / Datos6A$C4) * sqrt(qchisq(1 - alpha/2, Datos6A$N - 1) / (Datos6A$N - 1))
LI1 <- (media_DESV_est1 / Datos6A$C4) * sqrt(qchisq(alpha/2, Datos6A$N - 1) / (Datos6A$N - 1))

LS2 <- (media_DESV_est2 / Datos6A$C4) * sqrt(qchisq(1 - alpha/2, Datos6A$N - 1) / (Datos6A$N - 1))
LI2 <- (media_DESV_est2 / Datos6A$C4) * sqrt(qchisq(alpha/2, Datos6A$N - 1) / (Datos6A$N - 1))

# Agregar los límites de control a la tabla
Datos6A$LS1 <- LS1
Datos6A$LI1 <- LI1


Datos6A$LS2 <- LS2
Datos6A$LI2 <- LI2

# Crear una columna para identificar puntos fuera de los límites de control
Datos6A$MUESTRA <- ifelse(Datos6A$DESV_est1 > (LS - 4) | Datos6A$DESV_est1 < (LI - 2.5), "FUERA","DENTRO")

# Graficar la carta de control con los puntos coloreados según estén dentro o fuera de los límites
ggplot(data = Datos6A, aes(x = seq_along(DESV_est1), y = DESV_est1)) +
  geom_point(aes(color = MUESTRA)) +  # Colorear puntos según estén dentro o fuera de límites
  geom_line() +
  geom_line(aes(y = 0), linetype = "dashed", color = "red") +
  geom_line(aes(y = LS - 4), linetype = "dashed", color = "red") +
  geom_line(aes(y = LI - 2.5), linetype = "dashed", color = "red") +
  labs(title = "Carta de Control para Sbarra Estandar 1 ", x = "Secuencia de Muestreo", y = "Desviación Estándar Estandarizada") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("black", "red"))  # Definir colores para dentro y fuera de límites

# Crear una columna para identificar puntos fuera de los límites de control
Datos6A$MUESTRA <- ifelse(Datos6A$DESV_est2 > (LS - 4.5) | Datos6A$DESV_est2 < (LI - 2.5), "FUERA","DENTRO")

# Graficar la carta de control con los puntos coloreados según estén dentro o fuera de los límites
ggplot(data = Datos6A, aes(x = seq_along(DESV_est2), y = DESV_est2)) +
  geom_point(aes(color = MUESTRA)) +  # Colorear puntos según estén dentro o fuera de límites
  geom_line() +
  geom_line(aes(y = 0), linetype = "dashed", color = "red") +
  geom_line(aes(y = LS - 4.5), linetype = "dashed", color = "red") +
  geom_line(aes(y = LI - 2.5), linetype = "dashed", color = "red") +
  labs(title = "Carta de Control para Sbarra Estandar 2", x = "Secuencia de Muestreo", y = "Desviación Estándar Estandarizada") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("black", "red"))  # Definir colores para dentro y fuera de límites

#######################################

Datos6 <- read_excel("Datos6.xlsx", col_types = c("skip","numeric", "numeric",
                                                  "numeric", "numeric", "numeric"))
T1 <- scale(Datos6)
P <- as.data.frame(T1) 

P$Des <- apply(P,1,sd, na.rm = TRUE)
P$Med <- apply(P,1,mean, na.rm = TRUE)
P$N <- apply(P[,c(1:5)], 1, function(x) sum(!is.na(x)))

calcular_Sbarra <- function(ni, si) {
  m <- length(ni)
  numerador <- sum((ni - 1) * si^2)
  denominador <- sum(ni) - m
  Sbarra <- sqrt(numerador / denominador)
  return(Sbarra)
}

# Calcular Sbarra
Sbarra <- calcular_Sbarra(P$N, P$Des)

# Mostrar el valor de Sbarra
cat("Sbarra calculado:", Sbarra, "\n")

P$C4 <- apply(P, 1, function(x) (4*(x["N"] - 1)) / (4*x["N"] - 3))

alpha <- 0.05

# Calcular LS y LI para cada fila
LS <- (Sbarra / P$C4) * sqrt(qchisq(1 - alpha/2, P$N - 1) / (P$N - 1))
LI <- (Sbarra / P$C4) * sqrt(qchisq(alpha/2, P$N - 1) / (P$N - 1))

# Agregar las columnas de LS y LI a la tabla original
P$LS <- LS
P$LI <- LI

P

# Crear una columna para identificar puntos fuera de los límites de control
P$MUESTRAS <- ifelse(P$Des > P$LS | P$Des < P$LI, "FUERA", "DENTRO")

# Graficar la carta de control para Sbarra con límites móviles
ggplot(data = P, aes(x = seq_along(Des), y = Des)) +
  geom_point(aes(color = MUESTRAS), size = 2.5) +  # Agregar color según si está fuera de límites
  geom_line(color = '#03396c') +
  geom_line(aes(y = LS), linetype = 'dashed', color = 'black', lwd = 0.7) +
  geom_line(aes(y = LI), linetype = 'dashed', color = 'black', lwd = 0.7) +
  ggtitle('Carta de Control para Sbarra Estandar') +
  ylab('Desviación Estándar') +
  xlab('Secuencia de Muestreo') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("black", "red"))  # Definir colores para dentro y fuera de límites

## SOLO ILUSTRATIVO COMO SE VE LA CARTA X BARRA


# Calcular XBarra Barra
XBarra_Barra <- sum(P$N * P$Med) / sum(P$N)

# Calcular A_3
A_3 <- 3 / (P$C4 * sqrt(P$N))

# Calcular LS y LI para Xbarra
LS_Xbarra <- XBarra_Barra + A_3 * Sbarra
LI_Xbarra <- XBarra_Barra - A_3 * Sbarra

# Graficar la carta de control para Xbarra con límites móviles
ggplot(data = P, aes(x = seq_along(Med), y = Med)) +
  geom_point(aes(color = ifelse(Med > LS_Xbarra | Med < LI_Xbarra, "FUERA", "DENTRO")), size = 2.5) +  # Agregar color según si está fuera de límites
  geom_line(color = '#03396c') +
  geom_line(aes(y = XBarra_Barra), linetype = 'dashed', color = 'black', lwd = 0.7) +
  geom_line(aes(y = LS_Xbarra), linetype = 'dashed', color = 'black', lwd = 0.7) +
  geom_line(aes(y = LI_Xbarra), linetype = 'dashed', color = 'black', lwd = 0.7) +
  ggtitle('Carta de Control para Xbarra') +
  ylab('Desviación Estándar') +
  xlab('Secuencia de Muestreo') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +  # Quitar la leyenda
  scale_color_manual(values = c("black", "red"))  # Definir colores para dentro y fuera de límites

#### Ahora sin tener en cuenta las muestras que se salen de control en S 

# Filtrar el dataframe P para incluir solo las muestras dentro de los límites de control
P <- P[P$MUESTRAS == "DENTRO", ]

# Calcular Sbarra
Sbarra <- calcular_Sbarra(P$N, P$Des)

# Calcular LS y LI para cada fila
LS <- (Sbarra / P$C4) * sqrt(qchisq(1 - alpha/2, P$N - 1) / (P$N - 1))
LI <- (Sbarra / P$C4) * sqrt(qchisq(alpha/2, P$N - 1) / (P$N - 1))

# Agregar las columnas de LS y LI a la tabla original
P$LS <- LS
P$LI <- LI

# Graficar la carta de control para Sbarra con límites móviles
ggplot(data = P, aes(x = seq_along(Des), y = Des)) +
  geom_point(size = 2.5) +  # No es necesario colorear los puntos nuevamente
  geom_line(color = '#03396c') +
  geom_line(aes(y = LS), linetype = 'dashed', color = 'black', lwd = 0.7) +
  geom_line(aes(y = LI), linetype = 'dashed', color = 'black', lwd = 0.7) +
  ggtitle('Carta de Control para Sbarra Estandar') +
  ylab('Desviación Estándar') +
  xlab('Secuencia de Muestreo') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calcular XBarra Barra
XBarra_Barra <- sum(P$N * P$Med) / sum(P$N)

# Calcular A_3
A_3 <- 3 / (P$C4 * sqrt(P$N))

# Calcular LS y LI para Xbarra
LS_Xbarra <- XBarra_Barra + A_3 * Sbarra
LI_Xbarra <- XBarra_Barra - A_3 * Sbarra

# Graficar la carta de control para Xbarra con límites móviles
ggplot(data = P, aes(x = seq_along(Med), y = Med)) +
  geom_point(size = 2.5) +  # No es necesario colorear los puntos nuevamente
  geom_line(color = '#03396c') +
  geom_line(aes(y = XBarra_Barra), linetype = 'dashed', color = 'black', lwd = 0.7) +
  geom_line(aes(y = LS_Xbarra), linetype = 'dashed', color = 'black', lwd = 0.7) +
  geom_line(aes(y = LI_Xbarra), linetype = 'dashed', color = 'black', lwd = 0.7) +
  ggtitle('Carta de Control para Xbarra') +
  ylab('Desviación Estándar') +
  xlab('Secuencia de Muestreo') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")  # Quitar la leyenda

#################################################################################
################################ CODIGO PUNTO 2 #################################


# Cargar datos
Datos7 <- readxl::read_excel("Datos7.xlsx")
nuevos_nombres <- c("MUESTRA","x1","x2","x3","x4","x5")
names(Datos7) <- nuevos_nombres

### PUNTO 2 ----

# Calcular medias y desviaciones estándar
AA1 <- sqldf::sqldf("SELECT MUESTRA, (x1 + x2 + x3 + x4 + x5) / 5 AS Medias,
                            sqrt(avg(POWER(x1 - (x1 + x2 + x3 + x4 + x5) / 5, 2) +
                                     POWER(x2 - (x1 + x2 + x3 + x4 + x5) / 5, 2) +
                                     POWER(x3 - (x1 + x2 + x3 + x4 + x5) / 5, 2) +
                                     POWER(x4 - (x1 + x2 + x3 + x4 + x5) / 5, 2) +
                                     POWER(x5 - (x1 + x2 + x3 + x4 + x5) / 5, 2))) AS Desviaciones
                     FROM Datos7 
                     GROUP BY MUESTRA")


# Histograma
ggplot(data = AA1, aes(x = Medias)) +
  geom_histogram(binwidth = 1, fill = "#03396c", color = "#011f4b", 
                 aes(y = ..density..), alpha = 0.7) +
  geom_density(color = "#005b96") +
  labs(title = "Histograma de Medias", x = "Medias", y = "Densidad") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Q-Q Plot
qqnorm(AA1$Medias)
qqline(AA1$Medias, distribution = qnorm, probs = c(0.25, 0.75), col = "red")
title("Normal Q-Q Plot")

pearson.test(AA1$Medias)

# Calcular las constantes C4, B3 y B4
n <- 5  # Tamaño del subgrupo
C4 <- (4*(n - 1))/(4*n - 3)
B3 <- 1 - 3/(C4 * sqrt(2*(n - 1)))
B4 <- 1 + 3/(C4 * sqrt(2*(n - 1)))

# Calcular los límites de control para la carta S
Sbar <- mean(AA1$Desviaciones)
UCL_S <- B4 * Sbar
LCL_S <- B3 * Sbar

# Graficar la carta de control para la dispersión (carta S)
ggplot(data = AA1, aes(x = MUESTRA, y = Desviaciones)) +
  geom_point(color = 'black', size = 2.5) +  # Ajustar el color y el tamaño de los puntos
  geom_line(color = '#03396c') +  # Ajustar el color de la línea
  geom_hline(yintercept = c(UCL_S, LCL_S, Sbar), linetype = "dashed", color = "black") +
  labs(title = "Carta de Control para la Dispersión (carta S)", x = "Secuencia de Muestreo", y = "Desviación Estándar") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# Cálculos para la carta de control de X_barra
# Calcular la media de cada subgrupo
medias_subgrupos <- apply(Datos7[, -1], 1, mean)

# Calcular la media de esas medias
Xbar <- mean(medias_subgrupos)

A2 <- 0.577
UCL_Xbar <- Xbar + A2 * Sbar
LCL_Xbar <- Xbar - A2 * Sbar

# Verificar si los puntos están dentro de los límites de control para X_barra
AA1$MUESTRAS <- ifelse(AA1$Medias > UCL_Xbar | AA1$Medias < LCL_Xbar, "Fuera de Control", "En Control")

# Graficar la carta de control para X_barra
ggplot(data = AA1, aes(x = MUESTRA, y = Medias)) +
  geom_point(aes(color = MUESTRAS), size = 2.5) +  # Ajustar el color y el tamaño de los puntos
  geom_line(color = '#03396c') +  # Ajustar el color de la línea
  geom_hline(yintercept = c(UCL_Xbar, LCL_Xbar, Xbar), linetype = "dashed", color = 'black') +
  labs(title = "Carta de Control para la Media (X_barra)", x = "Secuencia de Muestreo", y = "Media de la Muestra") +
  scale_color_manual(values = c("black", "red"), labels = c("En Control", "Fuera de \n Control")) +  # Ajustar los colores
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# Resumen de resultados
cat("Carta de Control para la Dispersión (S):\n")
cat("  - Límite superior de control (UCL):", UCL_S, "\n")
cat("  - Límite inferior de control (LCL):", LCL_S, "\n")

cat("Carta de Control para la Media (X_barra):\n")
cat("  - Límite superior de control (UCL):", UCL_Xbar, "\n")
cat("  - Límite inferior de control (LCL):", LCL_Xbar, "\n")

#### PUNTO 2 ---------------

# Definir función de log-verosimilitud
log_likelihood <- function(tau, medias, mu0, sigma0, n) {
  # Calcular la suma de cuadrados de las diferencias
  sum_sq_diff <- sum((medias[1:tau] - mu0)^2) + sum((medias[(tau+1):length(medias)] - mu0 - delta * sigma0)^2)
  # Calcular la log-verosimilitud
  log_lik <- -(n / (2 * sigma0^2)) * sum_sq_diff
  return(log_lik)
}

# Inicializar variables
mu0 <- mean(AA1$Medias)  # Media inicial
sigma0 <- sd(AA1$Medias)  # Desviación estándar inicial
n <- 5  # Tamaño de los subgrupos
delta <- 1  # Multiplicador de la desviación estándar para el cambio de media

# Buscar el valor de tau que maximiza la log-verosimilitud
likelihoods <- sapply(1:(nrow(AA1) - 1), function(tau) log_likelihood(tau, AA1$Medias, mu0, sigma0, n))
tau_hat_MLE <- which.max(likelihoods)

# Calcular la nueva media estimada
mu1_hat <- mean(AA1$Medias[(tau_hat_MLE + 1):nrow(AA1)])

# Mostrar resultados
cat("Estimación de tau (MLE):", tau_hat_MLE, "\n")
cat("Estimación de mu1 (MLE):", mu1_hat, "\n")

# Crear dataframe para ggplot
df_likelihood <- data.frame(tau = 1:(nrow(AA1) - 1), log_likelihood = likelihoods)

# Graficar log-verosimilitud vs. tau con ggplot
ggplot(df_likelihood, aes(x = tau, y = log_likelihood)) +
  geom_line(color = "#0072B2") +
  labs(x = expression(tau), y = "Log-verosimilitud") +
  theme_minimal() +
  ggtitle("Log-verosimilitud vs. Tau") +
  theme(plot.title = element_text(hjust = 0.5))

# Crear dataframe para ggplot
df_medias <- data.frame(Muestra = seq_along(AA1$Medias), Media = AA1$Medias)

# Graficar medias y punto de cambio estimado con ggplot
ggplot(df_medias, aes(x = Muestra, y = Media)) +
  geom_line(color = "#009E73") +
  geom_vline(xintercept = tau_hat_MLE, linetype = "dashed", color = "#D55E00") +
  labs(x = "Muestra", y = "Media", title = "Medias y Punto de Cambio Estimado") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

##### CARTA X BARRA SIN MUESTRA FUERA DE CONTROL 

# Calcular la media de cada subgrupo
medias_subgrupos <- apply(Datos7[, -1], 1, mean)

# Calcular la media de esas medias
Xbar <- mean(medias_subgrupos)

A2 <- 0.577
UCL_Xbar <- Xbar + A2 * Sbar
LCL_Xbar <- Xbar - A2 * Sbar

# Verificar si los puntos están dentro de los límites de control para X_barra
AA1$MUESTRAS <- ifelse(AA1$Medias > UCL_Xbar | AA1$Medias < LCL_Xbar, "Fuera de Control", "En Control")

# Filtrar las muestras que están "En Control"
AA1_filtrado <- AA1[AA1$MUESTRAS == "En Control", ]

# Graficar la carta de control para X_barra después de eliminar muestras fuera de control
ggplot(data = AA1_filtrado, aes(x = MUESTRA, y = Medias)) +
  geom_point(size = 2.5, aes(color = MUESTRAS)) +  
  geom_line(color = '#03396c') +  
  geom_hline(yintercept = c(UCL_Xbar, LCL_Xbar, Xbar), linetype = "dashed", color = 'black') +
  labs(title = "Carta de Control para la Media (X_barra)", x = "Secuencia de Muestreo", y = "Media de la Muestra") +
  scale_color_manual(values = c("black", "red"), labels = c("En Control", "Fuera de Control")) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
