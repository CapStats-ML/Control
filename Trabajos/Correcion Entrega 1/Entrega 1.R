################################################################################
# Script Correcion Entrega 1: Control Estadistico de Calidad
# Autores: 
# - Cesar Augusto Prieto Sarmiento
# - Cristian Camilo Prieto Zambrano
# - Daniel Santiago Guzman 
# Fecha de Creación: 12/05/2024
# Ultima Fecha de Mod: 23/07/2024
# Descripción: Se plantea realizar la correccion del taller 1 de la asignatura 
#           teniendo en cuenta para esto la correcciones hechas por el profesor
#           y las nuevas habilidades adquiridas en el curso.
# Versión: 1.5
################################################################################

setwd("~/En Proceso/Control")
library(reshape2)
library(ggplot2)

# PUNTO 1 ---- Codigo realizado por Michael Meldivenson (https://github.com/Mendivenson)
# Sea $X \sim N(\mu, \sigma)$ una característica de calidad. Mediante simulaciones, 
# establezca el comportamiento del ARL (en control y fuera de él) de las Cartas $R$ y 
# $S$ para observaciones normales con límites $3 \sigma$ y muestras de tamaño (a) $n=3$ 
# y (b) $n=10$. ¿Qué regularidades observa?

RunLengthR_optimized = function(mu = 0, sigma = 1, k= 0,n = 3, m = 1000){
  #matrixStats: Librería para operaciones rowDiff y rowRange más eficientes (apply es muy lento)
  # Constantes d3 y d2 para carta R
  d3 = c(0.853, 0.888, 0.880, 0.864, 0.848, 0.833, 0.820, 0.808, 0.797, 0.787, 0.778, 0.770,
         0.763, 0.756, 0.750, 0.744, 0.739, 0.734, 0.729, 0.724, 0.72, 0.716, 0.712,0.708)
  d2 = c(1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 
         3.407, 3.472, 3.532, 3.588, 3.640, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931)
  
  d3 = d3[n-1]; d2 = d2[n-1]    # Selección de constantes específicas dado el tamaño de muestra
  UCL = (d2 + 3 * d3) * sigma;  # Límite superior para carta R
  LCL = (d2 - 3 * d3) * sigma   # Límite inferior para carta R
  
  RL = c();                     # Vector para guardar las longitudes de corrida
  last_count = 0                # Z longitudes de corrida sin señal al final de la matriz
  
  while(length(RL) < m){
    # Se generan m*100 subgrupos racionales, se toma el rango y la diferencia
    rangos = matrixStats::rowDiffs(matrixStats::colRanges(matrix(rnorm(n*m*100, 
                                                                       mean = mu,
                                                                       sd = sigma * (1 - k)),
                                                                 nrow = n, byrow = F)))
    # ¿Qué índices se salieron de control?
    OutControl = which(rangos < LCL | rangos > UCL)
    # ¿Cuánto tiempo hay entre estos índices?
    OutControl = diff(c(0, OutControl))
    # Se suman al primer RL la cantidad de subgrupos racionales sin señal 
    # del final del ciclo anterior
    OutControl[1] = OutControl[1] + last_count
    # Se actualiza a z subgrupos racionales al final de este ciclo sin dar señal
    last_count = length(rangos) - sum(OutControl)
    
    # Se agregan los resultados al final del vector de longitudes de corrida
    RL = c(RL,OutControl)}
  return(RL[1:m])}

RunLengthS_optimized = function(mu = 0, sigma = 1, k = 0,n = 3, m = 1000){
  # Constante c4 para corrección de sesgo
  c4 = sqrt(2/(n-1)) * gamma(n/2) / gamma((n-1)/2)
  LCL = sigma * (c4 - 3 * sqrt(1 - c4**2))
  UCL = sigma * (c4 + 3 * sqrt(1 - c4**2))
  
  RL= c()
  last_count = 0
  while (length(RL) < m){
    desviaciones = matrixStats::colSds(matrix(rnorm(n*m*100, 
                                                    mean = mu,
                                                    sd = sigma * (1 - k)),
                                              nrow = n, byrow = F))
    OutControl = which(desviaciones < LCL | desviaciones > UCL)
    OutControl = diff(c(0, OutControl))
    OutControl[1] = OutControl[1] + last_count
    last_count = length(desviaciones) - sum(OutControl)
    RL = c(RL,OutControl)}
  return(RL[1:m])}


# PUNTO 2 ----
# Sea X ~ N(mu, sigma) una característica de calidad de un producto. Se sabe que
# los valores objetivos de los parámetros del proceso son mu = mu_0 y sigma = sigma_0.
# Se debe construir las curvas OC de la carta S^2 con limites de probabilidad. 

# Definición de la función OC_S2

OC_S2 <- function(n, sigma2_0, delta){
  # Cálculo de los límites de control superior e inferior para la carta S^2
  UCL <- sigma2_0/(n-1) * qchisq(1 - 0.05/2, df = n-1)
  LCL <- sigma2_0/(n-1) * qchisq(0.05/2, df = n-1)
  
  # Cálculo del poder de detección (beta)
  beta <- pchisq((n-1)*UCL/(sigma2_0 + delta), df = n-1) - pchisq((n-1)*LCL/(sigma2_0 + delta), df = n-1)
  
  return(beta)
}

# Parámetros
sigma2_0 <- 1
deltas <- seq(0.1, 8, by = 0.1) # Valores de delta
n_values <- seq(3, 42, by = 3) # Tamaños de muestra de 3 en 3

# Colores para las curvas (4 colores diferentes)
colors <- c("orange", "red", "purple", "blue", "cyan")

# Inicializar la gráfica
plot(NULL, xlim = c(min(deltas), max(deltas)), ylim = c(0, 1),
     xlab = "Corrimiento (Delta)", ylab = "Poder de Detección (Beta)",
     main = "Curvas OC para la carta S^2",)


# Graficar las curvas en grupos de 4 colores
for (i in seq_along(n_values)) {
  n <- n_values[i]
  beta_values <- sapply(deltas, function(delta) OC_S2(n, sigma2_0, delta))
  
  # Determinar el grupo de color
  color_index <- (i - 1) %/% 3 + 1
  lines(deltas, beta_values, type = "l", col = colors[color_index], lwd = 1.2)
}

# Crear leyenda estructurada
legend_text <- c("Tam de muestra:", 
                 paste(paste("n =", paste(n_values[1:3], collapse = ", "))),
                 paste(paste("n =", paste(n_values[4:6], collapse = ", "))),
                 paste(paste("n =", paste(n_values[7:9], collapse = ", "))),
                 paste(paste("n =", paste(n_values[10:12], collapse = ", "))),
                 paste(paste("n =", paste(n_values[13:14], collapse = ", "))))

legend("topright", 
       legend = legend_text,
       col = c("black", colors),
       lty = c(0, rep(1, 5)),
       lwd = c(0, rep(1.2, 5)),
       bty = "n",
       cex = 0.6)

# Añadir una nota sobre los valores
mtext("Nota: Cada color representa un grupo de tres tamaños de muestra consecutivos", 
      side = 1, 
      line = 4, 
      cex = 0.8)

# PUNTO 3 ---- 
# Sea $X \sim N\left(\mu_0, \sigma_0\right)$ una característica de calidad. 
# Construya la carta $\bar{X}$ para el monitoreo de la media del proceso. 
# Genere 10 muestras de tamaño $n$ provenientes de $X$, de tal modo que la media
# muestral de ninguna de ellas caiga fuera de los límites de control. A partir del
# undécimo momento de monitoreo se pide generar muestras del mismo tamaño $n$ 
# provenientes de una distribución normal con media $\mu_1=\mu_0+k \sigma_0$ y 
# $\sigma_1=\sigma_0(\operatorname{con} k=1,0)$ hasta que la carta emita una señal 
# por primera vez. Si se asume que el proceso caracterizado por $X$ es estable y 
# que se desconoce el momento en el cual se produjo el incremento en el nivel medio, 
# ¿en qué muestra ocurrió el cambio en la media del proceso más probablemente?


miu <- 0
sd <- 1
n <- 5

UCL <- miu + 3*(sd/sqrt(n))
LCL <- miu - 3*(sd/sqrt(n))

M <- matrix(0, nrow = 5, ncol = 10)

for(i in 1:10){
  M[1:5,i] <- rnorm(n = n, mean = miu, sd = sd)
}

Means <- apply(M, 2, mean)

which(Means > UCL | Means < LCL)

N <- matrix(0, nrow = 5, ncol = 100)

for(i in 1:100){
  N[1:5,i] <- rnorm(n = n, mean = 1, sd = sd)
}

MeansN <- apply(N, 2, mean)

which(MeansN > UCL | MeansN < LCL)

N1 <- N[1:5, 1:12]

M1 <- cbind(M, N1)

T <- ncol(M1)
r <- seq(1, T, by = 1)

MM1 <- apply(M1, 2, mean)

Tau <- rep(0, T)

for(i in 1:T){
  Tau[i] <- (T - i)*(mean(MM1[i:T]) - miu)^2
}

which(max(Tau) == Tau)

# PUNTO 4 ---- Codigo realizado por Michael Meldivenson
# Sea $X \sim N(\mu_0, \sigma_0)$ una característica de calidad. Se pide:

# A) Mediante simulaciones, establezca el comportamiento del ARL de la Carta $\bar{X}$
#    con límites tres sigma para observaciones normales.

# B) Genere 20 subgrupos racionales de tamaño $n=3$ provenientes de $X$. Asúmase que
#    el proceso es estable en cuanto a dispersión y con los subgrupos iniciales, construya
#    la carta $\bar{X}$ como es habitual hasta verificar la estabilidad del proceso.
#    Establezca el comportamiento del ARL para la carta que se obtiene del análisis+
#    de Fase I realizado.

# PUNTO 5 ----

# Calcular el ARL de la Carta $\bar{X}$ mediante cadenas de Markov. Diseñar la carta 
# con límites de control ubicados a tres desviaciones estándar de la media y dividiendo
# la región de control estadístico en franjas de ancho igual a una desviación estándar.

# Cálculo de ARL0 para la carta X barra usando cadenas de Markov


# Cargar librerías necesarias
library(Matrix)
library(pracma)

# Definir los valores Z
Z <- c(pnorm(1) - pnorm(-1), 
       pnorm(2) - pnorm(1), 
       pnorm(3) - pnorm(2), 
       pnorm(-1) - pnorm(-2), 
       pnorm(-2) - pnorm(-3))

# Inicializar la matriz de resultados
resultados <- matrix(0, nrow = 5, ncol = 5)

# Rellenar la matriz con los valores correspondientes
for (i in 1:5) {
  for (j in 1:5) {
    resultados[i, j] <- Z[i]
  }
}

# Ajuste de valores específicos de la matriz
resultados[3, 3] <- 0
resultados[5, 5] <- 0

# Crear matriz T_0 con la misma dimensión que resultados
T_0 <- resultados

# Transponer la matriz resultados
resultados <- t(resultados)

# Definir la matriz Rf y el vector resultadosf
Rf <- resultados[1:5, 1:5]
If <- diag(1, nrow(Rf))
resultadosf <- rep(1, times = 5)

# Resolver el sistema lineal
R1f <- solve(If - Rf)
Af <- t(Rf) - If
Af <- rbind(Af, rep(1, 5))
bf <- c(rep(0, 5), 1)

# Calcular el vector de probabilidades pf
pf <- qr.solve(Af, bf)

# Calcular el ARL final
ARLf <- pf %*% R1f %*% resultadosf
ARLf

