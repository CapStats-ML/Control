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
# Versión: 1.2
################################################################################

setwd("~/En Proceso/Control")

library(reshape2)
library(ggplot2)


# PUNTO 2 ----
# Sea X ~ N(mu, sigma) una característica de calidad de un producto. Se sabe que
# los valores objetivos de los parámetros del proceso son mu = mu_0 y sigma = sigma_0.
# Se debe construir las curvas OC de la carta S^2 con limites de probabilidad. 

# Se define la función para calcular la curva OC

OC_S2 <- function(n, sigma2_0, delta){
  # Cálculo de los límites de control superior e inferior para la carta S^2
  UCL <- sigma2_0/(n-1) * qchisq(1 - 1/2, df = n-1)
  LCL <- sigma2_0/(n-1) * qchisq(1/2, df = n-1)
  
  # Cálculo del poder de detección (beta)
  beta <- pchisq((n-1)*UCL/(sigma2_0 + delta), df = n-1) - pchisq((n-1)*LCL/(sigma2_0 + delta), df = n-1)
  
  return(beta)
}

for (i in 3:10){ 
  x <- rep(i, 80)
  print(x)
}

tabla <- cbind(n = x, beta = seq(0.1, 8, 0.1), delta = rep(NA, 80))

for (n in c(3, 4, 5, 6, 7, 8, 9, 10)){
  for (delta in seq(0.1, 1, 0.1)){
    beta <- OC_S2(n, 1, delta)
    tabla[i] <- c(n, beta, delta)
    cat("n =", n, ", delta =", delta, ", beta =", beta, "\n")
  }
}

print(tabla)


###


# Parámetros
mu <- 3
s <- 1
n <- 3
UCL <- mu + 3 * (s / sqrt(n))
LCL <- mu - 3 * (s / sqrt(n))

# Definir la matriz de transición
P <- matrix(c(0.9, 0.1, 0.1, 0.9), nrow = 2, byrow = TRUE)

# Función para simular el proceso de la cadena de Markov
simulate_markov_chain <- function(P, mu, s, n, UCL, LCL, delta, num_simulations) {
  num_states <- nrow(P)
  ARL <- numeric(num_simulations)
  
  for (sim in 1:num_simulations) {
    state <- 1  # Comenzamos en el estado "En Control"
    count <- 0
    
    while (TRUE) {
      # Generar una observación con el corrimiento en la media
      obs <- mean(rnorm(n, mean = mu + delta, sd = s))
      
      # Comprobar si la observación está dentro de los límites de control
      if (obs <= UCL && obs >= LCL) {
        count <- count + 1
        # Transición de estado
        state <- sample(1:num_states, 1, prob = P[state, ])
      } else {
        break
      }
    }
    
    ARL[sim] <- count
  }
  
  return(mean(ARL))
}

# Número de simulaciones
num_simulations <- 1000

# Valores de corrimiento (delta)
deltas <- seq(0, 3, by = 0.1)  # Desde 0 hasta 3 con incrementos de 0.1

# Calcular la Longitud Media de Ejecución (MRL) para cada valor de delta
MRLs <- sapply(deltas, function(delta) simulate_markov_chain(P, mu, s, n, UCL, LCL, delta, num_simulations))

# Graficar MRL frente a delta
plot(deltas, MRLs, type = "b", pch = 19, col = "blue",
     main = "Longitud Media de Ejecución (MRL) vs Corrimiento (Delta)",
     xlab = "Corrimiento (Delta)", ylab = "Longitud Media de Ejecución (MRL)",
     ylim = c(min(MRLs) - 5, max(MRLs) + 5))

# Añadir una línea horizontal en la MRL sin corrimiento
abline(h = MRLs[1], col = "red", lwd = 2, lty = 2)

# Añadir una leyenda
legend("topright", legend = c("MRL", "MRL sin Corrimiento"), 
       col = c("blue", "red"), lty = c(1, 2), pch = c(19, NA), lwd = c(1, 2))





