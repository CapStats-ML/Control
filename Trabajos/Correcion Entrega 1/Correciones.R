# Se construye la carta para el nivel medio de un proceso normal en fase 2

mu = 3
s = 1
n = 3

UCL <- mu + 3*(s/sqrt(n))
LCL <- mu - 3*(s/sqrt(n))

obs <- mean(rnorm(n = n, mean = mu, sd = s))

RL <- rep(0, 5000)

for (i in 1:5000){
  obs <- 3
  count <- 0
    while(obs > LCL & obs < UCL){
    obs <- mean(rnorm(n = n, mean = mu + 1, sd = s))
    count <- count + 1
  }
  RL[i]<- count
}

MRL <- mean(RL)
MRL



# PUNTO 2 ----


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
     main = "Curvas OC para la carta S^2")

# Graficar las curvas en grupos de 4 colores
for (i in seq_along(n_values)) {
  n <- n_values[i]
  beta_values <- sapply(deltas, function(delta) OC_S2(n, sigma2_0, delta))
  
  # Determinar el grupo de color
  color_index <- (i - 1) %/% 3 + 1
  lines(deltas, beta_values, type = "l", col = colors[color_index], lwd = 1.2)
}


# PUNTO 5 ----

# Calcular el ARL de la Carta $\bar{X}$ mediante cadenas de Markov.
# Diseñar la carta con límites de control ubicados a tres desviaciones estándar de la media
# y dividiendo la región de control estadístico en franjas de ancho igual a una desviación estándar.

# Parámetros
mu <- 3
sigma <- 1
n <- 3

# Límites de control
UCL <- mu + 3 * sigma / sqrt(n)
LCL <- mu - 3 * sigma / sqrt(n)

# Probabilidades de transición
p <- 0.5
q <- 1 - p

# Matriz de transición
P <- matrix(c(q, p, 0, 0, 0, 0,
              q, 0, p, 0, 0, 0,
              0, q, 0, p, 0, 0,
              0, 0, q, 0, p, 0,
              0, 0, 0, q, 0, p,
              0, 0, 0, 0, q, 1),
            nrow = 6, byrow = TRUE)

# Vector de estado inicial
pi0 <- c(1, 0, 0, 0, 0, 0)

# Calcular la matriz de probabilidad de estado en el tiempo t
t_max <- 1000
pi_t <- matrix(0, nrow = 6, ncol = t_max)

for (t in 1:t_max) {
  pi_t[, t] <- pi0 %*% (P^t)
}

# Calcular el ARL
# Encuentra el primer instante en el que la probabilidad de estar en el estado 1 (más bajo)
# supera el límite superior o está por debajo del límite inferior.
ARL <- which.max(pi_t[1, ] > UCL | pi_t[1, ] < LCL)

# Mostrar el ARL
ARL

# PUNTO 5 ----

# Calcular el ARL de la Carta $\bar{X}$ mediante cadenas de Markov.
# Diseñar la carta con límites de control ubicados a tres desviaciones estándar de la media
# y dividiendo la región de control estadístico en franjas de ancho igual a una desviación estándar.

set.seed(123)  # Para reproducibilidad

# Parámetros
mu <- 0
n <- 3
sigma <- 1
Sigmas <- rep(0, 3)

for (i in 1:3){
  Sigmas[i] <- i/sqrt(n)
}

UCL <- mu + 3 * (sigma / sqrt(n))
LCL <- mu - 3 * (sigma / sqrt(n))

matrix_prob <- matrix(0, nrow = 6, ncol = 6)

# Probabilidades de transición

for (i in 1:6) {
  for (j in 1:6) {
    if (i == j) {
      matrix_prob[i, j] <- pnorm(Sigmas[j], mu, sigma) - pnorm(Sigmas[i - 1], mu, sigma)
    } else if (i == j + 1 | i == j - 1) {
      matrix_prob[i, j] <- pnorm(Sigmas[j], mu, sigma) - pnorm(Sigmas[j - 1], mu, sigma)
    } else {
      matrix_prob[i, j] <- 0
    }
  }
}


# PUNTO 5 - Versión Six Sigma ----
# Calcular el ARL de la Carta X-barra mediante cadenas de Markov.
# Diseñar la carta con límites de control ubicados a tres desviaciones estándar de la media
# y dividiendo la región de control estadístico en franjas de ancho igual a una desviación estándar.

# Parámetros
mu <- 0  # Media del proceso
sigma <- 1  # Desviación estándar del proceso
k <- 3  # Número de desviaciones estándar para los límites de control

# Función para calcular la probabilidad acumulada normal estándar
pnorm_diff <- function(a, b) {
  pnorm(b) - pnorm(a)
}

# Calcular probabilidades de transición
p_3 <- pnorm_diff(2, 3)
p_2 <- pnorm_diff(1, 2)
p_1 <- pnorm_diff(0, 1)
p_0 <- pnorm_diff(-1, 0)
p_m1 <- pnorm_diff(-2, -1)
p_m2 <- pnorm_diff(-3, -2)
p_out <- 1 - pnorm_diff(-3, 3)

# Construir la matriz de transición
P <- matrix(c(
  p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
  p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
  p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
  p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
  p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
  p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
  0,    0,    0,    0,    0,    0,    1
), nrow = 7, byrow = TRUE)

# Extraer la submatriz R
R <- P[1:6, 1:6]

# Calcular el ARL
ARL <- sum(solve(diag(6) - R) %*% rep(1, 6))

# Imprimir el resultado
cat("El ARL de la carta X-bar (Six Sigma) es:", round(ARL, 2))


### 

# Función para calcular el ARL usando cadenas de Markov
calculate_ARL <- function(mu, sigma) {
  k <- 3  # Número de desviaciones estándar para los límites de control
  
  # Función para calcular la probabilidad acumulada normal estándar
  pnorm_diff <- function(a, b) {
    pnorm((b - mu) / sigma) - pnorm((a - mu) / sigma)
  }
  
  # Calcular probabilidades de transición
  p_3 <- pnorm_diff(2*sigma, 3*sigma)
  p_2 <- pnorm_diff(sigma, 2*sigma)
  p_1 <- pnorm_diff(0, sigma)
  p_0 <- pnorm_diff(-sigma, 0)
  p_m1 <- pnorm_diff(-2*sigma, -sigma)
  p_m2 <- pnorm_diff(-3*sigma, -2*sigma)
  p_out <- 1 - pnorm_diff(-3*sigma, 3*sigma)
  
  # Construir la matriz de transición
  P <- matrix(c(
    p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
    p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
    p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
    p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
    p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
    p_m2, p_m1, p_0,  p_1,  p_2,  p_3,  p_out,
    0,    0,    0,    0,    0,    0,    1
  ), nrow = 7, byrow = TRUE)
  
  # Extraer la submatriz R
  R <- P[1:6, 1:6]
  
  # Calcular el ARL
  ARL <- sum(solve(diag(6) - R) %*% rep(1, 6))
  
  return(ARL)
}

# Parámetros de simulación
n_samples <- 1000  # Número de muestras
sample_size <- 10  # Tamaño de cada muestra
mu_0 <- 0  # Media del proceso bajo control
sigma_0 <- 1  # Desviación estándar del proceso bajo control

# Vector para almacenar los ARL calculados
ARL_values <- numeric(n_samples)

# Generar muestras y calcular ARL
set.seed(123)  # Para reproducibilidad
for (i in 1:n_samples) {
  # Generar una muestra
  sample_data <- rnorm(sample_size, mean = mu_0, sd = sigma_0)
  
  # Calcular la media y desviación estándar de la muestra
  sample_mean <- mean(sample_data)
  sample_sd <- sd(sample_data)
  
  # Calcular el ARL usando la función y guardarlo
  ARL_values[i] <- calculate_ARL(sample_mean, sample_sd)
}

# Resumen de los ARL calculados
cat("Resumen de los ARL calculados:\n")
print(summary(ARL_values))

# Histograma de los ARL
hist(ARL_values, main = "Distribución de ARL", xlab = "ARL", breaks = 30)

