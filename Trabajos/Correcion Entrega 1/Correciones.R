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

#Ahora hagamoslo par la carta S 

mu <- 3        # Valor verdadero de la media
s <- 1         # Valor verdadero de la desviación estándar
n <- 6         # Tamaño de la muestra
S_barra <- 3.14  # Valor verdadero de la desviación estándar

C4 <- sqrt(2 / (n - 1)) * (gamma(n/2) / gamma((n - 1) / 2))  # Factor C4 para n = 25

B3 <- 1 - (3/(C4 * sqrt(2*(n-1))))  # Factor B3 para n = 6
B4 <- 1 + (3/(C4 * sqrt(2*(n-1))))  # Factor B4 para n = 6

UCL <- B4 * S_barra
LCL <- B3 * S_barra

RL <- rep(0, 5000)  # Vector para almacenar los longitudes de corrida

for (i in 1:5000) {
  obs <- s
  count <- 0
  while (obs > LCL & obs < UCL) {
    muestra <- rnorm(n = n, mean = mu + 1, sd = s)  # Desplazamiento de 1 en la media
    obs <- sd(muestra)  # Desviación estándar observada
    count <- count + 1
  }
  RL[i] <- count
}

MRL <- mean(RL)  # Longitud promedio de la corrida
MRL




# PUNTO 5 ----

# Calcular el ARL de la Carta $\bar{X}$ mediante cadenas de Markov.
# Diseñar la carta con límites de control ubicados a tres desviaciones estándar de la media
# y dividiendo la región de control estadístico en franjas de ancho igual a una desviación estándar.

A = pnorm(1) - pnorm(-1)
B = pnorm(2) - pnorm(1)
C = pnorm(3) - pnorm(2)
D = pnorm(-1) - pnorm(-2)
E = pnorm(-2) - pnorm(-3)
F = 1 - pnorm(3)

A; B; C; D; E; F

P = matrix(ncol = 6, nrow = 6)

P[1:5,1]= A
P[1:5,2]= B
P[1:5,3]= C
P[1:5,4]= D
P[1:5,5]= E
P[1:5,6]= F
P[1:6,6] = c(0,0,0,0,0,1)
P[6,1:6] = c(0,0,0,0,0,1)

P[4,4] = P[4,6]; P[4,4] = 0
P[5,5] = P[5,6]; P[5,5] = 0

R = P[1:5,1:5]
I = diag(5)
p0 = c(1,0,0,0,0)







################################

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

######################

# Definición de funciones

# Función para calcular los límites de control de la carta X-barra
calcular_limites_control <- function(miu, sd, n) {
  UCL <- miu + 3 * (sd / sqrt(n))
  LCL <- miu - 3 * (sd / sqrt(n))
  return(list(UCL = UCL, LCL = LCL))
}

# Función para generar una matriz de datos simulados
generar_matriz <- function(n_filas, n_columnas, miu, sd) {
  matriz <- matrix(0, nrow = n_filas, ncol = n_columnas)
  for (i in 1:n_columnas) {
    matriz[, i] <- rnorm(n = n_filas, mean = miu, sd = sd)
  }
  return(matriz)
}

# Función para calcular las medias de las columnas de una matriz
calcular_medias <- function(matriz) {
  medias <- apply(matriz, 2, mean)
  return(medias)
}

# Función para verificar si alguna media excede los límites de control
verificar_salida_control <- function(medias, UCL, LCL) {
  indices_fuera_de_control <- which(medias > UCL | medias < LCL)
  return(indices_fuera_de_control)
}

# Función para calcular el estadístico Tau
calcular_tau <- function(M1, miu) {
  T <- ncol(M1)
  MM1 <- calcular_medias(M1)
  Tau <- rep(0, T)
  for (i in 1:T) {
    Tau[i] <- (T - i) * (mean(MM1[i:T]) - miu)^2
  }
  return(Tau)
}

# Parámetros de la carta
miu <- 0
sd <- 1
n <- 5

# Cálculo de los límites de control
limites_control <- calcular_limites_control(miu, sd, n)
UCL <- limites_control$UCL
LCL <- limites_control$LCL

# Generación de la matriz para la carta en control
M <- generar_matriz(n_filas = 5, n_columnas = 10, miu = miu, sd = sd)

# Cálculo de las medias de la matriz M
Means <- calcular_medias(M)

# Verificación de si alguna muestra está fuera de control en la carta M
salidas_control_M <- verificar_salida_control(Means, UCL, LCL)

# Generación de la matriz para la carta con media corrida
N <- generar_matriz(n_filas = 5, n_columnas = 100, miu = 1, sd = sd)

# Cálculo de las medias de la matriz N
MeansN <- calcular_medias(N)

# Verificación de si alguna muestra está fuera de control en la carta N
salidas_control_N <- verificar_salida_control(MeansN, UCL, LCL)

# Se seleccionan los datos hasta la primera salida de control (incluida)
if (length(salidas_control_N) > 0) {
  N1 <- N[, 1:salidas_control_N[1]]
} else {
  N1 <- N
}

# Combinar la matriz M con la matriz N1
M1 <- cbind(M, N1)

# Cálculo del estadístico Tau para la carta combinada M1
Tau <- calcular_tau(M1, miu)

# Identificación del valor máximo de Tau
max_tau_index <- which.max(Tau)

####


# Función para generar la matriz N con un corrimiento específico de miu
generar_matriz_corrimiento <- function(n_filas, n_columnas, miu, sd) {
  matriz <- matrix(0, nrow = n_filas, ncol = n_columnas)
  for (i in 1:n_columnas) {
    matriz[, i] <- rnorm(n = n_filas, mean = miu, sd = sd)
  }
  return(matriz)
}

# Parámetros
miu_base <- 0
sd <- 1
n <- 5
n_columna <- 10
corrimientos <- seq(0.1, 2, 0.1)

# Generar matriz M
M <- generar_matriz_corrimiento(n_filas = n, n_columnas = n_columna, miu = miu_base, sd = sd)

# Calcular medias de la matriz M
Means_M <- apply(M, 2, mean)

# Limites de control
limites_control <- calcular_limites_control(miu_base, sd, n)
UCL <- limites_control$UCL
LCL <- limites_control$LCL

# Crear una lista para almacenar las medias con diferentes corrimientos
medias_corrimiento <- matrix(0, ncol = length(corrimientos), nrow = n_columna)

# Calcular medias para cada corrimiento
for (j in 1:length(corrimientos)) {
  miu_corrimiento <- corrimientos[j]
  N_corrimiento <- generar_matriz_corrimiento(n_filas = n, n_columnas = 100, miu = miu_corrimiento, sd = sd)
  Means_N_corrimiento <- apply(N_corrimiento, 2, mean)
  medias_corrimiento[, j] <- Means_N_corrimiento[1:n_columna]
}

# Crear el gráfico
plot_data <- data.frame(
  Valor = c(Means_M, as.vector(medias_corrimiento)),
  Tiempo = rep(1:n_columna, times = length(corrimientos) + 1),
  Tipo = rep(c("Observado", rep("Corrimiento", length(corrimientos))), each = n_columna),
  Corrimiento = rep(c(miu_base, corrimientos), each = n_columna)
)

ggplot(plot_data, aes(x = Tiempo, y = Valor, color = Tipo, group = interaction(Tipo, Corrimiento))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("blue", rep("red", length(corrimientos)))) +
  labs(title = "Carta de Control con Corrimiento de Miú", 
       x = "Número de Muestra", 
       y = "Valor de la Media", 
       color = "Tipo") +
  theme_minimal()
