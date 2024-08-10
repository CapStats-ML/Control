################################################################################
# Script ACTIVIDAD EN CLASE - CONTROL ESTADISTICO DE CALIDAD
# Autores: Cesar Augusto Prieto Sarmiento
#          Cristian Camilo Prieto Zambrano
#          Daniel Santiago Guzman Villanueva
# Fecha de Creación: 13/03/2021
# Descripción: Se realizaron los ejercicios solicitados por el profesor
# Versión: 1
################################################################################

# LIBRERIAS USADAS

library(ggplot2)

################################################################################
############################### PUNTO 1 ########################################


# Función para calcular el rango de una muestra
calculate_range <- function(sample_data) {
  return(max(sample_data) - min(sample_data))
}

# Función para calcular la desviación estándar de una muestra
calculate_sd <- function(sample_data) {
  return(sd(sample_data))
}

# Función para calcular la media de una muestra
calculate_mean <- function(sample_data) {
  return(mean(sample_data))
}

# Función para calcular beta (probabilidad de no detectar un cambio)
calculate_beta <- function(mu_0, mu_1, sigma, L) {
  return(pnorm(L - abs(mu_1 - mu_0), 0, sigma) - pnorm(-L - abs(mu_1 - mu_0), 0, sigma))
}

# Función para calcular los parámetros de diseño de las cartas de control (B5 y B6)
calculate_control_limits <- function(sigma_0, L, beta) {
  B5 <- sigma_0 * (c_4 - 3 * sqrt(1 - c_4^2))
  B6 <- sigma_0 * (c_4 + 3 * sqrt(1 - c_4^2))
  return(list(B5 = B5, B6 = B6))
}

# Parámetros del proceso
mu_0 <- 0  # Media del proceso en control
sigma <- 1  # Desviación estándar
k <- 1  # Desplazamiento del proceso fuera de control
mu_1 <- mu_0 + k * sigma  # Media del proceso fuera de control
L <- 3  # Límites de control en sigmas
n_values <- c(3, 10)  # Tamaños de las muestras


# Constantes necesarias para el cálculo de los límites de control (puedes agregarlas)
c_4 <- (gamma(n_values / 2) / gamma((n_values - 1) / 2)) * sqrt(2 / (n_values - 1))

# Calcular beta para diferentes tamaños de muestra
beta_values <- sapply(n_values, function(n) {
  calculate_beta(mu_0, mu_1, sigma, L)
})

# Calcular los parámetros de diseño de las cartas de control (B5 y B6) usando beta
control_limits <- sapply(beta_values, function(beta) {
  calculate_control_limits(sigma, L, beta)
})

# Función para simular ARL de Cartas R, S y Xbarra
simulate_control_chart_ARL_both_phases <- function(n, sigma, control_limits, num_simulations = 1000, chart_type = "R") {
  arl_results_in_control <- numeric(num_simulations)
  arl_results_out_of_control <- numeric(num_simulations)
  
  # Fase 1: Establecer límites de control
  for (i in 1:num_simulations) {
    sample_data <- rnorm(n, mean = 0, sd = sigma)
    if (chart_type == "R") {
      sample_statistic <- calculate_range(sample_data)
    } else if (chart_type == "S") {
      sample_statistic <- calculate_sd(sample_data)
    } else if (chart_type == "Xbar") {
      sample_statistic <- calculate_mean(sample_data)
    } else {
      stop("Tipo de carta no válido. Debe ser 'R', 'S' o 'Xbar'.")
    }
    if (sample_statistic > control_limits[1]) {
      arl_results_out_of_control[i] <- 1
    } else {
      arl_results_in_control[i] <- 1
    }
  }
  
  # Fase 2: Monitorear el proceso
  for (i in 1:num_simulations) {
    if (arl_results_in_control[i] == 1) {
      sample_data <- rnorm(n, mean = 0, sd = sigma)
      if (chart_type == "R") {
        sample_statistic <- calculate_range(sample_data)
      } else if (chart_type == "S") {
        sample_statistic <- calculate_sd(sample_data)
      } else if (chart_type == "Xbar") {
        sample_statistic <- calculate_mean(sample_data)
      }
      if (sample_statistic > control_limits[1]) {
        arl_results_out_of_control[i] <- 1
      }
    }
  }
  
  # Calcular ARL
  arl_in_control <- cumsum(arl_results_in_control) / seq_along(arl_results_in_control)
  arl_out_of_control <- cumsum(arl_results_out_of_control) / seq_along(arl_results_out_of_control)
  
  return(data.frame(
    simulation_number = seq_along(arl_in_control),
    ARL_in_control = arl_in_control,
    ARL_out_of_control = arl_out_of_control
  ))
}

# Parámetros adicionales
num_simulations <- 1000  # Número de simulaciones
control_limits <- c(3 * sigma, 3 * sigma)


# Establecer semilla aleatoria para reproducibilidad
set.seed(123)

# Simular ARL para diferentes tamaños de muestra y tipos de carta
cartas <- c("R", "S", "Xbar")
arl_data <- lapply(cartas, function(cart) {
  lapply(seq_along(n_values), function(j) {
    simulate_control_chart_ARL_both_phases(n_values[j], sigma, control_limits[j], num_simulations, chart_type = cart)
  })
})

# Convertir datos a formato largo para ggplot
arl_long <- lapply(seq_along(cartas), function(i) {
  lapply(seq_along(n_values), function(j) {
    df_long <- tidyr::pivot_longer(arl_data[[i]][[j]], cols = c(ARL_in_control, ARL_out_of_control),
                                   names_to = "phase", values_to = "ARL")
    df_long$phase <- factor(df_long$phase, levels = c("ARL_in_control", "ARL_out_of_control"),
                            labels = c("En control", "Fuera de control"))
    df_long$chart_type <- cartas[i]
    df_long$n <- n_values[j]
    return(df_long)
  })
})

# Unir todos los resultados en un solo dataframe
arl_all <- do.call(rbind, do.call(c, arl_long))

# Graficar utilizando ggplot2
ggplot(arl_all, aes(x = simulation_number, y = ARL, color = phase, linetype = phase)) +
  geom_line(lwd = 1.2) +
  facet_grid(chart_type ~ n, scales = "free_y") +
  labs(x = "Número de simulaciones", y = "ARL",
       title = "Simulación del ARL para Cartas de Control") +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


################################################################################
############################### PUNTO 2 ########################################

# Función para calcular las probabilidades de no detectar una desviación en la carta S^2
OC_S2 <- function(mu, sigma, mu_0, sigma_0, n, alpha) {
  z <- qnorm(1 - alpha / 2)
  a <- (n - 1) * sigma_0^2 / sigma^2
  b <- (n - 1) * mu_0^2 / sigma^2
  c <- (n - 1) / sigma^2
  d <- -a * (1 - 1 / n)
  P_rechazar <- function(S2) {
    1 - pchisq((n - 1) * S2 / sigma_0^2, df = n - 1)
  }
  S2 <- seq(0, sigma_0^2 * 3, length.out = 1000)
  OC <- P_rechazar(S2)
  data.frame(S2, OC)
}

# Parámetros del proceso
mu_0 <- 5  # Media objetivo
sigma_0 <- 2  # Desviación estándar objetivo

# Parámetros de la muestra y nivel de significancia
n <- 20  # Tamaño de la muestra
alpha <- 0.05  # Nivel de significancia

# Calcular las curvas OC
oc_data <- OC_S2(mu = mu_0, sigma = sigma_0, mu_0 = mu_0, sigma_0 = sigma_0, n = n, alpha = alpha)

# Graficar las curvas OC con el formato deseado
ggplot(oc_data, aes(x = sqrt(S2), y = OC, color = "Curva OC")) +
  geom_line(lwd = 1.2) +
  labs(x = expression(sqrt(S^2)), y = "Probabilidad de rechazar H0",
       title = "Curvas OC de la Carta S^2") +
  scale_color_manual(values = c("#ae0001")) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

################################################################################
############################### PUNTO 4 ########################################


##################################3# Punto 4.a - Simulaciones ###################
# Establecer parámetros de una normal estándar y evaluamos para diferentes tamaños de muestra.
mu <- 0     
sigma <- 1  

# Parámetros de simulación
m <- 1000000    
n  <- 15    
L <- 3      
UCL <- mu + L * (sigma/sqrt(n))
LCL <- mu - L * (sigma/sqrt(n))

means <- numeric(m)
all <- numeric(m * n)
for (i in 1:m) {
  # Generar muestras
  muestras <- rnorm(n, mean = mu, sd = sigma)
  means[i] <- mean(muestras)
  all[((i - 1) * n + 1):(i * n)] <- muestras
}
f <- numeric(m)
for (i in 1:m) {  
  f[i] <- as.integer(means[i] > UCL | means[i] < LCL)
}
Pf <- sum(f)/m;
1/Pf


# Definimos los parametros para la estadistica de control #

Miu <- 20
Sigma <- 4


# Establemmos la semilla de generación de datos 

m <- 20
n <- 3

# Crear una matriz para almacenar las muestras generadas
set.seed(123)
muestras <- matrix(rnorm(m * n, mean = Miu, sd = Sigma), nrow = m)

# Asumimos estabilidad en la dispersión del proceso, por lo que hallamos el R para la contrucción de nuestra carta.

desviaciones <- apply(muestras, 1, sd)
Sbarra <- mean(desviaciones)

Rangos_muestrales <- apply(muestras, 1, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
Rbarra <- mean(Rangos_muestrales)

# Hallamos la estimación de Xbarra_barra #

Medias <- apply(muestras, 1, mean)
Xbb <- mean(Medias)

# Ahora construimos la carta Xbarra_barra  #

# Definimos la contaste A_2 poque vamos a trabajar con metodología 3sigma #

A_2 <- 0.577

# Definimos los límites y la línea central de la carta #

UCL = Xbb + (A_2 * Rbarra)
LCL = Xbb - (A_2 * Rbarra)

# Ahora realizamos la gráfica #

plot(Medias, type = "o", ylim = c(12, 28), xlab = "Secuencia muestreo", ylab = "Medias muestrales", main = "Carta de Control X-barra", legend = FALSE)
abline(h = UCL, col = "red", lty = 1)
abline(h = LCL, col = "red", lty = 1)
abline(h = mean(Medias), col = "blue", lty = 2)


# Podemos ver que el proceso está fuera de control, por lo que retiramos las muestras
# que quedaron fuera de los límites y recalculamos Xbarra_barra

# Suponiendo que tienes un dataframe llamado "dataframe" y quieres eliminar la fila número 5

muestras_i <- muestras[-16, ]

# Ahora recalculamos todo en base a los k valores restantes 

Rangos_muestrales_i <- apply(muestras_i, 1, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
Rbarra_i <- mean(Rangos_muestrales_i)

# Hallamos la estimación de Xbarra_barra #

Medias_i <- apply(muestras_i, 1, mean)
Xbb_i <- mean(Medias_i)

# Definimos los límites y la línea central de la carta #

UCL_i = Xbb_i + (A_2 * Rbarra_i)
LCL_i = Xbb_i - (A_2 * Rbarra_i)

# Ahora realizamos la gráfica #

plot(Medias_i, type = "o", ylim = c(12, 28), xlab = "Secuencia muestreo", ylab = "Medias muestrales", main = "Carta de Control X-barra", legend = FALSE)
abline(h = UCL_i, col = "red", lty = 1)
abline(h = LCL_i, col = "red", lty = 1)
abline(h = mean(Medias_i), col = "blue", lty = 2)

# Obetenemos nuestro estimador de miu bajo un proceso con normalidad, ahora, procedemos a calcular
# la carta en Fase II

UCL_ii = Xbb_i + 3 * (Rbarra/sqrt(n))

LCL_ii = Xbb_i - 3 * (Rbarra/sqrt(n))
warnings()

plot(Medias_i, type = "o", ylim = c(5, 35), xlab = "Secuencia muestreo", ylab = "Medias muestrales", main = "Carta de Control X-barra en Fase II", legend = FALSE)
abline(h = UCL_ii, col = "red", lty = 1)
abline(h = LCL_ii, col = "red", lty = 1)
abline(h = mean(Medias_i), col = "blue", lty = 2)

# Podemos evidenciar que el proceso cumple con estabilidad ahora #

# Evaluamos ARL_0 y ARL_1

# Para ARL_0 tenemos alpha = 0.0027
alpha <- 0.0027
ARL_0 <- 1/alpha

# Ahora, para ARL_1 tomamos los diferentes Betas y evaluamos 
# Ahora medimos el rendimiento de nuestra carta por medio del ARL - Longitud de corrida #

Beta_1 <- pnorm(3 - 1 * sqrt(5)) - pnorm(-3 - 1 * sqrt(5))

Beta_2 <- pnorm(3 - 2 * sqrt(5)) - pnorm(-3 - 2 * sqrt(5)) 

Beta_3 <- pnorm(3 - 3 * sqrt(5)) - pnorm(-3 - 3 * sqrt(5)) 

Beta_4 <- pnorm(3 - 4 * sqrt(5)) - pnorm(-3 - 4 * sqrt(5)) 

Beta_5 <- pnorm(3 - 5 * sqrt(5)) - pnorm(-3 - 5 * sqrt(5))  

Beta_6 <- pnorm(3 - 6 * sqrt(5)) - pnorm(-3 - 6 * sqrt(5))

Beta_7 <- pnorm(3 - 7 * sqrt(5)) - pnorm(-3 - 7 * sqrt(5))

Beta_8 <- pnorm(3 - 8 * sqrt(5)) - pnorm(-3 - 8 * sqrt(5))

Beta_9 <- pnorm(3 - 9 * sqrt(5)) - pnorm(-3 - 9 * sqrt(5))

Beta_10 <- pnorm(3 - 10 * sqrt(5)) - pnorm(-3 - 10 * sqrt(5))

Betas <- c(Beta_1,Beta_2,Beta_3,Beta_4,Beta_5,Beta_6,Beta_7,Beta_8,Beta_9,Beta_10)

ARL_1 <- 1 / (1 - Betas)
ARL_1

########################################## Para muestra m = 50 #########################



# Definimos los parametros para la estadistica de control #

Miu <- 20
Sigma <- 4


# Establemmos la semilla de generación de datos 



m <- 50
n <- 3

# Crear una matriz para almacenar las muestras generadas

set.seed(123)
muestras <- matrix(rnorm(m * n, mean = Miu, sd = Sigma), nrow = m)

# Asumimos estabilidad en la dispersión del proceso, por lo que hallamos el R para la contrucción de nuestra carta.

desviaciones <- apply(muestras, 1, sd)
Sbarra <- mean(desviaciones)

Rangos_muestrales <- apply(muestras, 1, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
Rbarra <- mean(Rangos_muestrales)

# Hallamos la estimación de Xbarra_barra #

Medias <- apply(muestras, 1, mean)
Xbb <- mean(Medias)

# Ahora construimos la carta Xbarra_barra  #

# Definimos la contaste A_2 poque vamos a trabajar con metodología 3sigma #

A_2 <- 0.577

# Definimos los límites y la línea central de la carta #

UCL = Xbb + (A_2 * Rbarra)
LCL = Xbb - (A_2 * Rbarra)

# Ahora realizamos la gráfica #

plot(Medias, type = "o", ylim = c(12, 28), xlab = "Secuencia muestreo", ylab = "Medias muestrales", main = "Carta de Control X-barra", legend = FALSE)
abline(h = UCL, col = "red", lty = 1)
abline(h = LCL, col = "red", lty = 1)
abline(h = mean(Medias), col = "blue", lty = 2)


# Podemos ver que el proceso está fuera de control, por lo que retiramos las muestras
# que quedaron fuera de los límites y recalculamos Xbarra_barra

# Suponiendo que tienes un dataframe llamado "dataframe" y quieres eliminar la fila número 5
# Índices de las filas que deseas eliminar
filas_a_eliminar <- c(6, 22)

# Eliminar las filas del dataframe
muestras_i <- muestras[-filas_a_eliminar, ]



# Ahora recalculamos todo en base a los k valores restantes 

Rangos_muestrales_i <- apply(muestras_i, 1, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
Rbarra_i <- mean(Rangos_muestrales_i)

# Hallamos la estimación de Xbarra_barra #

Medias_i <- apply(muestras_i, 1, mean)
Xbb_i <- mean(Medias_i)

# Definimos los límites y la línea central de la carta #

UCL_i = Xbb_i + (A_2 * Rbarra_i)
LCL_i = Xbb_i - (A_2 * Rbarra_i)

# Ahora realizamos la gráfica #

plot(Medias_i, type = "o", ylim = c(12, 28), xlab = "Secuencia muestreo", ylab = "Medias muestrales", main = "Carta de Control X-barra", legend = FALSE)
abline(h = UCL_i, col = "red", lty = 1)
abline(h = LCL_i, col = "red", lty = 1)
abline(h = mean(Medias_i), col = "blue", lty = 2)

# Obetenemos nuestro estimador de miu bajo un proceso con normalidad, ahora, procedemos a calcular
# la carta en Fase II

UCL_ii = Xbb_i + 3 * (Rbarra_i/sqrt(n))

LCL_ii = Xbb_i - 3 * (Rbarra_i/sqrt(n))
warnings()

plot(Medias_i, type = "o", ylim = c(5, 35), xlab = "Secuencia muestreo", ylab = "Medias muestrales", main = "Carta de Control X-barra en Fase II", legend = FALSE)
abline(h = UCL_ii, col = "red", lty = 1)
abline(h = LCL_ii, col = "red", lty = 1)
abline(h = mean(Medias_i), col = "blue", lty = 2)

# Podemos evidenciar que el proceso cumple con estabilidad ahora #

# Evaluamos ARL_0 y ARL_1

# Para ARL_0 tenemos alpha = 0.0027
alpha <- 0.0027
ARL_0 <- 1/alpha

# Ahora, para ARL_1 tomamos los diferentes Betas y evaluamos 
# Ahora medimos el rendimiento de nuestra carta por medio del ARL - Longitud de corrida #

Beta_1 <- pnorm(3 - 1 * sqrt(5)) - pnorm(-3 - 1 * sqrt(5))

Beta_2 <- pnorm(3 - 2 * sqrt(5)) - pnorm(-3 - 2 * sqrt(5)) 

Beta_3 <- pnorm(3 - 3 * sqrt(5)) - pnorm(-3 - 3 * sqrt(5)) 

Beta_4 <- pnorm(3 - 4 * sqrt(5)) - pnorm(-3 - 4 * sqrt(5)) 

Beta_5 <- pnorm(3 - 5 * sqrt(5)) - pnorm(-3 - 5 * sqrt(5))  

Beta_6 <- pnorm(3 - 6 * sqrt(5)) - pnorm(-3 - 6 * sqrt(5))

Beta_7 <- pnorm(3 - 7 * sqrt(5)) - pnorm(-3 - 7 * sqrt(5))

Beta_8 <- pnorm(3 - 8 * sqrt(5)) - pnorm(-3 - 8 * sqrt(5))

Beta_9 <- pnorm(3 - 9 * sqrt(5)) - pnorm(-3 - 9 * sqrt(5))

Beta_10 <- pnorm(3 - 10 * sqrt(5)) - pnorm(-3 - 10 * sqrt(5))

Betas <- c(Beta_1,Beta_2,Beta_3,Beta_4,Beta_5,Beta_6,Beta_7,Beta_8,Beta_9,Beta_10)

ARL_1 <- 1 / 1 - Betas
ARL_1



################################################################################
############################### PUNTO 5 ########################################


# Parámetros del proceso
mu <- 0  # Media del proceso
sigma <- 1  # Desviación estándar del proceso

# Definir franjas de control
lower_bound <- mu - sigma
upper_bound <- mu + 3 * sigma
num_intervals <- 4
interval_width <- sigma
interval_bounds <- seq(lower_bound, upper_bound, by = interval_width)

# Probabilidades de transición
transition_prob <- matrix(0, nrow = num_intervals, ncol = num_intervals)
for (i in 1:num_intervals) {
  for (j in 1:num_intervals) {
    if (i == j) {
      # Probabilidad de permanecer en la misma franja
      transition_prob[i, j] <- pnorm(interval_bounds[j + 1], mean = mu, sd = sigma) -
        pnorm(interval_bounds[j], mean = mu, sd = sigma)
    } else if (j == i + 1) {
      # Probabilidad de moverse a la franja adyacente a la derecha
      transition_prob[i, j] <- pnorm(interval_bounds[j + 1], mean = mu, sd = sigma) -
        pnorm(interval_bounds[j], mean = mu, sd = sigma)
    }
  }
}

# Matriz de transición
transition_prob

# Función para calcular ARL utilizando cadenas de Markov
calculate_arl_markov_chain <- function(transition_prob) {
  num_intervals <- nrow(transition_prob)
  absorbing_states <- num_intervals
  
  # Calcular matriz fundamental
  fundamental_matrix <- solve(diag(num_intervals) - transition_prob[1:num_intervals, 1:num_intervals])
  
  # Calcular ARL
  arl <- sum(fundamental_matrix[1,]) - sum(fundamental_matrix[absorbing_states,])
  return(arl)
}

# Calcular ARL
arl_xbar <- calculate_arl_markov_chain(transition_prob)
arl_xbar

