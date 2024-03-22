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


# Definimos los parámetros para la estadística de control
Miu <- 20
Sigma <- 4

m <- 20
n <- 3

# Establecemos la semilla de generación de datos 
set.seed(123)

# Creamos una matriz para almacenar las muestras generadas
muestras <- matrix(rnorm(m * n, mean = Miu, sd = Sigma), nrow = m)

# Calculamos la desviación estándar para cada muestra
desviaciones <- apply(muestras, 1, sd)
Sbarra <- mean(desviaciones)

# Calculamos los rangos muestrales para la construcción de la carta
Rangos_muestrales <- apply(muestras, 1, function(x) max(x) - min(x))
Rbarra <- mean(Rangos_muestrales)

# Calculamos la estimación de Xbarra_barra
Medias <- apply(muestras, 1, mean)
Xbb <- mean(Medias)

# Construimos la carta Xbarra_barra
A_2 <- 0.577
UCL <- Xbb + (A_2 * Rbarra)
LCL <- Xbb - (A_2 * Rbarra)

# Graficamos la carta de control X-barra
plot(Medias, type = "o", ylim = c(12, 28), xlab = "Secuencia de muestreo", ylab = "Medias muestrales", main = "Carta de Control X-barra", legend = FALSE)
abline(h = c(UCL, LCL, mean(Medias)), col = c("red", "red", "blue"), lty = c(1, 1, 2))

# Retiramos las muestras fuera de control
muestras_i <- muestras[-11, ]

# Recalculamos todo en base a los valores restantes
Rangos_muestrales_i <- apply(muestras_i, 1, function(x) max(x) - min(x))
Rbarra_i <- mean(Rangos_muestrales_i)
Medias_i <- apply(muestras_i, 1, mean)
Xbb_i <- mean(Medias_i)

# Calculamos los límites para la carta de control X-barra en Fase II
UCL_ii <- Xbb_i + 3 * (Rbarra / sqrt(n))
LCL_ii <- Xbb_i - 3 * (Rbarra / sqrt(n))

# Graficamos la carta de control X-barra en Fase II
plot(Medias_i, type = "o", ylim = c(5, 35), xlab = "Secuencia de muestreo", ylab = "Medias muestrales", main = "Carta de Control X-barra en Fase II", legend = FALSE)
abline(h = c(UCL_ii, LCL_ii, mean(Medias_i)), col = c("red", "red", "blue"), lty = c(1, 1, 2))

# Calculamos ARL_0 y ARL_1
alpha <- 0.0027
ARL_0 <- 1 / alpha

Beta <- sapply(1:10, function(i) {
  pnorm(3 - i * sqrt(5)) - pnorm(-3 - i * sqrt(5))
})
ARL_1 <- 1 / (1 - Beta)

# Mostramos los resultados
print(ARL_0)
print(ARL_1)


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

