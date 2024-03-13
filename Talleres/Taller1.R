################################################################################
############################### PUNTO 1 ########################################

library(ggplot2)

# Función para calcular el rango de una muestra
calculate_range <- function(sample_data) {
  return(max(sample_data) - min(sample_data))
}

# Función para simular ARL de Cartas R
simulate_R_chart_ARL_both_phases <- function(n, sigma, control_limits, num_simulations = 1000) {
  arl_results_in_control <- numeric(num_simulations)
  arl_results_out_of_control <- numeric(num_simulations)
  
  # Fase 1: Establecer límites de control
  for (i in 1:num_simulations) {
    sample_data <- rnorm(n, mean = 0, sd = sigma)
    sample_range <- calculate_range(sample_data)
    if (sample_range > control_limits) {
      arl_results_out_of_control[i] <- 1
    } else {
      arl_results_in_control[i] <- 1
    }
  }
  
  # Fase 2: Monitorear el proceso
  for (i in 1:num_simulations) {
    if (arl_results_in_control[i] == 1) {
      sample_data <- rnorm(n, mean = 0, sd = sigma)
      sample_range <- calculate_range(sample_data)
      if (sample_range > control_limits) {
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

# Parámetros
sigma <- 1  # Desviación estándar
control_limits <- 3 * sigma  # Límites de control
n_values <- c(3, 10)  # Tamaños de las muestras
num_simulations <- 1000  # Número de simulaciones

# Establecer semilla aleatoria para reproducibilidad
set.seed(123)

# Simular ARL para diferentes tamaños de muestra
arl_data <- lapply(n_values, function(n) {
  simulate_R_chart_ARL_both_phases(n, sigma, control_limits, num_simulations)
})

# Convertir datos a formato largo para ggplot
arl_long <- lapply(arl_data, function(df) {
  df_long <- tidyr::pivot_longer(df, cols = c(ARL_in_control, ARL_out_of_control),
                                 names_to = "phase", values_to = "ARL")
  df_long$phase <- factor(df_long$phase, levels = c("ARL_in_control", "ARL_out_of_control"),
                          labels = c("En control", "Fuera de control"))
  return(df_long)
})

# Graficar utilizando ggplot2
ggplot(arl_long[[1]], aes(x = simulation_number, y = ARL, color = phase, linetype = phase)) +
  geom_line() +
  labs(x = "Número de simulaciones", y = "ARL",
       title = "Simulación del ARL para Cartas R (n = 3)") +
  scale_color_manual(values = c("#ae0001", "#eeba30")) +
  scale_linetype_manual(values = c("solid","solid")) +
  theme_light()

ggplot(arl_long[[2]], aes(x = simulation_number, y = ARL, color = phase, linetype = phase)) +
  geom_line() +
  labs(x = "Número de simulaciones", y = "ARL",
       title = "Simulación del ARL para Cartas R (n = 10)") +
  scale_color_manual(values = c("#ae0001", "#eeba30")) +
  scale_linetype_manual(values = c("solid", "solid")) +
  theme_light()
