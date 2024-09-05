
## Ejercicio 4 ---- 
# Un grupo de mantenimiento mejora la efectividad de su trabajo de reparación monitoreando
# el número de solicitudes de mantenimiento que requieren de una segunda llamada para 
# completar el servicio. Se cuenta con los siguientes reportes correspondientes a 20 
# semanas de inspección (Base Datos5.txt):

# Leer los datos
Datos5 <- read.csv("C:/Users/Cesar Prieto/Desktop/Datos5.txt")
nombres <- c('Sem','TS','SVR')
colnames(Datos5) <- nombres

# Calcular estadísticas
TSbarra <- mean(Datos5$TS)
SVRbarras <- mean(Datos5$SVR)
Pbarra <- sum(Datos5$SVR)/sum(Datos5$TS)
Qbarra <- 1 - Pbarra

# Añadir columnas calculadas
Datos5$Pi <- Datos5$SVR / Datos5$TS
Datos5$Sp <- sqrt(Datos5$Pi * (1 - Datos5$Pi) / Datos5$TS)

# Función para calcular los límites de la carta P con tamaño de muestra variable
calcular_limites <- function(Pbarra, TS) {
  LCL <- pmax(0, Pbarra - 3 * sqrt(Pbarra * (1 - Pbarra) / TS))
  UCL <- pmin(1, Pbarra + 3 * sqrt(Pbarra * (1 - Pbarra) / TS))
  return(list(LCL = LCL, UCL = UCL))
}

# Calcular los límites
limites <- calcular_limites(Pbarra, Datos5$TS)

# Añadir los límites a Datos5
Datos5$LCL <- limites$LCL
Datos5$UCL <- limites$UCL

# Mostrar los resultados
print(Datos5)

# Si quieres ver un resumen de las estadísticas calculadas
cat("\nEstadísticas:\n")
cat("TSbarra:", TSbarra, "\n")
cat("SVRbarras:", SVRbarras, "\n")
cat("Pbarra:", Pbarra, "\n")
cat("Qbarra:", Qbarra, "\n")


# Graficar la carta P

library(ggplot2)

ggplot(Datos5, aes(x = Sem, y = Pi)) +
  geom_point() +
  geom_line(aes(y = Pbarra), color = "red") +
  geom_line(aes(y = LCL), linetype = "dashed", color = "blue") +
  geom_line(aes(y = UCL), linetype = "dashed", color = "blue") +
  labs(title = "Carta P para solicitudes de mantenimiento",
       x = "Semana",
       y = "Proporción de solicitudes que requieren segunda llamada") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
