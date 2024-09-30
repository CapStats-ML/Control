# Funcion para simular la carta Cusum 

# Función para simular una carta CUSUM
simular_cusum <- function(n = 100, mu0 = 0, sigma = 1, shift = 1, k = 0.5, h = 5) {
  # Generar datos
  x <- c(rnorm(n/2, mu0, sigma), rnorm(n/2, mu0 + shift * sigma, sigma))
  
  # Inicializar estadísticas CUSUM
  C_pos <- numeric(n)
  C_neg <- numeric(n)
  
  # Condiciones iniciales
  C_pos[1] <- max(0, (x[1] - mu0) / sigma - k)
  C_neg[1] <- min(0, (x[1] - mu0) / sigma + k)
  
  # Calcular CUSUM para el resto de los datos
  for (i in 2:n) {
    C_pos[i] <- max(0, C_pos[i-1] + (x[i] - mu0) / sigma - k)
    C_neg[i] <- min(0, C_neg[i-1] + (x[i] - mu0) / sigma + k)
  }
  
  # Graficar
  plot(1:n, x, type = "l", ylim = c(min(c(x, C_neg, -h/2)), max(c(x, C_pos, h/2)) - 10),
       main = "CARTA CUSUM SIMULADA", xlab = "Muestra", ylab = "Valor", cex.lab = 0.8,
       lwd = 1.2, font.main = 2, cex.axis = 0.8, cex.main = 1, col.main = 'black', family = 'sans',
       bty = 'L', fg = 'black', col.axis = 'black',col.lab = 'black', font.axis = 1, font.lab = 2)
  lines(1:n, C_pos, col = "#d50000", lwd = 2)
  lines(1:n, C_neg, col = "#432c9b", lwd = 2)
  abline(h = c(-h/2, h/2), col = "darkgreen", lty = 1)
  abline(h = mu0, col = "gray25", lty = 3)
  legend(x = 5, y = 20, legend = c("Datos", "CUSUM+", "CUSUM-", "Límites"), cex = 0.8,
         col = c("black", "#d50000", "#432c9b", "darkgreen"), lty = c(1, 1, 1, 2))
}

# Ejecutar la simulación
set.seed(123)
simular_cusum()


# Función para calcular ARL de la carta CUSUM
CUSUM_ARL = function(mu = 0, sd = 1,                 # Parámetros reales del proceso
                     n = 5,                          # Tamaño de subgrupo racional
                     k, h,                           # Parámetros CUSUM
                     m = 10000,                      # Cantidad de RLs generadas
                     Corrimiento = 0,                # Corrimiento de la media en múltiplos de sd
                     verbose = TRUE) {               # Mostrar o no la barra de progreso 
  sd_Mean = sd/sqrt(n)
  RL = numeric(m)
  longitud = 1
  indice = 1
  Cp = 0  # Inicializamos Cp en 0
  Cn = 0  # Inicializamos Cn en 0
  if (verbose) {pb = txtProgressBar(min = 0, max = m, style = 3)}
  while (indice <= m) {
    ControlSample = rnorm(n = 10000, mean = mu + sd * Corrimiento, sd = sd_Mean)
    for (i in ControlSample) {
      Cp = max(0, Cp + ((i - mu)/sd_Mean) - k)
      Cn = min(0, Cn + ((i - mu)/sd_Mean) + k)
      if (Cp < h & Cn > -h) {
        longitud = longitud + 1
      } else {
        RL[indice] = longitud
        longitud = 1
        Cp = 0  # Reiniciamos Cp
        Cn = 0  # Reiniciamos Cn
        if (verbose) {setTxtProgressBar(pb, value = indice)}
        indice = indice + 1
        if (indice > m) break  # Salimos del bucle si hemos alcanzado m
      }
    }
  }
  if (verbose) {close(pb)}
  return(list('ARL' = mean(RL), 'RL' = RL))
}

# Función para escoger los parámetros óptimos k y h
EscogerKH = function(mu = 0, sd = 1, n = 5, ARL_deseado = 200, k = 0.5, h_min = 0,
                     h_max = 7, m = 20000, tol = 0.1, max_iter = 50, verbose = TRUE) {
  iteraciones = 0
  ARL_actual = 0
  if (verbose) {pb = txtProgressBar(min = 0, max = max_iter, style = 3)}
  while (iteraciones < max_iter && abs(ARL_actual - ARL_deseado) > tol) {
    h_actual = (h_min + h_max) / 2
    
    resultado = CUSUM_ARL(mu = mu, sd = sd, n = n, k = k, h = h_actual, 
                          m = m, verbose = FALSE)
    
    ARL_actual = resultado$ARL
    
    if (ARL_actual > ARL_deseado) {
      h_max = h_actual
    } else {
      h_min = h_actual
    }
    if (verbose) {setTxtProgressBar(pb, value = iteraciones)}
    iteraciones = iteraciones + 1
  }
  
  if (verbose) {close(pb)}
  
  return(list('k' = k, 'h' = h_actual, 'ARL' = ARL_actual, 'Iteraciones' = iteraciones))
}

# Calcular parámetros para detectar un cambio de 0.5 desviaciones estándar
set.seed(1998)
k <- c(0.25, 0.5)
params_05 = lapply(k, function(k) EscogerKH(ARL_deseado = 200, k = k)$h)


# Evaluar el comportamiento del ARL
Corrimientos = seq(0, 2, 0.05)

ARL1.1 <- matrix(0, nrow = length(Corrimientos), ncol = 2)
for (i in 1:2){
    ARL1.1[,i] = sapply(Corrimientos, function(x) 
    CUSUM_ARL(k = k[i], h = params_05[[i]], Corrimiento = x, m = 20000)$ARL)
}

par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')
plot(Corrimientos, ARL[,1], type = "l", col = "red", xlim = c(0, 1), ylab = "ARL", 
     xlab = "Corrimientos en\nmúltiplos de σ", main = "ARL PARA CARTA CUSUM", cex.lab = 0.8,
     lwd = 1, font.main = 2, cex.axis = 0.8, cex.main = 1, col.main = 'black', family = 'sans',
     bty = 'L', fg = 'black', col.axis = 'black',col.lab = 'black', font.axis = 1, font.lab = 2)
lines(Corrimientos, ARL[,2], col = "orange")
abline(h = seq(0, 220, 25), col = 'grey75', lty = 'dotted')
abline(v = seq(0, 1, 0.1), col = 'grey75', lty = 'dotted')
legend("topright", title = 'Corrimiento', legend = c("0.25σ", "0.5σ"), text.font = 1,
       col = c("red", "orange"), lty = 1, title.font = 2)



#Tabla de parámetros óptimos
library(xtable)

params_05 <- unlist(params_05)
names(params_05) <- c("k = 0.25", "k = 0.5")
print(params_05)
params_05 = data.frame(params_05)
xtable(params_05, caption = "Parámetros óptimos para detectar un cambio de k-sigma", 
       label = "tab:params_05", digits = 2)

# Tabla de ARL

colnames(ARL) = c("k = 0.25", "k = 0.5")
rownames(ARL) = Corrimientos
Tabla = ARL[as.character(seq(0,2,0.05)), ];Tabla
Tabla = data.frame(Tabla)
xtable(Tabla, caption = "ARL para detectar un cambio de k-sigma", 
       label = "tab:ARL", digits = 2)

## Ahora probemos para un ARL de  370 

params_06 = lapply(k, function(k) EscogerKH(ARL_deseado = 370, k = k, h_max = 10)$h)

Corrimientos = seq(0, 2, 0.05)
ARL2.1 <- matrix(0, nrow = length(Corrimientos), ncol = 2)
for (i in 1:2){
    ARL2.1[,i] = sapply(Corrimientos, function(x) 
    CUSUM_ARL(k = k[i], h = params_06[[i]], Corrimiento = x, m = 20000)$ARL)
}


par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')
plot(Corrimientos, ARL[,1], type = "l", col = "red", xlim = c(0, 1), ylab = "ARL", 
     xlab = "Corrimientos en\nmúltiplos de σ", main = "ARL PARA CARTA CUSUM", cex.lab = 0.8,
     lwd = 1, font.main = 2, cex.axis = 0.8, cex.main = 1, col.main = 'black', family = 'sans',
     bty = 'L', fg = 'black', col.axis = 'black',col.lab = 'black', font.axis = 1, font.lab = 2)
lines(Corrimientos, ARL[,2], col = "orange")
abline(h = seq(0, 400, 40), col = 'grey75', lty = 'dotted')
abline(v = seq(0, 1, 0.1), col = 'grey75', lty = 'dotted')
legend("topright", title = 'Corrimiento', legend = c("0.25σ", "0.5σ"), text.font = 1,
       col = c("red", "orange"), lty = 1, title.font = 2)

# Grafica en escala log

par(mar = c(5.1, 5.1, 4.1, 2.1), bg = 'white')

plot(Corrimientos, ARL[,1], type = "l", col = "red", xlim = c(0, 1), ylab = "ARL", 
     xlab = "Corrimientos en\nmúltiplos de σ", main = "ARL PARA CARTA CUSUM", cex.lab = 0.8,
     lwd = 1, font.main = 2, cex.axis = 0.8, cex.main = 1, col.main = 'black', family = 'sans',
     bty = 'L', fg = 'black', col.axis = 'black',col.lab = 'black', font.axis = 1, font.lab = 2, log = "y")
lines(Corrimientos, ARL[,2], col = "orange")
#abline(h = seq(0, 150, 10), col = 'grey75', lty = 'dotted')
abline(v = seq(0, 1, 0.1), col = 'grey75', lty = 'dotted')
legend("topright", title = 'Corrimiento', legend = c("0.25σ", "0.5σ"), text.font = 1,
       col = c("red", "orange"), lty = 1, title.font = 2)

#Ahora para corrimientos negativos

Corrimientos = seq(-1, 0, 0.05)
ARL1.2 <- matrix(0, nrow = length(Corrimientos), ncol = 2)

for (i in 1:2){
    ARL1.2[,i] = sapply(Corrimientos, function(x) 
    CUSUM_ARL(k = k[i], h = params_05[[i]], Corrimiento = x, m = 20000)$ARL)
}

# Tablas para los ARL's de todas las simulaciones

Corrimientos = seq(0, 2, 0.05)
colnames(ARL1.1) = c("k = 0.25", "k = 0.5")
rownames(ARL1.1) = round(Corrimientos, 4)
Tabla1 = ARL1.1[as.character(seq(0,1,0.05)), ];Tabla1
Tabla1 = data.frame(Tabla1)

Corrimientos = seq(-1, 0, 0.05)
colnames(ARL1.2) = c("k = 0.25", "k = 0.5")
rownames(ARL1.2) = Corrimientos
Tabla2 = ARL1.2[as.character(seq(-1,0,0.05)), ];Tabla2
Tabla2 = data.frame(Tabla2)

Corrimientos = seq(0, 2, 0.05)
colnames(ARL2.1) = c("k = 0.25", "k = 0.5")
rownames(ARL2.1) = round(Corrimientos, 4)
Tabla3 = ARL2.1[as.character(seq(0,1,0.05)), ];Tabla3
Tabla3 = data.frame(Tabla3) 

print("Buscando un ARL de 200 con Corrimientos Positivos: \n");Tabla1[1:6,]
print("Buscando un ARL de 200 con Corrimientos Negativos: \n");Tabla2[16:21,]
print("Buscando un ARL de 370 con Corrimientos Positivos: \n");Tabla3[1:6,]



