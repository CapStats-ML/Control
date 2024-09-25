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

ARL <- matrix(0, nrow = length(Corrimientos), ncol = 2)
for (i in 1:2){
    ARL[,i] = sapply(Corrimientos, function(x) 
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
