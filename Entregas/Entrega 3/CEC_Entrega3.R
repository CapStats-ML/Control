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
  Cp = h/2
  Cn = -h/2
  if (verbose) {pb = txtProgressBar(min = 0, max = m, style = 3)}
  while (indice < m) {
    ControlSample = rnorm(n = 10000, mean = mu + sd * Corrimiento, sd = sd_Mean)
    for (i in ControlSample) {
      Cp = max(0, Cp + ((i - mu)/sd_Mean) - k)
      Cn = min(0, Cn + ((i - mu)/sd_Mean) + k)
      if (Cp < h & Cn > -h) {
        longitud = longitud + 1
      } else {
        Cp = 0
        Cn = 0
        RL[indice] = longitud
        longitud = 1
        if (verbose) {setTxtProgressBar(pb, value = indice)}
        indice = indice + 1
      }
    }
  }
  RL = RL[1:m]
  return(list('ARL' = mean(RL), 'RL' = RL))
}

# Función para escoger los parámetros óptimos k y h
EscogerKH = function(mu = 0, sd = 1, n = 5, ARL_deseado = 200, k = 0.5, h_min = 0,
                     h_max = 5, m = 20000, tol = 0.1, max_iter = 50, verbose = TRUE) {
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
  
  return(list('k' = k, 'h' = h_actual, 'ARL' = ARL_actual, 'Iteraciones' = iteraciones))
}

# Calcular parámetros para detectar un cambio de 0.5 desviaciones estándar
set.seed(1998)
k <- c(0.5, 1, 1.5)
params_05 = lapply(k, function(k) EscogerKH(ARL_deseado = 200, k = k)$h)


# Evaluar el comportamiento del ARL
Corrimientos = seq(0, 2, 0.05)

ARL <- matrix(0, nrow = length(Corrimientos), ncol = 3)
for (i in 1:2){
    ARL[,i] = sapply(Corrimientos, function(x) 
    CUSUM_ARL(k = k[i], h = params_05[[i]], Corrimiento = x, m = 20000)$ARL)
}

plot(Corrimientos, ARL[,1], type = "l", col = "blue", xlim = c(0, 1),
     xlab = "Corrimiento (en desviaciones estándar)", 
     ylab = "ARL", main = "Carta CUSUM con k = 0.5σ")
lines(Corrimientos, ARL[,2], col = "red")
lines(Corrimientos, ARL[,3], col = "violet")
legend("topright", legend = c("δ = 0.5σ", "δ = 1.0σ","δ = 1.5σ"), 
       col = c("blue", "red", "violet"), lty = 1)

colnames(ARL) = c("k = 0.5", "k = 1", "k = 1.5")
rownames(ARL) = Corrimientos
Tabla = ARL[as.character(seq(0,1.5,0.1)), ]
print(Tabla)

params_05 <- unlist(params_05)
names(params_05) <- c("k = 0.5", "k = 1", "k = 1.5")
print(params_05)
