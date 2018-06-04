# modelos a implementar:
#   - kvmc: k vecinos mas cercanos
#   - rlog: regresion logistica
#   - lda: analisis discriminante lineal
#   - qda: analisis discriminante cuadratico
# 
# ** para el caso particular de dos clases**
#   
# modelo.modelar(df, modelo, var_y, var_X, cota, ...)
#   # Además, parámetros adicionales son:
#   # - modelo = 'kvmc', ...: k = 1, escalado = ['estandar', 'rango'] 
#   # - modelo = 'rlog', ...: escalado = ['estandar', 'rango']
#   # - modelo = 'lda',  ...: mu = vector("numeric", dim(df)[2]), sigma2 = 1,
#   #                        pik = 1/length(mu) + vector("numeric", length(mu))
#   # - modelo = 'qda',  ...: mu, Sigma = cor(df[var_X]), pik

# entrenar <- function(modelo) { entrenadores[[modelo$nombre]](modelo) }

logistica <- function(x) { exp(x) / (1 + exp(x)) }
p <- function(X, b) { logistica(X %*% b) }


verosimilitud <- function(y, X, beta, tipo = 'natural') {
  # Predicciones de Pr(Y = 1 | X) para cara observacion
  yhat <- p(X, beta)
  # Las verosimilitudes individuales son las Pr(Y = i | X)
  verosimilitudes <- ifelse(y == 1, yhat, 1 - yhat)
  
  resultado <- ifelse(tipo == "natural",
                      prod(verosimilitudes),
                      sum(log(verosimilitudes)))
  return(resultado)
}

gradiente_logistico <- function(y, X, beta0) {
  t(X) %*% (y - p(X, beta0))
}

dg <- function(funcion, gradiente, beta0,
               alfa = 1e-4, epsilon = 1e-5, max_ciclos = 100000) {
  
  # Reservo el espacio para la matriz con los valores explorados de beta
  valores_beta <- matrix(ncol = max_ciclos + 1, nrow = length(beta0))
  valores_beta[,1] <- beta0
  
  # Reservo el espacio para los valores de la funcion en beta
  valores_funcion <- vector("numeric", max_ciclos + 1)
  valores_funcion[[1]] <- funcion(beta0)

  delta <- -Inf
  beta <- beta0
  ciclos_completos <- 0
  
  while ((delta < -epsilon) & (ciclos_completos < max_ciclos)) {
    beta <- beta + alfa * gradiente(beta)
    
    valores_beta[,ciclos_completos + 2] <- beta
    valores_funcion[[ciclos_completos + 2]] <- funcion(beta)

    delta <- valores_funcion[[ciclos_completos + 2]] - valores_funcion[[ciclos_completos + 1]]
    ciclos_completos <- ciclos_completos + 1
  }
  
  return(list(
    minimo_funcion = valores_funcion[[ciclos_completos + 1]],
    parametros = valores_beta[,ciclos_completos + 1],
    ciclos_completos = ciclos_completos,
    max_ciclos = max_ciclos,
    valores_funcion = valores_funcion[1:(ciclos_completos + 1)],
    valores_beta = valores_beta[,1:(ciclos_completos + 1)]
  ))
}


rlog <- function(formula, df, ...) {
  var_y <- as.character(formula[[2]])
  y <- df[[var_y]]
  X <- model.matrix(formula, df)
  
  verosimilitud_en_beta <- function(beta) { -verosimilitud(y, X, beta, "log") }
  gradiente_en_beta <- function(beta) { gradiente_logistico(y, X, beta) }
  beta0 <- vector("numeric", dim(X)[2])
  
  modelo <- dg(verosimilitud_en_beta, gradiente_en_beta, beta0, ...)
  modelo$probs <- p(X, modelo$parametros)
  modelo$predictor <- function(X) {p(X, modelo$parametros)}
  
  return(modelo)
}

clasificar_pred <- function(y, pred) {
  ifelse(y & pred, "TP",
         ifelse(pred, "FP",
                ifelse(y, "TN", "FN")))
}

aucroc <- function(y, yhat, alfas = seq(0,1, 0.01)) {
  tibble("y" = y, "yhat" = yhat, alfa = list(alfas)) %>%
    unnest %>%
    mutate(pred = yhat > alfa) %>%
    mutate(tipo_pred = clasificar_pred(y, pred)) %>%
    group_by(alfa, tipo_pred) %>%
    tally %>%
    spread(tipo_pred, n, fill = 0) %>%
    mutate(
      tpr = TP / (TP + FN),
      fpr = FP / (FP + TN))  
}
