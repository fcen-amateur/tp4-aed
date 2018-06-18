# modelos a implementar:
#   - kvmc: k vecinos mas cercanos
#   - rlog: regresion logistica
#   - lda: analisis discriminante lineal
#   - qda: analisis discriminante cuadratico

# ** para el caso particular de dos clases**
#   
#' Parámetros obligatorios comunes a las 4 funciones:
#' - formula,
#' - df
#' 
#' Parámetros obligatorios por modelo:
#' - rlog: ninguno?
#' - kvmc: k  
#' - lda: mu, pik (vectores), Sigma (matrix)
#' - qda: mu, pik (vectores), Sigma (lista de matrices)
#' 
#' Parámetros opcionales:
#' - escalador: funcion de escalado, por defecto base::scale
#' - ...: parámetros para pasarle ad (descenso por gradiente)
#' 

logistica <- function(x) { 
  #' Computa la función logística de cada elemento de un vector `x`
  exp(x) / (1 + exp(x))
}

p <- function(X, beta) {
  #' Computa las probabilidades asociadas a la matriz de diseño `X`
  #' y los coeficientes `beta` como la función logística del producto
  #' matricial `X %*% beta`
  logistica(X %*% as.matrix(beta))
}

verosimilitud <- function(y, X, beta, natural = FALSE) {
  #' Computa la función de log-verosimilitud asociada al vector de respuesta `y`,
  #' la matriz de diseño `X` y los coeficientes `beta`.
  #' Si natural = TRUE, devuelve la función de verosimilitud 'natural'
  #' Predicciones de Pr(Y = 1 | X) para cara observacion
  yhat <- p(X, beta)
  # Las verosimilitudes individuales son las Pr(Y = i | X)
  verosimilitudes <- ifelse(y == 1, yhat, 1 - yhat)
  
  # Toda función devuelve la última expresión evaluada
  if (natural) {
    prod(verosimilitudes)
  } else {
    sum(log(verosimilitudes))
  }
}

gradiente_logverosimilitud <- function(y, X, beta0) {
  #' Computa el gradiente de la función de log-verosimilitud en función de
  #' sus coeficientes (`beta`), evaluada en el punto beta = `beta0`
  t(X) %*% (y - p(X, beta0))
}

dg <- function(fun_obj, gradiente_fun, beta0,
               tasa_aprendizaje = 1e-4, min_delta = 1e-5, max_ciclos = 100000) {
  #' Estima el mínimo de la función objetivo `fun_obj`.
  #' Se espera que `fun_obj` tome como único parámetro un vector (por caso, 'beta'),
  #' y devuelva un escalar. Además, `gradiente_fun` será la función que devuelve el
  #' gradiente de `fun_obj` evaluado en un valor dado de 'beta'.
  #' 
  #' - beta0 (vector): Valor inicial de 'beta' en el cual evaluar `fun_obj`
  #' - tasa_aprendizaje (escalar): Controla la distancia entre valores consecutivos
  #'     de 'beta' en la búsqueda.
  #' - min_delta: Si la diferencia entre valores consecutivos de `fun_obj` es
  #'     menor que `min_delta`, la búsqueda se detiene.
  #' - max_ciclos: Si la búsqueda completó `max_ciclos`, se detiene.
  #'  
  #' Devuelve una lista con elementos:
  #' - minimo_fun (escalar): Mínimo local estimado de `fun_obj`
  #' - parametros (vector): Valor de los parámetros en que `fun_obj` alcanza
  #'     `min_fun`
  #' - ciclos_completos (escalar): Duración de la búsqueda, en "ciclos" de
  #'     descenso de gradiente.
  #' - max_ciclos (escalar)
  #' - valores_funcion (vector): Valores de `fun_obj` recorridos en la búsqueda.
  #'     `valores_funcion[[1]]` es el valor de `fun_obj(beta0)`, y `valores_funcion[[i+1]]`
  #'     es `fun_obj(beta)` en el i-ésimo ciclo .
  #' - valores_beta (matrix): La primera columna contiene `beta0`, y cada columna subsiguiente
  #'     el valor de 'beta' en el siguiente ciclo.
  
  # Instanciar las variables de respuesta antes de un ciclo mejora su performance
  valores_beta <- matrix(ncol = max_ciclos + 1, nrow = length(beta0))
  valores_beta[,1] <- beta0
  
  valores_funcion <- vector("numeric", max_ciclos + 1)
  valores_funcion[[1]] <- fun_obj(beta0)

  delta <- -Inf
  beta <- beta0
  ciclos_completos <- 0
  
  while ((delta < -min_delta) & (ciclos_completos < max_ciclos)) {
    beta <- beta - tasa_aprendizaje * gradiente_fun(beta)
    
    valores_beta[,ciclos_completos + 2] <- beta
    valores_funcion[[ciclos_completos + 2]] <- fun_obj(beta)

    delta <- valores_funcion[[ciclos_completos + 2]] - valores_funcion[[ciclos_completos + 1]]
    ciclos_completos <- ciclos_completos + 1
  }
  
  return(list(
    minimo_fun = valores_funcion[[ciclos_completos + 1]],
    parametros = valores_beta[,ciclos_completos + 1],
    ciclos_completos = ciclos_completos,
    max_ciclos = max_ciclos,
    valores_funcion = valores_funcion[1:(ciclos_completos + 1)],
    valores_beta = valores_beta[,1:(ciclos_completos + 1)]
  ))
}


rlog <- function(formula, df, ...) {
  #' Computa la regresión logística especificada por `formula` a partir de los
  #' datos de `df`. Acepta `...` argumentos adicionales que serán pasados a `dg`
  #' 
  #' Por defecto escala todas las columnas salvo la primera (la ordenada) de la
  #' matriz de diseño centrando cada una en su media y dividiendo por el desvío
  #' estándar.
  #' 
  #' Devuelve `modelo`, una lista con el resultado de la llamada a `dg`, más:
  #' - df (dataframe): el dataframe original, aumentado con las probabilidades
  #'     asociadas a cada observación, según la función logística evaluada en el beta0 hallado.
  #' - predecir (funcion): Función que estima probabilidades para observaciones
  #'     no incluidas en el conjunto de entrenamento.

  var_y <- as.character(formula[[2]])
  y <- df[[var_y]]
  
  X <- scale(model.matrix(formula, df))
  medias <- attr(X, "scaled:center")
  desvios <- attr(X, "scaled:scale")
  # La primera columna debe ser siembre 'puros unos', sin escalar
  X[,1] <- 1
  
  verosimilitud_en_beta <- function(beta) { -verosimilitud(y, X, beta) }
  gradiente_en_beta <- function(beta) { -gradiente_logverosimilitud(y, X, beta) }
  beta0 <- vector("numeric", dim(X)[2])
  
  modelo <- dg(verosimilitud_en_beta, gradiente_en_beta, beta0, ...)
  
  predecir <- function(df) {
    yhat <- vector("logical", nrow(df))
    X <- model.matrix(formula, df)
    X <- scale(X, center = medias, scale = desvios)
    X[,1] <- 1
    
    return(p(X, modelo$parametros) > 0.5)
  }

  modelo$predecir <- predecir
  
  return(modelo)
}

aucroc <- function(y, yhat, cotas = seq(0,1, 0.01)) {
  #' Dado un vector de respuesta `y` y uno de probabilidades `yhat`, 
  #' `aucroc` calcula una tabla resumen con varios estadísticos para cada una de
  #' las `cotas` provistas.
  #' La cota `alfa` se utiliza para predecir según `yhat > alfa`
  #' 
  #' Devuelve un 'tibble', con una fila por cota, y columnas:
  #' - cota: Valor de la cota evaluada,
  #' - TP: Número de verdaderos positivos,
  #' - TN: Número de verdaderos negativos,
  #' - FP: Número de falsos positivos,
  #' - FN: Número de falsos negativos,
  #' - tpr: "True Positive Rate"; tpr = TP / P = TP / (TP + FN)
  #' - fpr: "False Positive Rate"; fpr = FP / N = FP / (TN + FP)
  #' - tasa_aciertos: Proporción global de aciertos; (TP + TN) / (TP + TN + FP + FN)
  
  tibble("y" = y, "yhat" = yhat, cota = list(cotas)) %>%
    # Genera todas las combinaciones posibles de cotas y pares 
    # (respuesta, probabilidad)
    unnest %>%
    # Agrega predicciones
    mutate(pred = yhat > cota) %>%
    mutate(tipo_pred = clasificar_pred(y, pred)) %>%
    # Computa la cantidad de predicciones clasificadas por cota
    group_by(cota, tipo_pred) %>%
    tally %>%
    spread(tipo_pred, n, fill = 0) %>%
    mutate(
      tpr = TP / (TP + FN),
      fpr = FP / (FP + TN),
      tasa_aciertos = (TP + TN) / (TP + TN + FP + FN))
}

