qda <- function(formula, df, clases = NA, muk = NA, pik = NA, sigmak = NA) {
  #' Análisis del discriminante cuadratico
  #' Dada una `formula` de términos únicamente aditivos, 
  #' calcula el discriminante cuadrático según los datos de df
  #' Devuelve una con:
  #' - `df`: el `df` original aumentado con las predicciones, y
  #' - `predecir`: la fórmula que predice una clase según el dsicriminante obtenido
  
  params <- estimar_parametros_qda(formula, df)
  n <- nrow(df)
  var_y <- as.character(formula[[2]])
  
  if (is.na(clases)) { clases <- params[[var_y]] }
  if (is.na(muk)) { muk <- params[["muk"]] }
  if (is.na(pik)) { pik <- params[["pik"]] }
  if (is.na(sigmak)) { sigmak <- params[["sigmak"]] }
  
  predecir <- function(df) {
    predictor_individual <- generador_predecir_qda(clases, muk, pik, sigmak)
    # X es la matriz de diseño, sin y
    X <- df[,all.vars(formula[[3]])]
    # Le aplico `predictor_individual` a cada fila de X
    yhat <-  map(seq_len(nrow(df)), ~predictor_individual(X[.,])) %>% unlist()
    
    return(yhat)
  }
  
  return(list(
    predecir = predecir,
    parametros = params))
}

generador_delta_qda <- function(muk, pik, sigmak) {
  # numero de predictores implicitos
  p <- length(muk[[1]])
  # numero de clases
  k <- length(pik)
  
  # pik, muk y sigmak deben tener igual cantidad de elementos
  stopifnot(length(muk) == k)
  stopifnot(length(sigmak) == k)
  # Cada elemento de muk debe tener p elementos
  stopifnot(all(map_dbl(muk, length) == p))
  # Cada elemento de sigmak debe tener dimensiones (p, p)
  stopifnot(map(sigmak, dim) %>% map_lgl(~ all(. == c(p, p))) %>% all)
    
  
  # Mapeo la formula para un discriminante a las k clases
  function(cierto_x) { 
    pmap_dbl(
      .l = list("muk" = muk, "pik" = as.list(pik), "sigmak" = sigmak),
      .f = delta_k_qda,
      x = cierto_x
    )
  }
  
}
estimar_parametros_qda <- function(formula, df) {
  var_y <- as.character(formula[[2]])
  df %>%
    select(
      all.vars(formula)) %>%
    nest(-var_y) %>%
    mutate(
      clase = var_y,
      pik = map_dbl(data, nrow)/nrow(df),
      #' El primer map devuelve la media de cada columna en un tibble de 1 fila,
      #' el segundo convierte el tibble en un vector
      muk = map(data, summarise_all, .funs = mean) %>% map(unlist),
      sigmak = map(data, cov)) %>%
    select(-data)
}

delta_k_qda <- function(x, muk, pik, sigmak) {
  valor <- (
    #' Multiplicar una matrix por izquierda con un vector devuelve un vector fila,
    #' multiplicar por derecha devuelve un vector columna
    - 1/2 * (x - muk) %*% solve(sigmak) %*% (x - muk)
    - 1/2 * log(det(sigmak))
    + log(pik))
  
  return(valor)
}

generador_predecir_qda <- function(clases, muk, pik, sigmak) {
  delta <- generador_delta_qda(muk, pik, sigmak)
  function(x) { clases[[which.max( delta(x) )]] }
}