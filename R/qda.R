qda <- function(formula, df, muk = NA, pik = NA, sigmak = NA) {
  #' Análisis del discriminante cuadratico
  #' Dada una `formula` de términos únicamente aditivos, 
  #' calcula el discriminante cuadrático según los datos de df
  #' Devuelve una con:
  #' - `df`: el `df` original aumentado con las predicciones, y
  #' - `predecir`: la fórmula que predice una clase según el dsicriminante obtenido
  
  params <- estimar_parametros_qda(formula, df)
  n <- nrow(df)
  
  if (is.na(muk)) { muk <- params[["muk"]] }
  if (is.na(pik)) { pik <- params[["pik"]] }
  if (is.na(sigmak)) { sigmak <- params[["sigmak"]] }
  
  predecir <- generador_predecir_qda(muk, pik, sigmak)
  
  # X es la matriz de diseño, sin y
  X <- model.matrix(formula, df)[,-c(1)]
  
  df$yhat <- map_dbl(seq_len(n), ~predecir(X[.,]))
  
  modelo <- list(
    "df" = df,
    "predecir" = predecir
  )
  return(modelo)
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
      pik = map_dbl(data, nrow)/nrow(df),
      #' El primer map devuelve la media de cada columna en un tibble de 1 fila,
      #' el segundo convierte el tibble en un vector
      muk = map(data, summarise_all, .funs = mean) %>% map(unlist),
      sigmak = map(data, cov)) %>%
    select(-data) %>% as.list
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

generador_predecir_qda <- function(muk, pik, sigmak, ...) {
  delta <- generador_delta_qda(muk, pik, sigmak, ...)
  function(x) { which.max( delta(x) ) }
}

#' Esto lo podrpiamos reemplazar por un objeto "abalone" que se importe con el .R, es
#' la práctica más difundida de paquetes en R.
source('leer_abalone.R')
abalone <- leer_abalone("abalone.data")

# Caso de uso encapsulado
m1 <- adulto ~ anillos + peso.total + long.diametro + long.altura
qda1 <- qda(m1, abalone)

# Probando las funciones de a pedacitos
params <- estimar_parametros_qda(m1, abalone)
predecir_m1 <- generador_predecir_qda(params$muk, params$pik, params$sigmak)
predecir_m1(params$muk[[2]])
            
