qda <- function(formula, df, muk = NA, pik = NA, sigmak = NA) {
  #' Análisis del discriminante cuadratico
  #' Dada una `formula` de términos únicamente aditivos, 
  #' 

  params <- estimar_parametros_qda(formula, df)
  
  
  if (is.na(muk)) { muk <- params[["muk"]] }
  if (is.na(pik)) { pik <- params[["pik"]] }
  if (is.na(sigmak)) { sigmak <- params[["sigmak"]] }
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
  stopifnot(map(sigm, dim) %>% map_lgl(~ all(. == c(p, p))) %>% all)
    
  
  # Armo la formula para un determinante
  
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

delta_k <- function(x, muk, pik, sigmak) {
  valor <- (
    #' Multiplicar una matrix por izquierda con un vector devuelve un vector fila,
    #' multiplicar por derecha devuelve un vector columna
    - 1/2 * (x - muk) %*% solve(sigmak) %*% (x - muk)
    - 1/2 * log(det(sigmak))
    + log(pik))
  
  return(valor)
}

generador_delta <- function(muk, pik, sigmak) {
  function(cierto_x) { 
    pmap_dbl(
      .l = list("muk" = muk, "pik" = as.list(pik), "sigmak" = sigmak),
      .f = delta_k,
      x = cierto_x
      )
  }
}

generador_predecir <- function(muk, pik, sigmak, ...) {
  delta <- generador_delta(muk, pik, sigmak, ...)
  function(x) { which.max( delta(x) ) }
}

m1 <- adulto ~ anillos + peso.total + long.diametro + long.altura
params <- estimar_parametros_qda(m1, abalone)

delta_m1 <- generador_delta(params$muk, params$pik, params$sigmak)
predecir_m1 <- generador_predecir(params$muk, params$pik, params$sigmak)

which.max(delta_m1(params$muk[[1]]))
predecir_m1(rep(0, 4))

delta_m1(params$muk[[2]])
params
params$muk
delta_k(1:4, params$muk[[2]], params$pik[[2]], params$sigmak[[2]])

detect_index(.x = dd(params$muk[[1]]))
??index
v <- params$muk[[1]]




params$muk
delta_qda_i <- function()
1/2 * sum((xx - muca1) * solve(sigca1) * (xx -muca1)) - 1/2 * log(det(sigca1)) + log(pica1)


m1 <- adulto ~ anillos + peso.total + long.diametro + long.altura
estimar_parametros_qda(m1, abalone)
