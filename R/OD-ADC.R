source("ADL.R")

sigma_k <- function(clases) {
  covs_por_clase <- map( clases,~cov(.x) )
}




discriminante_cuadratico_de_una_clase <- function( mu , sigma , apriori ) {
  invSigma <- solve(sigma)
  detSigma <- det(sigma)
    delta_k <- function(x) { 
      (-0.5)*((x-mu) %*% invSigma) %*% (x-mu) - (1/2)*log(detSigma)  + log(apriori) } 
  return(delta_k)
}  

discriminante_cuadratico <- function(mu_vector, sigma, apriori_vector) {
  discriminante_cuadratico <- pmap(
    list( mu_vector , sigma , apriori_vector),
    ~ discriminante_cuadratico_de_una_clase(..1,..2,..3)
  )
 names(discriminante_cuadratico) <- names(mu_vector)
 return(discriminante_cuadratico)
}



armar_asignadora_de_clases <- function(deltas) {
  asignar <- function(x) {
    valor_de_los_delta <- map(deltas,~.x(x))
    seleccion <- which(valor_de_los_delta == max( as.double(valor_de_los_delta) ) )
    return( names(seleccion) )
 }
  return(asignar)
}



generador_el_predictor_adc <- function( formula, df ) {
  nombre.objetivo <- as.character(formula[[2]])
  predictores <- all.vars(formula[[3]])
  X <- df[predictores] 
  y <- df[[nombre.objetivo]]
  clases <- partir_en_clases(X,y)
  mu_hat <- mu_hat(clases)
  sigma_k <- sigma_k(clases)
  aprioris <- pi_k(X,clases)
  discriminante_cuadratico <- discriminante_cuadratico(mu_hat,sigma_k,aprioris)
  asignadora_escalar <- armar_asignadora_de_clases(discriminante_cuadratico)
  predecir <- function(df_prueba) {
    df_prueba <- df_prueba[predictores]
    columna <- map_lgl( seq ( nrow(df_prueba)),
      ~asignadora_escalar ( 
        as.double ( df_prueba[.x,]) 
      )
      %>% as.logical
    )
    return(columna)
  }
  return(list("predecir" = predecir))
}
