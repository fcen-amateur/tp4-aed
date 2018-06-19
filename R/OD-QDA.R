partir_en_clases <- function(X,y) {
	clases <- unique(y)
	names(clases) <- as.character( clases  )
	conjuntos <- map(clases,
    ~as.data.frame(
    X[(which(y==.x)),]
      )
    )
	return(conjuntos)
}



pi_k <- function(X,clases) {
  return(
    map_dbl(clases,~nrow(.x)/nrow(X)) 
  )   
}


mu_hat <- function(clases) {
	map(clases,colMeans)
}



restar_vector_a_filas <- function(X,v) {
	sustraendo <-  rep(v,each=nrow(X))
	return(X - sustraendo)
}




sumar <- function(a,b) {a+b}





razones_entre_desvios_de_dos_clases <- function(sigma_k) {
  sigma_k[[2]] / sigma_k[[1]]
}


sigma_k <- function(clases) {
  covs_por_clase <- map( clases,~cov(.x) )
}




discriminante_de_una_clase <- function( mu , sigma , apriori ) {
  invSigma <- solve(sigma)
  detSigma <- det(sigma)
    delta_k <- function(x) { 
      (-0.5)*((x-mu) %*% invSigma) %*% (x-mu) - (1/2)*log(detSigma)  + log(apriori) } 
  return(delta_k)
}  

discriminante <- function(mu_vector, sigma, apriori_vector) {
  discriminantes <- pmap(
    list( mu_vector , sigma , apriori_vector),
    ~ discriminante_de_una_clase(..1,..2,..3)
  )
 names(discriminantes) <- names(mu_vector)
 return(discriminantes)
}



armar_asignadora_de_clases <- function(deltas) {
  asignar <- function(x) {
    valor_de_los_delta <- map(deltas,~.x(x))
    seleccion <- which(valor_de_los_delta == max( as.double(valor_de_los_delta) ) )
    return( names(seleccion) )
 }
  return(asignar)
}



generador_predictor_qda <- function( formula, df ) {
  nombre.objetivo <- as.character(formula[[2]])
  predictores <- all.vars(formula[[3]])
  X <- df[predictores] 
  y <- df[[nombre.objetivo]]
  clases <- partir_en_clases(X,y)
  mu_hat <- mu_hat(clases)
  sigma_k <- sigma_k(clases)
  aprioris <- pi_k(X,clases)
  discriminante <- discriminante(mu_hat,sigma_k,aprioris)
  asignadora_escalar <- armar_asignadora_de_clases(discriminante)
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
