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


sigmas_por_clase <- function(clases,esperanzas) {
	n <- reduce (
		  map(clases,nrow),
		  sumar
		  )
	K <- length(clases)
	esperanzas <- map2(clases,esperanzas, 
			 ~colSums(
				 ( (restar_vector_a_filas(..1, ..2))^2)/(n - K)
				 )
			 )
	return( esperanzas )
}



razones_entre_desvios_de_dos_clases <- function(sigmas_por_clase) {
  sigmas_por_clase[[2]] / sigmas_por_clase[[1]]
}


sigma_hat <- function(clases) {
  covs_por_clase <- map(clases,~cov(.x)*nrow(.x))
  cov_unica <- reduce(
    covs_por_clase,
    sumar
    )
  K <- length(clases)
  n <-  map_dbl(clases,nrow) %>% sum
  return(cov_unica / (n - K) ) 
}




discriminante_clase <- function( mu , sigma , apriori ) {
  invSigma <- solve(sigma)
	delta_k <- function(x) { (x %*% invSigma) %*% mu - (1/2)*(mu%*%invSigma)%*%mu  + log(apriori) } 
  return(delta_k)
}  

discriminante <- function(mu_vector, sigma, apriori_vector) {
  discriminantes <- map2(mu_vector,
    apriori_vector,
    ~ discriminante_clase(.x,sigma,.y)
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



generar_el_predictor_adl <- function( formula, df ) {
  nombre.objetivo <- as.character(formula[[2]])
  predictores <- all.vars(formula[[3]])
  X <- df[predictores] 
  y <- df[[nombre.objetivo]]
  clases <- partir_en_clases(X,y)
  mu_hat <- mu_hat(clases)
  sigma_hat <- sigma_hat(clases)
  aprioris <- pi_k(X,clases)
  discriminante <- discriminante(mu_hat,sigma_hat,aprioris)
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
  return(list(predecir = predecir))
}
