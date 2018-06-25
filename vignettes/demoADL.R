library(tidyverse)

source("R/leer_abalone.R")

formula_c <- adulto ~ peso.viscera + anillos 

predictores_c <- all.vars ( formula_c[[3]]  )

set.seed(42)

var_y <- formula_c[[2]]
rifa <- sample(seq(abalone[[var_y]]),10,replace=F)
X_c <- abalone[rifa,]
y_c <- X_c[["adulto"]]
X_c <- X_c[ predictores_c  ]


#función para identificar las clases y obtener una lista de marcos de datos para cada una. 

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

clases_c <- partir_en_clases(X_c,y_c)

#función para obtener las probabilidades apriori de pertenecer a una clase:

pi_k <- function(X,clases) {
  return(
    map_dbl(clases,~nrow(.x)/nrow(X)) 
  )   
}

aprioris_c <- pi_k(X_c,clases_c)

#función para obtener las esperanzas por variable y clase es básicamente "colMeans". 
# es quizás la que aporta más fealdad a este programa, su salida es una lista de dataframes y es bastante incómoda de usar. 

mu_hat <- function(clases) {
	map(clases,colMeans)
}

esperanza_c <- mu_hat(clases_c)

#Para poder hallar los desvios necesito una función que le reste el mismo vector a todas las filas de una matriz.

restar_vector_a_filas <- function(X,v) {
	sustraendo <-  rep(v,each=nrow(X))
	return(X - sustraendo)
}


restada <- restar_vector_a_filas(clases_c[[1]],esperanza_c[[1]])

#no me sabía el nombre de la función que hace esto, también la necesito para los desvíos...

sumar <- function(a,b) {a+b}

# Esta función sirve para estimar la posible utilidad del ADL, ya que su hipótesis de funcionamiento es que las clases son normales con desvíos iguales. En el caso de estos datos, las normales van a terminar mostrando desvíos ampliamente distintos. 

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

#acá la uso para ver la hipótesis


razones_entre_desvios_de_dos_clases <- function(sigmas_por_clase) {
  sigmas_por_clase[[2]] / sigmas_por_clase[[1]]
}
sigmas_no_hat <- sigmas_por_clase(clases_c,esperanza_c)
razones_desvios <- razones_entre_desvios_de_dos_clases(sigmas_no_hat) 

# sólo long.diametro lo cumple bien, es la variable que hay que usar


# Esta función sirve para encontrar el vector con los desvíos estimados por clase. 

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


desvio_c <- sigma_hat(clases_c)

# esta es la función modelo de un discriminante. 

#discriminante_mono <- function( mu, sigma , apriori ) {
#	delta_k <- function(x) { x * (mu / (sigma^2)) - (mu^2)/(2*(sigma^2))  + log(apriori) } 
#}  

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

discriminantes_c <- discriminante(esperanza_c,desvio_c,aprioris_c)  

# Esta función toma los parámetros de una tabla y nos da los discriminantes para una variable elegida. Estos están en forma de lista, así que no se pueden evaluar directamente. 

#armar_discriminantes <- function(mu,sigma,apriori,variable) {
#  columna <- map(mu,~.x[variable])
#  columna  <- map(columna,unname)
#  deltas <- pmap(
#		 list(columna,apriori)
#		 ,~discriminante(..1,sigma[variable],..2)
#		 )
#  return(deltas)
#}
#
#delta_c <- armar_discriminantes(esperanza_c,desvio_c,aprioris_c,variable_c)


#esta función transforma la lista de discriminantes de una clase en una función que dado un dato nos da su clasificación de acuerdo a los discriminantes. 

armar_asignadora_de_clases <- function(deltas) {
  asignar <- function(x) {
    valor_de_los_delta <- map(deltas,~.x(x))
    seleccion <- which(valor_de_los_delta == max( as.double(valor_de_los_delta) ) )
    return( names(seleccion) )
 }
  return(asignar)
}

asignadora_c <- armar_asignadora_de_clases(discriminantes_c)


adl_para_dfs <- function( formula, df ) {
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
  return(predecir)
}

predecir_c <- adl_para_dfs(formula_c,abalone)
