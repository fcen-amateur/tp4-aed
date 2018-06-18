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


#función para obtener las probabilidades apriori de pertenecer a una clase:

pi_k <- function(X,clases) {
  return(
    map_dbl(clases,~nrow(.x)/nrow(X)) 
  )   
}


#función para obtener las esperanzas por variable y clase es "colMeans". 

mu_hat <- function(clases) {
	map(clases,colMeans)
}


#Para poder hallar los desvios necesito una función que le reste el mismo vector a todas las filas de una matriz.

restar_vector_a_filas <- function(X,v) {
	sustraendo <-  rep(v,each=nrow(X))
	return(X - sustraendo)
}

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

razones_entre_desvios_de_dos_clases <- function(sigmas_por_clase) {
  sigmas_por_clase[[2]] / sigmas_por_clase[[1]]
}

# Esta función sirve para encontrar el vector con los desvíos estimados por clase, que es parte del proceso de hacer ADL. 

sigma_hat <- function(clases,esperanzas) {
	n <- reduce (
		  map(clases,nrow),
		  sumar
		  )
	K <- length(clases)
	diferencias_al_cuadrado <- map2_df(clases,esperanzas, 
			 ~colSums(
				 ( (restar_vector_a_filas(..1, ..2))^2)/(n - K)
				 )
			 )
	desvios <- rowSums( as.data.frame (diferencias_al_cuadrado)) 
	names(desvios) <- names(clases[[1]]) 
	return(desvios)
}

# esta es la función modelo de un discriminante. 

discriminante <- function( mu, sigma , apriori ) {
	delta_k <- function(x) { x * (mu / (sigma^2)) - (mu^2)/(2*(sigma^2))  + log(apriori) } 
}  

# Esta función toma los parámetros de una tabla y nos da los discriminantes para una variable elegida. Estos están en forma de lista, así que no se pueden evaluar directamente. 

armar_discriminantes <- function(mu,sigma,apriori,variable) {
  columna <- map(mu,~.x[variable])
  columna  <- map(columna,unname)
  deltas <- pmap(
		 list(columna,apriori)
		 ,~discriminante(..1,sigma[variable],..2)
		 )
  return(deltas)
}

#esta función transforma la lista de discriminantes de una clase en una función que dado un dato nos da su clasificación de acuerdo a los discriminantes. 

armar_asignadora_de_clases <- function(deltas) {
  asignar <- function(x) {
    valor_de_los_delta <- map(deltas,~.x(x))
    seleccion <- which(valor_de_los_delta == max( as.double(valor_de_los_delta) ) )
    return( names(seleccion) )
 }
  return(asignar)
}


# integrando sin ninguna clase de elegancia o calidad lo antes programado llegamos a lo pactado: una función que dado un df y dos parámetros (dato a predecir, dato predictor) entrena una función predictora

#la función "clasificadora_segun_variable" construye una función cuya salida es un vector compuesto por las categorías de "nombre.objetivo" a las cuales pertenece cada elemento de "nombre.predictor" del parámetro df. Esta puede mapearse sobre una columna de un df para tener una columna de predicciones.

clasificadora_segun_variable <- function(df, nombre.predictor, nombre.objetivo) {
  nombre.predictor <- as.character(nombre.predictor)
  nombre.objetivo <- as.character(nombre.objetivo)
  x <- df[,which(colnames(abalone) != nombre.objetivo)]
  y <- df[[nombre.objetivo]]
  clases <- partir_en_clases(x,y)
  mu_hat <- mu_hat(clases)
  sigma_hat <- sigma_hat(clases,mu_hat)
  aprioris <- pi_k(x,clases)
  discriminantes <- armar_discriminantes(mu_hat,sigma_hat,aprioris,nombre.predictor)
  asignadora <- armar_asignadora_de_clases(discriminantes)
  return(asignadora)
}

# dada una formula que explica una variable segun un predictor, esta función entrena una clasificadora por discriminantes lineales. 

adl <- function(df, formula) {
  nombre.predictor <- as.character(formula[[3]])
  nombre.objetivo <- as.character(formula[[2]])
  x <- df[,which(colnames(abalone) != nombre.objetivo)]
  y <- df[[nombre.objetivo]]
  clases <- partir_en_clases(x,y)
  mu_hat <- mu_hat(clases)
  sigma_hat <- sigma_hat(clases,mu_hat)
  aprioris <- pi_k(x,clases)
  discriminantes <- armar_discriminantes(mu_hat,sigma_hat,aprioris,nombre.predictor)
  asignadora <- armar_asignadora_de_clases(discriminantes)
  return(asignadora)
}

aplicar_adl <- function(df,adl,predictor) {
  predictor_q <- enquo(predictor)
  nombre_columna <- paste0( "predicciones.",quo_name( predictor_q ) ) 
  df_preds <- mutate (
	  df,
	  !! nombre_columna := map_lgl(df[[predictor]],~ .x %>% adl %>% as.logical )
	  )
  return(df_preds)
}
