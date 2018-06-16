
library(tidyverse)

tipos_columnas <- cols(
  Sex = col_character(),
  Length = col_double(),
  Diameter = col_double(),
  Height = col_double(),
  WholeWeight = col_double(),
  SchuckedWeight = col_double(),
  VisceraWeight = col_double(),
  ShellWeight = col_double(),
  Rings = col_integer()
)

nombres_castellano <- c(
  "Sex" = "sexo",
  "Length" = "long.largo",
  "Diameter" = "long.diametro",
  "Height" = "long.altura",
  "WholeWeight" = "peso.total",
  "SchuckedWeight" = "peso.desenvainado",
  "VisceraWeight" = "peso.viscera",
  "ShellWeight" = "peso.caparazon",
  "Rings" = "anillos"
)

# Leo el CSV
read_csv("abalone.data", col_types = tipos_columnas) %>%
  plyr::rename(nombres_castellano) %>%
  # ¿Qué se pierde cuando desarmas un abalone?
  mutate(peso.dif = peso.total - peso.desenvainado - peso.viscera - peso.caparazon,
         adulto = sexo != "I") %>%
  select(adulto, anillos, long.largo:peso.dif) -> abalone

set.seed(42)

formula <- adulto ~ anillos + long.diametro
var_y <- as.character(formula[[2]])
y <- abalone[[var_y]]
X <- model.matrix( formula, abalone )
X <- X[,-c(1)] 

rifa <- sample(seq(abalone[[var_y]]),1000,replace=F)
X_c <- X[rifa,]
y_c <- y[rifa]

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

#función para obtener las esperanzas por variable y clase. 

mu_hat <- function(clases) {
	map(clases,colMeans)
}

esperanza_c <- mu_hat(clases_c)

restar_vector_a_filas <- function(X,v) {
	sustraendo <-  rep(v,each=nrow(X))
	return(X - sustraendo)
}

variable_c <- "long.diametro"
restada <- restar_vector_a_filas(clases_c[[1]],esperanza_c[[1]])

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

desvio_c <- sigma_hat(clases_c,esperanza_c)

discriminante <- function( mu, sigma , apriori ) {
	delta_k <- function(x) { x * (mu / (sigma^2)) - (mu^2)/(2*(sigma^2))  + log(apriori) } 
}  

armar_discriminantes <- function(mu,sigma,apriori,variable) {
  columna <- map(mu,~.x[variable])
  columna  <- map(columna,unname)
  deltas <- pmap(
		 list(columna,apriori)
		 ,~discriminante(..1,sigma[variable],..2)
		 )
  return(deltas)
}

delta_c <- armar_discriminantes(esperanza_c,desvio_c,aprioris_c,variable_c)

armar_asignadora_de_clases <- function(deltas) {
  asignar <- function(x) {
    valor_de_los_delta <- map(deltas,~.x(x))
    seleccion <- which(valor_de_los_delta == max( as.double(valor_de_los_delta) ) )
    return( names(seleccion) )
 }
  return(asignar)
}

asignadora_c <- armar_asignadora_de_clases(delta_c)


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

  x_g <- abalone[,which(colnames(abalone) != "adulto")]
  y_g <- abalone$"adulto"
  clases_g <- partir_en_clases(x_g,y_g)
  mu_hat_g <- mu_hat(clases_g)
  sigma_hat_g <- sigma_hat(clases_g,mu_hat_g)
  aprioris_g <- pi_k(x_g,clases_g)
  discriminantes_g <- armar_discriminantes(mu_hat_g,sigma_hat_g,aprioris_g,"long.diametro")
  asignadora_g <- armar_asignadora_de_clases(discriminantes_g)
  salida_g <- map( x_g$"long.diametro", asignadora_g )

#la función "clasificadora_segun_variable" construye una función cuya salida es un vector compuesto por las categorías de "nombre.objetivo" a las cuales pertenece cada elemento de "nombre.predictor" del parámetro df. Esta puede mapearse sobre una columna de un df para tener una columna de predicciones.


X_a <- abalone[rifa,]
clasificadora_ldiametro <- clasificadora_segun_variable( X_a, "long.diametro", "adulto" )

abalone2 <- mutate(abalone, predicc.ldiametr = map(long.diametro , clasificadora_ldiametro))
