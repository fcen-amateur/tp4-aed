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
rifa <- sample(seq(abalone[[var_y]]),2000,replace=F)
X_c <- abalone[rifa,]
y_c <- X_c[["adulto"]]

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

variable_c <- "long.diametro"

# Esta función sirve para encontrar el vector con los desvíos estimados por clase. 

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

delta_c <- armar_discriminantes(esperanza_c,desvio_c,aprioris_c,variable_c)


#esta función transforma la lista de discriminantes de una clase en una función que dado un dato nos da su clasificación de acuerdo a los discriminantes. 

armar_asignadora_de_clases <- function(deltas) {
  asignar <- function(x) {
    valor_de_los_delta <- map(deltas,~.x(x))
    seleccion <- which(valor_de_los_delta == max( as.double(valor_de_los_delta) ) )
    return( names(seleccion) )
 }
  return(asignar)
}

asignadora_c <- armar_asignadora_de_clases(delta_c)

# integrando sin ninguna clase de elegancia o calidad lo antes programado llegamos a lo pactado: una función que dado un df y dos parámetros (dato a predecir, dato predictor) entrena una función predictora

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


#la función "clasificadora_segun_variable" construye una función cuya salida es un vector compuesto por las categorías de "nombre.objetivo" a las cuales pertenece cada elemento de "nombre.predictor" del parámetro df. Esta puede mapearse sobre una columna de un df para tener una columna de predicciones.


X_a <- abalone[rifa,]
clasificadora_ldiametro <- clasificadora_segun_variable( X_a, "long.diametro", "adulto" )


abalone_val <- abalone[-rifa,]

abalone2 <- mutate(abalone_val, predicc.ldiametr = map(long.diametro , clasificadora_ldiametro))

# la tasa de acierto no es como para volverse loco, da un 22% de errores

aciertos <- which(abalone2$predicc.ldiametr == abalone2$adulto)
errores <- which(abalone2$predicc.ldiametr != abalone2$adulto)

porcentaje_de_errores <- length(errores)/nrow(abalone2)


# finalmente, agregué una con una forma más similar a lo que habíamos hablado
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

adl_viscera <- adl(abalone,adulto ~ peso.viscera)
abalone_cumbiero <- mutate(abalone_val, predicciones = map(abalone_val$peso.viscera,adl_visceras)) 

#aprovechamos que las categorías son lógicas y usamos "as.logic" para que sea más inmediato comparar las columnas


aplicar_adl <- function(df,adl,predictor) {
  predictor_q <- enquo(predictor)
  nombre_columna <- paste0( "predicciones.",quo_name( predictor_q ) ) 
  df_preds <- mutate (
	  df,
	  !! nombre_columna := map_lgl(df[[predictor]],~ .x %>% adl %>% as.logical )
	  )
  return(df_preds)
}

abalone_aplicado <- aplicar_adl(abalone_val,adl_viscera,"peso.viscera")
