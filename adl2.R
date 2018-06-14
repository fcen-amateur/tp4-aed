library(tidyverse)

# para controlar las cantidades, realizo sample con repeat=F hasta agotar los elementos, obteniendo el vector desordenado y después apareo con los valores de pliegue repetidos cada uno n_k veces. Por último vuelvo al orden original.

plegar <- function ( datos, pliegues ) {
  n <- length( datos )
  reparto <- sample( datos, n, replace=F )
  return( 
    map( datos, ~match( .x, reparto) %/% (n/pliegues) + 1 )
    )
  }

estimar_esperanza <- function(x) {return(sum(x)/length(x)) }
estimar_varianza <- function(x,mu,K) {
  return( sum( x - mu^2 ) / ( length(x) - K ) )
  }

# el algoritmo puede ser vectorial, para cada clase 

# esta función nos da el vector de esperanzas para cada clase  


estimar_parametros <- function(var_y, var_x, df) {
  
abalone %>%
  select(var_y, var_x) %>%
  group_by_(var_y) %>%
  mutate_( "mu_hat" = mean(var_x),
          "var_x_centrada2" = (var_x - df$mu_hat)^2)
}
          
estimar_parametros("adulto", "anillos", abalone)
          
    data_escalada = map(data, scale),
    nk = map_dbl(data, nrow),
    pik = nk/(nrow(abalone)),
    mu_hat = map(data_escalada, function(x){attr(x, "scaled:center")}),
    sigma_hat = map(data_escalada, function(x){attr(x, "scaled:scale")})) %>%
  select(adulto, nk, pik, mu_hat, sigma_hat) -> medidas_clase

delta_x <- function(x, pik, mu_hat, sigma2_hat) {
  x * (mu_hat / sigma_hat^2) - (mu_hat^2 / (2 * sigma_hat^2)) + log(pik)
}


discriminar_linealmente <- function(X, pik, mu_hat, sigma_hat) {
  
  
}
  
  
#adl <- function( formula, df, escalador = base::scale, ...) {
#  var_y <- as.character(formula[[2]])
#  y <- df[[var_y]]
#  X <- model.matrix(formula,df)
#  X[,-c(1)] <- escalador( X[,-c(1)] )
#    } 
#  }
