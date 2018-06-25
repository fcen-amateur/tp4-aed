identificar_extremos  <-   function( formula, df) {
   variables <- all.vars(formula[[3]])
   df <- df[variables]
   extremos <-map2_df (df, names(df),
            ~list(columna = .y, minimo = min(.x), maximo = max(.x))
            )
   return(extremos)
}


armar_grilla <- function(extremos, lado) {
  x <- seq(extremos[[1,2]],extremos[[1,3]], length = lado) 
  y <- seq(extremos[[2,2]],extremos[[2,3]], length = lado) 
  grilla <-expand.grid(x,y)
  colnames(grilla) <- extremos$columna
  return(grilla)
}

agregar_columna_defasada  <- function(df, col){
  df[ paste0(col,"_corrido") ]  <- c (
                    df[col][seq(2,nrow(df)),] ,
                    F
                    )
  return(df)
}


es_frontera  <- function(fila) {
  ifelse(fila[[2]] == fila[[5]] && fila[[3]] != fila[[6]], T, F)
}


fronteras  <-  function(df) {
  frontera <- map_lgl(
                   seq( nrow(df) ),
                   ~ es_frontera( 
                                 df[.x,]
                                 )
                   )
  df["frontera"] <- frontera  
  return(df)
}

trazar_area <- function(formula, datos , algo ) {
# Dados un df, una fórmula de dos variables y el generador de predictor de un algoritmo, nos devuelve un gráfico de 2 dimensiones con su frontera. 
  extremos <- identificar_extremos(formula, datos)
  predictora <- (algo(formula,datos))$predecir
  grilla <- armar_grilla(extremos, lado = 40)
  grilla <- mutate(grilla, preds = predictora(grilla))
  variable <- colnames(grilla)
  for (v in variable) {
    grilla <- agregar_columna_defasada(grilla,v)
  }
  grilla <- (fronteras(grilla))[c(1,2,7)]
  grilla <- as_tibble(grilla) 
  grilla <- filter(grilla, frontera==T)
  return( grilla ) 
}


# Tuve que crear una función especial porque rlog depende de que el DF tenga todas las columnas, incluida la de y, acá lo engaño con una y de F. 

rlog_area <- function(formula, datos , algo ) {
# Dados un df, una fórmula de dos variables y el generador de predictor de un algoritmo, nos devuelve un gráfico de 2 dimensiones con su frontera. 
  extremos <- identificar_extremos(formula, datos)
  predictora <- (algo(formula,datos))$predecir
  grilla <- armar_grilla(extremos, lado = 40)
  grilla <- mutate(grilla, adulto = rep(FALSE))
  grilla <- mutate(grilla, preds = predictora(grilla))
  variable <- colnames(grilla)
  for (v in variable) {
    grilla <- agregar_columna_defasada(grilla,v)
  }
  grilla <- grilla[c(1,2,4,5,6,8)]
  grilla <- (fronteras(grilla))[c(1,2,7)]
  grilla <- as_tibble(grilla) 
  grilla <- filter(grilla, frontera==T)
  return( grilla ) 
}
