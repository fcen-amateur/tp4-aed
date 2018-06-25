# creamos una grilla de puntos que barre toda la región en franjas horizontales. Estos van a ser clasificados. Los que sean frontera van a ser identificados y sirven para trazar más tarde mediante poligonales las áreas de influencia.

load('/home/octavio/AnálisisExploratorio/tp4-aed/pruebagraficos.RData')

setwd("~/AnálisisExploratorio/tp4-aed/")
library("tidyverse")
source("R/leer_abalone.R")
source("vignettes/demoADL.R")
source("R/k_vecinos.R")
source("R/ADL.R")
source("R/OD-ADC.R")



# es de notar que expand.grid me da la función ordenada según la segunda variablie. Esto hace más convieniente optar en primera instancia por un barrido vertical.  

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


# a esta altura, añado la columna de predicciones y el df pasa a tener una tercera columna. 
# El objetivo ahora es elegir para cada valor de x fijo aquellos y donde pred(x,y) cambia de valor respecto al previo. 


# las y me dan la línea que estoy barriendo. Si dos datos tienen la misma y, x contiguas (de esto se hace cargo el corrimiento de la columna) y diferente predicción, uno de ellos fue el punto de cambio. 
#Para ver esto, "adoso" un df similar adelantado un lugar y aplico una función que evalua para cada fila si y e ycorrida son iguales y si las predicciones son distintas vale T. 

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

trazar_area <- function(formula,datos,algo) {
# Dados un df, una fórmula de dos variables y el generador de predictor de un algoritmo, nos devuelve un gráfico de 2 dimensiones con su frontera. 
  extremos <- identificar_extremos(formula, datos)
  predictora <- (algo(formula,datos))$predecir
  grilla <- armar_grilla(extremos, lado = 100)
  grilla <- mutate(grilla, preds = predictora(grilla))
  variables <- colnames(grilla)
  for (v in variables) {
    grilla  <- agregar_columna_defasada(grilla,v) 
  }
  grilla <- (fronteras(grilla))[c(1,2,7)]
  grilla <- as_tibble(grilla) 
  return( grilla ) 
}

#grilla_kvcm <- trazar_area(adulto ~ anillos + peso.viscera, abalone, generar_k_vecinos_con_k_optimo) 
grilla_adl <- trazar_area(adulto ~ anillos + peso.viscera, abalone, generar_el_predictor_adl) 
grilla_adc <- trazar_area(adulto ~ anillos + peso.viscera, abalone, generar_el_predictor_adc) 

