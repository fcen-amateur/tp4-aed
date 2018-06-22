library(tidyverse)

source("R/leer_abalone.R")
source("R/k_vecinos.R")

abalone <- leer_abalone("data/abalone.data")

cross_validar_k_y_modelos <- function(modelos, valores_k, df) {
  test_status <- sample(c(T, F), prob = c(.2, .8), size = nrow(df), replace = T)
  test_df <- df[test_status, ]
  train_df <- df[!test_status, ]
  
  crear_predictor_knn <- function(k, modelo) {
    k_vecinos(train_df, modelo, k)
  }
  calcular_tasa_de_aciertos <- function(predicciones) {
    sum(test_df[[variables$y]] == predicciones) / nrow(test_df)
  }
  
  crossval <-
    cross_df(list(
      k = valores_k,
      modelo = modelos)) %>%
    mutate(
      nombre_modelo = map_chr(modelo, nombre_de_formula),
      funcion_predecir = map2(k, modelo, crear_predictor_knn),
      y_hat = map(funcion_predecir, function(predecir) { predecir(test_df) }),
      tasa_de_aciertos = map_dbl(y_hat, calcular_tasa_de_aciertos)
    )
  
  return (crossval)
}

df <- abalone
modelos <- list(
  adulto ~ peso.viscera,
  adulto ~ anillos + peso.total,
  adulto ~ peso.total + long.diametro,
  adulto ~ anillos + peso.viscera + long.diametro,
  adulto ~ anillos + peso.viscera + long.diametro + long.largo + long.altura
)
valores_k <- seq(1, 199, 2)
crossval <- cross_validar_k_y_modelos(modelos, valores_k, df)
tasa_por_k <-
  crossval %>%
  group_by(k) %>%
  summarise(tasa_media = mean(tasa_de_aciertos),
            tasa_q50 = median(tasa_de_aciertos)) %>%
  arrange(desc(tasa_media))

k_optimo <- head(tasa_por_k, 1)$k
mejor_tasa_media <- round(head(tasa_por_k, 1)$tasa_media, 2)

tasa_por_k %>%
  ggplot() +
  aes(x = k, y = tasa_media, color = tasa_q50) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = mejor_tasa_media, linetype = "dotted") +
  geom_vline(xintercept = k_optimo, linetype = "dotted") +
  annotate("text", x = k_optimo, y = mejor_tasa_media + .01,
           label = (str_c("Tasa media = ", as.character(mejor_tasa_media)))) +
  ggtitle(str_c("Mejor K para K-vecinos = ", k_optimo))

save.image("busqueda_k_optimo.RData")
