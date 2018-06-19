library(glue)

generar_k_vecinos_con_k_optimo <- function(modelo, df) {
  # Extraemos los nombres de variables del `modelo`
  vars_xy <- all.vars(modelo)
  var_y <- vars_xy[1]
  vars_x <- vars_xy[-1]

  # Elegimos 20 valores de k entre 1 y sqrt(n) con n = tamaño del dataset
  valores_k <- seq(from = 1, to = ceiling(sqrt(nrow(df))),
                   length.out = min(20, nrow(df)))
  valores_k <- map_dbl(valores_k, ceiling)

  test_status <- sample(c(T, F), prob = c(.2, .8), size = nrow(df), replace = T)
  test_df <- df[test_status, vars_xy]
  train_df <- df[!test_status, vars_xy]

  crear_predictor_knn <- function(k) { k_vecinos(train_df, vars_x, var_y, k) }
  calcular_tasa_de_aciertos <- function(predicciones) {
    sum(test_df[[var_y]] == predicciones) / nrow(test_df)
  }
  crossval <- tibble(
    k = valores_k,
    predictor = map(k, crear_predictor_knn),
    y_hat = map(predictor, function(predictor) { predictor(test_df) }),
    tasa_de_aciertos = map_dbl(y_hat, calcular_tasa_de_aciertos)
  )

  k_optimo <- (crossval %>% arrange(desc(tasa_de_aciertos)) %>% head(1))[["k"]]
  mejor_knn <- k_vecinos(train_df, vars_x, var_y, k_optimo)
  return (list(predecir = mejor_knn))
}

k_vecinos <- function(train_df, vars_x, var_y, k) {
  #' Entrena un predictor de `k`-vecinos más cercanos a partir de los datos
  #' de `df`. Las variables predictoras son `vars_x` y la variable a estimar
  #' es `var_y`.
  #'
  #' Todas las variables predictoras son escaladas, centrando cada una en su
  #' media y dividiendo por el desvío estándar. Las observaciones también
  #' son escaladas antes de la clasificación.
  #'
  #' Devuelve una lista con:
  #'  - predecir (funcion): Función que dado un df de observaciones,
  #'      devuelve un vector de predicciones (los valores estimados para la
  #'      variable var_y).
  #'
  #' Ejemplo de uso:
  #'
  #'   > modelo <- foo ~ bar + baz
  #'   > knn <- k_vecinos(modelo, train_df)
  #'   > y_hat <- knn$predecir(test_df)

  # Escalamos el `train_df`. Guardamos las medias y desvíos para escalar también
  # las nuevas observaciones antes de clasificarlas:
  escalado <- train_df[vars_x] %>% scale
  train_df_escalado <- train_df %>% select(vars_xy)
  train_df_escalado[, vars_x] <- escalado

  medias <- attr(escalado, "scaled:center")
  desvios <- attr(escalado, "scaled:scale")

  calcular_distancias <- function(observacion_escalada) {
    # Calcula la distancia euclidea entre una observación y todos los
    # puntos del train_df_escalado OJO: Espera una observaión *ya escalada* !
    # Las distancias serán calculadas en un espacio que sólo considera
    # las variables X (vars_x).

    # m: número de observaciones en el df
    # n: número de covariables X

    obs_id <- observacion_escalada["obs_id"]
    train_matrix <- train_df_escalado[vars_x] %>% data.matrix # m x n
    obs_vector <- observacion_escalada[vars_x] # n x 1
    # Repetimos el vector de la observación en m filas, para comparar cada
    # una de esas repeticiones con una fila del training set:
    obs_matrix <- matrix(obs_vector, ncol = length(obs_vector),
                         nrow = nrow(train_df_escalado), byrow = T) # m x n
    diferencias_al_cuadrado <- (train_matrix - obs_matrix)^2 # m x n
    vector_de_unos <- rep(1, length(vars_x))
    vector_distancias <-
      ((diferencias_al_cuadrado %*% vector_de_unos)^(1/2))[, 1] # m x 1
    distancias <-
      train_df_escalado %>%
      mutate(distancia = vector_distancias, obs_id = as.integer(obs_id)) %>%
      rowid_to_column("train_id")

    # Distancias entre *una* observación y *todos* los puntos de training:
    return (distancias)
  }

  predecir <- function(test_df) {
    # Esta función presupone definidos: k, medias, desvios, vars_x, var_y
    # y una función calcular_distancias que usa un train_df.
    #
    # Dado un `test_df`, cada observación es clasificada según la categoría
    # de los k vecinos más cercanos.
    #
    # Devuelve un vector de valores para la var_y (estimaciones).

    # Escalamos cada nueva observación con media y sd de los datos de training
    test_df_escalado <-
      select(test_df, vars_x) %>%
      scale(center = medias, scale = desvios) %>%
      as_tibble %>%
      rowid_to_column("obs_id")
    test_df_escalado[[var_y]] <- NA

    print(glue("Prediciendo con k = {k}"))
    # Distancias entre *todas* las observaciones y *todos* los puntos de training:
    distancias <-
      bind_rows(apply(test_df_escalado, 1, calcular_distancias)) %>%
      arrange(obs_id, distancia)

    vecinos_mas_cercanos <-
      distancias %>%
      group_by(obs_id) %>%
      arrange(distancia) %>%
      do( head(., k) )

    votos_por_categoria <-
      vecinos_mas_cercanos %>%
      group_by_("obs_id", var_y) %>%
      summarise(votes = n(), prob = n() / k) %>%
      arrange(obs_id, votes)

    predicciones <-
      votos_por_categoria %>%
      group_by(obs_id) %>%
      do( head(., 1) ) # Si hay un empate, elige categoría aleatoriamente

    return (predicciones[[var_y]])
  }

  return (predecir)
}
