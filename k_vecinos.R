entrenar_k_vecinos <- function(modelo, train_df) {
  #' Entrena un predictor de K-vecinos más cercanos a partir de todos los datos
  #' de `train_df`. Las variables predictoras y la variable a estimar son
  #' tomadas del `modelo`, una formula de la forma:
  #'
  #'   var_y ~ var_x1 + var_x2 + ... + var_xn
  #'
  #' Todas las variables predictoras son escaladas, centrando cada una en su
  #' media y dividiendo por el desvío estándar. Las observaciones también
  #' son escaladas antes de la clasificación.
  #'
  #' Devuelve una lista con:
  #'  - predecir (funcion): Función que dado un df de observaciones no
  #'      incluidas en el conjunto de entrenamiento, devuelve un vector
  #'      de predicciones.
  #'
  #' Ejemplo de uso:
  #'   > modelo <- var_y ~ var_x1 + var_x2 + var_x3
  #'   > knn <- entrenar_k_vecinos(modelo, train_df)
  #'   > resultado <- knn$predecir(test_df = df, k = 5)
  #'   > resultado$y_hat # predicciones
  #'   > resultado$probs # probabilidades de las predicciones
  #'   > resultado$df # test_df aumentado

  # Extraemos los nombres de variables de la `modelo`
  vars_xy <- all.vars(modelo)
  var_y <- vars_xy[1]
  vars_x <- vars_xy[-1]

  # Escalamos el `train_df`. Guardamos las medias y desvíos para escalar también
  # las nuevas observaciones antes de clasificarlas:
  escalado <- train_df[vars_x] %>% scale
  train_df_escalado <- train_df %>% select(vars_xy)
  train_df_escalado[, vars_x] <- escalado

  medias <- attr(escalado, "scaled:center")
  desvios <- attr(escalado, "scaled:scale")

  calcular_distancias <- function(observacion) {
    # Calcula la distancia euclidea entre una observación y todos los
    # puntos del training set

    # m: número de observaciones de training set
    # n: número de covariables X

    obs_id <- observacion["obs_id"]
    train_matrix <- train_df_escalado[vars_x] %>% data.matrix # m x n
    obs_vector <- observacion[vars_x] # n x 1
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

  predecir <- function(test_df, k) {
    # Dado un `test_df` y un valor de `k`, usa al `train_df` para clasificar
    # cada observación según la categoría de los k vecinos más cercanos.
    #
    # Devuelve una lista con:
    #  - df: el test set con las predicciones y sus probas
    #  -

    # Escalamos cada nueva observación con media y sd de los datos de training
    test_df_escalado <-
      select(test_df, vars_x) %>%
      scale(center = medias, scale = desvios) %>%
      as_tibble %>%
      rowid_to_column("obs_id") %>%
      mutate_(var_y = NA)

    # Distancias entre *todas* las observaciones y *todos* los puntos de training:
    distancias <-
      bind_rows(apply(test_df_escalado, 1, calcular_distancias)) %>%
      arrange(obs_id, distancia)

    vecinos_mas_cercanos <-
      distancias %>%
      group_by(obs_id) %>%
      arrange(distancia) %>%
      do( head(., k) )

    votos <-
      vecinos_mas_cercanos %>%
      group_by_("obs_id", var_y) %>%
      summarise(votes = n(), prob = n() / k) %>%
      arrange(obs_id, votes)

    predicciones <-
      votos %>%
      group_by(obs_id) %>%
      do( head(., 1) ) # Si hay un empate, desambigua aleatoriamente

    test_df_con_predicciones <- test_df
    # FIXME: mutate_ se rompía:
    test_df_con_predicciones[[var_y]] <- predicciones[[var_y]]
    test_df_con_predicciones[["probs"]] <- predicciones[["probs"]]

    return (list(
      k = k,
      vecinos = vecinos_mas_cercanos,
      votos = votos,
      predicciones = predicciones,
      df = test_df_con_predicciones,
      probs = test_df_con_predicciones[["probs"]],
      y_hat = test_df_con_predicciones[[var_y]]
    ))
  }

  return(list(predecir = predecir))
}
