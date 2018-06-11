k_vecinos <- function(formula, train_df, k) {
  #' Entrena un predictor de K-vecinos más cercanos a partir de todos los datos
  #' de `train_df`. Las variables predictoras y la variable a estimar son
  #' tomadas de la `formula`.
  #'
  #' Todas las variables predictoras son escaladas, centrando cada una en su
  #' media y dividiendo por el desvío estándar. Las observaciones también
  #' son escaladas antes de la clasificación.
  #'
  #' Devuelve una lista con:
  #'  - predictor (funcion): Función que dado un df de observaciones no
  #'      incluidas en el conjunto de entrenamiento, devuelve un vector
  #'      de predicciones.

  # Extraemos los nombres de variables de la `formula`
  agregar_sufijo <- function (s) { paste(s, "1", sep="") }
  var_y <- all.vars(formula)[1]
  vars_x <- all.vars(formula)[-1]
  var_y_con_sufijo <- agregar_sufijo(var_y)
  vars_x_con_sufijo <- sapply(vars_x, agregar_sufijo, USE.NAMES = F)

  # Escalamos el `train_df`. Guardamos las medias y desvíos para escalar también
  # las nuevas observaciones antes de clasificarlas:
  medias <- list()
  desvios <- list()
  train_df_escalado <- select_(train_df, var_y)
  for (var_x in vars_x) {
    x <- train_df[[var_x]]
    medias[[var_x]] <- mean(x)
    desvios[[var_x]] <- sd(x)
    train_df_escalado[[var_x]] <- ((x - medias[[var_x]]) / desvios[[var_x]])
  }

  # Construimos la función `predictor`, que dado un `test_df` usa al
  # `train_df` para clasificar cada observación.
  predictor <- function(test_df) {
    # Escalamos cada nueva observación con media y sd de los datos de training
    test_df_escalado <- select(test_df, vars_x)
    for (var_x in vars_x) {
      serie_escalada <- (test_df[[var_x]] - medias[[var_x]]) / desvios[[var_x]]
      test_df_escalado[[var_x]] <- serie_escalada
    }
    test_df_escalado[[var_y]] <- NA

    # Cruzamos cada nueva observación con cada dato de training
    distancias <-
      test_df_escalado %>%
      rowid_to_column("obs_id") %>%
      crossing(train_df_escalado)

    calcular_distancia <- function(obs_vs_dato) {
      observacion <- obs_vs_dato[vars_x]
      # Las columnas de training recibieron el sufijo "1":
      dato_train <- obs_vs_dato[vars_x_con_sufijo]
      dist(rbind(observacion, dato_train), method = "euclidean")[[1]]
    }

    distancias[["distancia"]] <- apply(distancias, 1, calcular_distancia)

    vecinos_mas_cercanos <-
      distancias %>%
      group_by(obs_id) %>%
      top_n(-k, distancia) %>% # Incluye empates!
      arrange(obs_id, distancia)

    votos <-
      vecinos_mas_cercanos %>%
      group_by_("obs_id", var_y_con_sufijo) %>%
      summarise(votes = n()) %>%
      arrange(obs_id)

    # FIXME: no resuelve los empates.
    # TODO: Comparar con una versión enlatada de knn
    result <- top_n(categorias, 1, votes)[[var_y_con_sufijo]]

    return (result)
  }

  return (list(predictor = predictor))
}
