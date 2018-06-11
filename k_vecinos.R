k_vecinos <- function(formula, train_df, k, weighted_vote = F) {
  #' Entrena un predictor de K-vecinos más cercanos a partir de todos los datos
  #' de `train_df`.
  #'
  #' Escala todas las variables centrando cada una en su media y dividiendo
  #'  por el desvío estándar.
  #'
  #' Por defecto, gana la categoría que obtiene el "majority vote".
  #' Si `weigthed_vote` = T, los `k` vecinos más cercanos aportan un voto de
  #' peso inversamente proporcional a la distancia a la observación.
  #'
  #' Devuelve una lista con:
  #'  - predictor (funcion): Función que dado un df de observaciones no
  #'      incluidas en el conjunto de entrenamiento, devuelve un vector
  #'      de predicciones.

  # Extraemos los nombres de variables de la `formula`
  agregar_sufijo <- function (s) { glue("{s}1") }
  var_y <- all.vars(formula)[1]
  vars_x <- all.vars(formula)[-1]
  var_y_con_sufijo <- agregar_sufijo(var_y)
  vars_x_con_sufijo <- sapply(vars_x, agregar_sufijo, USE.NAMES = F)

  # Escalamos el `train_df`. Guardamos las medias y desvíos para escalar también
  # las nuevas observaciones antes de clasificarlas:
  medias <- list()
  desvios <- list()
  train_df_escalado <- select(train_df, vars_x, var_y) # Y como última columna
  for (var_x in vars_x) {
    x <- train_df[[var_x]]
    media <- mean(x)
    desvio <- sd(x)

    medias[[var_x]] <- media
    desvios[[var_x]] <- desvio
    train_df_escalado[[var_x]] <- ((x - media) / desvio)
  }

  # Construimos la función `predictor`, que dado un `test_df` usa al
  # `train_df` para clasificar cada observación.
  predictor <- function(test_df) {
    test_df[[var_y]] <- NA
    test_df_escalado <- select(test_df, vars_x, var_y)
    # Escalamos cada nueva observación con media y sd de los datos de training
    for (var_x in vars_x) {
      test_df_escalado[[var_x]] <- (
        (test_df[[var_x]] - medias[[var_x]]) / desvios[[var_x]]
      )
    }
    # Cruzamos cada nueva observación con cada dato de training
    distancias <-
      test_df_escalado %>%
      rowid_to_column("obs_id") %>%
      crossing(train_df_escalado)

    calcular_distancia <- function(test_df_row) {
      dato_nuevo <- test_df_row[vars_x]
      # Las columnas de training recibieron sufijo "1":
      dato_train <- test_df_row[vars_x_con_sufijo]
      dist(rbind(dato_nuevo, dato_train))[[1]]
    }

    distancias[["distancia"]] <- apply(distancias, 1, calcular_distancia)
    distancias[["peso"]] <- 1/distancias[["distancia"]]

    categorias <-
      distancias %>%
      group_by(obs_id) %>%
      top_n(-k, distancia) %>%
      group_by_("obs_id", var_y_con_sufijo) %>%
      summarise(votes = n(), weighted_votes = sum(peso)) %>%
      arrange(obs_id)

    # FIXME: la version weighted_vote tiene Inf colados cuando la distancia es 0
    # FIXME: la version !weighted_vote no resuelve los empates.
    # TODO: Comparar con una versión enlatada de knn
    if (weighted_vote) {
      result <- top_n(categorias, 1, weighted_votes)[[var_y_con_sufijo]]
    } else {
      result <- top_n(categorias, 1, votes)[[var_y_con_sufijo]]
    }
    return (result)
  }

  return (list(predictor = predictor))
}
