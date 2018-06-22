generar_k_vecinos_con_k_optimo <- function(formula, df) {
  #' Generar un predictor de K-vecinos más cercanos a partir del modelo de
  #' `formula` y entrenado sobre el `df`.
  K_OPTIMO <- 1 # Testeado por separado
  return (list(predecir = k_vecinos(df, formula, K_OPTIMO)))
}

extraer_variables_de_formula <- function(formula) {
  vars_xy <- all.vars(formula)
  return (list(
    xy = vars_xy,
    y = vars_xy[1],
    X = vars_xy[-1]))
}

k_vecinos <- function(train_df, formula, k) {
  #' Entrena un predictor de `k`-vecinos más cercanos a partir de los datos
  #' de `df`. Las variables predictoras son tomadas de la `formula`.
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
  variables <- extraer_variables_de_formula(formula)
  escalado <- train_df[variables$X] %>% scale
  train_df_escalado <- train_df %>% select(variables$y, variables$X)
  train_df_escalado[, variables$X] <- escalado

  medias <- attr(escalado, "scaled:center")
  desvios <- attr(escalado, "scaled:scale")

  calcular_distancias <- function(observacion_escalada) {
    # Calcula la distancia euclidea entre una observación y todos los
    # puntos del train_df_escalado OJO: Espera una observaión *ya escalada* !
    # Las distancias serán calculadas en un espacio que sólo considera
    # las variables X.

    # m: número de observaciones en el df
    # n: número de covariables X
    obs_id <- observacion_escalada["obs_id"]
    train_matrix <- train_df_escalado[variables$X] %>% data.matrix # m x n
    obs_vector <- observacion_escalada[variables$X] # n x 1
    # Repetimos el vector de la observación en m filas, para comparar cada
    # una de esas repeticiones con una fila del training set:
    obs_matrix <- matrix(obs_vector, ncol = length(obs_vector),
                         nrow = nrow(train_df_escalado), byrow = T) # m x n
    diferencias_al_cuadrado <- (train_matrix - obs_matrix)^2 # m x n
    vector_de_unos <- rep(1, length(variables$X))
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
    #' Dado un `test_df`, cada observación es clasificada según la categoría
    #' de los k vecinos más cercanos. Devuelve un vector de estimaciones.

    # Escalamos cada nueva observación con media y sd de los datos de training
    test_df_escalado <-
      select(test_df, variables$X) %>%
      scale(center = medias, scale = desvios) %>%
      as_tibble %>%
      rowid_to_column("obs_id")

    vecinos_mas_cercanos <- function(distancias) {
      distancias %>%
        group_by(obs_id) %>%
        arrange(distancia) %>%
        do( head(., k) )
    }

    votos_por_categoria <- function(vecinos) {
      vecinos %>%
        group_by_("obs_id", variables$y) %>%
        summarise(conteo_votos = n(), prob = n() / k)
    }

    elegir_categoria_mas_votada <- function(votos) {
      categoria_mas_votada <-
        votos %>%
        group_by(obs_id) %>%
        arrange(desc(conteo_votos)) %>%
        do( head(., 1) )
      
      categoria_mas_votada[[variables$y]]
    }
    
    # mini_test_df_escalado <- test_df_escalado[1:10, ]
    print(str_c("KVCM trabajando con K = ", k))
    test_df_escalado <-
      test_df_escalado %>%
      mutate(
        distancias = pmap(., ~calcular_distancias(c(...))),
        k_vecinos = map(distancias, vecinos_mas_cercanos),
        votos = map(k_vecinos, votos_por_categoria),
        predicciones = map_lgl(votos, elegir_categoria_mas_votada)
      )

    return (test_df_escalado$predicciones)
  }

  return (predecir)
}

nombre_de_formula <- function(formula) {
  return (paste(deparse(formula), collapse = ""))
}

