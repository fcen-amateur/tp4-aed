library(tidyverse)

set.seed(42)

for (archivo in file.path("R", dir("R"))) {
  source(archivo)
}

abalone <- leer_abalone("data/abalone.data")

generar_formulas <- function(p, y_nombre, X_nombre) {
  lados_derechos <- unlist(
      combn(x = X_nombre, m = p,
            simplify = F, FUN = paste, collapse = " + "))
  
  map(str_c(y_nombre, "~", lados_derechos, sep = " "), as.formula)
}

nombres_predictores <- abalone %>%
  select(anillos:peso.caparazon) %>%
  names

max_p <- 8

formulas <- map(seq_len(max_p), generar_formulas,
    y_nombre = "adulto", X_nombre = nombres_predictores) %>%
  imap(~ tibble("p" = .y, "formula" = .x)) %>%
  bind_rows


algoritmos <- list(
  #"qda" = qda,
  "rlog" = rlog,
  "adc" = generar_el_predictor_adc,
  "kvmc" = generar_k_vecinos_con_k_optimo,
  "adl" = generar_el_predictor_adl
)

pliegos <-  10

abalone <- abalone %>%
#  sample_n(100) %>%
  mutate(pliego = sample(pliegos, n(), replace = T))


get_train <- function(k, df) { df %>% filter(pliego != k) }
get_test <-  function(k, df) { df %>% filter(pliego == k) }

asistente_modelado_pliego <- function(algo, modelo, k, df, verbose = F) {
    #' Entrena el `modelo` usando el algoritmo `algo` sobre el pliego `k`, 
    #' aprendiendo sobre k-1 pliegos de df, y prediciendo sobre el pliego `k`.
  if (verbose) {
    print(str_c(algo, deparse(modelo), k, sep = " | "))
    cat("Entrenando... ")
  }
  
  t0 <- Sys.time()
  llamada <- algoritmos[[algo]](modelo, get_train(k, df))
  t1 <- Sys.time()
  t_entrenar <- t1 - t0
  
  predictora <- llamada$predecir
  
  if (verbose) {
    print(t_entrenar)
    cat("Prediciendo... ")
  }
  
  t0 <- Sys.time()
  yhat <- predictora(get_test(k, df))
  t1 <- Sys.time()
  t_predecir <- t1 - t0
  
  if (verbose) {
    print(t_predecir)
  }
  
  tasa_acierto <- tasa_aciertos(
    get_test(k, df)[[y_nombre]], yhat)
  
  return(lst(llamada, predictora, yhat, tasa_acierto,
             t_entrenar, t_predecir))
}

y_nombre <- "adulto"
X_nombre <- names(
  abalone %>% select(anillos:peso.caparazon))

espacio_modelos <- tibble(
  p = 1:max_p,
  modelo = map(p, generar_formulas, y_nombre, X_nombre))

cross_df(
  list(
    k = seq_len(pliegos),
    algo = names(algoritmos))) %>%
  crossing(espacio_modelos) %>%
  unnest(modelo) %>%
#  mutate(orden = runif(n())) %>%
#  arrange(orden) %>%
  mutate(
    resultados = pmap(
      list(algo, modelo, k),
      safely(asistente_modelado_pliego),
      df = abalone, verbose = T)) -> df

save.image("resultados-p8.RData")
