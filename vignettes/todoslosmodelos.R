library(tidyverse)

for (archivo in file.path("R", dir("R"))) {
  source(archivo)
}

nombres_predictores <- abalone %>%
  select(anillos:peso.caparazon) %>%
  names
max_p <- 2

listado_formulas <- vector("list", max_p)

for (i in seq_len(max_p)) {
  nuevas_formulas <- paste(
    "adulto ~",
    combn(
      x = predictores, m = i,
      simplify = F, FUN = paste, collapse = " + "))
  
  listado_formulas[[i]] <- tibble (p = i, formula_chr = nuevas_formulas)
}

formulas <- bind_rows(listado_formulas) %>%
  mutate(formula = map(formula_chr, as.formula))


algoritmos <- list(
  "rlog" = rlog,
  "qda" = qda
)

pliegos <- 1

abalone <- abalone %>%
  mutate(pliego = sample(pliegos, n(), replace = T))

get_train <- function(k) { abalone %>% filter(pliego != k) }
get_test <-  function(k) { abalone %>% filter(pliego == k) }

abalone %>% group_by(pliego) %>% tally()

asistente_modelado <- function(nombre_algo, formula, k) {
  print(paste(nombre_algo, formula))
  algoritmos[[nombre_algo]](formula, get_train(k))
}

crossing(
  tibble(nombre_algo = names(algoritmos), algo = algoritmos),
  formulas,
  tibble(k = seq_len(pliegos))
  ) %>% arrange(rnorm(nrow(.))) %>%
  mutate(
    call = pmap(
      list(nombre_algo, formula, k),
      asistente_modelado),
    predecir = map(call, "predecir")) -> df