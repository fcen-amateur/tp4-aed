
tasa_aciertos <- function(verdad, prediccion) {
  stopifnot(all(
    is_logical(verdad),
    is_logical(prediccion),
    length(verdad) == length(prediccion)))
  
  mean(verdad == prediccion)
}


clasificar_pred <- function(verdad, prediccion) {
  #' Dado un vector de respuestas `y` y uno de predicciones `pred`,
  #' devuelve un vector de clasificación de resultados en TP, TN, FP, FN.
  
  stopifnot(length(verdad) == length(prediccion))
  
  ifelse(verdad == pred,
         # La predicción es correcta
         ifelse(pred == T, "TP", "TN"),
         # La predicción es incorrecta
         ifelse(pred == T, "FP", "FN"))
}
