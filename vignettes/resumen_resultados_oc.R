

library(tidyverse)

df %>%
  mutate(
    modelo_desc = 
      map(modelo, deparse) %>%
      map_chr(str_c, collapse = ""),
    n_k =
      map(resultados, c("result", "yhat")) %>%
      map_int(length),
    tasa_acierto =
      map(resultados, "result") %>%
      map_dbl("tasa_acierto") %>%
      round(5)) %>%
  group_by(p, algo, modelo_desc) %>%
  summarise(
    tasa_pesada = weighted.mean(tasa_acierto, n_k)) -> df2

df2 %>%
  group_by(p, algo) %>%
  summarise(tasa_maxima = max(tasa_pesada)) %>%
  
  

write_csv(df2, "~/AnálisisExploratorio/tp4-aed/data/resumen_resultados.csv")
