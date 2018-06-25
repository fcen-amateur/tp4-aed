load("resultados-p8.RData")
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
      map_dbl("tasa_acierto")) %>%
  group_by(p, algo, modelo_desc) %>%
  summarise(
    tasa_pesada = weighted.mean(tasa_acierto, n_k)) %>%
  filter(rank(desc(tasa_pesada)) == 1) -> df2

df2 %>%
  ggplot(aes(p, tasa_pesada, color = algo)) +
  geom_line()

write_csv(df2, "data/resumen_resultados-p8.csv")
