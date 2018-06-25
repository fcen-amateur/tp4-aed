library(GGally)
library(tidyverse)
source("R/leer_abalone.R")

abalone <- leer_abalone("data/abalone.data")

pares <- abalone %>%
  select(adulto:peso.caparazon) %>%
  ggpairs(mapping = aes(color = adulto, alpha = 0.1))

ggsave("imagenes/ggpairs-color.png", pares, width = 16, height = 9)
