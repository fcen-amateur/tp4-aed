library(tidyverse)

tipos_columnas <- cols(
  Sex = col_character(),
  Length = col_double(),
  Diameter = col_double(),
  Height = col_double(),
  WholeWeight = col_double(),
  SchuckedWeight = col_double(),
  VisceraWeight = col_double(),
  ShellWeight = col_double(),
  Rings = col_integer()
)

nombres_castellano <- c(
  "Sex" = "sexo",
  "Length" = "long.largo",
  "Diameter" = "long.diametro",
  "Height" = "long.altura",
  "WholeWeight" = "peso.total",
  "SchuckedWeight" = "peso.vaina",
  "VisceraWeight" = "peso.viscera",
  "ShellWeight" = "peso.caparazon",
  "Rings" = "anillos"
)

# Leo el CSV
read_csv("abalone.data", col_types = tipos_columnas) %>%
  plyr::rename(nombres_castellano) %>%
  # ¿Qué se pierde cuando desarmas un abalone?
  mutate(peso.dif = peso.total - peso.vaina - peso.viscera - peso.caparazon,
         adulto = sexo != "I") %>%
  select(adulto, anillos, long.largo:peso.dif) -> abalone

source("tp4-lib.R")

rlog1 <- rlog(adulto ~ anillos + long.diametro, abalone,
              alfa = 1e-5, epsilon = 1e-1, max_ciclos = 10000)

rlog2 <- rlog(adulto ~ anillos, abalone,
              alfa = 1e-5, epsilon = 1e-1)

rlog3 <- rlog(adulto ~ long.diametro, abalone,
              alfa = 1e-5, epsilon = 1e-1)

rlog4 <- rlog(adulto ~ scale(long.diametro), abalone,
              alfa = 1e-5, epsilon = 1e-1)


str(rlog1)
str(rlog2)
str(rlog3)
str(rlog4)

evolucion_parametros <- function(valores_beta) {
  valores_beta %>% t() %>% as_tibble() %>%
    gather(coef, valor) %>%
    group_by(coef) %>%
    mutate(x = row_number()) %>%
    ggplot(aes(x, valor, color = coef)) +
    geom_line()
}

evolucion_parametros(rlog3$valores_beta)
evolucion_parametros(rlog4$valores_beta)

grafica_roc <- function(aucroc) { aucroc %>% ggplot(aes(fpr, tpr)) + geom_line() }

roc3 <- aucroc(abalone$adulto, as.vector(rlog3$probs), seq(0, 1, 0.1))
roc4 <- aucroc(abalone$adulto, as.vector(rlog4$probs), seq(0, 1, 0.1))

grafica_roc(roc3)
grafica_roc(roc4)

roc3 <- aucroc(abalone$adulto, as.vector(rlog3$probs), seq(0, 1, 0.001))
grafica_roc(roc3)

tibble(x = as.vector(rlog3$probs)) %>% ggplot(aes(x)) + geom_histogram()
tibble(x = as.vector(rlog4$probs)) %>% ggplot(aes(x)) + geom_histogram()
mean(rlog3$probs)
mean(rlog4$probs)

##### Grafiquitos ####
GGally::ggpairs(abalone)

variables_explicativas <- names(select(abalone, anillos:peso.dif))

freqpoly <- function(df, var_y, var_x) {
  ggplot(df, aes_string(x = var_x, color = var_y)) +
    geom_freqpoly()
}

violin <- function(df, nombre_x, nombre_y) {
  ggplot(df, aes_string(x = nombre_x, y = nombre_y, color = nombre_x)) +
    geom_boxplot()
}

IMG_DIR = "imagenes/"
graficar_var <- function(df, var_x) {
  ggsave(paste(IMG_DIR, "boxplot_", var_x, ".png", sep = ""), violin(df, "adulto", var_x))
  ggsave(paste(IMG_DIR, "freqpoly_", var_x, ".png", sep = ""), freqpoly(df, "adulto", var_x))
}


walk(variables_explicativas, ~graficar_var(abalone, .))

abalone %>% 
  group_by(adulto) %>%
  summarise_all(mean)

abalone %>%
  ggplot(aes(long.altura, peso.total, color = adulto)) +
  geom_point()

abalone <- abalone %>%
  mutate(long.altura = ifelse(long.altura > 0.3, NA, long.altura))

# abalone %>% select(adulto:peso.dif)


abalone %>% 
  group_by(adulto) %>%
  summarise(q = n()) %>%
  mutate(p = q/sum(q))

abalone %>% 
  group_by(adulto) %>%
  summarise_if(is.numeric, mean, na.rm = T)

abalone %>%
  ggplot(aes(long.altura, sqrt(peso.caparazon), color = adulto)) +
  geom_jitter(alpha = 0.3) +
  guides(alpha = "none")
