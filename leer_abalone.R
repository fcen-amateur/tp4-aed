library(tidyverse)

leer_abalone <- function(filename) {
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
    "SchuckedWeight" = "peso.desenvainado",
    "VisceraWeight" = "peso.viscera",
    "ShellWeight" = "peso.caparazon",
    "Rings" = "anillos"
  )
  
  read_csv(filename, col_types = tipos_columnas) %>%
    plyr::rename(nombres_castellano) %>%
    # ¿Qué se pierde cuando desarmas un abalone?
    mutate(peso.dif = peso.total - peso.desenvainado - peso.viscera - peso.caparazon,
           adulto = sexo != "I") %>%
    rowid_to_column("id") %>%
    select(id, adulto, anillos, long.largo:peso.dif)
}
