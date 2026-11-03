library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

dataset <- read_xlsx("dataset.xlsx")

eu <- st_read("C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/EU_27_FINAL.gpkg")

eu_country <- eu %>%
  group_by(NAME_ENGL) %>%
  summarise(geometry = st_union(geom), .groups = "drop")

