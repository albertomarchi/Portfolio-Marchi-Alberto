setwd("C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/Dati")

library(readxl)
library(dplyr)
library(WDI)
library(tidyr)

import_dependency <- read_excel("Energy import dependency.xlsx")
gdp_pc <- read_excel("real GDP per capita.xlsx")
re_consumption <- read_excel("Renewable Consumption.xlsx") 
RD_share_gdp <- read_excel("R_D.xlsx")
energy_productivity <- read_excel("Energy Productivity.xlsx")
Co2_pc <- read_excel("CO2 per capita.xls")
meteo_data <- read_xlsx("era5_country_year.xlsx")
Energy_consumption <- read_xlsx("Energy_Consumption.xlsx")
RE_share <- read_xlsx("RE share.xlsx") 
Population <- read_xlsx("Population.xlsx")
## R&D

RD_share_gdp <- RD_share_gdp %>%
  rename(country = `GEO (Labels)`)

RD_share_gdp <- RD_share_gdp %>%
  mutate(
    across(`2010`:`2024`, as.numeric)
  )

RD_share_gdplong <- RD_share_gdp %>%
  pivot_longer(
    cols = `2010`:`2024`,
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(year = as.integer(year))

rd_share_gdp <- RD_share_gdplong

rm(RD_share_gdp, RD_share_gdplong)

rd_share_gdp <- rd_share_gdp %>%
  rename(Country = country)

## CO2

str(Co2_pc)

Co2_pclong <- Co2_pc %>%
  pivot_longer(
    cols = `2010`:`2024`,
    names_to = "year",
    values_to = "Co2_pc"
  ) %>%
  mutate(year = as.integer(year))

co2_pc <- Co2_pclong

rm(Co2_pclong, Co2_pc)

co2_pc <- co2_pc %>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))

# Population

Population <- Population %>%
  mutate(
    across(`2010`:`2024`, as.numeric)
  )

Population_long <- Population %>%
  pivot_longer(
    cols = `2010`:`2024`,
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(year = as.integer(year))

Population <- Population_long

Population <- Population %>%
  rename(Country = country)

## energy productivity

energy_productivity <- energy_productivity %>%
  rename(country = `GEO (Labels)`)

energy_productivity <- energy_productivity %>%
  rename(energy_productivity = `...3`)

energy_productivity <- energy_productivity %>%
  rename(Country = country)


gdp_pc <- gdp_pc %>%
  rename(Country = GEO)

import_dependency <- import_dependency %>%
  rename(Country = GEO)

import_dependency <- import_dependency %>%
  rename(year = TIME)

gdp_pc <- gdp_pc %>%
  rename(year = TIME)

energy_productivity <- energy_productivity %>%
  rename(year = TIME)

RE_share <- RE_share %>%
  rename(Country = `GEO`)

RE_share <- RE_share %>%
  rename(year = `TIME`)


## Merge dati

str(re_consumption) # da mod
str(rd_share_gdp)
str(meteo_data)
str(import_dependency) # da mod
str(gdp_pc) # da mod
str(energy_productivity) # da mod
str(co2_pc)
str(Energy_consumption)

re_consumption <- re_consumption %>%
  mutate(year = as.integer(year))

import_dependency <- import_dependency %>%
  mutate(year = as.integer(year))

gdp_pc <- gdp_pc %>%
  mutate(year = as.integer(year))

energy_productivity <- energy_productivity %>%
  mutate(year = as.integer(year))

meteo_data <- meteo_data %>%
  mutate(year = as.integer(year))

rd_share_gdp <- rd_share_gdp %>%
  rename(RD_share_gdp = value)

Energy_consumption <- Energy_consumption %>%
  mutate(year = as.integer(year))

RE_share <- RE_share %>%
  mutate(year = as.integer(year))

Population <- Population %>%
  mutate(year = as.integer(year))

panel <- re_consumption

panel <- panel %>%
  left_join(gdp_pc,        by = c("Country", "year")) %>%
  left_join(energy_productivity, by = c("Country", "year")) %>%
  left_join(import_dependency, by = c("Country", "year")) %>%
  left_join(rd_share_gdp,         by = c("Country", "year")) %>%
  left_join(meteo_data,       by = c("Country", "year")) %>%
  left_join(co2_pc,       by = c("Country", "year")) %>%
  left_join(Energy_consumption,       by = c("Country", "year"))%>%
  left_join(RE_share,       by = c("Country", "year"))%>%
  left_join(Population, by = c("Country", "year"))

panel <- panel %>%
  rename(Population = value)

library(writexl)
write_xlsx(
  panel,
  "C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/Dati/dataset.xlsx"
)



