library(readxl)
library(dplyr)
library(corrplot)
library(tidyr)
library(lmtest)
library(sandwich)
library(plm)     # panel models + Hausman
library(car)     # VIF

dataset <- read_xlsx("dataset.xlsx")

pdata <- pdata.frame(dataset, index = c("Country", "year"))


# 2) matrice di correlazione tra regressori
X <- dataset %>% 
  select(RE_cons, Real_GDP_pc, energy_productivity,
         Import_dependency, RD_share_gdp,
         Co2_pc, Energy_cons)

# Rinomina variabili (abbreviate in italiano)
colnames(X) <- c(
  "RE",
  "PIL_pc",
  "Prod_Ener",
  "Dip_Ener",
  "R&S",
  "CO2_pc",
  "Cons_Ener"
)

# Calcola matrice di correlazione
cor_mat <- cor(X, use = "pairwise.complete.obs")
round(cor_mat, 3)

# Grafico
corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.8
)

# 3) TEST COLLINEARITÀ (formale): VIF
# Nota: VIF si calcola su una regressione "pooled"/OLS classica (screening pratico)
ols <- lm(RE_cons ~ Real_GDP_pc + energy_productivity + Import_dependency + RD_share_gdp + Co2_pc + Energy_cons, data = pdata)
vif(ols)

# 4) FE vs POOLED: F-test (esistono effetti individuali?)
fe_mod <- plm(RE_cons ~ Real_GDP_pc + energy_productivity + Import_dependency + RD_share_gdp + Co2_pc + Energy_cons, data = pdata, model = "within")
pool_mod <- plm(RE_cons ~ Real_GDP_pc + energy_productivity + Import_dependency + RD_share_gdp + Co2_pc + Energy_cons, data = pdata, model = "pooling")
pFtest(fe_mod, pool_mod)

# 5) RE model
re_mod <- plm(RE_cons ~ Real_GDP_pc + energy_productivity + Import_dependency + RD_share_gdp + Co2_pc + Energy_cons, data = pdata, model = "random")

# 6) HAUSMAN: FE vs RE
phtest(fe_mod, re_mod)

# 7) (Consigliato) Stime FE con SE cluster per paese
# (robustezza standard: eterosched + autocorr intra-paese)
library(lmtest)
library(sandwich)

coeftest(fe_mod, vcov = vcovHC(fe_mod, type = "HC1", cluster = "group"))

# regressioni FE baseline

install.packages("fixest")
library(fixest)

install.packages(c("modelsummary", "tinytable", "openxlsx"))
library(modelsummary)


pdata <- pdata %>%
  mutate(
    l_RE_cons      = log(RE_cons),
    l_Energy_cons  = log(Energy_cons),
    l_GDP_pc       = log(Real_GDP_pc)
  )

pdata$Co2_pc_lag1 <- lag(pdata$Co2_pc, 1)

#  modelli interazioni post2022

yr <- as.integer(as.character(index(pdata)$year))

pdata$post2022 <- as.integer(yr >= 2022)
pdata$post2021 <- as.integer(yr >= 2021)


FE1_2022 <- plm(
  l_RE_cons ~ energy_productivity + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    post2022:Import_dependency +
    post2022:energy_productivity +
    post2022:RD_share_gdp +
    post2022:Co2_pc_lag1,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary(FE1_2022)

## 2

FE2_2022 <- plm(
  l_RE_cons ~ l_GDP_pc + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    post2022:Import_dependency +
    post2022:l_GDP_pc +
    post2022:RD_share_gdp +
    post2022:Co2_pc_lag1,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary(FE2_2022)



## 3

FE3_2022 <- plm(
  l_RE_cons ~ l_Energy_cons + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    post2022:Import_dependency +
    post2022:l_Energy_cons +
    post2022:RD_share_gdp +
    post2022:Co2_pc_lag1,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary(FE3_2022)

summary_cluster_country <- function(mod){
  coeftest(mod, vcov = vcovHC(mod, type = "HC1", cluster = "group"))
}


# esempi (metti i tuoi modelli qui)
summary_cluster_country(FE1_2022)
summary_cluster_country(FE2_2022)
summary_cluster_country(FE3_2022)

modelsummary(
  list(
    "(1)" = FE1_2022,
    "(2)" = FE2_2022,
    "(3)" = FE3_2022
  ),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = c(
    "energy_productivity" = "Produttività energetica",
    "l_GDP_pc"            = "Log PIL pro capite",
    "l_Energy_cons"       = "Log consumo energetico",
    "RD_share_gdp"        = "Spesa in R&S (% del PIL)",
    "Import_dependency"   = "Dipendenza energetica (%)",
    "Co2_pc_lag1"         = "Emissioni di CO₂ pro capite (t−1)",
    
    "Import_dependency:post2022"   = "Post × Dipendenza energetica (%)",
    
    "energy_productivity:post2022" = "Post × Produttività energetica",
    
    "l_GDP_pc:post2022"            = "Post × Log PIL pro capite",
    
    "l_Energy_cons:post2022"       = "Post × Log consumo energetico",
    
    "RD_share_gdp:post2022"        = "Post × Spesa in R&S",
    
    "Co2_pc_lag1:post2022"         = "Post × Emissioni di CO₂ pro capite (t−1)"
  ),
  title = "Driver economici e di policy post-2022 (FE)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi di paese e anno inclusi.",
    "Errori standard model-based (non robusti)."
  ),
  output = "FE_post2022_standard_SE.docx"
)

vcov_FE1_2022 <- vcovHC(FE1_2022, type = "HC1", cluster = "group")
vcov_FE2_2022 <- vcovHC(FE2_2022, type = "HC1", cluster = "group")
vcov_FE3_2022 <- vcovHC(FE3_2022, type = "HC1", cluster = "group")

modelsummary(
  list(
    "(1)" = FE1_2022,
    "(2)" = FE2_2022,
    "(3)" = FE3_2022
  ),
  vcov = list(vcov_FE1_2022, vcov_FE2_2022, vcov_FE3_2022),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = c(
    "energy_productivity" = "Produttività energetica",
    "l_GDP_pc"            = "Log PIL pro capite",
    "l_Energy_cons"       = "Log consumo energetico",
    "RD_share_gdp"        = "Spesa in R&S (% del PIL)",
    "Import_dependency"   = "Dipendenza energetica (%)",
    "Co2_pc_lag1"         = "Emissioni di CO₂ pro capite (t−1)",
    
    "Import_dependency:post2022"   = "Post × Dipendenza energetica (%)",
    
    "energy_productivity:post2022" = "Post × Produttività energetica",
    
    "l_GDP_pc:post2022"            = "Post × Log PIL pro capite",
    
    "l_Energy_cons:post2022"       = "Post × Log consumo energetico",
    
    "RD_share_gdp:post2022"        = "Post × Spesa in R&S",
    
    "Co2_pc_lag1:post2022"         = "Post × Emissioni di CO₂ pro capite (t−1)"
  ),
  title = "Driver economici e di policy post-2022 (FE)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi di paese e anno inclusi.",
    "Errori standard robusti, clusterizzati a livello paese (HC1)."
  ),
  output = "FE_post2022_clustered_SE.docx"
)



## post-2021

FE1_2021 <- plm(
  l_RE_cons ~ energy_productivity + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    post2021:Import_dependency +
    post2021:energy_productivity +
    post2021:RD_share_gdp +
    post2021:Co2_pc_lag1,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary(FE1_2021)


FE2_2021 <- plm(
  l_RE_cons ~ l_GDP_pc + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    post2021:Import_dependency +
    post2021:l_GDP_pc +
    post2021:RD_share_gdp +
    post2021:Co2_pc_lag1,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary(FE2_2021)

FE3_2021 <- plm(
  l_RE_cons ~ l_Energy_cons + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    post2021:Import_dependency +
    post2021:l_Energy_cons +
    post2021:RD_share_gdp +
    post2021:Co2_pc_lag1,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary(FE3_2021)

summary_cluster_country(FE1_2021)
summary_cluster_country(FE2_2021)
summary_cluster_country(FE3_2021)

modelsummary(
  list(
    "(1)" = FE1_2021,
    "(2)" = FE2_2021,
    "(3)" = FE3_2021
  ),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = c(
    "energy_productivity" = "Produttività energetica",
    "l_GDP_pc"            = "Log PIL pro capite",
    "l_Energy_cons"       = "Log consumo energetico",
    "RD_share_gdp"        = "Spesa in R&S (% del PIL)",
    "Import_dependency"   = "Dipendenza energetica (%)",
    "Co2_pc_lag1"         = "Emissioni di CO₂ pro capite (t−1)",
    
    "Import_dependency:post2021"   = "Post × Dipendenza energetica (%)",
    
    "energy_productivity:post2021" = "Post × Produttività energetica",
    
    "l_GDP_pc:post2021"            = "Post × Log PIL pro capite",
    
    "l_Energy_cons:post2021"       = "Post × Log consumo energetico",
    
    "RD_share_gdp:post2021"        = "Post × Spesa in R&S",
    
    "Co2_pc_lag1:post2021"         = "Post × Emissioni di CO₂ pro capite (t−1)"
  ),
  title = "Driver economici e di policy post-2021 (FE)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi di paese e anno inclusi.",
    "Errori standard model-based (non robusti)."
  ),
  output = "FE_post2021_standard_SE.docx"
)

vcov_FE1_2021 <- vcovHC(FE1_2021, type = "HC1", cluster = "group")
vcov_FE2_2021 <- vcovHC(FE2_2021, type = "HC1", cluster = "group")
vcov_FE3_2021 <- vcovHC(FE3_2021, type = "HC1", cluster = "group")

modelsummary(
  list(
    "(1)" = FE1_2021,
    "(2)" = FE2_2021,
    "(3)" = FE3_2021
  ),
  vcov = list(vcov_FE1_2021, vcov_FE2_2021, vcov_FE3_2021),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = c(
    "energy_productivity" = "Produttività energetica",
    "l_GDP_pc"            = "Log PIL pro capite",
    "l_Energy_cons"       = "Log consumo energetico",
    "RD_share_gdp"        = "Spesa in R&S (% del PIL)",
    "Import_dependency"   = "Dipendenza energetica (%)",
    "Co2_pc_lag1"         = "Emissioni di CO₂ pro capite (t−1)",
    
    "Import_dependency:post2021"   = "Post × Dipendenza energetica (%)",
    
    "energy_productivity:post2021" = "Post × Produttività energetica",
    
    "l_GDP_pc:post2021"            = "Post × Log PIL pro capite",
    
    "l_Energy_cons:post2021"       = "Post × Log consumo energetico",
    
    "RD_share_gdp:post2021"        = "Post × Spesa in R&S",
    
    "Co2_pc_lag1:post2021"         = "Post × Emissioni di CO₂ pro capite (t−1)"
  ),
  title = "Driver economici e di policy post-2021 (FE)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi di paese e anno inclusi.",
    "Errori standard robusti, clusterizzati a livello paese (HC1)."
  ),
  output = "FE_post2021_clustered_SE.docx"
)


#interazioni clima post2022

# medie climatiche PRE-shock (2010–2021)

pdata$year_num <- as.integer(as.character(index(pdata)$year))

clim_pre <- pdata %>%
  filter(year_num <= 2021) %>%
  group_by(Country) %>%
  summarise(
    solar_pre = mean(solar, na.rm = TRUE),
    wind_pre  = mean(wind,  na.rm = TRUE),
    temp_pre  = mean(temp,  na.rm = TRUE)
  )

pdata <- left_join(pdata, clim_pre, by = "Country")

# standardizzazione (z-score)

# medie pre-shock
mu_wind  <- mean(pdata$wind_pre[pdata$year_num <= 2021],  na.rm = TRUE)
mu_solar <- mean(pdata$solar_pre[pdata$year_num <= 2021], na.rm = TRUE)
mu_temp  <- mean(pdata$temp_pre[pdata$year_num <= 2021],  na.rm = TRUE)

# deviazioni standard pre-shock
sd_wind  <- sd(pdata$wind_pre[pdata$year_num <= 2021],  na.rm = TRUE)
sd_solar <- sd(pdata$solar_pre[pdata$year_num <= 2021], na.rm = TRUE)
sd_temp  <- sd(pdata$temp_pre[pdata$year_num <= 2021],  na.rm = TRUE)


pdata$wind_pre_z  <- (pdata$wind_pre  - mu_wind)  / sd_wind
pdata$solar_pre_z <- (pdata$solar_pre - mu_solar) / sd_solar
pdata$temp_pre_z  <- (pdata$temp_pre  - mu_temp)  / sd_temp



clima_FE_1 <- plm(
  l_RE_cons ~ energy_productivity + Import_dependency +
    RD_share_gdp + Co2_pc_lag1 + post2022:temp_pre_z + post2022:wind_pre_z + post2022:solar_pre_z,
  data = pdata,
  model = "within",
  effect = "twoways"
)
summary(clima_FE_1)

## 2

clima_FE_2 <- plm(
  l_RE_cons ~ l_GDP_pc + Import_dependency +
    RD_share_gdp + Co2_pc_lag1 + post2022:temp_pre_z + post2022:wind_pre_z + post2022:solar_pre_z,
  data = pdata,
  model = "within",
  effect = "twoways"
)
  summary(clima_FE_2)
  
## 3

clima_FE_3 <- plm(
  l_RE_cons ~ l_Energy_cons + Import_dependency +
    RD_share_gdp + Co2_pc_lag1 + post2022:temp_pre_z + post2022:wind_pre_z + post2022:solar_pre_z,
  data = pdata,
  model = "within",
  effect = "twoways"
)
summary(clima_FE_3)



coef_map_clima <- c(
  "l_GDP_pc"            = "Log PIL pro capite",
  "energy_productivity" = "Produttività energetica",
  "RD_share_gdp"        = "Spesa in R&S (% PIL)",
  "Co2_pc_lag1"         = "CO₂ pro capite (t−1)",
  "l_Energy_cons"       = "Log consumo energetico",
  "Import_dependency"   = "Dipendenza energetica (%)",
  
  "post2022:temp_pre_z"   = "Post × Temperatura",
  
  "wind_pre_z:post2022"   = "Post × Vento",
  
  "post2022:solar_pre_z"  = "Post × Radiazione solare"
)

modelsummary(
  list(
    "(1)" = clima_FE_1,
    "(2)" = clima_FE_2,
    "(3)" = clima_FE_3
  ),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = coef_map_clima,
  title = "Driver climatici post-shock (FE, std.error standard)",
  notes = c(
    "Variabile dipendente: Log del consumo di energia rinnovabile.",
    "Effetti fissi paese e anno inclusi.",
    "Le variabili climatiche sono medie pre-shock a livello paese (2010-2021) e sono standardizzate (z-score).
    I coefficienti delle interazioni Post × Clima misurano l’effetto di un aumento di una deviazione standard del potenziale climatico nel periodo post-shock."
  ),
  output = "clima_FE_SEnormal.docx"
)

summary_cluster_country(clima_FE_1)
summary_cluster_country(clima_FE_2)
summary_cluster_country(clima_FE_3)


vcov_clima_1 <- vcovHC(clima_FE_1, type = "HC1", cluster = "group")
vcov_clima_2 <- vcovHC(clima_FE_2, type = "HC1", cluster = "group")
vcov_clima_3 <- vcovHC(clima_FE_3, type = "HC1", cluster = "group")



modelsummary(
  list(
    "(1)" = clima_FE_1,
    "(2)" = clima_FE_2,
    "(3)" = clima_FE_3
  ),
  vcov = list(vcov_clima_1, vcov_clima_2, vcov_clima_3),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = coef_map_clima,
  title = "Driver climatici post-shock (FE: specificazioni, SE cluster paese)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi paese e anno inclusi (two-way FE).",
    "Errori standard robusti clusterizzati per paese (HC1).",
    "Le variabili climatiche sono medie pre-shock a livello paese (2010-2021) e sono standardizzate (z-score). 
     I coefficienti delle interazioni Post × Clima misurano l’effetto di un aumento di una deviazione standard del potenziale climatico nel periodo post-shock."
  ),
  output = "clima_FE_clustered_SE.docx"
)


#interazioni clima post2021

clima_2021 <- plm(
  l_RE_cons ~ energy_productivity + Import_dependency +
    RD_share_gdp + Co2_pc_lag1 + post2021:temp_pre_z + post2021:wind_pre_z + post2021:solar_pre_z,
  data = pdata,
  model = "within",
  effect = "twoways"
)
summary(clima_2021)

## 2

clima_2021_2 <- plm(
  l_RE_cons ~ l_GDP_pc + Import_dependency +
    RD_share_gdp + Co2_pc_lag1 + post2021:temp_pre_z + post2021:wind_pre_z + post2021:solar_pre_z,
  data = pdata,
  model = "within",
  effect = "twoways"
)
summary(clima_2021_2)

## 3

clima_2021_3 <- plm(
  l_RE_cons ~ l_Energy_cons + Import_dependency +
    RD_share_gdp + Co2_pc_lag1 + post2021:temp_pre_z + post2021:wind_pre_z + post2021:solar_pre_z,
  data = pdata,
  model = "within",
  effect = "twoways"
)
summary(clima_2021_3)



coef_map_clima_2021 <- c(
  "l_GDP_pc"            = "Log PIL pro capite",
  "energy_productivity" = "Produttività energetica",
  "RD_share_gdp"        = "Spesa in R&S (% PIL)",
  "Co2_pc_lag1"         = "CO₂ pro capite (t−1)",
  "l_Energy_cons"       = "Log consumo energetico",
  "Import_dependency"   = "Dipendenza energetica (%)",
  
  "post2021:temp_pre_z"   = "Post × Temperatura",
  
  "wind_pre_z:post2021"   = "Post × Vento",
  
  "post2021:solar_pre_z"  = "Post × Radiazione solare"
)

modelsummary(
  list(
    "(1)" = clima_2021,
    "(2)" = clima_2021_2,
    "(3)" = clima_2021_3
  ),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = coef_map_clima_2021,
  title = "Driver climatici post-shock (FE: SE stendard)",
  notes = c(
    "Variabile dipendente: Log del consumo di energia rinnovabile.",
    "Effetti fissi paese e anno inclusi (two-way FE).",
    "Errori standard robusti clusterizzati per paese (HC1).",
    "Le variabili climatiche sono medie pre-shock a livello paese (2010-2021) e sono standardizzate (z-score).
    I coefficienti delle interazioni Post × Clima misurano l’effetto di un aumento di una deviazione standard del potenziale climatico nel periodo post-shock."
  ),
  output = "clima_FE2021_normalSE.docx"
)

summary_cluster_country(clima_2021)
summary_cluster_country(clima_2021_2)
summary_cluster_country(clima_2021_3)

vcov_clima_1 <- vcovHC(clima_2021, type = "HC1", cluster = "group")
vcov_clima_2 <- vcovHC(clima_2021_2, type = "HC1", cluster = "group")
vcov_clima_3 <- vcovHC(clima_2021_3, type = "HC1", cluster = "group")

modelsummary(
  list(
    "(1)" = clima_2021,
    "(2)" = clima_2021_2,
    "(3)" = clima_2021_3
  ),
  vcov = list(vcov_clima_1, vcov_clima_2, vcov_clima_3),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = coef_map_clima_2021,
  title = "Driver climatici post-shock (FE: SE cluster paese)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi paese e anno inclusi (two-way FE).",
    "Errori standard robusti clusterizzati per paese (HC1).",
    "Le variabili climatiche sono medie pre-shock a livello paese (2010-2021) e sono standardizzate (z-score). 
     I coefficienti delle interazioni Post × Clima misurano l’effetto di un aumento di una deviazione standard del potenziale climatico nel periodo post-shock."
  ),
  output = "clima_FE2021_clusteredSE.docx"
)


# dipendente consumo RE per capita

pdata$RE_cons_pc <- pdata$RE_cons / pdata$Population

pdata <- pdata %>%
  mutate(RE_cons_pc = as.integer(RE_cons_pc))

pdata$RE_cons_pc <- pdata$RE_cons / (pdata$Population / 1e6)

hist(pdata$RE_cons_pc)

pdata$l_RE_cons_pc <- log(pdata$RE_cons_pc)

hist(pdata$l_RE_cons_pc)


FE_pc_1_2022 <- plm(
  l_RE_cons_pc ~ energy_productivity + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    Import_dependency:post2022 +
    energy_productivity:post2022 +
    RD_share_gdp:post2022 +
    Co2_pc_lag1:post2022,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

FE_pc_2_2022 <- plm(
  l_RE_cons_pc ~ l_GDP_pc + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    Import_dependency:post2022 +
    l_GDP_pc:post2022 +
    RD_share_gdp:post2022 +
    Co2_pc_lag1:post2022,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

FE_pc_3_2022 <- plm(
  l_RE_cons_pc ~ l_Energy_cons + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    Import_dependency:post2022 +
    l_Energy_cons:post2022 +
    RD_share_gdp:post2022 +
    Co2_pc_lag1:post2022,
  data  = pdata,
  model = "within",
  effect = "twoways"
)


summary_cluster_country(FE_pc_1_2022)
summary_cluster_country(FE_pc_2_2022)
summary_cluster_country(FE_pc_3_2022)

vcov_FE1_pc_2022 <- vcovHC(FE_pc_1_2022, type = "HC1", cluster = "group")
vcov_FE2_pc_2022 <- vcovHC(FE_pc_2_2022, type = "HC1", cluster = "group")
vcov_FE3_pc_2022 <- vcovHC(FE_pc_3_2022, type = "HC1", cluster = "group")

modelsummary(
  list(
    "(1)" = FE_pc_1_2022,
    "(2)" = FE_pc_2_2022,
    "(3)" = FE_pc_3_2022
  ),
  vcov = list(vcov_FE1_pc_2022, vcov_FE2_pc_2022, vcov_FE3_pc_2022),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = c(
    "energy_productivity" = "Produttività energetica",
    "l_GDP_pc"            = "Log PIL pro capite",
    "l_Energy_cons"       = "Log consumo energetico",
    "RD_share_gdp"        = "Spesa in R&S (% del PIL)",
    "Import_dependency"   = "Dipendenza energetica (%)",
    "Co2_pc_lag1"         = "Emissioni di CO₂ pro capite (t−1)",
    
    "Import_dependency:post2022"   = "Post × Dipendenza energetica (%)",
    
    "energy_productivity:post2022" = "Post × Produttività energetica",
    
    "l_GDP_pc:post2022"            = "Post × Log PIL pro capite",
    
    "l_Energy_cons:post2022"       = "Post × Log consumo energetico",
    
    "RD_share_gdp:post2022"        = "Post × Spesa in R&S",
    
    "Co2_pc_lag1:post2022"         = "Post × Emissioni di CO₂ pro capite (t−1)"
  ),
  title = "Driver economici e di policy post-2022 (FE)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi di paese e anno inclusi.",
    "Errori standard robusti, clusterizzati a livello paese (HC1)."
  ),
  output = "FE_post2022_robustness.docx"
)


FE_pc_1_2021 <- plm(
  l_RE_cons_pc ~ energy_productivity + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    Import_dependency:post2021 +
    energy_productivity:post2021 +
    RD_share_gdp:post2021 +
    Co2_pc_lag1:post2021,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

FE_pc_2_2021 <- plm(
  l_RE_cons_pc ~ l_GDP_pc + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    Import_dependency:post2021 +
    l_GDP_pc:post2021 +
    RD_share_gdp:post2021 +
    Co2_pc_lag1:post2021,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

FE_pc_3_2021 <- plm(
  l_RE_cons_pc ~ l_Energy_cons + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    Import_dependency:post2021 +
    l_Energy_cons:post2021 +
    RD_share_gdp:post2021 +
    Co2_pc_lag1:post2021,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary_cluster_country(FE_pc_1_2021)
summary_cluster_country(FE_pc_2_2021)
summary_cluster_country(FE_pc_3_2021)

vcov_FE1_pc_2021 <- vcovHC(FE_pc_1_2021, type = "HC1", cluster = "group")
vcov_FE2_pc_2021 <- vcovHC(FE_pc_2_2021, type = "HC1", cluster = "group")
vcov_FE3_pc_2021 <- vcovHC(FE_pc_3_2021, type = "HC1", cluster = "group")

modelsummary(
  list(
    "(1)" = FE_pc_1_2021,
    "(2)" = FE_pc_2_2021,
    "(3)" = FE_pc_3_2021
  ),
  vcov = list(vcov_FE1_pc_2021, vcov_FE2_pc_2021, vcov_FE3_pc_2021),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = c(
    "energy_productivity" = "Produttività energetica",
    "l_GDP_pc"            = "Log PIL pro capite",
    "l_Energy_cons"       = "Log consumo energetico",
    "RD_share_gdp"        = "Spesa in R&S (% del PIL)",
    "Import_dependency"   = "Dipendenza energetica (%)",
    "Co2_pc_lag1"         = "Emissioni di CO₂ pro capite (t−1)",
    
    "Import_dependency:post2021"   = "Post × Dipendenza energetica (%)",
    
    "energy_productivity:post2021" = "Post × Produttività energetica",
    
    "l_GDP_pc:post2021"            = "Post × Log PIL pro capite",
    
    "l_Energy_cons:post2021"       = "Post × Log consumo energetico",
    
    "RD_share_gdp:post2021"        = "Post × Spesa in R&S",
    
    "Co2_pc_lag1:post2021"         = "Post × Emissioni di CO₂ pro capite (t−1)"
  ),
  title = "Driver economici e di policy post-2021 (FE)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile normalizzata pro capite (GWh ogni mln di abitanti).",
    "Effetti fissi di paese e anno inclusi.",
    "Errori standard robusti, clusterizzati a livello paese (HC1)."
  ),
  output = "FE_post2021_robustness.docx"
)

# clima

clima_FE1_2022 <- plm(
  l_RE_cons_pc ~ energy_productivity + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    wind_pre_z:post2022 +
    solar_pre_z:post2022 +
    temp_pre_z:post2022,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

clima_FE2_2022 <- plm(
  l_RE_cons_pc ~ l_GDP_pc + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    wind_pre_z:post2022 +
    solar_pre_z:post2022 +
    temp_pre_z:post2022,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

clima_FE3_2022 <- plm(
  l_RE_cons_pc ~ l_Energy_cons + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    wind_pre_z:post2022 +
    solar_pre_z:post2022 +
    temp_pre_z:post2022,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary_cluster_country(clima_FE1_2022)
summary_cluster_country(clima_FE2_2022)
summary_cluster_country(clima_FE3_2022)

vcov_clima1_pc_2022 <- vcovHC(clima_FE1_2022, type = "HC1", cluster = "group")
vcov_clima2_pc_2022 <- vcovHC(clima_FE2_2022, type = "HC1", cluster = "group")
vcov_clima3_pc_2022 <- vcovHC(clima_FE3_2022, type = "HC1", cluster = "group")

modelsummary(
  list(
    "(1)" = clima_FE1_2022,
    "(2)" = clima_FE2_2022,
    "(3)" = clima_FE3_2022
  ),
  vcov = list(vcov_clima1_pc_2022, vcov_clima2_pc_2022, vcov_clima3_pc_2022),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = coef_map_clima,
  title = "Driver climatici post-shock (FE: specificazioni, SE cluster paese)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi paese e anno inclusi (two-way FE).",
    "Errori standard robusti clusterizzati per paese (HC1).",
    "Le variabili climatiche sono medie pre-shock a livello paese (2010-2021) e sono standardizzate (z-score). 
     I coefficienti delle interazioni Post × Clima misurano l’effetto di un aumento di una deviazione standard del potenziale climatico nel periodo post-shock."
  ),
  output = "clima_FE2022_Robustness.docx"
)

clima_FE1_2021 <- plm(
  l_RE_cons_pc ~ energy_productivity + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    wind_pre_z:post2021 +
    solar_pre_z:post2021 +
    temp_pre_z:post2021,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

clima_FE2_2021 <- plm(
  l_RE_cons_pc ~ l_GDP_pc + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    wind_pre_z:post2021 +
    solar_pre_z:post2021 +
    temp_pre_z:post2021,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

clima_FE3_2021 <- plm(
  l_RE_cons_pc ~ l_Energy_cons + RD_share_gdp +
    Co2_pc_lag1 + Import_dependency +
    wind_pre_z:post2021 +
    solar_pre_z:post2021 +
    temp_pre_z:post2021,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

summary_cluster_country(clima_FE1_2021)
summary_cluster_country(clima_FE2_2021)
summary_cluster_country(clima_FE3_2021)

vcov_clima1_pc_2021 <- vcovHC(clima_FE1_2021, type = "HC1", cluster = "group")
vcov_clima2_pc_2021 <- vcovHC(clima_FE2_2021, type = "HC1", cluster = "group")
vcov_clima3_pc_2021 <- vcovHC(clima_FE3_2021, type = "HC1", cluster = "group")

modelsummary(
  list(
    "(1)" = clima_FE1_2021,
    "(2)" = clima_FE2_2021,
    "(3)" = clima_FE3_2021
  ),
  vcov = list(vcov_clima1_pc_2021, vcov_clima2_pc_2021, vcov_clima3_pc_2021),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "IC|Log|Adj|RMSE",
  coef_map = coef_map_clima_2021,
  title = "Driver climatici post-shock (FE: SE cluster paese)",
  notes = c(
    "Variabile dipendente: log del consumo di energia rinnovabile.",
    "Effetti fissi paese e anno inclusi (two-way FE).",
    "Errori standard robusti clusterizzati per paese (HC1).",
    "Le variabili climatiche sono medie pre-shock a livello paese (2010-2021) e sono standardizzate (z-score). 
     I coefficienti delle interazioni Post × Clima misurano l’effetto di un aumento di una deviazione standard del potenziale climatico nel periodo post-shock."
  ),
  output = "clima_FE2021_Robustness.docx"
)
