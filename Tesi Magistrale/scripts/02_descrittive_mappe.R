library(sf)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(readxl)
library(readxl)
library(tidyr)
library(plm)
library(tibble)
library(summarytools)

dataset <- read_xlsx("dataset.xlsx")

eu <- st_read("C:/Users/albio/OneDrive/Desktop/Tesi Magistrale/EU_27_FINAL.gpkg")

eu_country <- eu %>%
  group_by(NAME_ENGL) %>%
  summarise(geometry = st_union(geom), .groups = "drop")

eu_country <- st_as_sf(eu_country)


## Tabelle descrittive tutte le variabili

vars <- c(
  "RE_cons",
  "Real_GDP_pc",
  "energy_productivity",
  "solar",
  "wind",
  "temp",
  "Import_dependency",
  "Energy_cons",
  "Co2_pc",
  "RD_share_gdp"
)

descr(dataset[, vars])

## SD BETWEEN E WITHIN
panel_sd <- function(x, id){
  xbar_i <- ave(x, id, FUN = function(z) mean(z, na.rm = TRUE))
  tibble(
    Overall_SD = sd(x, na.rm = TRUE),
    Between_SD = sd(xbar_i, na.rm = TRUE),
    Within_SD  = sd(x - xbar_i, na.rm = TRUE),
    N          = sum(!is.na(x))
  )
}

table2 <- dataset %>%
  select(Country, all_of(vars)) %>%
  pivot_longer(-Country, names_to = "Variable", values_to = "value") %>%
  group_by(Variable) %>%
  group_modify(~panel_sd(.x$value, .x$Country)) %>%
  ungroup()

table2


## MAPPA var. dipendente

num_vars <- dataset %>%
  select(where(is.numeric)) %>%
  colnames()

num_vars <- setdiff(num_vars, "year")

dataset_pre <- dataset %>%
  filter(year >= 2010, year <= 2020) %>%
  group_by(Country) %>%
  summarise(
    across(
      all_of(num_vars),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

dataset_post <- dataset %>%
  filter(year >= 2021, year <= 2024) %>%
  group_by(Country) %>%
  summarise(
    across(
      all_of(num_vars),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

map_pre <- eu_country %>%
  left_join(dataset_pre, by = c("NAME_ENGL" = "Country"))

map_post <- eu_country %>%
  left_join(dataset_post, by = c("NAME_ENGL" = "Country"))

map_pre <- map_pre %>%
  mutate(
    REcons_class = cut(
      RE_cons,
      breaks = c(-Inf, 20000, 40000, 80000, Inf),
      labels = c("< 20k", "20k–40k", "40k–80k", "> 80k"),
      include.lowest = TRUE
    )
  )

ggplot(map_pre) +
  geom_sf(
    aes(fill = REcons_class),
    color = "grey30",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    name = "Renewable energy consumption\n(mean 2010–2020)",
    values = c(
      "< 20k"      = "#deebf7",
      "20k–40k"    = "#9ecae1",
      "40k–80k"    = "#3182bd",
      "> 80k"      = "#08519c"
    ),
    drop = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

## trend share

trend_ue <- dataset %>%
  group_by(year) %>%
  summarise(RE_Share = mean(RE_Share, na.rm = TRUE), .groups = "drop")

library(ggplot2)

ggplot(data = dataset, aes(x = year, y = RE_Share)) +
  
  # Area post-2021
  annotate("rect",
           xmin = 2021, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  
  geom_vline(xintercept = 2021,
             linetype = "dashed",
             linewidth = 0.8) +
  
  annotate("text",
           x = 2021.3,
           y = max(dataset$RE_Share, na.rm = TRUE),
           label = "Shock energetico (2021)",
           hjust = 0,
           vjust = 1,
           size = 3.4) +
  
  scale_x_continuous(
    breaks = seq(min(dataset$year),
                 max(dataset$year),
                 by = 2)
  ) +
  
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = "Quota di energia rinnovabile sul consumo finale",
    subtitle = "Media campionaria UE, 2010–2023",
    x = NULL,
    y = "Quota rinnovabili",
    caption = "Nota: linea tratteggiata indica l'inizio della crisi energetica (2021)."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.minor = element_blank()
  )
  )

## scatterplot dipendente


pre <- dataset %>%
  filter(year < 2022) %>%
  group_by(Country) %>%
  summarise(RE_pre = mean(RE_cons, na.rm = TRUE), .groups = "drop")

post <- dataset %>%
  filter(year >= 2022) %>%
  group_by(Country) %>%
  summarise(RE_post = mean(RE_cons, na.rm = TRUE), .groups = "drop")

scatter_df <- left_join(pre, post, by = "Country") %>%
  mutate(delta = RE_post - RE_pre)

# scegli quanti paesi etichettare (top 6 qui)
top_labels <- scatter_df %>%
  arrange(desc(delta)) %>%
  slice_head(n = 5) %>%
  mutate(highlight = TRUE)

ggplot(scatter_df, aes(x = RE_pre, y = RE_post)) +
  geom_point(size = 3, alpha = 0.75) +
  geom_point(data = top_labels, color = "red", size = 3.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.9) +
  geom_text_repel(
    data = top_labels,
    aes(label = Country),
    size = 3.6,
    max.overlaps = Inf
  ) +
  labs(
    x = "Media (2010–2021)",
    y = "Media (2022–2024)",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


## top-bottom tasso di crescita

scatter_df <- scatter_df %>%
  mutate(
    growth_rate = (RE_post - RE_pre) / RE_pre * 100
  )
top5 <- scatter_df %>%
  arrange(desc(growth_rate)) %>%
  slice_head(n = 5)

bottom5 <- scatter_df %>%
  arrange(growth_rate) %>%
  slice_head(n = 5)

ranking <- bind_rows(
  top5 %>% mutate(Group = "Top 5 crescita"),
  bottom5 %>% mutate(Group = "Bottom 5 crescita")
)
## boxplot R&D investments

rd_pre <- dataset %>%
  filter(year <= 2021) %>%
  group_by(Country) %>%
  summarise(RD = mean(RD_share_gdp, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = "2010–2021")

rd_post <- dataset %>%
  filter(year >= 2022) %>%
  group_by(Country) %>%
  summarise(RD = mean(RD_share_gdp, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = "2022–2024")

rd_box <- bind_rows(rd_pre, rd_post)


ggplot(rd_box, aes(x = period, y = RD, fill = period)) +
  
  geom_boxplot(
    width = 0.6,
    alpha = 0.8,
    outlier.alpha = 0.25,
    outlier.size = 1.8
  ) +
  
  # Punto della media
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3,
    color = "black"
  ) +
  
  scale_fill_manual(values = c("Pre-2022" = "grey80",
                               "Post-2021" = "grey40")) +
  
  labs(
    x = NULL,
    y = "Spesa in R&D (% del PIL)"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 8))
  )

map_post <- dataset %>%
  filter(year >= 2022) %>%
  group_by(Country) %>%
  summarise(
    RD_post = mean(RD_share_gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(eu_country, by = c("Country" = "NAME_ENGL")) %>%
  st_as_sf()

q <- quantile(map_post$RD_post, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

map_post <- map_post %>%
  mutate(
    rd_class = cut(
      RD_post,
      breaks = c(-Inf, 1, 2, 3, Inf),
      labels = c("< 1%", "1–2%", "2–3%", "> 3%"),
      include.lowest = TRUE
    )
  )

ggplot(map_post) +
  geom_sf(aes(fill = rd_class), color = "white", size = 0.2) +
  scale_fill_manual(
    name = "Spesa in R&D (% PIL)",
    values = c(
      "< 1%" = "#d9d9d9",
      "1–2%" = "#a6bddb",
      "2–3%" = "#3690c0",
      "> 3%" = "#034e7b"
    )
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

## mappa import_dependency


map_2021 <- dataset %>%
  filter(year == 2021) %>%
  left_join(eu_country, by = c("Country" = "NAME_ENGL")) %>%
  st_as_sf()

map_2021 <- map_2021 %>%
  mutate(
    import_class = cut(
      Import_dependency,
      breaks = c(-Inf, 30, 50, 70, Inf),
      labels = c("< 30%", "30–50%", "50–70%", "> 70%"),
      include.lowest = TRUE
    )
  )


ggplot(map_2021) +
  geom_sf(
    aes(fill = import_class),
    color = "grey30",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    name = "Importazione energia (% fabbisogno)",
    values = c(
      "< 30%"      = "#deebf7",
      "30–50%"    = "#9ecae1",
      "50–70%"    = "#3182bd",
      "> 70%"      = "#08519c"
    ),
    drop = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )


