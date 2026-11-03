# ============================================================
# PROGETTO: Shot Location - Top 5 European Leagues
# SCRIPT: Preparazione e controllo dei dati
# ============================================================


# Pulizia dell'ambiente
rm(list = ls())


# Pacchetti
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(car)
library(ggplot2)
library(ggrepel)


# Percor# Percor# Percorso del file Excel
file_excel <- "Dati Progetto Tiri.xlsx"


# Controlliamo i nomi dei fogli
excel_sheets(file_excel)

serie_a <- read_excel(file_excel, sheet = "Serie A")
premier_league <- read_excel(file_excel, sheet = "Premier League")
laliga <- read_excel(file_excel, sheet = "LaLiga")
bundesliga <- read_excel(file_excel, sheet = "Bundesliga")
ligue_1 <- read_excel(file_excel, sheet = "Ligue1")

serie_a <- serie_a %>%
  mutate(
    campionato = "Serie A",
    stagione = "2025/26"
  )

premier_league <- premier_league %>%
  mutate(
    campionato = "Premier League",
    stagione = "2025/26"
  )

laliga <- laliga %>%
  mutate(
    campionato = "LaLiga",
    stagione = "2025/26"
  )

bundesliga <- bundesliga %>%
  mutate(
    campionato = "Bundesliga",
    stagione = "2025/26"
  )

ligue_1 <- ligue_1 %>%
  mutate(
    campionato = "Ligue 1",
    stagione = "2025/26"
  )

dati_tiri <- bind_rows(
  serie_a,
  premier_league,
  laliga,
  bundesliga,
  ligue_1
)

dim(dati_tiri)

table(dati_tiri$campionato)

# ============================================================
# RINOMINA DELLE VARIABILI
# ============================================================

dati_tiri <- dati_tiri %>%
  rename(
    squadra = `Squadra`,
    tiri_totali = `Totale Tiri`,
    tiri_fuori_area = `Fuori Area`,
    tiri_area_piccola = `Area Piccola`,
    tiri_area_rigore = `Area Rigore`,
    tiri_fuori = `Tiri Fuori`,
    legni = `Legni`,
    tiri_in_porta = `Tiri Porta`,
    tiri_respinti = `Respinti`,
    gol_totali = `Totale goal`,
    gol_area_piccola = `Goal Area Piccola`,
    gol_area_rigore = `Goal Area Rigore`,
    gol_fuori_area = `Goal Fuori Area`
  )


# Controllo dei nuovi nomi
names(dati_tiri)

# ============================================================
# CONTROLLI DEL DATASET COMPLETO
# ============================================================


# ------------------------------------------------------------
# Struttura generale
# ------------------------------------------------------------

# Dimensioni del dataset
dim(dati_tiri)

# Nomi delle variabili
names(dati_tiri)

# Struttura delle variabili
str(dati_tiri)

# Prime righe
head(dati_tiri)

# Statistiche descrittive generali
summary(dati_tiri)


# ------------------------------------------------------------
# Numero di squadre
# ------------------------------------------------------------

# Numero totale di osservazioni
nrow(dati_tiri)

# Numero di squadre per campionato
table(dati_tiri$campionato)

# Numero di squadre uniche
n_distinct(dati_tiri$squadra)


# ------------------------------------------------------------
# Controllo duplicati
# ------------------------------------------------------------

duplicati <- dati_tiri %>%
  count(campionato, squadra) %>%
  filter(n > 1)

duplicati

# Se tutto è corretto, dovrebbe restituire 0 righe


# ------------------------------------------------------------
# Controllo valori mancanti
# ------------------------------------------------------------

colSums(is.na(dati_tiri))

# Visualizziamo eventuali righe che contengono almeno un NA

righe_con_na <- dati_tiri %>%
  filter(if_any(everything(), is.na))

righe_con_na


# ------------------------------------------------------------
# Controllo valori negativi
# ------------------------------------------------------------

# Selezioniamo tutte le variabili numeriche e controlliamo
# se esistono valori inferiori a zero

dati_tiri %>%
  select(where(is.numeric)) %>%
  summarise(
    across(
      everything(),
      ~ sum(.x < 0, na.rm = TRUE)
    )
  )


# ------------------------------------------------------------
# CONTROLLO FONDAMENTALE:
# le tre zone devono ricostruire i tiri totali
# ------------------------------------------------------------

controllo_zone_tiri <- dati_tiri %>%
  mutate(
    tiri_zone =
      tiri_fuori_area +
      tiri_area_rigore +
      tiri_area_piccola,
    
    differenza_tiri =
      tiri_totali - tiri_zone
  )

# Frequenza delle differenze
table(controllo_zone_tiri$differenza_tiri)

# Numero di osservazioni non coerenti
sum(controllo_zone_tiri$differenza_tiri != 0, na.rm = TRUE)

# Visualizziamo eventuali anomalie
controllo_zone_tiri %>%
  filter(differenza_tiri != 0) %>%
  select(
    squadra,
    campionato,
    tiri_totali,
    tiri_fuori_area,
    tiri_area_rigore,
    tiri_area_piccola,
    tiri_zone,
    differenza_tiri
  )


# ------------------------------------------------------------
# CONTROLLO GOL PER ZONA
# ------------------------------------------------------------

# ATTENZIONE:
# gol_totali comprende anche gli autogol.
# Per questo NON ci aspettiamo necessariamente che
# gol_totali sia uguale alla somma dei gol delle tre zone.

controllo_zone_gol <- dati_tiri %>%
  mutate(
    gol_zone =
      gol_fuori_area +
      gol_area_rigore +
      gol_area_piccola,
    
    differenza_gol =
      gol_totali - gol_zone
  )

# Distribuzione della differenza
table(controllo_zone_gol$differenza_gol)

# Squadre per cui gol_totali e gol_zone differiscono
controllo_zone_gol %>%
  filter(differenza_gol != 0) %>%
  select(
    squadra,
    campionato,
    gol_totali,
    gol_fuori_area,
    gol_area_rigore,
    gol_area_piccola,
    gol_zone,
    differenza_gol
  ) %>%
  arrange(desc(differenza_gol))


# ------------------------------------------------------------
# CONTROLLO DELLE VARIABILI DI PRECISIONE
# ------------------------------------------------------------

# NOTA:
# i legni vengono trattati separatamente e NON devono
# essere utilizzati per ricostruire tiri_totali.

summary(
  dati_tiri %>%
    select(
      tiri_totali,
      tiri_fuori,
      tiri_in_porta,
      tiri_respinti,
      legni
    )
)


# Controlliamo che nessuna componente abbia valori
# palesemente impossibili rispetto ai tiri totali

dati_tiri %>%
  filter(
    tiri_in_porta > tiri_totali |
      tiri_fuori > tiri_totali |
      tiri_respinti > tiri_totali
  ) %>%
  select(
    squadra,
    campionato,
    tiri_totali,
    tiri_in_porta,
    tiri_fuori,
    tiri_respinti,
    legni
  )


# ------------------------------------------------------------
# RIEPILOGO FINALE DEI CONTROLLI
# ------------------------------------------------------------

cat(
  "\nNumero osservazioni:", nrow(dati_tiri),
  "\nNumero squadre uniche:", n_distinct(dati_tiri$squadra),
  "\nDuplicati squadra-campionato:", nrow(duplicati),
  "\nValori mancanti totali:", sum(is.na(dati_tiri)),
  "\nAnomalie nelle zone di tiro:",
  sum(controllo_zone_tiri$differenza_tiri != 0, na.rm = TRUE),
  "\n"
)

# ============================================================
# CREAZIONE DELLE VARIABILI DERIVATE
# ============================================================


dati_analisi <- dati_tiri %>%
  
  mutate(
    
    # --------------------------------------------------------
    # GOL CLASSIFICATI PER ZONA
    # --------------------------------------------------------
    
    # Non utilizziamo gol_totali per la distribuzione geografica
    # perché comprende anche gli autogol.
    
    gol_zone =
      gol_fuori_area +
      gol_area_rigore +
      gol_area_piccola,
    
    
    # --------------------------------------------------------
    # DISTRIBUZIONE DEI TIRI PER ZONA (%)
    # --------------------------------------------------------
    
    quota_tiri_fuori_area =
      (tiri_fuori_area / tiri_totali) * 100,
    
    quota_tiri_area_rigore =
      (tiri_area_rigore / tiri_totali) * 100,
    
    quota_tiri_area_piccola =
      (tiri_area_piccola / tiri_totali) * 100,
    
    
    # --------------------------------------------------------
    # DISTRIBUZIONE DEI GOL PER ZONA (%)
    # --------------------------------------------------------
    
    # Il denominatore è gol_zone e NON gol_totali.
    
    quota_gol_fuori_area =
      (gol_fuori_area / gol_zone) * 100,
    
    quota_gol_area_rigore =
      (gol_area_rigore / gol_zone) * 100,
    
    quota_gol_area_piccola =
      (gol_area_piccola / gol_zone) * 100,
    
    
    # --------------------------------------------------------
    # CONVERSION RATE PER ZONA (%)
    # --------------------------------------------------------
    
    # Quanti tiri effettuati da ciascuna zona diventano gol.
    
    conversione_fuori_area =
      (gol_fuori_area / tiri_fuori_area) * 100,
    
    conversione_area_rigore =
      (gol_area_rigore / tiri_area_rigore) * 100,
    
    conversione_area_piccola =
      (gol_area_piccola / tiri_area_piccola) * 100,
    
    
    # --------------------------------------------------------
    # PRECISIONE GENERALE (%)
    # --------------------------------------------------------
    
    precisione_tiri =
      (tiri_in_porta / tiri_totali) * 100,
    
    
    # --------------------------------------------------------
    # TIRI BLOCCATI / RESPINTI (%)
    # --------------------------------------------------------
    
    quota_tiri_respinti =
      (tiri_respinti / tiri_totali) * 100,
    
    
    # --------------------------------------------------------
    # TIRI FUORI (%)
    # --------------------------------------------------------
    
    quota_tiri_fuori =
      (tiri_fuori / tiri_totali) * 100,
    
    
    # --------------------------------------------------------
    # LEGNI
    # --------------------------------------------------------
    
    # WhoScored non li include nel conteggio dei tiri totali
    # come categoria esclusiva.
    # Li manteniamo quindi come indicatore separato.
    
    legni_per_100_tiri =
      (legni / tiri_totali) * 100
  )

summary(dati_analisi)


# ============================================================
# ANALISI DESCRITTIVA PER CAMPIONATO
# ============================================================

descrittive_campionato <- dati_analisi %>%
  group_by(campionato) %>%
  summarise(
    
    # Numero di squadre
    n_squadre = n(),
    
    # --------------------------------------------------------
    # DISTRIBUZIONE MEDIA DEI TIRI
    # --------------------------------------------------------
    
    quota_fuori_area_media =
      mean(quota_tiri_fuori_area, na.rm = TRUE),
    
    quota_area_rigore_media =
      mean(quota_tiri_area_rigore, na.rm = TRUE),
    
    quota_area_piccola_media =
      mean(quota_tiri_area_piccola, na.rm = TRUE),
    
    
    # --------------------------------------------------------
    # DISTRIBUZIONE MEDIA DEI GOL
    # --------------------------------------------------------
    
    quota_gol_fuori_media =
      mean(quota_gol_fuori_area, na.rm = TRUE),
    
    quota_gol_area_media =
      mean(quota_gol_area_rigore, na.rm = TRUE),
    
    quota_gol_area_piccola_media =
      mean(quota_gol_area_piccola, na.rm = TRUE),
    
    
    # --------------------------------------------------------
    # CONVERSION RATE MEDIO
    # --------------------------------------------------------
    
    conversione_fuori_media =
      mean(conversione_fuori_area, na.rm = TRUE),
    
    conversione_area_media =
      mean(conversione_area_rigore, na.rm = TRUE),
    
    conversione_area_piccola_media =
      mean(conversione_area_piccola, na.rm = TRUE),
    
    
    # --------------------------------------------------------
    # PRECISIONE
    # --------------------------------------------------------
    
    precisione_media =
      mean(precisione_tiri, na.rm = TRUE)
    
  ) %>%
  
  # Arrotondiamo solo per visualizzazione
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )


descrittive_campionato


# ============================================================
# GRAFICO 
# QUOTA MEDIA DEI TIRI PER ZONA E CAMPIONATO
# ============================================================

# ============================================================
# PREPARAZIONE DATI PER IL GRAFICO DEI TIRI PER ZONA
# ============================================================

grafico_tiri_zone <- descrittive_campionato %>%
  
  select(
    campionato,
    quota_fuori_area_media,
    quota_area_rigore_media,
    quota_area_piccola_media
  ) %>%
  
  pivot_longer(
    cols = c(
      quota_fuori_area_media,
      quota_area_rigore_media,
      quota_area_piccola_media
    ),
    names_to = "zona",
    values_to = "percentuale"
  ) %>%
  
  mutate(
    
    zona = dplyr::recode(
      zona,
      "quota_fuori_area_media" = "Fuori area",
      "quota_area_rigore_media" = "Area di rigore",
      "quota_area_piccola_media" = "Area piccola"
    ),
    
    zona = factor(
      zona,
      levels = c(
        "Fuori area",
        "Area di rigore",
        "Area piccola"
      )
    )
  )

grafico_tiri_zone



ggplot(
  grafico_tiri_zone,
  aes(
    x = reorder(campionato, percentuale),
    y = percentuale
  )
) +
  
  geom_col(
    width = 0.7
  ) +
  
  # Valore percentuale sopra ogni barra
  geom_text(
    aes(label = paste0(round(percentuale, 1), "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  
  # Un pannello per ogni zona
  facet_wrap(
    ~ zona,
    scales = "free_y"
  ) +
  
  labs(
    title = "Da dove tirano le squadre nei Top 5 campionati europei?",
    subtitle = "Quota media dei tiri per zona — stagione 2025/26",
    x = NULL,
    y = "Quota media dei tiri (%)"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.major.x = element_blank()
  )

# ============================================================
# FIGURE FINALI PER L'ARTICOLO
# ============================================================

library(patchwork)


# ============================================================
# PALETTE DEI CAMPIONATI
# ============================================================
# Manteniamo gli stessi colori utilizzati negli scatterplot

colori_campionati <- c(
  "Bundesliga"     = "#D55E00",  # arancio/rosso
  "LaLiga"         = "#E6AB02",  # giallo ocra
  "Ligue 1"        = "#009E73",  # verde
  "Premier League" = "#0072B2",  # blu
  "Serie A"        = "#CC79A7"   # viola/magenta
)


# ============================================================
# TEMA COMUNE
# ============================================================
# Questo tema verrà utilizzato in tutti i pannelli.
# In questo modo le figure dell'articolo avranno lo stesso stile.

tema_articolo <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    axis.title.x = element_blank(),
    
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 9
    ),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    axis.title.y = element_text(
      size = 10
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 13,
      hjust = 0.5
    )
  )


# ============================================================
# FIGURA 1
# DOVE TIRANO LE SQUADRE?
# ============================================================


# ------------------------------------------------------------
# PANNELLO A - FUORI AREA
# ------------------------------------------------------------

p_tiri_fuori <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = quota_tiri_fuori_area,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    title = "Fuori area",
    x = NULL,
    y = "Quota di tiri (%)"
  ) +
  tema_articolo


# ------------------------------------------------------------
# PANNELLO B - AREA DI RIGORE
# ------------------------------------------------------------

p_tiri_rigore <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = quota_tiri_area_rigore,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    title = "Area di rigore",
    x = NULL,
    y = "Quota di tiri (%)"
  ) +
  tema_articolo


# ------------------------------------------------------------
# PANNELLO C - AREA PICCOLA
# ------------------------------------------------------------

p_tiri_piccola <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = quota_tiri_area_piccola,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    title = "Area piccola",
    x = NULL,
    y = "Quota di tiri (%)"
  ) +
  tema_articolo


# ------------------------------------------------------------
# UNIONE DEI TRE PANNELLI -> FIGURA 1
# ------------------------------------------------------------

figura_1 <- (
  p_tiri_fuori |
    p_tiri_rigore |
    p_tiri_piccola
) +
  plot_annotation(
    title = "Dove tirano le squadre dei principali campionati europei?",
    subtitle = "Distribuzione della quota di tiri per zona — stagione 2025/26"
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey35"
    )
  )

figura_1




# ============================================================
# FIGURA 2
# DOVE SI SEGNA E CON QUALE EFFICIENZA?
# ============================================================


# ============================================================
# PRIMA RIGA - DISTRIBUZIONE DEI GOL
# ============================================================


# ------------------------------------------------------------
# PANNELLO A - GOL DA FUORI AREA
# ------------------------------------------------------------

p_gol_fuori <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = quota_gol_fuori_area,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    title = "Fuori area",
    x = NULL,
    y = "Quota di gol (%)"
  ) +
  tema_articolo


# ------------------------------------------------------------
# PANNELLO B - GOL DALL'AREA DI RIGORE
# ------------------------------------------------------------

p_gol_rigore <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = quota_gol_area_rigore,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    title = "Area di rigore",
    x = NULL,
    y = "Quota di gol (%)"
  ) +
  tema_articolo


# ------------------------------------------------------------
# PANNELLO C - GOL DALL'AREA PICCOLA
# ------------------------------------------------------------

p_gol_piccola <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = quota_gol_area_piccola,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    title = "Area piccola",
    x = NULL,
    y = "Quota di gol (%)"
  ) +
  tema_articolo



# ============================================================
# SECONDA RIGA - CONVERSIONE DEI TIRI
# ============================================================


# ------------------------------------------------------------
# PANNELLO D - CONVERSIONE FUORI AREA
# ------------------------------------------------------------

p_conv_fuori <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = conversione_fuori_area,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    x = NULL,
    y = "Conversione (%)"
  ) +
  tema_articolo


# ------------------------------------------------------------
# PANNELLO E - CONVERSIONE AREA DI RIGORE
# ------------------------------------------------------------

p_conv_rigore <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = conversione_area_rigore,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    x = NULL,
    y = "Conversione (%)"
  ) +
  tema_articolo


# ------------------------------------------------------------
# PANNELLO F - CONVERSIONE AREA PICCOLA
# ------------------------------------------------------------

p_conv_piccola <- ggplot(
  dati_analisi,
  aes(
    x = campionato,
    y = conversione_area_piccola,
    fill = campionato,
    color = campionato
  )
) +
  geom_boxplot(
    width = 0.60,
    alpha = 0.30,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.70,
    size = 2
  ) +
  scale_fill_manual(
    values = colori_campionati
  ) +
  scale_color_manual(
    values = colori_campionati
  ) +
  labs(
    x = NULL,
    y = "Conversione (%)"
  ) +
  tema_articolo

# ============================================================
# ETICHETTE DELLE RIGHE
# ============================================================

etichetta_gol <- ggplot() +
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = "QUOTA\nDI GOL",
    fontface = "bold",
    size = 4.2,
    lineheight = 1.1
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void()


etichetta_conversione <- ggplot() +
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = "CONVERSIONE",
    fontface = "bold",
    size = 4.2
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void()

# ============================================================
# FIGURA 2 - STRUTTURA A MATRICE
# ============================================================

riga_gol <- (
  etichetta_gol |
    p_gol_fuori |
    p_gol_rigore |
    p_gol_piccola
) +
  plot_layout(
    widths = c(0.35, 1, 1, 1)
  )


riga_conversione <- (
  etichetta_conversione |
    p_conv_fuori |
    p_conv_rigore |
    p_conv_piccola
) +
  plot_layout(
    widths = c(0.35, 1, 1, 1)
  )


figura_2 <- (
  riga_gol /
    riga_conversione
) +
  plot_annotation(
    title = "Dove arrivano i gol e quanto sono efficaci le conclusioni?",
    subtitle = "Distribuzione dei gol e conversione dei tiri per zona — stagione 2025/26"
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey35"
    )
  )


figura_2

# ============================================================
# SCATTER PLOT A QUADRANTI
# SHOT SELECTION vs CONVERSIONE
# Colori = campionato
# Stagione 2025/26
# ============================================================


# ------------------------------------------------------------
# FUORI AREA
# ------------------------------------------------------------

media_quota_fuori <- mean(
  dati_analisi$quota_tiri_fuori_area,
  na.rm = TRUE
)

media_conversione_fuori <- mean(
  dati_analisi$conversione_fuori_area,
  na.rm = TRUE
)


# ------------------------------------------------------------
# IDENTIFICAZIONE DELLE SQUADRE DA ETICHETTARE
# ------------------------------------------------------------

# Soglie del 10° e 90° percentile
q10_quota_fuori <- quantile(
  dati_analisi$quota_tiri_fuori_area,
  0.10,
  na.rm = TRUE
)

q90_quota_fuori <- quantile(
  dati_analisi$quota_tiri_fuori_area,
  0.90,
  na.rm = TRUE
)

q10_conversione_fuori <- quantile(
  dati_analisi$conversione_fuori_area,
  0.10,
  na.rm = TRUE
)

q90_conversione_fuori <- quantile(
  dati_analisi$conversione_fuori_area,
  0.90,
  na.rm = TRUE
)


# Selezioniamo le squadre particolarmente estreme
etichette_fuori <- dati_analisi %>%
  filter(
    quota_tiri_fuori_area <= q10_quota_fuori |
      quota_tiri_fuori_area >= q90_quota_fuori |
      conversione_fuori_area <= q10_conversione_fuori |
      conversione_fuori_area >= q90_conversione_fuori
  )


# ------------------------------------------------------------
# GRAFICO
# ------------------------------------------------------------

ggplot(
  dati_analisi,
  aes(
    x = quota_tiri_fuori_area,
    y = conversione_fuori_area,
    color = campionato
  )
) +
  
  # Linea verticale = media quota tiri
  geom_vline(
    xintercept = media_quota_fuori,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey35"
  ) +
  
  # Linea orizzontale = media conversione
  geom_hline(
    yintercept = media_conversione_fuori,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey35"
  ) +
  
  # Tutte le squadre
  geom_point(
    size = 2.7,
    alpha = 0.65
  ) +
  
  # Etichette solo per le squadre più estreme
  ggrepel::geom_text_repel(
    data = etichette_fuori,
    aes(label = squadra),
    size = 3.2,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.25,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  
  # Palette meno sgargiante
  scale_color_manual(
    values = c(
      "Bundesliga"     = "#D55E00",  # arancio/rosso
      "LaLiga"         = "#E6AB02",  # giallo ocra
      "Ligue 1"        = "#009E73",  # verde
      "Premier League" = "#0072B2",  # blu
      "Serie A"        = "#CC79A7"   # viola/magenta
    )
  ) +
  
  labs(
    title = "Tirare più spesso da fuori significa essere più efficaci?",
    subtitle = "Ogni punto rappresenta una squadra — stagione 2025/26",
    x = "Tiri da fuori area (%)",
    y = "Conversione da fuori area (%)",
    color = "Campionato"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# ============================================================
# SCATTER PLOT - AREA DI RIGORE
# Shot selection vs conversione
# Etichette solo per le squadre più estreme
# ============================================================


# ------------------------------------------------------------
# MEDIE COMPLESSIVE
# ------------------------------------------------------------

media_quota_area <- mean(
  dati_analisi$quota_tiri_area_rigore,
  na.rm = TRUE
)

media_conversione_area <- mean(
  dati_analisi$conversione_area_rigore,
  na.rm = TRUE
)


# ------------------------------------------------------------
# IDENTIFICAZIONE DELLE SQUADRE DA ETICHETTARE
# ------------------------------------------------------------

q10_quota_area <- quantile(
  dati_analisi$quota_tiri_area_rigore,
  0.10,
  na.rm = TRUE
)

q90_quota_area <- quantile(
  dati_analisi$quota_tiri_area_rigore,
  0.90,
  na.rm = TRUE
)

q10_conversione_area <- quantile(
  dati_analisi$conversione_area_rigore,
  0.10,
  na.rm = TRUE
)

q90_conversione_area <- quantile(
  dati_analisi$conversione_area_rigore,
  0.90,
  na.rm = TRUE
)


etichette_area <- dati_analisi %>%
  filter(
    quota_tiri_area_rigore <= q10_quota_area |
      quota_tiri_area_rigore >= q90_quota_area |
      conversione_area_rigore <= q10_conversione_area |
      conversione_area_rigore >= q90_conversione_area
  )


# ------------------------------------------------------------
# GRAFICO
# ------------------------------------------------------------

ggplot(
  dati_analisi,
  aes(
    x = quota_tiri_area_rigore,
    y = conversione_area_rigore,
    color = campionato
  )
) +
  
  geom_vline(
    xintercept = media_quota_area,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey35"
  ) +
  
  geom_hline(
    yintercept = media_conversione_area,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey35"
  ) +
  
  geom_point(
    size = 2.7,
    alpha = 0.65
  ) +
  
  ggrepel::geom_text_repel(
    data = etichette_area,
    aes(label = squadra),
    size = 3.2,
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.25,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Bundesliga"     = "#D55E00",  # arancio/rosso
      "LaLiga"         = "#E6AB02",  # giallo ocra
      "Ligue 1"        = "#009E73",  # verde
      "Premier League" = "#0072B2",  # blu
      "Serie A"        = "#CC79A7"   # viola/magenta
    )
  ) +
  
  labs(
    title = "Tirare più spesso dall'area significa essere più efficaci?",
    subtitle = "Ogni punto rappresenta una squadra — stagione 2025/26",
    x = "Tiri dall'area di rigore (%)",
    y = "Conversione dall'area di rigore (%)",
    color = "Campionato"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# ============================================================
# SCATTER PLOT - AREA PICCOLA
# Shot selection vs conversione
# Etichette solo per le squadre più estreme
# ============================================================


# ------------------------------------------------------------
# MEDIE COMPLESSIVE
# ------------------------------------------------------------

media_quota_piccola <- mean(
  dati_analisi$quota_tiri_area_piccola,
  na.rm = TRUE
)

media_conversione_piccola <- mean(
  dati_analisi$conversione_area_piccola,
  na.rm = TRUE
)


# ------------------------------------------------------------
# IDENTIFICAZIONE DELLE SQUADRE DA ETICHETTARE
# ------------------------------------------------------------

q10_quota_piccola <- quantile(
  dati_analisi$quota_tiri_area_piccola,
  0.10,
  na.rm = TRUE
)

q90_quota_piccola <- quantile(
  dati_analisi$quota_tiri_area_piccola,
  0.90,
  na.rm = TRUE
)

q10_conversione_piccola <- quantile(
  dati_analisi$conversione_area_piccola,
  0.10,
  na.rm = TRUE
)

q90_conversione_piccola <- quantile(
  dati_analisi$conversione_area_piccola,
  0.90,
  na.rm = TRUE
)


etichette_piccola <- dati_analisi %>%
  filter(
    quota_tiri_area_piccola <= q10_quota_piccola |
      quota_tiri_area_piccola >= q90_quota_piccola |
      conversione_area_piccola <= q10_conversione_piccola |
      conversione_area_piccola >= q90_conversione_piccola
  )


# ------------------------------------------------------------
# GRAFICO
# ------------------------------------------------------------

ggplot(
  dati_analisi,
  aes(
    x = quota_tiri_area_piccola,
    y = conversione_area_piccola,
    color = campionato
  )
) +
  
  geom_vline(
    xintercept = media_quota_piccola,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey70"
  ) +
  
  geom_hline(
    yintercept = media_conversione_piccola,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey70"
  ) +
  
  geom_point(
    size = 2.7,
    alpha = 0.65
  ) +
  
  ggrepel::geom_text_repel(
    data = etichette_piccola,
    aes(label = squadra),
    size = 3.2,
    color = "white",
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.color = "grey60",
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Bundesliga"     = "#D55E00",
      "LaLiga"         = "#E6AB02",
      "Ligue 1"        = "#009E73",
      "Premier League" = "#0072B2",
      "Serie A"        = "#CC79A7"
    )
  ) +
  
  labs(
    title = "Tirare più spesso dall'area piccola significa essere più efficaci?",
    subtitle = "Ogni punto rappresenta una squadra — stagione 2025/26",
    x = "Tiri dall'area piccola (%)",
    y = "Conversione dall'area piccola (%)",
    color = "Campionato"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.background = element_rect(
      fill = "black",
      color = NA
    ),
    panel.background = element_rect(
      fill = "black",
      color = NA
    ),
    
    # elimina completamente la griglia
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # testi bianchi
    plot.title = element_text(
      color = "white",
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = "grey80"
    ),
    axis.title = element_text(
      color = "white"
    ),
    axis.text = element_text(
      color = "grey85"
    ),
    
    # legenda
    legend.position = "right",
    legend.background = element_rect(
      fill = "black",
      color = NA
    ),
    legend.key = element_rect(
      fill = "black",
      color = NA
    ),
    legend.title = element_text(
      color = "white"
    ),
    legend.text = element_text(
      color = "white"
    )
  )
