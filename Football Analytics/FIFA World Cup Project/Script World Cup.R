# ==============================================================================
# ANALISI DEL TIRO DA FUORI AREA - MONDIALI 2026
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. SETUP: LIBRERIE, IMPORT DATI, CONTROLLI
# ------------------------------------------------------------------------------

packages <- c(
  "readxl",     # per leggere il file Excel
  "dplyr",      # manipolazione dati (mutate, select, arrange, filter...)
  "readr",      # write_csv() per esportare tabelle
  "ggplot2",    # grafici
  "ggrepel",    # etichette nei grafici senza sovrapposizioni
  "ggcorrplot"  # matrice di correlazione visuale
)

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(packages, library, character.only = TRUE))


team_stats_raw <- read_excel("team_stats_final_pulito.xlsx")


names(team_stats_raw)

team_stats <- team_stats_raw %>%
  rename(
    tiri_da_fuori_area_sum      = `Tiri da fuori area_sum`,
    tiri_da_fuori_area_mean     = `Tiri da fuori area_mean`,
    x_g_diretta_it_sum          = `xG_direttaIT_sum`,
    tiri_totale_fifa            = Tiri,
    tiri_in_porta_fifa          = `Tiri in porta`,
    goal_fuori_area_sum         = Goal_fuori_area_sum,
    gol_totali_fifa             = Gol,
    efficacia_xg                = `Efficacia xG`,
    possesso_percent            = `Possesso(%)`,
    palloni_toccati_area_avv_mean = `Palloni toccati nellarea avversaria_mean`,
    grandi_occasioni_mean       = `Grandi occasioni_mean`,
    passaggi_filtranti_riusciti_mean = `Passaggi filtranti riusciti_mean`
  )


stopifnot(nrow(team_stats) == 48)

# Parametri di esportazione grafici, definiti una volta sola.
larghezza_grafico <- 10
altezza_grafico   <- 7
risoluzione_dpi   <- 300


# ------------------------------------------------------------------------------
# 2. CALCOLO DELLE FEATURE DERIVATE
# ------------------------------------------------------------------------------

dividi_sicuro <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}


team_stats <- team_stats %>%
  mutate(

    # === TIRI DA FUORI AREA: VOLUME E RATIO =================================

   
    ratio_fuori_area = dividi_sicuro(tiri_da_fuori_area_sum, tiri_totale_fifa),

    
    tiri_totali_mean = dividi_sicuro(tiri_totale_fifa, n_partite),


    # === QUALITA' E PRECISIONE DEL TIRO ======================================

    
    xg_per_tiro = dividi_sicuro(x_g_diretta_it_sum, tiri_totale_fifa),

    
    precisione_specchio = dividi_sicuro(tiri_in_porta_fifa, tiri_totale_fifa),


    # === DAI TIRI AI GOL REALI ===============================================

  
    tasso_conversione_fuori_area = dividi_sicuro(
      goal_fuori_area_sum, tiri_da_fuori_area_sum
    ),

    
    quota_gol_fuori_area = dividi_sicuro(goal_fuori_area_sum, gol_totali_fifa),


  
    tocchi_area_per_tiro = dividi_sicuro(
      palloni_toccati_area_avv_mean, tiri_totali_mean
    ),

    grandi_occasioni_per_tiro = dividi_sicuro(
      grandi_occasioni_mean, tiri_totali_mean
    ),

    passaggi_filtranti_per_tiro = dividi_sicuro(
      passaggi_filtranti_riusciti_mean, tiri_totali_mean
    )
  )


# ------------------------------------------------------------------------------
# 3. SEZIONE ARTICOLO 1 - CLASSIFICA VOLUME ASSOLUTO
# ------------------------------------------------------------------------------

classifica_volume <- team_stats %>%
  select(team, tiri_da_fuori_area_mean, n_partite) %>%
  arrange(desc(tiri_da_fuori_area_mean))

print(classifica_volume, n = 48)

grafico_volume <- ggplot(classifica_volume, aes(x = reorder(team, tiri_da_fuori_area_mean),
                                                  y = tiri_da_fuori_area_mean,
                                                  fill = n_partite)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Tiri da fuori area per partita - Mondiali 2026",
    subtitle = "Il colore indica il numero di partite giocate (proxy di affidabilita' del dato)",
    x = NULL,
    y = "Tiri da fuori area (media a partita)",
    fill = "N. partite"
  ) +
  theme_minimal()

grafico_volume

ggsave("01_classifica_volume_fuori_area.png", plot = grafico_volume,
       width = larghezza_grafico, height = altezza_grafico, dpi = risoluzione_dpi)


# ------------------------------------------------------------------------------
# 4. SEZIONE ARTICOLO 2 - RATIO TATTICO (VOLUME vs PROPENSIONE)
# ------------------------------------------------------------------------------

classifica_ratio <- team_stats %>%
  select(team, ratio_fuori_area, tiri_da_fuori_area_mean, n_partite) %>%
  arrange(desc(ratio_fuori_area))

print(classifica_ratio, n = 48)

grafico_ratio <- ggplot(team_stats, aes(x = tiri_da_fuori_area_mean, y = ratio_fuori_area)) +
  geom_point(aes(color = n_partite), size = 3) +
  geom_text_repel(aes(label = team), size = 3, max.overlaps = 15) +
  geom_hline(yintercept = mean(team_stats$ratio_fuori_area), linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = mean(team_stats$tiri_da_fuori_area_mean), linetype = "dashed", color = "grey50") +
  labs(
    title = "Volume vs propensione al tiro da fuori area",
    subtitle = "Le linee tratteggiate indicano la media del torneo su ciascun asse",
    x = "Tiri da fuori area (media a partita)",
    y = "Ratio tiri da fuori area / tiri totali",
    color = "N. partite"
  ) +
  theme_minimal()

grafico_ratio

ggsave("02_volume_vs_ratio_quadranti.png", plot = grafico_ratio,
       width = larghezza_grafico, height = altezza_grafico, dpi = risoluzione_dpi)


# ------------------------------------------------------------------------------
# 5. SEZIONE ARTICOLO 3 - COSA SPIEGA LA PROPENSIONE AL TIRO DA FUORI AREA
# ------------------------------------------------------------------------------

# --- 5a. Possesso palla --------------------------------------------------
cor_possesso <- cor.test(team_stats$possesso_percent, team_stats$ratio_fuori_area,
                          method = "pearson")
print(cor_possesso)

# --- 5b. Tocchi in area avversaria per tiro (il predittore piu' forte) ---
cor_tocchi <- cor.test(team_stats$tocchi_area_per_tiro, team_stats$ratio_fuori_area,
                        method = "pearson")
print(cor_tocchi)

# --- 5c. Grandi occasioni per tiro ---------------------------------------
cor_occasioni <- cor.test(team_stats$grandi_occasioni_per_tiro, team_stats$ratio_fuori_area,
                           method = "pearson")
print(cor_occasioni)

# Grafico del predittore piu' forte (tocchi area per tiro), quello da
# mettere in evidenza nell'articolo.
grafico_tocchi <- ggplot(team_stats, aes(x = tocchi_area_per_tiro, y = ratio_fuori_area)) +
  geom_point(aes(color = n_partite), size = 3) +
  geom_text_repel(aes(label = team), size = 3, max.overlaps = 15) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linetype = "dashed") +
  labs(
    title = "Tocchi in area avversaria per tiro vs propensione al tiro da fuori",
    subtitle = paste0("Pearson r = ", round(cor_tocchi$estimate, 3),
                       " | p = ", signif(cor_tocchi$p.value, 3)),
    x = "Palloni toccati in area avversaria per tiro",
    y = "Ratio tiri da fuori area / tiri totali",
    color = "N. partite"
  ) +
  theme_minimal()

grafico_tocchi

ggsave("03_tocchi_area_vs_ratio.png", plot = grafico_tocchi,
       width = larghezza_grafico, height = altezza_grafico, dpi = risoluzione_dpi)

# Grafico del possesso, mantenuto per completezza (era il primo predittore
# testato, resta valido anche se meno forte degli altri due).
grafico_possesso <- ggplot(team_stats, aes(x = possesso_percent, y = ratio_fuori_area)) +
  geom_point(aes(color = n_partite), size = 3) +
  geom_text_repel(aes(label = team), size = 3, max.overlaps = 15) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linetype = "dashed") +
  labs(
    title = "Possesso palla vs propensione al tiro da fuori area",
    subtitle = paste0("Pearson r = ", round(cor_possesso$estimate, 3),
                       " | p = ", signif(cor_possesso$p.value, 3)),
    x = "Possesso palla medio (%)",
    y = "Ratio tiri da fuori area / tiri totali",
    color = "N. partite"
  ) +
  theme_minimal()

grafico_possesso

ggsave("04_possesso_vs_ratio.png", plot = grafico_possesso,
       width = larghezza_grafico, height = altezza_grafico, dpi = risoluzione_dpi)


variabili_predittori <- team_stats %>%
  select(
    ratio_fuori_area,
    possesso_percent,
    tocchi_area_per_tiro,
    grandi_occasioni_per_tiro,
    passaggi_filtranti_per_tiro,
    xg_per_tiro
  )

matrice_correlazioni <- cor(variabili_predittori, use = "pairwise.complete.obs",
                             method = "spearman")

nomi_leggibili <- c(
  "Ratio fuori area",
  "Possesso palla",
  "Tocchi area/tiro",
  "Grandi occ./tiro",
  "Filtranti/tiro",
  "xG per tiro"
)
colnames(matrice_correlazioni) <- nomi_leggibili
rownames(matrice_correlazioni) <- nomi_leggibili

grafico_matrice <- ggcorrplot(
  matrice_correlazioni,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  method = "square",
  colors = c("steelblue", "white", "firebrick"),
  outline.color = "white",
  ggtheme = theme_minimal()
) +
  labs(
    title = "Matrice di correlazione tra i predittori del tiro da fuori area",
    subtitle = "Correlazioni di Spearman, n = 48 squadre",
    fill = "Spearman"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

grafico_matrice

ggsave("05_matrice_correlazione_predittori.png", plot = grafico_matrice,
       width = larghezza_grafico, height = altezza_grafico, dpi = risoluzione_dpi)


# ------------------------------------------------------------------------------
# 6. SEZIONE ARTICOLO 4 - QUALITA' DEL TIRO
# ------------------------------------------------------------------------------

cor_xg <- cor.test(team_stats$ratio_fuori_area, team_stats$xg_per_tiro, method = "pearson")
print(cor_xg)

grafico_xg <- ggplot(team_stats, aes(x = ratio_fuori_area, y = xg_per_tiro)) +
  geom_point(aes(color = n_partite), size = 3) +
  geom_text_repel(aes(label = team), size = 3, max.overlaps = 15) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linetype = "dashed") +
  labs(
    title = "Propensione al tiro da fuori area vs qualita' del tiro (xG/tiro)",
    subtitle = paste0("Pearson r = ", round(cor_xg$estimate, 3),
                       " | p = ", signif(cor_xg$p.value, 3)),
    x = "Ratio tiri da fuori area / tiri totali",
    y = "xG medio per tiro",
    color = "N. partite"
  ) +
  theme_minimal()

grafico_xg

ggsave("06_qualita_tiro_xg.png", plot = grafico_xg,
       width = larghezza_grafico, height = altezza_grafico, dpi = risoluzione_dpi)

cor_precisione <- cor.test(team_stats$ratio_fuori_area, team_stats$precisione_specchio,
                            method = "pearson")
print(cor_precisione)

grafico_precisione <- ggplot(team_stats, aes(x = ratio_fuori_area, y = precisione_specchio)) +
  geom_point(aes(color = n_partite), size = 3) +
  geom_text_repel(aes(label = team), size = 3, max.overlaps = 15) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linetype = "dashed") +
  labs(
    title = "Propensione al tiro da fuori area vs precisione allo specchio",
    subtitle = paste0("Pearson r = ", round(cor_precisione$estimate, 3),
                       " | p = ", signif(cor_precisione$p.value, 3)),
    x = "Ratio tiri da fuori area / tiri totali",
    y = "Quota di tiri totali in porta",
    color = "N. partite"
  ) +
  theme_minimal()

grafico_precisione

ggsave("07_precisione_specchio.png", plot = grafico_precisione,
       width = larghezza_grafico, height = altezza_grafico, dpi = risoluzione_dpi)


# ------------------------------------------------------------------------------
# 7. SEZIONE ARTICOLO 5 - DAI TIRI AI GOL REALI
# ------------------------------------------------------------------------------

tabella_conversione <- team_stats %>%
  select(team, ratio_fuori_area, tasso_conversione_fuori_area,
         goal_fuori_area_sum, n_partite) %>%
  arrange(desc(ratio_fuori_area))

print(tabella_conversione, n = 48)

n_squadre_zero_gol <- sum(team_stats$goal_fuori_area_sum == 0)
cat("Squadre senza gol da fuori area:", n_squadre_zero_gol, "su", nrow(team_stats), "\n")

perc_gol_fuori_torneo <- sum(team_stats$goal_fuori_area_sum) / sum(team_stats$gol_totali_fifa) * 100
cat("Percentuale di gol del torneo segnata da fuori area:",
    round(perc_gol_fuori_torneo, 1), "%\n")


# ------------------------------------------------------------------------------
# 8. SEZIONE ARTICOLO 6 - CONTROLLO DI RIGORE
# ------------------------------------------------------------------------------


cor_efficacia <- cor.test(team_stats$efficacia_xg, team_stats$ratio_fuori_area,
                           method = "pearson")
print(cor_efficacia)

grafico_efficacia <- ggplot(team_stats, aes(x = ratio_fuori_area, y = efficacia_xg)) +
  geom_point(aes(color = n_partite), size = 3) +
  geom_text_repel(aes(label = team), size = 3, max.overlaps = 15) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linetype = "dashed") +
  labs(
    title = "Controllo di rigore: efficacia di finalizzazione vs ratio fuori area",
    subtitle = paste0("Pearson r = ", round(cor_efficacia$estimate, 3),
                       " | p = ", signif(cor_efficacia$p.value, 3),
                       " (relazione attesa debole o assente)"),
    x = "Ratio tiri da fuori area / tiri totali",
    y = "Efficacia xG (Gol / xG)",
    color = "N. partite"
  ) +
  theme_minimal()

grafico_efficacia

ggsave("08_efficacia_xg_controllo.png", plot = grafico_efficacia,
       width = larghezza_grafico, height = altezza_grafico, dpi = risoluzione_dpi)


# ------------------------------------------------------------------------------
# 9. RIEPILOGO FINALE
# ------------------------------------------------------------------------------

riepilogo_correlazioni <- tibble(
  Relazione = c(
    "Ratio fuori area <-> Possesso palla",
    "Ratio fuori area <-> Tocchi area avversaria per tiro",
    "Ratio fuori area <-> Grandi occasioni per tiro",
    "Ratio fuori area <-> xG per tiro",
    "Ratio fuori area <-> Precisione specchio",
    "Ratio fuori area <-> Efficacia xG generale"
  ),
  Pearson_r = c(
    cor_possesso$estimate, cor_tocchi$estimate, cor_occasioni$estimate,
    cor_xg$estimate, cor_precisione$estimate, cor_efficacia$estimate
  ),
  p_value = c(
    cor_possesso$p.value, cor_tocchi$p.value, cor_occasioni$p.value,
    cor_xg$p.value, cor_precisione$p.value, cor_efficacia$p.value
  )
) %>%
  mutate(
    Pearson_r = round(Pearson_r, 3),
    p_value = signif(p_value, 3),
    Significativo_005 = p_value < 0.05
  ) %>%
  arrange(desc(abs(Pearson_r)))  

print(riepilogo_correlazioni)

write_csv(riepilogo_correlazioni, "riepilogo_correlazioni.csv")
write_csv(team_stats, "dataset_mondiali_unificato_arricchito.csv")

# ==============================================================================
# FINE SCRIPT
# ==============================================================================
