
# Tiri da fuori area: scelta tattica o soluzione di ripiego?
### Un'analisi empirica dei Mondiali FIFA 2026

**Football Data Analytics | FIFA World Cup 2026**

Analisi quantitativa delle scelte offensive delle 48 nazionali partecipanti ai Mondiali FIFA 2026, con particolare attenzione alla propensione al tiro da fuori area e alla sua relazione con la capacità di costruire occasioni da gol.

L'obiettivo è verificare se il ricorso alle conclusioni dalla distanza rappresenti una scelta offensiva oppure sia associato a difficoltà nel penetrare l'area di rigore avversaria.

## Dataset

- **Fonti:** FIFA, SofaScore, Diretta.it
- **Campione:** 48 nazionali
- **Competizione:** FIFA World Cup 2026
- **Unità di osservazione:** Singola Nazionale
- **Partite osservate:** da 3 a 8 per squadra

Le principali variabili utilizzate comprendono:

- Tiri totali e tiri da fuori area
- Possesso palla
- Tocchi nell'area di rigore avversaria
- Grandi occasioni create
- Expected Goals (xG)
- Precisione dei tiri
- Gol realizzati

## Metodologia

L'analisi combina statistica descrittiva, costruzione di indicatori normalizzati e analisi delle correlazioni.

L'indicatore principale è la **propensione al tiro da fuori area**, calcolata come:

**Propensione = Tiri da fuori area / Tiri totali**

Per valutare le relazioni tra le variabili sono utilizzate correlazioni di Spearman, accompagnate da scatterplot, matrici di correlazione e rappresentazioni grafiche comparative.

## Risultati principali

- **Penetrazione offensiva:** la propensione al tiro da fuori area è fortemente e negativamente associata ai tocchi nell'area avversaria per tiro (ρ = -0,673; p < 0,001).
- **Creazione di occasioni:** le squadre che producono meno grandi occasioni per tiro tendono a concludere più frequentemente dalla distanza (ρ = -0,560; p < 0,001).
- **Qualità dei tiri:** una maggiore propensione al tiro da fuori area è associata a un valore inferiore di xG per tiro (ρ = -0,576; p < 0,001).
- **Efficienza realizzativa:** non emerge una relazione statisticamente significativa tra propensione al tiro da fuori ed efficacia nella finalizzazione misurata dal rapporto Gol/xG (ρ = 0,100; p = 0,499).

I risultati suggeriscono che il tiro dalla distanza possa rappresentare anche una conseguenza della difficoltà nel costruire occasioni all'interno dell'area di rigore, senza necessariamente tradursi in una maggiore efficienza realizzativa.

## Competenze applicate

`Football Data Analysis` · `Data Cleaning` · `Statistica descrittiva` · `KPI offensivi` · `Correlazione di Spearman` · `Data Visualization`

## Limiti

L'analisi riguarda un singolo torneo, con un numero limitato di partite per nazionale. Le correlazioni individuate descrivono associazioni statistiche e non dimostrano relazioni causali.

## Materiali del progetto

- [Articolo completo (PDF)](Articolo%281%29.pdf) — Analisi, grafici e interpretazione dei risultati.
- [Dataset (Excel)](team_stats_final_pulito%281%29.xlsx) — Dati utilizzati per lo studio.

## Autori

**Alberto Marchi · Alessio Salustri**

Football Data Analysts
