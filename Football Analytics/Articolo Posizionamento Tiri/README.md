
# Shot Analysis | La mappa del tiro nei Top 5 campionati europei

**Football Data Analytics | Stagione 2025/26**

Analisi comparativa delle scelte di tiro e dell'efficienza realizzativa delle squadre dei cinque principali campionati europei: **Premier League, Serie A, LaLiga, Bundesliga e Ligue 1**.

L'obiettivo è comprendere come varia la distribuzione spaziale delle conclusioni e verificare se una maggiore frequenza di tiro da determinate zone si accompagni a una maggiore efficacia realizzativa.

## Dataset

- **Fonte:** WhoScored
- **Campione:** 96 squadre
- **Stagione:** 2025/26
- **Unità di osservazione:** squadra
- **Zone analizzate:** fuori area, area di rigore, area piccola

Per ciascuna squadra sono disponibili il numero di tiri effettuati e i gol realizzati nelle tre zone. Gli autogol sono esclusi dall'analisi.

## Metodologia

Lo studio utilizza tre indicatori principali:

| Indicatore | Calcolo | Obiettivo |
|---|---|---|
| Quota di tiri | Tiri dalla zona / Tiri totali × 100 | Misurare la distribuzione delle conclusioni |
| Quota di gol | Gol dalla zona / Gol nelle tre zone × 100 | Identificare il contributo realizzativo di ciascuna zona |
| Tasso di conversione | Gol dalla zona / Tiri dalla zona × 100 | Valutare l'efficienza realizzativa |

L'analisi combina **statistica descrittiva, boxplot comparativi e scatterplot** per confrontare campionati e singole squadre.

## Risultati principali

- **Serie A:** maggiore incidenza delle conclusioni da fuori area, senza un corrispondente vantaggio nella conversione.
- **Premier League:** maggiore frequenza di tiri dall'area piccola e maggiore contributo di questa zona alla produzione di gol, ma non necessariamente maggiore efficienza.
- **Differenze tra squadre:** anche a parità di distribuzione dei tiri, emergono livelli di conversione molto differenti.
- **Risultato generale:** frequenza delle conclusioni ed efficienza realizzativa rappresentano dimensioni complementari del profilo offensivo.

## Visualizzazioni

Lo studio comprende tre visualizzazioni principali:

1. **Shot Selection:** distribuzione delle quote di tiro per zona nei cinque campionati.
2. **Goal Output & Efficiency:** confronto tra quote di gol e tassi di conversione nelle tre zone.
3. **Shot Frequency vs Conversion:** scatterplot tra frequenza delle conclusioni dall'area piccola ed efficienza realizzativa delle singole squadre.

## Competenze applicate

`Football Data Analysis` · `Data Cleaning` · `Statistica descrittiva` · `KPI offensivi` · `Data Visualization` · `Analisi comparativa`

## Materiali del progetto

- [Articolo completo (PDF)](Articolo.pdf) — Metodologia, grafici e interpretazione dei risultati.
- [Dataset (Excel)](Dati%20Progetto%20Tiri.xlsx) — Dati utilizzati per l'analisi.

## Autore

**Alberto Marchi**  
Football Data Analyst | AIAPC
