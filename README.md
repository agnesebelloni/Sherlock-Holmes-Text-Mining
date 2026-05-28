# 🔎 Sherlock Holmes Text Mining & NLP Analysis

Questo progetto analizza un insieme di testi delle opere di Sherlock Holmes utilizzando tecniche di Natural Language Processing in R.

Il progetto è esplorativo e confronta due approcci diversi di text mining:
- tidytext (workflow moderno e tidy)
- tm package (approccio classico basato su corpus)

---

## 📚 Dataset

Il dataset proviene dal package R:

- `sherlock` (Emil Hvitfeldt)

Include testi di varie opere di Arthur Conan Doyle, tra cui:
- A Study in Scarlet  
- A Scandal in Bohemia  
- The Hound of the Baskervilles  
- His Last Bow  
- e altri racconti

---

## 🎯 Obiettivi

- Analizzare la frequenza delle parole nei diversi libri
- Confrontare il linguaggio tra opere
- Identificare parole caratteristiche di ciascun testo
- Applicare tecniche di clustering su dati testuali
- Confrontare approcci NLP diversi in R

---

## 🧰 Librerie utilizzate

- tidyverse
- tidytext
- stringr
- dplyr
- ggplot2
- wordcloud
- RColorBrewer
- tm
- SnowballC
- cluster
- viridis

---

## 🔄 Pipeline 1 — Approccio tidytext

### 🔹 Preprocessing
- Tokenizzazione dei testi
- Estrazione parole con `unnest_tokens`
- Rimozione stopwords
- Creazione variabile chapter

### 🔹 Analisi
- Frequenza parole globali e per libro
- Top 10 parole per ciascuna opera
- Analisi percentuale delle parole

### 🔹 Visualizzazione
- Barplot facettati per libro
- Grafici delle parole più frequenti
- Wordcloud

### 🔹 Statistica
- Correlazione tra frequenze locali e globali

---

## 🔄 Pipeline 2 — Approccio tm (classico)

### 🔹 Preprocessing corpus
- Creazione corpus da directory
- Pulizia: punteggiatura, numeri, maiuscole
- Rimozione stopwords personalizzate
- Stemming con `SnowballC`
- Normalizzazione manuale delle parole

### 🔹 Feature extraction
- Document-Term Matrix (DTM)
- Term-Document Matrix (TDM)

### 🔹 Analisi
- Frequenze assolute
- Associazioni tra parole (es. Holmes, Watson)

### 🔹 Clustering
- Clustering gerarchico (Ward.D2)
- K-means clustering
- Visualizzazione cluster (dendrogrammi e clusplot)

---

## 📊 Risultati principali

- Le parole più frequenti sono coerenti tra i libri ma con differenze stilistiche
- Holmes e Watson emergono come termini centrali e fortemente associati
- Il clustering separa parzialmente testi e vocaboli in gruppi coerenti
- L’approccio tidytext è più leggibile e riproducibile
- L’approccio tm è più manuale ma consente maggiore controllo fine

---

## ⚠️ Nota sul progetto

Questo è un progetto esplorativo realizzato durante lo studio di text mining in R.

Il codice include:
- esperimenti progressivi
- pulizia manuale estensiva
- test di tecniche diverse
- alcune parti non ottimizzate o ridondanti

---

## 🚀 Come eseguire

```r
install.packages(c(
  "sherlock", "tidyverse", "tidytext",
  "tm", "SnowballC", "wordcloud",
  "cluster", "viridis"
))

source("sherlock_analysis.R")
