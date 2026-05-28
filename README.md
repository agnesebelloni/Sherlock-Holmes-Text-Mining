# 🔎 Text Mining su Sherlock Holmes — Analisi NLP in R

Questo progetto analizza l’intero corpus delle opere di Sherlock Holmes di Arthur Conan Doyle attraverso tecniche di **text mining e natural language processing (NLP)** in R.

L’analisi confronta due approcci complementari:
- **tidytext (tidyverse approach)** per analisi esplorativa delle parole
- **tm package (corpus approach)** per analisi strutturata e clustering

---

## 👥 Autori

- Giuditta Adezio  
- Agnese Belloni  
- Luca Martegani  

---

## 📚 Dataset

Il dataset proviene dal package:

- `sherlock` (Emil Hvitfeldt)

Contiene testi completi delle opere principali di Arthur Conan Doyle, tra cui:
- *The Adventures of Sherlock Holmes*
- *The Memoirs of Sherlock Holmes*
- *The Return of Sherlock Holmes*
- *His Last Bow*
- *The Case-Book of Sherlock Holmes*

---

## 🎯 Obiettivi del progetto

- Analizzare la frequenza delle parole nei testi
- Confrontare linguaggio tra più libri
- Identificare parole caratteristiche dei protagonisti e del contesto narrativo
- Applicare tecniche di clustering su parole e documenti
- Confrontare due pipeline NLP differenti in R

---

# 🧪 PARTE 1 — ANALISI CON TIDYVERSE / TIDYTEXT

## 🔧 Preprocessing
- Tokenizzazione dei testi con `unnest_tokens`
- Estrazione parole per libro
- Creazione variabile capitolo
- Rimozione stopwords

---

## 📊 Analisi esplorativa

- Frequenze assolute delle parole
- Top parole per ciascun libro
- Analisi globale del corpus

Risultato principale:
- Le parole più frequenti includono *holmes*, *sir*, *time*, *watson*
- *The Hound of the Baskervilles* mostra frequenze più elevate rispetto agli altri testi

---

## 📈 Visualizzazioni

- Barplot delle parole più frequenti per libro
- Grafici facettati per confronto tra testi
- Wordcloud delle parole globali

---

## 📉 Analisi statistica

- Frequenze relative per libro e globali
- Correlazione tra distribuzioni lessicali
- Identificazione parole più rappresentative per ciascun testo

---

# 🧪 PARTE 2 — ANALISI CON TM / CORPUS APPROACH

## 🔧 Preprocessing del corpus

- Creazione corpus da file locali
- Pulizia testo:
  - rimozione punteggiatura e numeri
  - conversione in minuscolo
  - rimozione stopwords
- Stemming con `SnowballC`
- Normalizzazione manuale di termini problematici

---

## 🧠 Feature extraction

- Document-Term Matrix (DTM)
- Term-Document Matrix (TDM)
- Calcolo frequenze assolute dei termini

---

## 🔍 Analisi lessicale

- Termini più frequenti nel corpus
- Associazioni lessicali con *holmes* e *watson*
- Analisi parole caratteristiche del contesto investigativo

---

## 📊 Clustering parole

- Clustering gerarchico (Ward.D2)
- K-means clustering
- Visualizzazione con dendrogrammi e clusplot

Risultati principali:
- *holmes* forma un cluster isolato
- *watson* e *sir* emergono come gruppo distinto
- Forte separazione tra parole altamente frequenti e il resto del vocabolario

---

## 📚 Clustering documenti

- Clustering dei libri basato su DTM
- 5 cluster (coerenti con macro-raccolte editoriali)
- Confronto tra clustering gerarchico e k-means

Osservazioni:
- I cluster non corrispondono perfettamente alle raccolte narrative
- La struttura editoriale non riflette necessariamente contenuti tematici

---

# 📌 RISULTATI PRINCIPALI

- *Holmes* è la parola dominante in tutti gli approcci
- Il lessico è fortemente legato a contesto investigativo (crime, case, inspector)
- Il dataset mostra forte squilibrio tra frequenze delle parole
- Il clustering delle parole è più informativo rispetto a quello dei documenti
- I due approcci NLP producono risultati coerenti ma con diversa granularità

---

# ⚖️ CONFRONTO TRA APPROCCI

| Metodo | Punti di forza | Limiti |
|--------|----------------|--------|
| tidytext | semplice, leggibile, rapido | meno controllo sul preprocessing |
| tm | potente, controllabile | più complesso e manuale |

---

# 🚀 CONCLUSIONI

Il progetto mostra come tecniche diverse di NLP in R permettano di esplorare lo stesso corpus da prospettive differenti.

Il linguaggio delle opere di Sherlock Holmes è fortemente caratterizzato da:
- ripetizione dei protagonisti
- vocabolario investigativo
- forte dominanza di pochi termini chiave

---

# 🧰 Librerie utilizzate

- tidyverse
- tidytext
- stringr
- tm
- SnowballC
- ggplot2
- wordcloud
- cluster
- viridis
- RColorBrewer

---

# ▶️ Come eseguire il progetto

```r
install.packages(c(
  "tidyverse", "tidytext", "sherlock",
  "tm", "SnowballC", "wordcloud",
  "cluster", "viridis"
))

# eseguire lo script principale
source("sherlock_project.R")
