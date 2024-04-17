library(tm)
docs <- Corpus(DirSource("/Users/computer/Desktop/UNI/Terzo anno/Data Mining/Sherlock Holmes"))
docs
summary(docs)

# Inizio del pre-processing
# Funzione che assegna uno " " (spazio) a un simbolo definito.
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")

# Rimozione della punteggiatura.
docs <- tm_map(docs, removePunctuation)
# Trasformazione di tutte le lettere in lettere minuscole.
docs <- tm_map(docs,content_transformer(tolower))
# Rimozione dei numeri.
docs <- tm_map(docs, removeNumbers)

# Rimozione stopwords utilizzando la lista standard 'english'
docs <- tm_map(docs, removeWords, stopwords("english"))
# Rimozione spazi bianchi.
docs <- tm_map(docs, stripWhitespace)

library(SnowballC)
# Fase di Stemming
docs <- tm_map(docs,stemDocument)

# Creazioen della tabella Document-Term.
dtm <- DocumentTermMatrix(docs)
# Visualizzazione di una parte della tabella.
inspect(dtm[1:2,1000:1005])

# Conteggio delle frequenze di apparizione delle parole su tutti i documenti.
freq <- colSums(as.matrix(dtm))
# La lunghezza del vettore 'freq' corrisponde al numero di termini/parole apparse in ogni documento.
length(freq)

# Ordiniamo in maniera decrescente i termini in base alla loro frequenza di apparizione.
ord <- order(freq,decreasing=TRUE)
# Verifichiamo la frequenza di apparizione dei primi termini.
freq[head(ord)]

# Verifichiamo la frequenza di apparizione degli ultimi termini.
freq[tail(ord)]

# Ordiniamo in maniera decrescente i termini in base alla loro frequenza di apparizione.
ordr <- order(freq,decreasing=TRUE)
# Verifichiamo ora le paroli più frequenti.
findFreqTerms(dtm,lowfreq=80)
