# devtools::install_github("EmilHvitfeldt/sherlock")
# dplyr::glimpse(holmes)
library(sherlock)
library(tidyverse)
library(stringr)
library(tidytext)

sherlock::holmes
str(holmes)
summary(holmes)
head(holmes)

unique(holmes$book)
tail(holmes$book)

astudyinscarlet <- holmes %>% 
                      filter(book == 'A Study In Scarlet')
astudyinscarlet <- astudyinscarlet %>%
                             select(text) %>%
                               mutate(chapter = cumsum(str_detect(text, "^CHAPTER")))
astudyinscarlet %>%
  unnest_tokens(word, text)

titles <- c('A Study In Scarlet',"A Scandal in Bohemia", 
            "The Adventure of the Devil's Foot", "The Hound of the Baskervilles", 
            "The Red-Headed League", "His Last Bow")

books <- list(filter(holmes, book == "A Study In Scarlet"), 
              filter(holmes, book == "A Scandal in Bohemia"),
              filter(holmes, book == "The Adventure of the Devil's Foot"),
              filter(holmes, book == "The Hound of the Baskervilles"),
              filter(holmes, book == "The Red-Headed League"),
              filter(holmes, book == "His Last Bow") )

series <- tibble()

for (i in seq_along(titles)) {
  clean <- books[[i]]%>%
    select(text)%>%
    mutate(chapter = cumsum(str_detect(text, "^CHAPTER"))) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
  
}

series
series$book <- factor(series$book, levels = rev(titles))

series %>% 
  anti_join(stop_words) %>% 
    count(word, sort = T)

series %>% 
  anti_join(stop_words) %>% 
  group_by(book) %>% 
  count(word, sort = T) %>% 
  top_n(10)

library(RColorBrewer)

df <- series %>%
        anti_join(stop_words) %>%
        group_by(book) %>%
        count(word, sort = TRUE) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(book = factor(book, levels = titles),
              text_order = nrow(.):1)
  
ggplot(df, aes(reorder(word, n), n, fill = book)) +
  geom_bar(stat = "identity", color = 'black') +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = NULL, y = "Frequency") +
  coord_flip()+
  scale_fill_brewer(palette = 'Pastel2') + 
  theme(legend.position="none") 


sherlock_pct <- series %>%
  anti_join(stop_words) %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

(frequency <- series %>%
  anti_join(stop_words) %>%
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(sherlock_pct) %>%
  arrange(desc(book_words)) %>%
  ungroup())


corr <- frequency %>%
        group_by(book) %>%
           summarize(correlation = cor(book_words, all_words),
              p_value = cor.test(book_words, all_words)$p.value)

top_20 <- frequency %>%
  arrange(desc(book_words)) %>%
  top_n(60) 

ggplot(top_20, aes(x = reorder(word, -book_words), y = book_words, fill = book)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "PERCENTUALE DI PAROLE TOTALI") +
  scale_fill_manual(values = c('navy', 'seagreen2', 'blue', 'purple1', 'magenta', 'olivedrab2', 'violet')) +
  theme_minimal() 

# consigliati da chat colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a")


ggplot(corr, aes(x = reorder(book, correlation), y = correlation)) +
  geom_bar(stat = "identity", fill = 'slateblue', color = 'black', width = 0.5) +
  geom_text(aes(label = paste0(round(correlation, 2))), vjust = 0.5, hjust = - 0.15, size = 4) +
  labs(x = NULL, y = "Correlation", title = "CORRELAZIONE TRA LE PAROLE PIU' FREQUENTI PER OGNI LIBRO E QUELLE PIU' FREQUENTI IN ASSOLUTO") +
  theme_minimal() +
  coord_flip()
  

numero <- series %>%
            anti_join(stop_words) %>%
              count(word, sort = TRUE)

library(wordcloud)
set.seed(123)

wordcloud(numero$word, numero$n, min.freq = 40, colors = viridis::plasma(30))


# senza tidyverse

library(tm)
library(SnowballC)

dati <- Corpus(DirSource("C:/Users/Utente/Desktop/Uni/Data Mining/Sherlock Holmes"))

dati <- tm_map(dati, removePunctuation)
dati <- tm_map(dati, removeNumbers)
dati <- tm_map(dati, content_transformer(tolower))

dati <- tm_map(dati, removeWords, stopwords('english'))
dati <- tm_map(dati, stripWhitespace)

testo_corpus <- sapply(dati, as.character)
# fltro le parole nel corpus trasformato in testo per cercare di 
# cosa sia la radice speckl
(parole_speckl <- unlist(str_extract_all(testo_corpus, "\\bspeckl\\w*\\b")))
(parole_lodg <- unlist(str_extract_all(testo_corpus, "\\blodg\\w*\\b")))
(parole_nobl <- unlist(str_extract_all(testo_corpus, "\\bnobl\\w*\\b")))
(parole_boscomb <- unlist(str_extract_all(testo_corpus, "\\bboscomb\\w*\\b")))
(parole_beech <- unlist(str_extract_all(testo_corpus, "\\bbeech\\w*\\b"))) # faggio
(parole_grang <- unlist(str_extract_all(testo_corpus, "\\bgrang\\w*\\b")))
(parole_holm <- unlist(str_extract_all(testo_corpus, "\\bholmesesesd\\w*\\b"))) # mi dice numeric 0 ma se stampo freq ord mi dice che ce ne sono 340
(parole_dwhat <- unlist(str_extract_all(testo_corpus, "\\bdthe\\w*\\b"))) # non so cosa significa

dati <- tm_map(dati, stemDocument)

dati <- tm_map(dati, content_transformer(gsub), pattern = 'baskervill', 
               replacement = 'baskerville')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'adventur', 
               replacement = 'adventure')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'studi', 
               replacement = 'study')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'hous', 
               replacement = 'house')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'misteri', 
               replacement = 'mistery')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'ladi', 
               replacement = 'lady')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'priori', 
               replacement = 'priority')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'holm', 
               replacement = 'holmes')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'speckl', 
               replacement = 'speckled')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'mysteri', 
               replacement = 'mystery')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'lodg', 
               replacement = 'lodge')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'nobl', 
               replacement = 'noble')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'boscomb', 
               replacement = 'boscombe')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'grang', 
               replacement = 'grange')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'leagu', 
               replacement = 'league')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'therefor', 
               replacement = 'therefore')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'outsid', 
               replacement = 'outside')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'forc', 
               replacement = 'force')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'terribl', 
               replacement = 'terrible')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'littl', 
               replacement = 'little')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'everi', 
               replacement = 'every')
dati <- tm_map(dati, content_transformer(gsub), pattern = 'howev', 
               replacement = 'however')

myStopwords <- c("will", "say", "think", "see", "said", "must", "may", "look", "little",
                 "know", "come", "can", "came", "ask")
dati <- tm_map(dati, removeWords, myStopwords)

dtm <- DocumentTermMatrix(dati)

str(dtm)
inspect(dtm[1:2, 100:105])

freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq, decreasing = T)
freq[head(ord, 100)]
freq[tail(ord)]

mfw <- freq[head(ord, 30)]

wf <- data.frame(term = names(freq),
                 occurrences = freq)

findFreqTerms(dtm, lowfreq = 100)

library(ggplot2)
ggplot(subset(wf, freq > 800), aes(term, occurrences)) +
  geom_bar(stat = 'identity', fill = 'slateblue') +
  coord_flip() +
  labs(title = 'parole più frequenti')

library(wordcloud)
wordcloud(names(freq), freq, min.freq = 300, colors = viridis::plasma(100))

# creazione di una nuova DTM contenente solo le parole più frequenti
new_dtm <- dtm[, names(mfw)]

matrix <- as.matrix(t(new_dtm))

d <- dist(matrix)
gruppi <- hclust(d, method = 'ward.D2')

plot(gruppi, hang = -1)
cutree(gruppi, 4)

kfit <- kmeans(d, centers = 4, nstart = 2)

par(lwd = 2)
rect.hclust(gruppi, k = 4, border = 'olivedrab2')

par(lwd = 1)
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color = T, shade = T, labels = 1, lines = 0)

print(kfit)
kfit$size






tdm <- TermDocumentMatrix(dati)
str(tdm)
inspect(tdm[100:105, 1:2])

freq_tdm <- colSums(as.matrix(tdm))
length(freq_tdm)

ord_tdm <- order(freq_tdm, decreasing = T)
freq[head(ord_tdm, 100)]
freq[tail(ord_tdm)]

mfw_tdm <- freq[head(ord_tdm, 30)]

wf_tdm <- data.frame(term = names(freq_tdm),
                 occurrences = freq_tdm)

findFreqTerms(tdm, lowfreq = 100)

library(ggplot2)
ggplot(subset(wf_tdm, freq > 100), aes(term, occurrences)) +
  geom_bar(stat = 'identity', fill = 'slateblue') +
  coord_flip() +
  labs(title = 'documenti più frequenti')

library(wordcloud)
wordcloud(names(freq_tdm), freq_tdm, min.freq = 300, colors = viridis::plasma(100))

# creazione di una nuova TDM contenente solo le parole più frequenti
new_tdm <- tdm[names(mfw_tdm), ]

matrix_tdm <- as.matrix(t(new_tdm))

d_tdm <- dist(matrix_tdm)
gruppi_tdm <- hclust(d_tdm, method = 'ward.D2')

plot(gruppi_tdm, hang = -1)
cutree(gruppi_tdm, 4)

kfit_tdm <- kmeans(d_tdm, centers = 4, nstart = 2)

par(lwd = 2)
rect.hclust(gruppi_tdm, k = 4, border = 'olivedrab2')

par(lwd = 1)
library(cluster)
clusplot(as.matrix(d_tdm), kfit_tdm$cluster, color = T, shade = T, labels = 1, lines = 0)

print(kfit_tdm)
kfit_tdm$size
