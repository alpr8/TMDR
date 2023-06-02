####  Loading Libraries

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(dplyr)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(tidytext)
library(tidyselect)
library(tidyr)
library(stopwords)
library(topicmodels)
library(NLP)
library(BH)
library(modeltools)
library(slam)
library(tm)
library(stringr)
library(LDAvis)
library(proxy)
library(RJSONIO)
library(servr)
library(SnowballC)
library(stm)
library(huge)
library(cluster)
library(factoextra)
library(ldatuning)
library(readr)
library(textplot)
library(stm)

## Datenimport der CSV-File

raw.Source =  read_delim("Datenbasis_Artikel.csv", delim = ";",
                         escape_double = FALSE, trim_ws = TRUE)
addit.Data = read_delim("Tab_addit_Daten.csv", delim = ";",
                       escape_double = FALSE, trim_ws = TRUE)

DataSource= merge(raw.Source, addit.Data, by = "Journal im Sample")

##################################
## Erstellen eines Corpusformats
corp= corpus(DataSource, text_field = "Content/Text", docid_field = "Journal im Sample")
corp

#### Bennen wichtiger Variablen
year = corp$'Publication Year'
Title = corp$'Article Title'
Journal = corp$'Source Title'

#### noch docvars einfügen??
### Tokenizing
Tok_corp1 = tokens(corp, what = "word", remove_punct = T, remove_symbols = T, remove_numbers = T,
                   remove_url = T, remove_separators = T)
Tok_corp2 = tokens_tolower(Tok_corp1, keep_acronyms = T)
Tok_corp3 = tokens_remove(Tok_corp2, stopwords("english"), padding = F, verbose = T)
Tok_corp4 = tokens_select(Tok_corp3,
                          c("a", "an", "the", "still", "and", "that", "even", "often",
                            "however", "also", "recent", "whether", "well", "rather",
                            "thus", "us", "likely", "also", "furthermore", "first",
                            "second", "third", "of", "to", "in", "for", "by", "from",
                            "across", "beyond", "since", "within", "forward", "many",
                            "see", "help", "like", "take", "might", "make", "made",
                            "may", "need", "use", "used", "using", "could", "would",
                            "give", "given", "gave", "must", "f", "g", "p", "et", "al",
                            "e", "year", "years", "about", "above",
                            "after", "again", "against", "all", "am", "and", "any",
                            "are", "aren't", "as", "at", "be", "because", "been",
                            "before", "being", "below", "between", "both", "but","can",
                            "can't", "cannot", "could", "couldn't", "ct", "did", "didn't",
                            "do", "does", "doesn't", "don't", "down", "during", "each",
                            "few", "for", "from", "further", "had", "hadn't", "has",
                            "hasn't", "have", "haven't", "having", "he", "he'd", "he'll",
                            "he's", "her", "here", "here's", "herself", "him", "himself",
                            "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've",
                            "if","in", "into", "is", "isn't", "it", "it's", "its",
                            "itself", "let's", "me", "more", "most", "much", "musn't", "my",
                            "myself", "no", "nor", "not", "of", "off", "on", "once","one",
                            "only", "or", "other", "ought", "our", "ours", "ourselves",
                            "out", "over", "own", "same", "she", "she'd", "she'll",
                            "she's", "should", "shouldn't", "so", "some", "such", "than",
                            "that", "that's", "the", "their", "theirs", "them", "themselves",
                            "then", "there", "there's", "these", "they", "they'd", "they'll",
                            "they're", "they've", "this", "those", "through", "to", "too","two",
                            "under", "until", "up", "very", "was", "wasn't", "we", "we'd",
                            "we'll","we're", "we've", "were", "weren't", "what", "what's",
                            "when", "when's", "where", "where's", "which", "while",
                            "who", "who's", "whom", "why", "why's", "with", "won't",
                            "would", "wouldn't", "you", "you'd", "you'll", "you're",
                            "you've", "your", "yours", "yourself", "yourselves", "jbl", "jsis", "jpim"),
                          selection = "remove",
                          valuetype = "glob", padding = F, verbose = T)
Tok_corp5 = tokens_wordstem(Tok_corp4, language = "eng")




##################################
## Erstellen einer Dokument-Term-Matrix/Document-feature-matrix
#### Für Wort-Ansatz
dtm_word = dfm(Tok_corp5, verbose = T)
dtm_word2 =
  dfm_trim(
    dtm_word,
    min_docfreq = 0.1,
    # min 7.5%
    max_docfreq = 0.9,
    #  max 90%
    docfreq_type = "prop",
    verbose = T
  )



##########################################################
##Text Statistik für Wörter Ansatz

#### häufigste Wörter im Corpus
top_word = topfeatures(dtm_word)
print (top_word)
#plot(dtm_word$feature[1:50], top_word, type = "s",xlab = feature)
#### frequency
###### Absolut
freq = textstat_frequency(dtm_word, n =50)
#plot(freq, type "s", )
freq$feature <- with(freq, reorder(feature, -frequency))
ggplot(data = freq, aes(x = feature, y = frequency)) +
  ggtitle("Häufigste Wörter im Textcorpus") +
  xlab("") +
  ylab("Worthäufigkeit") +
  geom_point() +
  theme(axis.text.x = element_text(angle = 5000, hjust = 1))



######Gruppierung nach Jahr (Beispielhaft -->freq######Gruppierung nach Jahr (Beispielhaft --> Welche Wollen wir )
freq_y = textstat_frequency(dtm_word, n = 20, group = year )
freq_y
ggplot(freq_y, aes(x = group, y = frequency))+
  geom_point()+
  xlab("") +
  ylab("Worthäufigkeit")+
  ggtitle("Worthäufigkeit nach Jahr")
freq_j = textstat_frequency(dtm_word, n = 20, group = Journal)

######## Visualisierung nach Jahr (beispiel Wort research)
freq_open = subset(freq_y, freq_y$feature %in% "pandem*")
ggplot(freq_open, aes(x = group, y = frequency)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 5000)
                     ) +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##### Relative Frequency
dtm.freq_rel = dfm_weight(dtm_word2, scheme = "prop")
head(dtm.freq_rel)

freq_rel = textstat_frequency(dtm.freq_rel)
ggplot(data = freq_rel, aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 100, hjust = 1))



#### Collocations (siehe Blaheta & Johnson 2001 wg Interpretation von 𝜆, ähnlich wie ngrams)
collocation = textstat_collocations(Tok_corp5, method = "lambda", size = 2, min_count = 2,
                      smoothing = 0.5, tolower = T)
textplot_wordcloud(collocation)

#### Distance & Similarity
#word_dist = textstat_dist(dtm_word, margin = c("features"), method = c("euclidean"),)
doc_dist = textstat_dist(dtm_word, margin = c("documents"), method = c("euclidean"),)

#word_sim = textstat_simil(dtm_word, margin = c("features"), method = c("cosine"))
doc_sim = textstat_simil(dtm_word, margin = c("documents"), method = c("cosine"))

####Entropie of documents or features
textstat_entropy(dtm_word, margin = c( "features"), base = 2)
textstat_entropy(dtm_word, margin = c("documents"), base = 2)



#### Keyness of Words (nach Jahr)
y2018 =docvars(dtm_word)$'Publication Year' == '2018'
keyn_2018 = textstat_keyness(dtm_word, y2018)
head(keyn_2018, 20) ### Welche häufige Worte
tail(keyn_2018, 20) ### Welche QWorte werden im Vergleich zu anderen wenig genutzt/unterrepräsentiert
textplot_keyness(keyn_2018)

y2019 =docvars(dtm_word)$'Publication Year' == '2019'
keyn_2019 = textstat_keyness(dtm_word, y2019)
head(keyn_2019, 20) ### Welche häufige Worte
tail(keyn_2019, 20) ### Welche QWorte werden im Vergleich zu anderen wenig genutzt/unterrepräsentiert
textplot_keyness(keyn_2019)

y2020 =docvars(dtm_word)$'Publication Year' == '2020'
keyn_2020 = textstat_keyness(dtm_word, y2020)
head(keyn_2020, 20) ### Welche häufige Worte
tail(keyn_2020, 20) ### Welche QWorte werden im Vergleich zu anderen wenig genutzt/unterrepräsentiert
textplot_keyness(keyn_2020)

y2021 =docvars(dtm_word)$'Publication Year' == '2021'
keyn_2021 = textstat_keyness(dtm_word, y2021)
head(keyn_2021, 20) ### Welche häufige Worte
tail(keyn_2021, 20) ### Welche QWorte werden im Vergleich zu anderen wenig genutzt/unterrepräsentiert
textplot_keyness(keyn_2021)


#### Kalkulation der lexikalischen Diversität
lexdiv = textstat_lexdiv(Tok_corp5, c("TTR", "MATTR", "C"), MATTR_window = 20)
lexdiv

### Summary
textstat_summary(corp)
textstat_summary(dtm_word)

#### Keywords in Context
####Keywords in Context
### Review Process
kw.Review= kwic(Tok_corp5, pattern = "review", window = 10)
head(kw.Review, 10)
Rev_corp = corpus(kw.Review)
Rev_dtm = dfm(Rev_corp)
textplot_wordcloud(Rev_dtm, max_words = 50)

#### Pandemic
kw.pandemic = kwic(Tok_corp4, pattern = "pandemic", window = 10)
head(kw.pandemic, 10)
corp_pandem = corpus(kw.pandemic)
corp_dfm = dfm(corp_pandem)
textplot_wordcloud(corp_dfm, max_word = 50)

textplot_xray(kw.Review, kw.Open, scale = "relative")
textplot_xray(kw.open_access, kw.OpenScience)

#### Open
kw.Open= kwic(Tok_corp5, pattern = "open", window = 10)
head(kw.Open, 10)
corp_open = corpus(kw.Open)
dfm_open = dfm(corp_open)
textplot_wordcloud(dfm_open, max_word = 50)

####editorial team
kw.editorialTeam = kwic(Tok_corp5, pattern = "editorial team", window = 10)
head(kw.editorialTeam, 10)



####Social Media
kw.socialMedia = kwic(Tok_corp5, pattern = "social media", window = 10)
head(kw.socialMedia, 10)

####peer review
kw.PeerReview = kwic(Tok_corp5, pattern = "peer review", window = 10)
head(kw.PeerReview, 10)

#### qualitative research
kw.qualiRes = kwic(Tok_corp5, pattern = "qualitative research", window = 10)
head(kw.qualiRes, 10)

######mixed methods
kw.mixedMeth = kwic(Tok_corp5, pattern = "mixed methods", window = 10)
head(kw.mixedMeth, 10)

#### United States
kw.US = kwic(Tok_corp5, pattern = "united states", window = 10)
head(kw.US, 10)

## Impact Factor
kw.impactfactor = kwic(Tok_corp5, pattern = "impact", window = 10,)
head(kw.impactfactor, 10,)

#### Disruption
kw.disruption = kwic(Tok_corp5, pattern = "develop*", window = 3)
head(kw.disruption, 3)
###Visualisierung der KW in einem Plot

textplot_xray(kw.impactfactor, kw.PeerReview, scale = "relative")
textplot_xray(kw.open_access, kw.OpenScience)

#### Visualisierung
######## Wordcloud
textplot_wordcloud(dtm_word, min_size = 0.1, max_size = 5, min_count = 5,
                   max_words = 100, color = c("blue", "pink", "orange"))

textplot_wordcloud(dtm_word,
                   'Publication Year' %in% c("2018", "2019", "2020", "2021"),
                   min_size = 0.1, max_size = 5, min_count = 5,
                                      max_words = 100, color = c("blue", "pink", "orange"),
                                      group = ( docvars(dtm_word)$'Publication Year'))

























##########################################################
##Topicmodeling
#### Transformation zu Topic Models
dfm2topicmodels = convert(dtm_word, to = "topicmodels")

##### Schätzung der Themenanzahl mittels Perplexity Methode
set.seed(10)

train = sample(rownames(dfm2topicmodels), nrow(dfm2topicmodels)* .50)
dtm_train = dfm2topicmodels[rownames(dfm2topicmodels) %in% train, ]
dtm_test = dfm2topicmodels[!rownames(dfm2topicmodels) %in% train, ]

LDA_train5 = LDA(dtm_train, method = "Gibbs", k = 5, control = list(alpha = 0.01))
##Calculation of perplexity
perplexity(LDA_train5, dtm_test)

## create a dataframe to store the perplexity scores for different values of k
set.seed(10)
p = data.frame(k = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30), perplexity = NA)
## loop over the values of k in data.frame p
for (i in 1:nrow(p)) {
  print(p$k[i])
  ## calculate perplexity for the given value of k
  m = LDA(dtm_train, method = "Gibbs", k = p$k[i],  control = list(alpha = 0.05))
  ## store result in our data.frame
  p$perplexity[i] = perplexity(m, dtm_test)
}

###Plot Perplexity
set.seed(10)
ggplot(p, aes(x=k, y=perplexity)) + geom_line()


#### Andere Möglichkeit --> Was bedeutet das? Nachschauen
set.seed(10)
lda.metriken = FindTopicsNumber(dfm2topicmodels, topics = seq(from =2, to = 30, by =1),
                                metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                method = "Gibbs", control = list(seed = 10), mc.cores = 2L, verbose = TRUE)
FindTopicsNumber_plot(lda.metriken)

##################
## Finale LDA
set.seed(10)
k = 25
lda.model = LDA(dfm2topicmodels, k , "Gibbs")

dtm_word$topic <- topics(lda.model)



####Visualisierung/Exploration der Wörter und Themen
######Anzeigen der Termen pro Themen (Anzahl der Terms pro Thema auswählbar)
df_lda = as.data.frame(terms(lda.model, 10))

corp_lda = corpus(df_lda)
dtm_lda = dfm(df_lda)
terms(lda.model,10)


# Anzeigen von Haupttopics der einzelnen Dokumente
data.frame(Thema = topics(lda.model))
slotNames(lda.model)
topics(lda.model)

## Name Topics
get_topics(lda.model, 5)



# Posterior distribution of words and documents to topics,
#can be used to plot a term cloud proportional to occurence
topic1 = 1
words1 = posterior(lda.model)$terms[topic1,]
topwords_1 = head(sort(words1, decreasing = T),n=20)
topwords_1


topic2 = 2
words2 = posterior(lda.model)$terms[topic2,]
topwords_2 = head(sort(words2, decreasing = T),n=20)
topwords_2

topic3 = 3
words3 = posterior(lda.model)$terms[topic3,]
topwords_3= head(sort(words3, decreasing = T),n=20)
topwords_3

topic4 = 4
words4 = posterior(lda.model)$terms[topic4,]
topwords_4= head(sort(words4, decreasing = T),n=20)
topwords_4

topic5 = 5
words5 = posterior(lda.model)$terms[topic5,]
topwords_5 = head(sort(words5, decreasing = T),n=20)
topwords_5



## Zeigt Dokumente an, die Thema X am meisten beinhalten
topic = 1
topic.docs1 = posterior(lda.model)$topics[, topic]
topic.docs1 = sort(topic.docs1, decreasing = T)
head(topic.docs1)

topic = 2
topic.docs2 = posterior(lda.model)$topics[, topic]
topic.docs2 = sort(topic.docs2, decreasing = T)
head(topic.docs2)

topic = 3
topic.docs3 = posterior(lda.model)$topics[, topic]
topic.docs3 = sort(topic.docs3, decreasing = T)
head(topic.docs3)

topic = 4
topic.docs4 = posterior(lda.model)$topics[, topic]
topic.docs4 = sort(topic.docs4, decreasing = T)
head(topic.docs4)

topic = 5
topic.docs5 = posterior(lda.model)$topics[, topic]
topic.docs5 = sort(topic.docs5, decreasing = T)
head(topic.docs5)



### so kann man die Top Dokumente lesen
topdoc = names(topic.docs1)[1]
topdoc_corp = corp[docnames(corp) ==topdoc]
texts(topdoc_corp)


######Statistische Beschreibung
### Ergebnisse in einem tibble format, Beta zeigt "the associated probability with that topic
## Summe von Beta sollte 1 sein
lda.tbeta = tidy(lda.model, matrix = "beta")
lda.tbeta

### Visualisierung von beta
#### Gruppierung der Worter by Thema
beta_top_terms = lda.tbeta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)%>%
  ungroup () %>%
  arrange(topic, -beta)


####Display the grouped terms on the charts
beta_top_terms %>%
  mutate(term = reorder_within(term, beta, topic))%>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  ggtitle("Probability of term per topic") +
  geom_col(show.legend = T) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

Tab <- table(beta_top_terms$topic, Corr_Df)
merge()
rgraph(Corr_Df)

edges <- graph(as.matrix(Corr_Df), directed = F)
plot(edges, vertex.label = NA)

edges = df_lda # let's build the edges for our igraph plot
  for (i in 10) {
    edges = rbind(edges,expand.grid(beta_top_terms[,i],beta_top_terms[,i])) # cartesian
  }
  edges[,1] = as.character(edges[,1])
  edges[,2] = as.character(edges[,2])
  alphabetorder = function(x) {
    s = edges[x,]
    edges[x,] <<- edges[x,order(s)]
  }
  sapply(1:dim(edges)[1],alphabetorder)
  edges = unique(edges)
  edges = edges[edges[,1]!=edges[,2],]
  net = graph.data.frame(edges,directed=F)
  plot(net,vertex.shape='none',vertex.size=0,vertex.color='white',layout=layout.fruchterman.reingold,
       vertex.label.color='black',vertex.label.cex=1,vertex.label.family='Arial',
       vertex.label.font=2)



beta_top_terms %>% mutate(NameTopic = recode(beta_top_terms$topic,
       1 = "Logistics_Supply_chain",
       2 = "Education",
       3 = "NA",
       4 = "Publication issues",
       5 = "research studies",
       6 = "geopolitics & war",
       7 = "women & gender",
       8 = "Open science & access",
       9 = "Media & Communication",
       10 = "Service & Management",
       11 = "Politics & Racism",
       12 = "theoretical articles",
       13 = "Journal Editoring",
       14 = "Covid 19",
       15 = "children health",
       16 = "social work & refugees",
       17 = "spatial economics & research",
       18 = "Review",
       19 = "Data in research",
       20 = "Management & Business"))

#### Examining per document per topic probability
gamma_documents = tidy(lda.model, matrix = "gamma")
gamma_documents

##### Visualisierung Gamma
ggplot(data = gamma_documents,
       mapping = aes(x = document,
                     y = gamma, group= factor(topic),color=factor(topic))) +
  geom_line() +
  facet_wrap(~factor(topic),ncol=1)
####Textplot network (funktioniert nur mit ngrams)
set.seed(10)
textplot_network(
  dtm_word2,
  topic,
  top_n = 20,
  title = "Term cooccurrences",
  subtitle = list(),
  vertex_color = "darkgreen",
  edge_color = "grey",
  base_family = "",)

#### Textstat Keyness für Themen
topic1 = docvars(dtm_word)$topic == '1'
topic1_keyness = textstat_keyness(dtm_word, target = topic1)
textplot_keyness(topic1_keyness)

topic2 = docvars(dtm_word)$topic == '2'
topic2_keyness = textstat_keyness(dtm_word, target = topic2)
textplot_keyness(topic2_keyness)

topic3 = docvars(dtm_word)$topic == '3'
topic3_keyness = textstat_keyness(dtm_word, target = topic3)
textplot_keyness(topic3_keyness)

topic4 = docvars(dtm_word)$topic == '4'
topic4_keyness = textstat_keyness(dtm_word, target = topic4)
textplot_keyness(topic4_keyness)

topic5 = docvars(dtm_word)$topic == '5'
topic5_keyness = textstat_keyness(dtm_word, target = topic5)
textplot_keyness(topic5_keyness)

topic6 = docvars(dtm_word)$topic == '6'
topic6_keyness = textstat_keyness(dtm_word, target = topic6)
textplot_keyness(topic6_keyness)

topic7 = docvars(dtm_word)$topic == '7'
topic7_keyness = textstat_keyness(dtm_word, target = topic7)
textplot_keyness(topic7_keyness)

topic8 = docvars(dtm_word)$topic == '8'
topic8_keyness = textstat_keyness(dtm_word, target = topic8)
textplot_keyness(topic8_keyness)

topic9 = docvars(dtm_word)$topic == '9'
topic9_keyness = textstat_keyness(dtm_word, target = topic9)
textplot_keyness(topic9_keyness)

topic10 = docvars(dtm_word)$topic == '10'
topic10_keyness = textstat_keyness(dtm_word, target = topic10)
textplot_keyness(topic10_keyness)

topic11 = docvars(dtm_word)$topic == '11'
topic11_keyness = textstat_keyness(dtm_word, target = topic11)
textplot_keyness(topic11_keyness)

topic12 = docvars(dtm_word)$topic == '12'
topic12_keyness = textstat_keyness(dtm_word, target = topic12)
textplot_keyness(topic12_keyness)

topic13 = docvars(dtm_word)$topic == '13'
topic13_keyness = textstat_keyness(dtm_word, target = topic13)
textplot_keyness(topic13_keyness)

topic14 = docvars(dtm_word)$topic == '14'
topic14_keyness = textstat_keyness(dtm_word, target = topic14)
textplot_keyness(topic14_keyness)

topic15 = docvars(dtm_word)$topic == '15'
topic15_keyness = textstat_keyness(dtm_word, target = topic15)
textplot_keyness(topic15_keyness)

topic16 = docvars(dtm_word)$topic == '16'
topic16_keyness = textstat_keyness(dtm_word, target = topic16)
textplot_keyness(topic16_keyness)

topic17 = docvars(dtm_word)$topic == '17'
topic17_keyness = textstat_keyness(dtm_word, target = topic17)
textplot_keyness(topic17_keyness)

topic18 = docvars(dtm_word)$topic == '18'
topic18_keyness = textstat_keyness(dtm_word, target = topic18)
textplot_keyness(topic18_keyness)

topic19 = docvars(dtm_word)$topic == '19'
topic19_keyness = textstat_keyness(dtm_word, target = topic19)
textplot_keyness(topic19_keyness)

topic20 = docvars(dtm_word)$topic == '20'
topic20_keyness = textstat_keyness(dtm_word, target = topic20)
textplot_keyness(topic20_keyness)


####Visualisierung der Themen
###heatmap für 5 Themen nach Research Area
docs = docvars(dtm_word)[match(rownames(dfm2topicmodels), docnames(dtm_word)),]
tpp = aggregate(posterior(lda.model)$topics, by=docs["Publication Year"], mean)
rownames(tpp) = tpp$`Publication Year`
heatmap(as.matrix(tpp[-1]))

####Visualization
set.seed(10)
dtm = dtm_word[slam::row_sums(dtm_word) > 0,]
phi = as.matrix(posterior(lda.model)$terms)
theta <- as.matrix(posterior(lda.model)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm_word)
term.freq = slam::col_sums(dtm_word)[match(vocab, colnames(dtm_word))]
json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)


#### Wie ähnlich sind sich die Themen/Angeschlossenes Clustering
lda.similarity = as.data.frame(lda.model@beta) %>%
  scale()%>%
  dist(method = "cosine") %>%
  hclust(method = "ward.D2")
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

### speichern
lda.topics =as.matrix(topics(lda.model))
write.csv(lda.topics, file = paste("LDA", k, "DocsToTopics.csv"))

lda.terms =as.matrix(terms(lda.model, 10))
write.csv(lda.terms, file = paste("LDA", k, "TpocsToTerms.csv"))

topicPropabilities = as.data.frame(lda.model@gamma)
write.csv(topicPropabilities, file = paste ("LDA", k, "TopicProbabilities.csv"))

### Plot der Probability zum Vergleich
#plot.ts(log(lda.model_5[lda.model_5$k == 1, "beta"]))
#lines(log(lda.model_5[lda.model_5$k == 2, "beta"]), col = 2)

####Textplot network (funktioniert nur mit ngrams)
set.seed(10)
textplot_network(
  lda.model$topic,
  min_freq = 0.05,
  omit_isolated = TRUE,
  edge_color = "#1F78B4",
  edge_alpha = 0.5,
  edge_size = 2,
  vertex_color = "#4D4D4D",
  vertex_size = 2,
  vertex_labelcolor = NULL,
  vertex_labelfont = NULL,
  vertex_labelsize = 5,
  offset = NULL)

Topic_corr = cor(theta)
Corr = as.data.frame(Topic_corr)
topics = as.data.frame(lda.model$topics)
ggnet(Corr)
as.network(df_lda, directed = F, vertices = Corr)

g = graph_from_data_frame(Corr, directed = F)
plot.igraph(g)
print(g)
library(igraph)
graph_from_edgelist(Corr_Df)
as.network(Topic_corr)
as_data_frame()
library(asnipe)
correlation = corrcoef(lda.tbeta);
get_network(model = lda.model,
            method = 'simple',
            cutoff = 0.001,
            cutiso = TRUE)

#####################
## Clustering K-Means
set.seed(10)
matrix_word = as.matrix(dtm_word)
distance_word = get_dist(matrix_word)
fviz_dist(distance_word, gradient = list(low = "blue", mid = "yellow", high = "red"))

#### Cluster-Anzahl bestimmen
###### Siluette Methode
set.seed(10)
sil_word = function(k) {km.res <- kmeans(matrix_word, centers = k, nstart = 25)
  ss=silhouette(km.res$cluster, dist(matrix_word))
  mean(ss[,3])
  }
### Compute and Plot wss
k.values <- 2:30
####Extract avg silhouette for 2-30 clusters
silword_val = map_dbl(k.values, sil_word)
plot(k.values, silword_val, type = "b", pch = 19, frame = False,
     xlab = "Number of clusters K",
     ylab ="average silhouette")
fviz_nbclust(matrix_word, kmeans, method = "silhouette")

####### Cluster Anzahl bestimmen - Elbow Method
set.seed(10)
#### function to compute total within cluster sum of square
wss = function(k) {kmeans(matrix_word, k, nstart = 100)$tot.withinss}
####Compute and plot wss for k1 bis 30
k.values = 1:30
wss_values = map_dbl(k.values, wss)
plot(k.values, wss_values, type = "b", pch = 19, frame = F,
     xlab = "Number of clusters K",
     ylab = "total within-cluster sum of square")

##### Finales Kmeans
set.seed(10)
k = 2 #flexibel austauschen
kmeans = kmeans(matrix_word, k, nstart = 100)
print(kmeans$cluster)
fviz_cluster(kmeans, data = matrix_word)
write.csv(kmeans$cluster, file = "Kmeans.csv")

####Entropie of documents or features
dtm_kmeans = dfm(kmeans)
textstat_entropy(dtm_kmeans, margin = c( "features"), base = 2)
textstat_entropy(dtm_word, margin = c("documents"), base = 2)



DataSource$cluster <- kmeans$cluster

###### Hierarchisches Clustering Wort Ansatz
distance = dist(matrix_word, method = "euclidean")
hclust = hclust(distance, method = "ward.D2" )
plot(hclust,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")
clusters = rect.hclust(hclust, h = 300)
x <- identify(hclust)
DataSource$hcluster <- clusters




















