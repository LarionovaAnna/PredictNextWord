#Libraries loading
library(tm)
library(rJava)
library(RWeka)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(tm.plugin.webmining)
library(stringr)

#Data loading
news <- readLines("en_US/en_US.news.txt", warn = FALSE, n = 50000)
blogs <- readLines("en_US/en_US.blogs.txt", warn = FALSE, n = 50000)
twitter <- readLines("en_US/en_US.twitter.txt", warn = FALSE, n = 50000)

#Data processing
##Convert documents into a corpus
news_vec <- VectorSource(news)
news_corpus <- Corpus(news_vec)
blogs_vec <- VectorSource(blogs)
blogs_corpus <- Corpus(blogs_vec)
twitter_vec <- VectorSource(twitter)
twitter_corpus <- Corpus(twitter_vec)
corpus <- sample(c(news_corpus, blogs_corpus, twitter_corpus), 150000, replace = FALSE)

##Transforming data
###convert to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
###remove numbers
corpus <- tm_map(corpus, removeNumbers)
###remove punctuation
corpus <- tm_map(corpus, removePunctuation)
###remove non-ASCII
corpus <- tm_map(corpus, removeNonASCII)
###remove stopwords
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
###remove profane words
#
###remove whitespaces (for all text)
corpus <- tm_map(corpus, stripWhitespace)


###transform to PlainTextDocument
#corpus <- tm_map(corpus, PlainTextDocument)

##Build TDM
#corpus_tdm <- DocumentTermMatrix(corpus)

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
FourgramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))

tdm_t1 <- TermDocumentMatrix(corpus, control = list(tokenize = UnigramTokenizer))
tdm_t2 <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)) 
tdm_t3 <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
tdm_t4 <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))

freq_unig <- findFreqTerms(tdm_t1, 5000)
freq_big <- findFreqTerms(tdm_t2, 500)
freq_trig <- findFreqTerms(tdm_t3, 50)
freq_fourg <- findFreqTerms(tdm_t4, 50)

frequency_unig <- rowSums(as.matrix(tdm_t1[freq_unig,]))
frequency_unig <- data.frame(unigram = names(frequency_unig), frequency = frequency_unig)
frequency_unig <- frequency_unig[with(frequency_unig, order(frequency, decreasing = TRUE)),]
frequency_big <- rowSums(as.matrix(tdm_t2[freq_big,]))
frequency_big <- data.frame(bigram = names(frequency_big), frequency = frequency_big)
frequency_big <- frequency_big[with(frequency_big, order(frequency, decreasing = TRUE)),]
frequency_trig <- rowSums(as.matrix(tdm_t3[freq_trig,]))
frequency_trig <- data.frame(trigram = names(frequency_trig), frequency = frequency_trig)
frequency_trig <- frequency_trig[with(frequency_trig, order(frequency, decreasing = TRUE)),]
frequency_fourg <- rowSums(as.matrix(tdm_t4[freq_fourg,]))
frequency_fourg <- data.frame(fourgram = names(frequency_fourg), frequency = frequency_fourg)
frequency_fourg <- frequency_fourg[with(frequency_fourg, order(frequency, decreasing = TRUE)),]

###Probability of ngrams
frequency_unig$prob <- frequency_unig$frequency/sum(frequency_unig$frequency)
frequency_big$prob <- frequency_big$frequency/sum(frequency_big$frequency)
frequency_trig$prob <- frequency_trig$frequency/sum(frequency_trig$frequency)
frequency_fourg$prob <- frequency_fourg$frequency/sum(frequency_fourg$frequency)

#wc_big <- wordcloud(freq_big, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

#Plotting frequency of Ngrams
##Plotting frequency of unigrams
plot_freq_unig <- ggplot(frequency_unig, 
                         aes(x = reorder(unigram, frequency), y = frequency)) +
    geom_bar(stat = "identity") +
    xlab("Unigram") + ylab("Frequency") +
    labs(title = "Top Unigrams by Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_freq_unig)
##Plotting frequency of bigrams
plot_freq_big <- ggplot(frequency_big, 
                        aes(x = reorder(bigram, frequency), y = frequency)) +
    geom_bar(stat = "identity") +
    xlab("Bigram") + ylab("Frequency") +
    labs(title = "Top Bigrams by Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_freq_big)
##Plotting frequency of trigrams
plot_freq_trig <- ggplot(frequency_trig, 
                         aes(x = reorder(trigram, frequency), y = frequency)) +
    geom_bar(stat = "identity") +
    xlab("Trigram") + ylab("Frequency") +
    labs(title = "Top Trigrams by Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_freq_trig)
##Plotting frequency of fourgrams
plot_freq_fourg <- ggplot(frequency_fourg, 
                          aes(x = reorder(fourgram, frequency), y = frequency)) +
    geom_bar(stat = "identity") +
    xlab("Fourgram") + ylab("Frequency") +
    labs(title = "Top Fourgrams by Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_freq_fourg)
