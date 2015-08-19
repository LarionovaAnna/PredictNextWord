#Libraries loading
library(tm)
library(rJava)
library(RWeka)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(tm.plugin.webmining)
library(stringr)
library(slam)
#
setwd("E:/Personal/Repos/Coursera-SwiftKey")
#Data loading
news <- readLines("en_US/en_US.news.txt", warn = FALSE, encoding="UTF-8")
#news <- sample(news, size = round(length(news)*0.2), replace = FALSE)
blogs <- readLines("en_US/en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
#blogs <- sample(blogs, size = round(length(blogs)*0.2), replace = FALSE)
twitter <- readLines("en_US/en_US.twitter.txt", warn = FALSE, encoding="UTF-8")
#twitter <- sample(twitter, size = round(length(twitter)*0.2), replace = FALSE)
profanity <- readLines("bad_words.txt", warn = FALSE, encoding="UTF-8")
all<-c(twitter,blogs,news)
rm (twitter,blogs,news)
all<-gsub("ain't", " not ", all)
all<-gsub("'m", " am ", all)
all<-gsub("'ve", " have ", all)
all<-gsub("'ll", " will ", all)
all<-gsub("'re", " are ", all)
all<-gsub("can't", " cannot ", all)
all<-gsub("n't", " not ", all)
all<-gsub("'d", " would ", all)
all<-gsub("o'clock", "of clock", all)
all<-gsub("'s", " is ", all)
#all<-gsub(" '","",all)
#all<-gsub("' ","",all)
#all<-gsub("[']{2,}","",all)
#all<-gsub("[[:punct:]]"," ",all)
#all<-gsub("[[:digit:]]"," ",all)
all<-gsub("[^[:alpha:]]"," ",all)
all<-gsub("[[:space:]]{1,}"," ",all)
all<-tolower(all)
all<-str_trim(all)
all<-all[1:667339]
corpus <- Corpus(VectorSource(all))
rm(all)

##Transforming data
###remove numbers
corpus <- tm_map(corpus, removeNumbers)
###remove punctuation
corpus <- tm_map(corpus, removePunctuation)
###remove non-ASCII
corpus <- tm_map(corpus, removeNonASCII)
###remove stopwords
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
###remove profane words
corpus <- tm_map(corpus, removeWords, profanity)
###remove whitespaces (for all text)
corpus <- tm_map(corpus, stripWhitespace)
###convert to lower case
corpus <- tm_map(corpus, content_transformer(tolower))

###transform to PlainTextDocument
#corpus <- tm_map(corpus, PlainTextDocument)

##Build TDM
#corpus_tdm <- DocumentTermMatrix(corpus)

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
FourgramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))
FivegramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 5, max = 5))

#tdm_t1 <- TermDocumentMatrix(corpus, control = list(tokenize = UnigramTokenizer))
tdm_t2 <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
tdm_t2  <- rollup(tdm_t2, 2, na.rm=TRUE, FUN = sum)

tdm_t3 <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
tdm_t3 <- rollup(tdm_t3, 2, na.rm=TRUE, FUN = sum)

tdm_t4 <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
tdm_t4 <- rollup(tdm_t4, 2, na.rm=TRUE, FUN = sum)

tdm_t5 <- TermDocumentMatrix(corpus, control = list(tokenize = FivegramTokenizer))
tdm_t5 <- rollup(tdm_t5, 2, na.rm=TRUE, FUN = sum)

rm(corpus)
#freq_unig <- findFreqTerms(tdm_t1, 5000)
freq_big <- findFreqTerms(tdm_t2, 4)
freq_trig <- findFreqTerms(tdm_t3, 2)
freq_fourg <- findFreqTerms(tdm_t4, 2)
freq_fiveg <- findFreqTerms(tdm_t5, 2)
##tdm to df
#uni
#frequency_unig <- rollup(tdm_t1[freq_unig,], 2, na.rm=TRUE, FUN = sum)
#frequency_unig <- as.data.frame(inspect(frequency_unig),namestringsAsFactors = FALSE)
#names(frequency_unig)[1] <- "frequency"
#frequency_unig$unigram <- rownames(frequency_unig)
#rownames(frequency_unig) <- NULL
#frequency_unig <- frequency_unig[,c(2,1)]
#frequency_unig <- frequency_unig[with(frequency_unig, order(frequency, decreasing = TRUE)),]
#frequency_unig$prob <- frequency_unig$frequency/sum(frequency_unig$frequency)


#bi
 
frequency_big <- as.data.frame(inspect(tdm_t2[freq_big,]),namestringsAsFactors = FALSE)
rm(tdm_t2,freq_big)
names(frequency_big)[1] <- "frequency"
frequency_big$bigram <- rownames(frequency_big)
rownames(frequency_big) <- NULL
frequency_big <- frequency_big[,c(2,1)]
frequency_big <- frequency_big[with(frequency_big, order(frequency, decreasing = TRUE)),]
frequency_big$prob <- frequency_big$frequency/sum(frequency_big$frequency)
write.csv(frequency_big, "big.csv")
#rm(frequency_big)

#tri

frequency_trig <- as.data.frame(inspect(tdm_t3[freq_trig,]),namestringsAsFactors = FALSE)
rm(tdm_t3,freq_trig)
names(frequency_trig)[1] <- "frequency"
frequency_trig$trigram <- rownames(frequency_trig)
rownames(frequency_trig) <- NULL
frequency_trig <- frequency_trig[,c(2,1)]
frequency_trig <- frequency_trig[with(frequency_trig, order(frequency, decreasing = TRUE)),]
frequency_trig$prob <- frequency_trig$frequency/sum(frequency_trig$frequency)
write.csv(frequency_trig, "trig.csv")
#rm(frequency_trig)

#four

frequency_fourg <- as.data.frame(inspect(tdm_t4[freq_fourg,]),namestringsAsFactors = FALSE)
rm(tdm_t4,freq_fourg)
names(frequency_fourg)[1] <- "frequency"
frequency_fourg$fourgram <- rownames(frequency_fourg)
rownames(frequency_fourg) <- NULL
frequency_fourg <- frequency_fourg[,c(2,1)]
frequency_fourg <- frequency_fourg[with(frequency_fourg, order(frequency, decreasing = TRUE)),]
frequency_fourg$prob <- frequency_fourg$frequency/sum(frequency_fourg$frequency)
write.csv(frequency_fourg, "fourg.csv")
#rm(frequency_fourg)

#five
frequency_fiveg <- as.data.frame(inspect(tdm_t5[freq_fiveg,]),namestringsAsFactors = FALSE)
rm(tdm_t5,freq_fiveg)
names(frequency_fiveg)[1] <- "frequency"
frequency_fiveg$fivegram <- rownames(frequency_fiveg)
rownames(frequency_fiveg) <- NULL
frequency_fiveg <- frequency_fiveg[,c(2,1)]
frequency_fiveg <- frequency_fiveg[with(frequency_fiveg, order(frequency, decreasing = TRUE)),]
frequency_fiveg$prob <- frequency_fiveg$frequency/sum(frequency_fiveg$frequency)
write.csv(frequency_fiveg, "fiveg.csv")
#rm(frequency_fiveg)


#rm (tdm_t1,tdm_t2,tdm_t3,tdm_t4)
#rm (freq_unig,freq_big, freq_trig, freq_fourg)

#wc_big <- wordcloud(freq_big, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

#Plotting frequency of Ngrams
##Plotting frequency of unigrams
#plot_freq_unig <- ggplot(frequency_unig, 
#                         aes(x = reorder(unigram, frequency), y = frequency)) +
#  geom_bar(stat = "identity") +
#  xlab("Unigram") + ylab("Frequency") +
#  labs(title = "Top Unigrams by Frequency") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#print(plot_freq_unig)
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


#test prediction
#1) The guy in front of me just bought a pound of bacon, a bouquet, and a case of: beer, soda, cheese, pretzels
#2) You're the reason why I smile everyday. Can you follow me please? It would mean the: world, best, universe, most
#3) Hey sunshine, can you follow me and make me the: happiest, bluest, smelliest, saddest
#4) Very early observations on the Bills game: Offense still struggling but the: crowd, referees, defense, players
#5) Go on a romantic date at the: mall, movies, beach, grocery
#6) Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my: phone, way, horse, motorcycle
#7) Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some: thing, years, time, weeks
#8) After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little: eyes, ears, toes, fingers
#9) Be grateful for the good times and keep the faith during the: worse, bad, sad, hard
#10) If this isn't the cutest thing you've ever seen, then you must be: callous, insane, asleep, insensitive

input <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
input <- c("You're the reason why I smile everyday. Can you follow me please? It would mean the")
input <- c("Hey sunshine, can you follow me and make me the")
input <- c("Very early observations on the Bills game: Offense still struggling but the")
input <- c("Go on a romantic date at the")
input <- c("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
input <- c("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
input <- c("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
input <- c("Be grateful for the good times and keep the faith during the")
input <- c("If this isn't the cutest thing you've ever seen, then you must be")
#process input

input<- Corpus(VectorSource(input))
input <- tm_map(input, content_transformer(tolower))
###remove numbers
input <- tm_map(input, removeNumbers)
###remove punctuation
input <- tm_map(input, removePunctuation)
###remove non-ASCII
input <- tm_map(input, removeNonASCII)
###remove stopwords
#input <- tm_map(input, removeWords, stopwords("english"))
###remove profane words
#
###remove whitespaces (for all text)
input <- tm_map(input, stripWhitespace)
input <- str_trim(input[[1]]$content)
##посчитать число слов и тогда только запускать 3 слова - 4хграмма, 2слова-3грамма,1слово-биграма,0-слов униграмма
wordcount <- sapply(gregexpr("\\W+", input), length) + 1

if (wordcount >= 4) {
    last_fourgram <- paste0(word(input,-4)," ", word(input,-3)," ", word(input,-2)," ", word(input,-1))
    seekfive <- grepl(paste0("^",last_fourgram, " "),frequency_fiveg$fivegram)
    subfive<- frequency_fiveg[seekfive,]
}

if (wordcount >= 3) {
    last_trigram <- paste0(word(input,-3)," ", word(input,-2)," ", word(input,-1))
    seekfour <- grepl(paste0("^",last_trigram, " "),frequency_fourg$fourgram)
    subfour<- frequency_fourg[seekfour,]
}
if (wordcount >= 2) {
    last_bigram <- paste0(word(input,-2)," ", word(input,-1))
    seektri <- grepl(paste0("^",last_bigram, " "), frequency_trig$trigram)
    subtri<- frequency_trig[seektri,]
}
if (wordcount >= 1) {
    last_unig <- word(input,-1)
    seekbi <- grepl(paste0("^",last_unig," "),frequency_big$bigram)
    subbi <- frequency_big[seekbi,]}
#if (wordcount == 0) {subuni <- frequency_unig[1,]}
Ngram <- c(as.character(subfive[1:3,]$fivegram),as.character(subfour[1:3,]$fourgram), as.character(subtri[1:3,]$trigram), as.character(subbi[1:3,]$bigram))
n_frequency <-c(subfive[1:3,]$frequency,subfour[1:3,]$frequency, subtri[1:3,]$frequency, subbi[1:3,]$frequency) 
N_prob <- c(subfive[1:3,]$prob, subfour[1:3,]$prob, subtri[1:3,]$prob, subbi[1:3,]$prob)
predictWord <- data.frame(N_gram=Ngram,freq = n_frequency, score=N_prob,stringsAsFactors = F)
predictWord$word <- sub('^.* ([[:alnum:]]+)$', '\\1', Ngram)
predictWord<-na.omit(predictWord)
#predictWord <- predictWord[with(predictWord, order(score, decreasing = TRUE)),]
predictWord
predictWord$word