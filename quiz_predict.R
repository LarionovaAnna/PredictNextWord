setwd("E:/Personal/Repos/Coursera-SwiftKey")
library(tm)
library(rJava)
library(RWeka)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(tm.plugin.webmining)
library(stringr)
library(slam)
#load data
profanity <- readLines("bad_words.txt", warn = FALSE)
#unigr<-read.csv("unig_agr_filt.csv")
#bigr<-read.csv("big_agr_filt.csv")
#trigr<-read.csv("trig_agr_filt.csv")
#fourgr<-read.csv("fourg_agr_filt.csv")
#fivegr<-read.csv("fiveg_agr_filt.csv")
#unigr<-unigr[2:3]
#unigr$prob <- unigr$frequency/sum(unigr$frequency)
#bigr<-bigr[2:3]
#bigr$prob <- bigr$frequency/sum(bigr$frequency)
#trigr<-trigr[2:3]
#trigr$prob <- trigr$frequency/sum(trigr$frequency)
#fourgr<-fourgr[2:3]
#fourgr$prob <- fourgr$frequency/sum(fourgr$frequency)
#fivegr<-fivegr[2:3]
#fivegr$prob <- fivegr$frequency/sum(fivegr$frequency)
#quiz 2
bigr<-read.csv("bigram_score.csv", stringsAsFactors = F)
bigr<-bigr[,-1]
#
trigr<-read.csv("trigram_score.csv", stringsAsFactors = F)
trigr<-trigr[,-1]
#
fourgr<-read.csv("fourgram_score.csv", stringsAsFactors = F)
fourgr<-fourgr[,-1]
#
fivegr<-read.csv("fivegram_score.csv", stringsAsFactors = F)
fivegr<-fivegr[,-1]

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
#quiz3
input <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
input <- c("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
input <- c("I'd give anything to see arctic monkeys this")
input <- c("Talking to your mom has the same effect as a hug and helps reduce your")
input <- c("When you were in Holland you were like 1 inch away from me but you hadn't time to take a")
input <- c("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the")
input <- c("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")
input <- c("Every inch of you is perfect from the bottom to the")
input <- c("I’m thankful my childhood was filled with imagination and bruises from playing")
input <- c("I like how the same people are in almost all of Adam Sandler's")

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
input <- tm_map(input, removeWords, profanity)
###remove whitespaces (for all text)
input <- tm_map(input, stripWhitespace)
input <- str_trim(input[[1]]$content)
##посчитать число слов и тогда только запускать 3 слова - 4хграмма, 2слова-3грамма,1слово-биграма,0-слов униграмма
wordcount <- sapply(gregexpr("\\W+", input), length) + 1
N_KNN<-c()
Predict<-c()
if (wordcount >= 4) {
    last_fourgram <- paste0(word(input,-4)," ", word(input,-3)," ", word(input,-2)," ", word(input,-1))
    seekfive <- grepl(paste0("^",last_fourgram, " "),fivegr$fivegram)
    subfive<- fivegr[seekfive,]
    Predict <- append(Predict, subfive$predict)
    N_KNN <- append(N_KNN, subfive$fivegram_KNN)
}

if (wordcount >= 3) {
    last_trigram <- paste0(word(input,-3)," ", word(input,-2)," ", word(input,-1))
    seekfour <- grepl(paste0("^",last_trigram, " "),fourgr$fourgram)
    subfour<- fourgr[seekfour,]
    Predict <- append(Predict, subfour$predict)
    N_KNN <- append(N_KNN, subfour$fourgram_KNN)
}
if (wordcount >= 2) {
    last_bigram <- paste0(word(input,-2)," ", word(input,-1))
    seektri <- grepl(paste0("^",last_bigram, " "), trigr$trigram)
    subtri<- trigr[seektri,]
    Predict <- append(Predict, subtri$predict)
    N_KNN <- append(N_KNN, subtri$trigram_KNN)
}
if (wordcount >= 1) {
    last_unig <- word(input,-1)
    seekbi <- grepl(paste0("^",last_unig," "),bigr$bigram)
    subbi <- bigr[seekbi,]
    Predict <- append(Predict, subbi$predict)
    N_KNN <- append(N_KNN, subbi$bigram_KNN)
}
#if (wordcount == 0) {subuni <- frequency_unig[1,]}
#Ngram <- c(as.character(subfive$fivegram),as.character(subfour$fourgram), as.character(subtri$trigram), as.character(subbi$bigram))
#N_KNN <- c(subfive$fivegram_KNN, subfour$fourgram_KNN, subtri$trigram_KNN, subbi$bigram_KNN)
predictWord <- data.frame(Predict=Predict,score=N_KNN,stringsAsFactors = F)
predictWord<-na.omit(predictWord)
predictWord<-aggregate(predictWord[,c("score")],by=list(predictWord$Predict),sum)
names(predictWord)[1:2]<-c("next word","score")
predictWord<-predictWord[with(predictWord,order(score, decreasing = T)),]

head(predictWord,100)
