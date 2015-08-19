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
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
FourgramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))
FivegramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 5, max = 5))
var1 <-readLines("en_US/en_US.blogs.txt", warn = FALSE, encoding="UTF-8")
var1<-gsub("ain't", " not ", var1)
var1<-gsub("'m", " am ", var1)
var1<-gsub("'ve", " have ", var1)
var1<-gsub("'ll", " will ", var1)
var1<-gsub("'re", " are ", var1)
var1<-gsub("can't", " cannot ", var1)
var1<-gsub("n't", " not ", var1)
var1<-gsub("'d", " would ", var1)
var1<-gsub("o'clock", "of clock", var1)
var1<-gsub("'s", " is ", var1)
var1<-gsub(" '","",var1)
var1<-gsub("' ","",var1)
var1<-gsub("[']{2,}","",var1)
var1 <- var1[600001:length(var1)]
corpus <- Corpus(VectorSource(var1))
rm (var1)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNonASCII)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
var1 <- TermDocumentMatrix(corpus, control = list(tokenize = FivegramTokenizer))
var1 <- rollup(var1[findFreqTerms(var1, 2),], 2, na.rm=TRUE, FUN = sum)
var1 <- as.data.frame(inspect(var1),namestringsAsFactors = FALSE)
names(var1)[1] <- "frequency"
var1$fivegram <- rownames(var1)
rownames(var1) <- NULL
var1 <- var1[,c(2,1)]
var1 <- var1[with(var1, order(frequency, decreasing = TRUE)),]
var1$prob <- var1$frequency/sum(var1$frequency)
write.csv(var1, "blog_p3_fiveg.csv")
rm(var1)
var1 <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
var1 <- rollup(var1[findFreqTerms(var1, 2),], 2, na.rm=TRUE, FUN = sum)
var1 <- as.data.frame(inspect(var1),namestringsAsFactors = FALSE)
names(var1)[1] <- "frequency"
var1$fourgram <- rownames(var1)
rownames(var1) <- NULL
var1 <- var1[,c(2,1)]
var1 <- var1[with(var1, order(frequency, decreasing = TRUE)),]
var1$prob <- var1$frequency/sum(var1$frequency)
write.csv(var1, "blog_p3_fourg.csv")
rm(var1)
var1 <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
var1 <- rollup(var1[findFreqTerms(var1, 2),], 2, na.rm=TRUE, FUN = sum)
var1 <- as.data.frame(inspect(var1),namestringsAsFactors = FALSE)
names(var1)[1] <- "frequency"
var1$trigram <- rownames(var1)
rownames(var1) <- NULL
var1 <- var1[,c(2,1)]
var1 <- var1[with(var1, order(frequency, decreasing = TRUE)),]
var1$prob <- var1$frequency/sum(var1$frequency)
write.csv(var1, "blog_p3_trig.csv")
rm(var1)
var1 <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
rm(corpus)
var1 <- rollup(var1[findFreqTerms(tdm_t2, 4),], 2, na.rm=TRUE, FUN = sum)
var1 <- as.data.frame(inspect(var1),namestringsAsFactors = FALSE)
names(var1)[1] <- "frequency"
var1$bigram <- rownames(var1)
rownames(var1) <- NULL
var1 <- var1[,c(2,1)]
var1 <- var1[with(var1, order(frequency, decreasing = TRUE)),]
var1$prob <- var1$frequency/sum(var1$frequency)
write.csv(var1, "blog_p3_big.csv")
rm(var1)
###
blog_bigr_1 <- read.csv("blog_p1_big.csv", header = TRUE, sep=',')
blog_bigr_2 <- read.csv("blog_p2_big.csv", header = TRUE, sep=',')
blog_bigr_3 <- read.csv("blog_p3_big.csv", header = TRUE, sep=',')
news_big <- read.csv("news_big.csv", header = TRUE, sep=',')
bigr <- rbind(blog_bigr_1, blog_bigr_2, blog_bigr_3,news_big)
rm(blog_bigr_1, blog_bigr_2, blog_bigr_3,news_big)
twitter_bigr_1 <- read.csv("twitter_p1_big.csv", header = TRUE, sep=',')
twitter_bigr_2 <- read.csv("twitter_p2_big.csv", header = TRUE, sep=',')
twitter_bigr_3 <- read.csv("twitter_p3_big.csv", header = TRUE, sep=',')
twitter_bigr_4 <- read.csv("twitter_p4_big.csv", header = TRUE, sep=',')
twitter_bigr_5 <- read.csv("twitter_p5_big.csv", header = TRUE, sep=',')
bigr <- rbind(bigr, twitter_bigr_1, twitter_bigr_2, twitter_bigr_3, twitter_bigr_4, twitter_bigr_5)
rm(twitter_bigr_1, twitter_bigr_2, twitter_bigr_3, twitter_bigr_4, twitter_bigr_5)
bigr <- bigr[c("bigram","frequency")]
bigr <- aggregate(frequency ~ bigram, data=bigr,sum)
bigr<-subset(bigr,bigr$frequency > 1)
bigr<-bigr[with(bigr, order(frequency, decreasing = TRUE)),]
write.csv(bigr, "big_agr_filt.csv")
rm(big)
blog_trig_1 <- read.csv("blog_p1_trig.csv", header = TRUE, sep=',')
blog_trig_2 <- read.csv("blog_p2_trig.csv", header = TRUE, sep=',')
blog_trig_3 <- read.csv("blog_p3_trig.csv", header = TRUE, sep=',')
news_trig <- read.csv("news_trig.csv", header = TRUE, sep=',')
trig <- rbind(blog_trig_1, blog_trig_2, blog_trig_3, news_trig)
rm(blog_trig_1, blog_trig_2, blog_trig_3,news_trig)
trig<-subset(trig,trig$frequency > 1)
trig <- aggregate(frequency ~ trigram, data = trig, sum)
twitter_trig_1 <- read.csv("twitter_p1_trig.csv", header = TRUE, sep=',')
twitter_trig_2 <- read.csv("twitter_p2_trig.csv", header = TRUE, sep=',')
twitter_trig_3 <- read.csv("twitter_p3_trig.csv", header = TRUE, sep=',')
twitter_trig_4 <- read.csv("twitter_p4_trig.csv", header = TRUE, sep=',')
twitter_trig_5 <- read.csv("twitter_p5_trig.csv", header = TRUE, sep=',')
trig <- rbind(trig, twitter_trig_1[2:3], twitter_trig_2[2:3], twitter_trig_3[2:3], twitter_trig_4[2:3], twitter_trig_5[2:3])
rm(twitter_trig_1, twitter_trig_2, twitter_trig_3, twitter_trig_4, twitter_trig_5)
trig <- trig[c("trigram","frequency")]
trig<-subset(trig,trig$frequency > 1)
trig <- aggregate(frequency ~ trigram, data = trig, sum)
trig<-trig[with(trig, order(frequency, decreasing = TRUE)),]
write.csv(trig, "trig_agr_filt.csv")
rm(trig)
blog_fourg_1 <- read.csv("blog_p1_fourg.csv", header = TRUE, sep=',')
blog_fourg_2 <- read.csv("blog_p2_fourg.csv", header = TRUE, sep=',')
blog_fourg_3 <- read.csv("blog_p3_fourg.csv", header = TRUE, sep=',')
news_fourg <- read.csv("news_fourg.csv", header = TRUE, sep=',')
fourg <- rbind(blog_fourg_1, blog_fourg_2, blog_fourg_3, news_fourg)
fourg<-subset(fourg,fourg$frequency > 1)
rm(blog_fourg_1, blog_fourg_2, blog_fourg_3, news_fourg)
fourg <- aggregate(frequency ~ fourgram, data = fourg, sum)
twitter_fourg_1 <- read.csv("twitter_p1_fourg.csv", header = TRUE, sep=',')
twitter_fourg_2 <- read.csv("twitter_p2_fourg.csv", header = TRUE, sep=',')
twitter_fourg_3 <- read.csv("twitter_p3_fourg.csv", header = TRUE, sep=',')
twitter_fourg_4 <- read.csv("twitter_p4_fourg.csv", header = TRUE, sep=',')
twitter_fourg_5 <- read.csv("twitter_p5_fourg.csv", header = TRUE, sep=',')
fourg <- rbind(fourg, twitter_fourg_1[2:3], twitter_fourg_2[2:3], twitter_fourg_3[2:3], twitter_fourg_4[2:3], twitter_fourg_5[2:3])
rm(twitter_fourg_1, twitter_fourg_2, twitter_fourg_3, twitter_fourg_4, twitter_fourg_5)
fourg<-subset(fourg,fourg$frequency > 1)
fourg <- fourg[c("fourgram","frequency")]
fourg <- aggregate(frequency ~ fourgram, data = fourg, sum)
fourg<-fourg[with(fourg, order(frequency, decreasing = TRUE)),]
write.csv(fourg, "fourg_agr_filt.csv")
rm(fourg)

blog_fiveg_1 <- read.csv("blog_p1_fiveg.csv", header = TRUE, sep=',')
blog_fiveg_1<-subset(blog_fiveg_1,blog_fiveg_1$frequency > 1)
blog_fiveg_2 <- read.csv("blog_p2_fiveg.csv", header = TRUE, sep=',')
blog_fiveg_2<-subset(blog_fiveg_2,blog_fiveg_2$frequency > 1)
blog_fiveg_3 <- read.csv("blog_p3_fiveg.csv", header = TRUE, sep=',')
blog_fiveg_3<-subset(blog_fiveg_3,blog_fiveg_3$frequency > 1)

news_fiveg <- read.csv("news_fiveg.csv", header = TRUE, sep=',')
news_fiveg<-subset(news_fiveg,news_fiveg$frequency > 1)
fiveg <- rbind(blog_fiveg_1, blog_fiveg_2, blog_fiveg_3, news_fiveg)
rm(blog_fiveg_1, blog_fiveg_2, blog_fiveg_3, news_fiveg)

fiveg <- aggregate(frequency ~ fivegram, data = fiveg, sum)
twitter_fiveg_1 <- read.csv("twitter_p1_fiveg.csv", header = TRUE, sep=',')
twitter_fiveg_1<-subset(twitter_fiveg_1,twitter_fiveg_1$frequency > 1)
twitter_fiveg_2 <- read.csv("twitter_p2_fiveg.csv", header = TRUE, sep=',')
twitter_fiveg_2<-subset(twitter_fiveg_2,twitter_fiveg_2$frequency > 1)
twitter_fiveg_3 <- read.csv("twitter_p3_fiveg.csv", header = TRUE, sep=',')
twitter_fiveg_3<-subset(twitter_fiveg_3,twitter_fiveg_3$frequency > 1)
twitter_fiveg_4 <- read.csv("twitter_p4_fiveg.csv", header = TRUE, sep=',')
twitter_fiveg_4<-subset(twitter_fiveg_4,twitter_fiveg_4$frequency > 1)
twitter_fiveg_5 <- read.csv("twitter_p5_fiveg.csv", header = TRUE, sep=',')
twitter_fiveg_5<-subset(twitter_fiveg_5,twitter_fiveg_5$frequency > 1)
fiveg <- rbind(fiveg, twitter_fiveg_1[2:3], twitter_fiveg_2[2:3], twitter_fiveg_3[2:3], twitter_fiveg_4[2:3], twitter_fiveg_5[2:3])
rm(twitter_fiveg_1, twitter_fiveg_2, twitter_fiveg_3, twitter_fiveg_4, twitter_fiveg_5)
fiveg <- fiveg[c("fivegram","frequency")]
fiveg<-subset(fiveg,fiveg$frequency > 1)
fiveg <- aggregate(frequency ~ fivegram, data = fiveg, sum)

fiveg<-fiveg[with(fiveg, order(frequency, decreasing = TRUE)),]
write.csv(fiveg, "fiveg_agr_filt.csv")
rm(fiveg)


#maximum likelyhood estimate

unigram<-read.csv("unigram_agr_filt.csv")
bigram<-read.csv("big_agr_filt.csv")

unigram <- unigram[2:3]
names(unigram)[2] <- "unigram_frequency"
unigram$unigram_prob <- unigram$unigram_frequency / sum(unigram$unigram_frequency)
bigram <- bigram[2:3]
names(bigram)[2] <- "bigram_frequency"
bigram$bigram_prob <- bigram$bigram_frequency / sum(bigram$bigram_frequency)
bigram$unigram <- word(bigram$bigram, 1)
all <- merge(x = bigram, y = unigram, by="unigram", all.x = TRUE)
#MLE= maximum likelyhoude estimate = freq_bigr/freq_unig
all$bigram_MLE<-all$bigram_frequency/all$unigram_frequency
all<-all[with(all, order(bigram_frequency, decreasing = TRUE)),]
#bigram<-all
write.csv(all,"bigram_MLE.csv")
#trigram
trigram<-read.csv("trig_agr_filt.csv")
trigram <- trigram[2:3]
names(trigram)[2] <- "trigram_frequency"
trigram$trigram_prob <- trigram$trigram_frequency / sum(trigram$trigram_frequency)
trigram$bigram<-paste0(word(trigram$trigram,1)," ",word(trigram$trigram,2))
#inner join trigram on bigram by bigram column
all <- merge(x = trigram, y = all, by="bigram")
all$trigram_MLE<-all$trigram_frequency/all$bigram_frequency
all<-all[with(all, order(trigram_frequency, decreasing = TRUE)),]
write.csv(all,"trigram_MLE.csv")
#fourgram
fourgram<-read.csv("fourg_agr_filt.csv")
fourgram <- fourgram[2:3]
names(fourgram)[2] <- "fourgram_frequency"
fourgram$fourgram_prob <- fourgram$fourgram_frequency / sum(fourgram$fourgram_frequency)
fourgram$trigram<-paste0(word(fourgram$fourgram,1)," ",word(fourgram$fourgram,2)," ",word(fourgram$fourgram,3))
#inner join trigram on bigram by bigram column
all <- merge(x = fourgram, y = all, by="trigram")
all$fourgram_MLE<-all$fourgram_frequency/all$trigram_frequency
all<-all[with(all, order(fourgram_frequency, decreasing = TRUE)),]
write.csv(all,"fourgram_MLE.csv")
#fivegram
fivegram<-read.csv("fiveg_agr_filt.csv")
fivegram <- fivegram[2:3]
names(fivegram)[2] <- "fivegram_frequency"
fivegram$fivegram_prob <- fivegram$fivegram_frequency / sum(fivegram$fivegram_frequency)
fivegram$fourgram<-paste0(word(fivegram$fivegram,1)," ",word(fivegram$fivegram,2)," ",word(fivegram$fivegram,3)," ",word(fivegram$fivegram,4))
#inner join trigram on bigram by bigram column
all <- merge(x = fivegram, y = all, by="fourgram")
all$fivegram_MLE<-all$fivegram_frequency / all$fourgram_frequency
all<-all[with(all, order(fivegram_frequency, decreasing = TRUE)),]
write.csv(all,"fivegram_MLE.csv")


#unigtdm
var1<-c(readLines("en_US/en_US.news.txt", warn = FALSE, encoding="UTF-8"),readLines("en_US/en_US.blogs.txt", warn = FALSE, encoding = "UTF-8"),readLines("en_US/en_US.twitter.txt", warn = FALSE, encoding="UTF-8"))
x<-split(var1,ceiling(seq_along(sample(var1))/667339))

for (i in 1:5){
var1 <- Corpus(VectorSource(x[i]))
var1 <- tm_map(var1, removeNumbers)

var1 <- tm_map(var1, removePunctuation)

var1 <- tm_map(var1, removeNonASCII)

var1 <- tm_map(var1, stripWhitespace)

var1 <- tm_map(var1, content_transformer(tolower))
var1 <- TermDocumentMatrix(var1)
var1<-rollup(var1[findFreqTerms(var1, 2),], 2, FUN = sum)
var1 <- as.data.frame(inspect(var1),namestringsAsFactors = FALSE)
names(var1)[1] <- "unigram_frequency"
var1$unigram <- rownames(var1)
rownames(var1) <- NULL
var1 <- var1[,c(2,1)]
var1 <- var1[with(var1, order(unigram_frequency, decreasing = TRUE)),]
var1$unigram_prob <- var1$unigram_frequency/sum(var1$unigram_frequency)
write.csv(var1, paste0("unig_p",as.character(i),".csv"))
}

###gsub

all <-c(readLines("en_US/en_US.news.txt", warn = FALSE, encoding = "UTF-8"),readLines("en_US/en_US.blogs.txt", warn = FALSE, encoding = "UTF-8"),readLines("en_US/en_US.twitter.txt", warn = FALSE, encoding = "UTF-8"))
all<-gsub("ain't", " not ", all)#substitute \u0093 on '
all<-gsub("['\u0093]m", " am ", all)
all<-gsub("['\u0093]ve", " have ", all)
all<-gsub("['\u0093]ll", " will ", all)
all<-gsub("['\u0093]re", " are ", all)
all<-gsub("can['\u0093]t", " cannot ", all)
all<-gsub("n['\u0093]t", " not ", all)
all<-gsub("['\u0093]d", " would ", all)
all<-gsub("o['\u0093]clock", "of clock", all)
all<-gsub("['\u0093]s", " is ", all)
all<-gsub("[^ a-zA-Z]"," ",all)
all<-gsub("[ ]{2,}", " ", all)
all<-tolower(all)
all<-str_trim(all)

test <- grepl(input,all, ignore.case = T)
all[test]


#group by freq
bigram<-read.csv("bigram_MLE.csv")
bigram<-bigram[,-1]
bigram_temp1<-bigram
bigram_temp1$count<-1
count<-aggregate(bigram_temp1[,c("count")], by = list(bigram_temp1$bigram_frequency), sum)
d=count[1,2]/(count[1,2]+2*count[2,2]) #count d=N3/(n3+2*n4)
#our discount
d=0.75
x<-bigram_temp1[bigram_temp1$unigram_frequency<bigram_temp1$bigram_frequency,]
y<-bigram_temp1[bigram_temp1$unigram_frequency>=bigram_temp1$bigram_frequency,]
x$unigram_frequency<-x$bigram_frequency+1
bigram_temp1<-rbind(x,y)
rm (x,y)
bigram_temp1$bigram_MLE<-(bigram_temp1$bigram_frequency-d)/bigram_temp1$unigram_frequency
#lambda coef for each unigram-bigram
bigram_temp1$count<-1
bigram_temp2<-aggregate(bigram_temp1[,c("count")],by=list(bigram_temp1$unigram),sum)


names(bigram_temp2)[2]<-"number_cont_bigr" #number of bigrams that starts from particular unigram
names(bigram_temp2)[1]<-"unigram"
bigram_temp1<-merge(x=bigram_temp1,y=bigram_temp2,by="unigram")
bigram_temp1$bigram_lamda<-d/bigram_temp1$unigram_frequency*bigram_temp1$number_cont_bigr
#p-continuention
bigram_temp1$predict_word<-word(bigram_temp1$bigram,-1)

bigram_temp1$count<-1

bigram_temp2<-aggregate(bigram_temp1[,c("count")],by=list(bigram_temp1$predict_word),sum)
names(bigram_temp2)[2]<-"number_novel_cont" #number of bigrams that starts from particular unigram
names(bigram_temp2)[1]<-"predict_word"
bigram_temp2<-merge(x=bigram_temp1,y=bigram_temp2,by="predict_word")
bigram_temp2$bigram_KNN<-bigram_temp2$bigram_MLE+bigram_temp2$bigram_lamda*(bigram_temp2$number_novel_cont/sum(bigram_temp2$bigram_frequency))
bigram_temp1 <- grepl("[^[:alpha:] ]{1,}",bigram_temp2$bigram)
bigram_temp2<- bigram_temp2[!bigram_temp1,]
bigram_temp1<-bigram_temp2[c("bigram","unigram","unigram_frequency","unigram_prob","bigram_frequency","bigram_prob","bigram_KNN")]
bigram_temp1<-bigram_temp1[with(bigram_temp1, order(bigram_KNN, decreasing = TRUE)),]

write.csv(bigram_temp1,"bigram_knn.csv")
#trigram
trigram<-read.csv("trigram_MLE.csv")
trigram_temp1<-merge(x=trigram[c("trigram","trigram_frequency","trigram_prob", "trigram_MLE","bigram")], y=bigram_temp1,by="bigram")
trigram_temp2 <- grepl("[^[:alpha:] ]",trigram_temp1$trigram)
trigram_temp1<- trigram_temp1[!trigram_temp2,]

trigram_temp1$trigram_MLE<-(trigram_temp1$trigram_frequency-d)/trigram_temp1$bigram_frequency
trigram_temp1$count<-1
trigram_temp1$predict_word<-word(trigram_temp1$trigram,-1)
trigram_temp2<-aggregate(trigram_temp1[,c("count")],by=list(trigram_temp1$bigram),sum)
names(trigram_temp2)[2]<-"number_cont" #number of bigrams that starts from particular unigram
names(trigram_temp2)[1]<-"bigram"
trigram_temp1<-merge(x=trigram_temp1,y=trigram_temp2,by="bigram")
trigram_temp1$count<-1
trigram_temp2<-aggregate(trigram_temp1[,c("count")],by=list(trigram_temp1$predict_word),sum)
names(trigram_temp2)[2]<-"number_novel_cont" #number of bigrams that starts from particular unigram
names(trigram_temp2)[1]<-"predict_word"
trigram_temp1<-merge(x=trigram_temp1,y=trigram_temp2,by="predict_word")
trigram_temp1$trigram_lamda<-d/trigram_temp1$bigram_frequency*trigram_temp1$number_cont
trigram_temp1$trigram_KNN<-trigram_temp1$trigram_MLE+trigram_temp1$trigram_lamda*(trigram_temp1$number_novel_cont/sum(trigram_temp1$trigram_frequency))
trigram_temp1<-trigram_temp1[with(trigram_temp1, order(trigram_KNN, decreasing = TRUE)),]
write.csv(trigram_temp1,"trigram_KNN.csv")
trigram_temp2<-trigram_temp1[c("bigram","unigram","unigram_frequency","unigram_prob","bigram_frequency","bigram_prob","bigram_KNN","trigram","trigram_frequency","trigram_prob", "trigram_KNN")]
#four
fourgram<-read.csv("fourgram_MLE.csv")
fourgram_temp1<-merge(x=trigram_temp2, y=fourgram[c("fourgram","fourgram_MLE","fourgram_prob","fourgram_frequency","trigram")],by="trigram")
fourgram_temp2 <- grepl("[^[:alpha:] ]",fourgram_temp1$fourgram)
fourgram_temp1<- fourgram_temp1[!fourgram_temp2,]

fourgram_temp1$fourgram_MLE<-(fourgram_temp1$fourgram_frequency-d)/fourgram_temp1$trigram_frequency
fourgram_temp1$count<-1
fourgram_temp1$predict_word<-word(fourgram_temp1$fourgram,-1)
fourgram_temp2<-aggregate(fourgram_temp1[,c("count")],by=list(fourgram_temp1$trigram),sum)
names(fourgram_temp2)[2]<-"number_cont" #number of bigrams that starts from particular unigram
names(fourgram_temp2)[1]<-"trigram"
fourgram_temp1<-merge(x=fourgram_temp1,y=fourgram_temp2,by="trigram")
fourgram_temp1$count<-1
fourgram_temp2<-aggregate(fourgram_temp1[,c("count")],by=list(fourgram_temp1$predict_word),sum)
names(fourgram_temp2)[2]<-"number_novel_cont" #number of bigrams that starts from particular unigram
names(fourgram_temp2)[1]<-"predict_word"
fourgram_temp1<-merge(x=fourgram_temp1,y=fourgram_temp2,by="predict_word")
fourgram_temp1$fourgram_lamda<-d/fourgram_temp1$trigram_frequency*fourgram_temp1$number_cont
fourgram_temp1$fourgram_KNN<-fourgram_temp1$fourgram_MLE+fourgram_temp1$fourgram_lamda*(fourgram_temp1$number_novel_cont/sum(fourgram_temp1$fourgram_frequency))
fourgram_temp1<-fourgram_temp1[with(fourgram_temp1, order(fourgram_KNN, decreasing = TRUE)),]
write.csv(fourgram_temp1,"fourgram_KNN.csv")
fourgram_temp2<-fourgram_temp1[c("bigram","unigram","unigram_frequency","unigram_prob","bigram_frequency","bigram_prob","bigram_KNN","trigram","trigram_frequency","trigram_prob", "trigram_KNN","fourgram","fourgram_KNN","fourgram_prob","fourgram_frequency")]
#five
fivegram<-read.csv("fivegram_MLE.csv")
fivegram_temp1<-merge(x=fourgram_temp2, y=fivegram[c("fivegram","fivegram_MLE","fivegram_prob","fivegram_frequency","fourgram")],by="fourgram")
fivegram_temp2 <- grepl("[^[:alpha:] ]",fivegram_temp1$fivegram)
fivegram_temp1<- fivegram_temp1[!fivegram_temp2,]

fivegram_temp1$fivegram_MLE<-(fivegram_temp1$fivegram_frequency-d)/fivegram_temp1$fourgram_frequency
fivegram_temp1$count<-1
fivegram_temp1$predict_word<-word(fivegram_temp1$fivegram,-1)
fivegram_temp2<-aggregate(fivegram_temp1[,c("count")],by=list(fivegram_temp1$fourgram),sum)
names(fivegram_temp2)[2]<-"number_cont" #number of bigrams that starts from particular unigram
names(fivegram_temp2)[1]<-"fourgram"
fivegram_temp1<-merge(x=fivegram_temp1,y=fivegram_temp2,by="fourgram")
fivegram_temp1$count<-1
fivegram_temp2<-aggregate(fivegram_temp1[,c("count")],by=list(fivegram_temp1$predict_word),sum)
names(fivegram_temp2)[2]<-"number_novel_cont" #number of bigrams that starts from particular unigram
names(fivegram_temp2)[1]<-"predict_word"
fivegram_temp1<-merge(x=fivegram_temp1,y=fivegram_temp2,by="predict_word")
fivegram_temp1$fivegram_lamda<-d/fivegram_temp1$fourgram_frequency*fivegram_temp1$number_cont
fivegram_temp1$fivegram_KNN<-fivegram_temp1$fivegram_MLE+fivegram_temp1$fivegram_lamda*(fivegram_temp1$number_novel_cont/sum(fivegram_temp1$fivegram_frequency))
fivegram_temp1<-fivegram_temp1[with(fivegram_temp1, order(fivegram_KNN, decreasing = TRUE)),]
write.csv(fivegram_temp1,"fivegram_KNN.csv")
fivegram_temp2<-fivegram_temp1[c("bigram","unigram","unigram_frequency","unigram_prob","bigram_frequency","bigram_prob","bigram_KNN","trigram","trigram_frequency","trigram_prob", "trigram_KNN","fourgram","fourgram_KNN","fourgram_prob","fourgram_frequency","fivegram","fivegram_MLE","fivegram_prob","fivegram_frequency")]
