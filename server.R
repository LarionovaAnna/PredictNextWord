library(shiny)
library(RWeka)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(tm.plugin.webmining)
library(stringr)
library(slam)

profanity <- readLines("./Data/bad_words.txt", warn = FALSE)
bigr<-read.csv("./Data/bigram_score.csv", stringsAsFactors = F)
bigr<-bigr[,-1]
trigr<-read.csv("./Data/trigram_score.csv", stringsAsFactors = F)
trigr<-trigr[,-1]
fourgr<-read.csv("./Data/fourgram_score.csv", stringsAsFactors = F)
fourgr<-fourgr[,-1]
fivegr<-read.csv("./Data/fivegram_score.csv", stringsAsFactors = F)
fivegr<-fivegr[,-1]

pred_word <- function(input){
    input<- Corpus(VectorSource(input))
    input <- tm_map(input, content_transformer(tolower))
    input <- tm_map(input, removeNumbers)
    input <- tm_map(input, removePunctuation)
    input <- tm_map(input, removeNonASCII)
    input <- tm_map(input, removeWords, profanity)
    input <- tm_map(input, stripWhitespace)
    input <- str_trim(input[[1]]$content)
    N_KNN <- c()
    predict<-c()
    wordcount <- sapply(gregexpr("\\W+", input), length) + 1
    
    if (wordcount >= 4) {
        last_fourgram <- paste0(word(input,-4)," ", word(input,-3)," ", word(input,-2)," ", word(input,-1))
        seekfive <- grepl(paste0("^",last_fourgram, " "),fivegr$fivegram)
        subfive<- fivegr[seekfive,]
        predict <- append(predict,subfive$predict)
        N_KNN <-  append(N_KNN,subfive$fivegram_KNN)
    }
    
    if (wordcount >= 3) {
        last_trigram <- paste0(word(input,-3)," ", word(input,-2)," ", word(input,-1))
        seekfour <- grepl(paste0("^",last_trigram, " "),fourgr$fourgram)
        subfour<- fourgr[seekfour,]
        
        predict <- append(predict,subfour$predict)
        N_KNN <- append(N_KNN, subfour$fourgram_KNN)
    }
    if (wordcount >= 2) {
        last_bigram <- paste0(word(input,-2)," ", word(input,-1))
        seektri <- grepl(paste0("^",last_bigram, " "), trigr$trigram)
        subtri<- trigr[seektri,]
        predict <- append(predict, subtri$predict)
        N_KNN <- append(N_KNN, subtri$trigram_KNN)
    }
    if (wordcount >= 1) {
        last_unig <- word(input,-1)
        seekbi <- grepl(paste0("^",last_unig," "),bigr$bigram)
        subbi <- bigr[seekbi,]
        predict <- append(predict, subbi$predict)
        N_KNN <- append(N_KNN, subbi$bigram_KNN)
    }
    
    predictWord <- data.frame(predict=predict,score=N_KNN,stringsAsFactors = F)
    predictWord<-na.omit(predictWord)
    x<-apply(predictWord, 1, function(x) !any(x %in% profanity))
    predictWord<-predictWord[x,]
    if(nrow(predictWord) == 0) return(predictWord)
    predictWord<-aggregate(predictWord[,c("score")],by=list(predictWord$predict),sum)
    names(predictWord)[1:2]<-c("next word","score")
    predictWord<-predictWord[with(predictWord,order(score, decreasing = T)),]
    rownames(predictWord)<-NULL
    head(predictWord,30) 
    
    
}

shinyServer(
    function(input, output) {
        result<-reactive({pred_word(input$input)})
        output$message<- renderText({if (nrow(result())==0) print(c("nothing was found"))})
        output$Prediction <- renderTable({
            withProgress(message = 'Calculation in progress', value=0, {
                for (i in 1:10) {
                    
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/10, detail = paste("Doing part", i))
                    
                    # Pause for 0.1 seconds to simulate a long computation.
                    Sys.sleep(0.1)}
            })
            
            result()}, digits=6)
        output$plot <- renderPlot({
            if (nrow(result())>0)
            ggplot(result(), 
                   aes(x = reorder(`next word`, score), y = score)) +
                   geom_bar(stat = "identity") +
                   xlab("Next word") + ylab("Score") +
                   labs(title = "Top next words by score") +
                   theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
    }
)