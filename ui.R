library(shiny)
library(tm)
library(rJava)
library(RWeka)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(tm.plugin.webmining)
library(stringr)
library(slam)

shinyUI(fluidPage(
    includeCSS("./Data/bootstrap.css"),
    pageWithSidebar(
        headerPanel("Data Science Specialization - Capstone Project of Larionova Anna"),
        sidebarPanel(
            h3("About"),
            p("Enter your text in text area, press 'Submit' button. First start might take some time to load data."),
            p("You must enter at least one word in English language."),
            p("You will recieve an error if no ngram is found for your input. But this is pretty rare unless you mistype :)"),
            p("Ngrams were created with 'tm' and 'Rweka' packages, ngrams with frequency 2 and more were placed in dictionary."),
            p("This app uses Kneser-Ney Smoothing for ngram interpolation. Max ngram order is 5."),
            p("Profane words are deleted but stopwords aren't. Punctuation, numbers and extra-spaces are deleted. Text is set to lower case.")
            
        ),
        mainPanel(
            textInput("input", "Input Area:", ""),
            submitButton('Submit'),
            h3("Results"),
            textOutput("message"),
            tabsetPanel(type = "tabs",
                tabPanel("Table", tableOutput("Prediction")),
                tabPanel("Plot", plotOutput('plot'))
            )
        )
    )
)
)