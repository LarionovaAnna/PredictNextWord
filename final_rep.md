Coursera Capstone Project: Predict Next Word App
========================================================
author: LarionovaAnna
date: 19.08.2015
transition: rotate


Introduction
========================================================
<style>
body {
background-image: -moz-linear-gradient(top, #9cc3ff, #96a2b6);
background-image: -ms-linear-gradient(top, #9cc3ff, #96a2b6);
background-image: -o-linear-gradient(top, #9cc3ff, #96a2b6);
background-image: -webkit-gradient(linear, center top, center bottom, from(#9cc3ff), to(#96a2b6));
background-image: -webkit-linear-gradient(top, #9cc3ff, #96a2b6);
background-image: linear-gradient(top, #9cc3ff, #96a2b6);
}
</style>

- Predictive text is an input technology currently available in a lot of different devices.
- Predictive text makes efficient use of fewer device keys to input writing into a text message, an e-mail, an address book, a calendar, among others.
- The goal of this project is to develop a predictive text model that can predict the next word when a user enters a phrase.
- The trained model will be integrated into a Shiny App and deployed online.

Text Predictive Model
========================================================

- This app uses Kneser-Ney Smoothing for ngram interpolation. Max ngram order is 5.
- For each ngram we counted its frequency, probability, maximum likelihood estimation, lambda coefficient, probability of continuation, ngram types count, discount coefficient for Kneser-Ney Smoothing.
- Profane words are deleted but stopwords aren't. Punctuation, numbers and extra-spaces are deleted. Text is set to lower case.
- All ngrams that occured once in corpus were deleted from model.

User Interface
========================================================

- The Shiny App is available [here](https://larionovaanna.shinyapps.io/PredictNextWord)

![picture of Coursera-SwiftKey](final_rep-figure/1.PNG)

Future Improvements
========================================================

- Use additional corpora of ideoms, proverbs, expressions, etc.
- Use recurrent neural network language model
