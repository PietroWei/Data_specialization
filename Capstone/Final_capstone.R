# Installing necessary libraries
library(readr)
library(tm)
library(stringr)
library(ggplot2)
library(hunspell)
library(tokenizers)
library(textclean)
library(tidytext)
library(stringr)
library(purrr)
library(dplyr)
library(data.table)
library(tidyr)

##### Cleaning #####
combined_lines <- readLines("cleaned_lines.txt", warn = FALSE)

unigrams <- tokenize_words(combined_lines) %>% unlist()
bigrams <- tokenize_ngrams(combined_lines, n = 2) %>% unlist()
trigrams <- tokenize_ngrams(combined_lines, n = 3) %>% unlist()




library(dplyr)

# Calculate probabilities function
calculate_ngram_probabilities <- function(ngrams, vocabulary_size) {
  ngram_counts <- data.frame(ngrams = ngrams) %>%
    count(ngrams, name = "freq")  # Efficiently count n-grams
  
  # Add-1 smoothing and calculate probabilities
  ngram_counts <- ngram_counts %>%
    mutate(probability = (freq + 1) / (sum(freq) + vocabulary_size))  # Add-1 smoothing
  
  return(ngram_counts)
}

vocabulary_size <- length(unique(unigrams))

# Calculate unigram probabilities
unigram_data <- calculate_ngram_probabilities(unigrams, vocabulary_size)
unigram_data <- unigram_data %>%
  mutate(prob = freq / sum(freq))
unigram_data <- unigram_data %>% filter(freq >= 5)

# Extract word pairs for bigrams and trigrams
bigram_data <- data.frame(ngrams = bigrams) %>%
  separate(ngrams, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  count(word1, word2, name = "freq")
bigram_data <- bigram_data %>% filter(freq >= 2)


trigram_data <- data.frame(ngrams = trigrams) %>%
  separate(ngrams, into = c("word1", "word2", "word3"), sep = " ", remove = FALSE) %>%
  count(word1, word2, word3, name = "freq")
trigram_data <- trigram_data %>% filter(freq > 1)

# Calculate probabilities for bigrams
bigram_data <- bigram_data %>%
  group_by(word1) %>%
  mutate(prob = freq / sum(freq)) %>%
  ungroup()

# Calculate probabilities for trigrams
trigram_data <- trigram_data %>%
  group_by(word1, word2) %>%
  mutate(prob = freq / sum(freq)) %>%
  ungroup()

# The data frames `unigram_data`, `bigram_data`, and `trigram_data` should now have the calculated probabilities

get_probability <- function(word1, word2 = NULL, word3 = NULL) {
  if (!is.null(word3)) {
    # Check trigram probability
    prob <- trigram_data %>%
      filter(word1 == !!word1 & word2 == !!word2 & word3 == !!word3) %>%
      pull(prob)
    if (length(prob) > 0) return(prob)
  }
  
  if (!is.null(word2)) {
    # Check bigram probability
    prob <- bigram_data %>%
      filter(word1 == !!word1 & word2 == !!word2) %>%
      pull(prob)
    if (length(prob) > 0) return(prob)
  }
  
  # Default to unigram probability
  prob <- unigram_data %>%
    filter(ngrams == word1) %>%
    pull(prob)
  return(ifelse(length(prob) > 0, prob, 0))
}

# Function to predict the next word using backoff method
predict_next_word <- function(sentence) {
  words <- str_split(sentence, " ")[[1]]
  n <- length(words)
  
  if (n >= 2) {
    word1 <- words[n-1]
    word2 <- words[n]
    
    # Try trigrams
    trigram_matches <- trigram_data %>%
      filter(word1 == !!word1 & word2 == !!word2) %>%
      arrange(desc(prob))
    if (nrow(trigram_matches) > 0) {
      return(trigram_matches$word3[1])
    }
    
    # Try bigrams
    bigram_matches <- bigram_data %>%
      filter(word1 == !!word2) %>%
      arrange(desc(prob))
    if (nrow(bigram_matches) > 0) {
      return(bigram_matches$word2[1])
    }
  }
  
  # Default to unigrams
  unigram_matches <- unigram_data %>%
    arrange(desc(prob))
  return(unigram_matches$ngrams[1])
}

# Example usage
sentence <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
next_word <- predict_next_word(sentence)
print(next_word)



