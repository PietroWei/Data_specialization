# Installing necessary libraries
library(readr)
library(tm)
library(stringr)
library(ggplot2)
library(hunspell)
library(tokenizers)
library(textclean)

##### Cleaning #####
## Cleaning
# Read bad words from a CSV
bad_words <- read.csv("popular_profanity_100.csv", header = FALSE, stringsAsFactors = FALSE)
bad_words <- as.character(bad_words[[1]])
# Escape special characters for regex
bad_words <- str_replace_all(bad_words, "([.|()\\[\\]{}+*?^$\\\\])", "\\\\\\1")

# Make sure the file paths are correct
files <- c("final/en_US/en_US.news.txt", "final/en_US/en_US.blogs.txt", "final/en_US/en_US.twitter.txt")

# Read and combine the files
combined_lines <- lapply(files, function(file) {
  readLines(file, warn = FALSE, encoding = "UTF-8")
})

# Flatten the list into a single vector
combined_lines <- unlist(combined_lines)

# Sample 10% of the combined lines
set.seed(123)  # For reproducibility
sampled_lines <- sample(combined_lines, size = round(0.025 * length(combined_lines)))
cleaned_lines <- clean_text(sampled_lines)
writeLines(cleaned_lines, "cleaned_lines.txt")


###### FUNCTIONSSSS ##########
# Clean the text function
clean_text <- function(lines) {
  
  # Step 2: Create a text corpus
  corpus <- Corpus(VectorSource(lines))
  
  # Step 3: Preprocessing with `tm_map`
  corpus <- tm_map(corpus, content_transformer(tolower))                 # Convert to lowercase
  corpus <- tm_map(corpus, content_transformer(function(x) replace_contraction(x)))
  # Function to correct spelling
  correct_spelling <- function(x) {
    words <- unlist(strsplit(x, " "))
    corrected_words <- sapply(words, function(word) {
      suggestions <- hunspell::hunspell(word)
      if (length(suggestions[[1]]) > 0) {
        return(suggestions[[1]][1])  # Take the first suggestion
      } else {
        return(word)  # If no suggestion, keep the original word
      }
    })
    return(paste(corrected_words, collapse = " "))
  }
  
  # Apply to your corpus
  corpus <- tm_map(corpus, content_transformer(correct_spelling))
  corpus <- tm_map(corpus, content_transformer(trimws))                  # Trim whitespace
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\s+", " ", x))) # Replace multiple spaces with single space
  corpus <- tm_map(corpus, removePunctuation)                            # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers)                                # Remove numbers
  corpus <- tm_map(corpus, stripWhitespace)                              # Remove extra whitespace
  corpus <- tm_map(corpus, removeWords, stopwords("english"))            # Remove stopwords
  
  # Custom function to remove bad words one by one to avoid large regex issues
  for (word in bad_words) {
    corpus <- tm_map(corpus, content_transformer(function(x) gsub(word, "", x)))
  }
  
  # Step 5: Remove lines that are too short or too long
  corpus <- corpus[sapply(corpus, function(doc) nchar(as.character(doc)) > 20)]
  
  # Step 6: Remove duplicates
  lines_cleaned <- unique(sapply(corpus, as.character))
  corpus <- tm_map(corpus, stripWhitespace)                              # Remove extra whitespace
  # Return the cleaned lines
  return(lines_cleaned)
}



