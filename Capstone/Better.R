#######
#Installing

install.packages("readr")
library(readr)
profanity_data <- read.csv("English.csv", stringsAsFactors = FALSE)

clean_text <- function(files) {
  # Step 1: Read the data from all files
  lines <- readLines(files, warn = FALSE)
  
  # Step 2: Remove empty lines
  lines <- lines[lines != ""]
  
  # Step 3: Remove duplicate lines (case-insensitive)
  lines <- unique(tolower(lines))
  
  # Step 4: Remove unnecessary content (e.g., non-alphanumeric characters and extra spaces)
  lines <- gsub("[^a-zA-Z0-9[:space:]]", "", lines)  # Remove non-alphanumeric characters
  lines <- gsub("\\s+", " ", lines)  # Replace multiple spaces with a single space
  
  # Step 5: Optionally, remove lines that are too short or too long
  lines <- lines[nchar(lines) > 20 ]  # Length filter, adjust as needed
  # Step 5: Remove vulgar words
  # Remove lines containing any bad words from the list
  lines <- lines[!grepl(paste(bad_words, collapse = "|"), lines)]
  
  # Step 6: Remove stopwords (optional, you can use the previous stopword list here)
  
  stopwords <- c("the", "and", "is", "in", "to", "of", "a", "for", "on", "with", "it", "an", "as", "i". "my")  # Example stopwords
  words <- unlist(strsplit(lines, " "))
  words <- words[!words %in% stopwords]
  
  # Recombine the words back into lines
  lines <- paste(words, collapse = " ")
  
  
  # Return the cleaned lines
  return(lines)
}

# Apply the function to the three text files
cleaned_lines_news <- clean_text("en_US.news.txt")
cleaned_lines_blogs <- clean_text("en_US.blogs.txt")
cleaned_lines_twitt <- clean_text("en_US.twitter.txt")
