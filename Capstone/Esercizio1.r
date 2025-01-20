#####
# Define file paths
blogs_file <- "en_US.blogs.txt"
news_file <- "en_US.news.txt"
twitter_file <- "en_US.twitter.txt"

# Function to find the longest line in a file
find_longest_line <- function(file) {
  lines <- readLines(file, warn = FALSE)  # Read all lines
  max_length <- max(nchar(lines))        # Get the maximum length of lines
  return(max_length)
}

# Compute the longest line for each file
longest_blogs <- find_longest_line(blogs_file)
longest_news <- find_longest_line(news_file)
longest_twitter <- find_longest_line(twitter_file)

# Find the maximum among all datasets
overall_longest <- max(longest_blogs, longest_news, longest_twitter)

# Print results
cat("Longest line in Blogs:", longest_blogs, "characters\n")
cat("Longest line in News:", longest_news, "characters\n")
cat("Longest line in Twitter:", longest_twitter, "characters\n")
cat("Longest line overall:", overall_longest, "characters\n")

##### love/hate twitter

# Read the file line by line
lines <- readLines("en_US.twitter.txt", warn = FALSE)

# Split lines into words
words <- unlist(strsplit(lines, " "))

# Count love/hate
count <- sum(words == "love")/sum(words == "hate")

# Print the result
cat("The  ratio love/hate appears", count)

######

matching_lines <- grep("biostats", lines, value = TRUE)
print(matching_lines)

######
twitt = "A computer once beat me at chess, but it was no match for me at kickboxing"

count_twittt <- sum(lines == twitt)
cat("The  twitt appears", count_twittt, " times")
