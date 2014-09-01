## Code adapted from:
## https://www.youtube.com/watch?v=j1V2McKbkLo

################################################################################
# Load libraries
################################################################################
libs <- c("class",
          "plyr",
          "SnowballC", # Required for stemming
          "tm")
lapply(libs, require, character.only=TRUE)

################################################################################
# Setting global options
################################################################################
options(stringAsFactors = FALSE)

################################################################################
# Configuration options
################################################################################
authors <- c("kunalj", "tavish")
blog.url <- "http://www.analyticsvidhya.com/blog/author/"
limit.date <- "2014-07-07"
post.storage <- "../data"
post.cache <- "../cache"

################################################################################
# Helper functions
################################################################################
# Load data
loadPosts <- function(post.cache) {
    posts <- read.csv(paste(post.cache, "posts.csv", sep="/"))
    posts$title <- as.character(posts$title)
    posts$week.day <- as.integer(posts$week.day)
    posts$date <- as.POSIXct(posts$date)
    posts$content <- as.character(posts$content)
    return(posts)
}

loadTags <- function(post.cache) {
    tags <- read.csv(paste(post.cache, "tags.csv", sep="/"))
    tags$title <- as.character(tags$title)
    return(tags)
}

# Custom cleaning functions
convertPrettyApostrophe <- function(x) gsub("’", "'", x)
separateSlashWords <- function(x) gsub("/", " ", x)
separateEmail <- function(x) gsub("@", " ", x)

# Function to clean Corpus text
cleanCorpus <- function(corpus, language = "english",
                        custom.functions = c(), custom.stopwords = c()) {
    
    # Apply Text Mining Clean-up Functions

    corpus.tmp <- corpus
    print(inherits(corpus.tmp, "TextDocument"))
    
    for (f in custom.functions) {
        corpus.tmp <- tm_map(corpus.tmp, f)
    }
    corpus.tmp <- tm_map(corpus.tmp, tolower)
    corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
    corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
    corpus.tmp <- tm_map(corpus.tmp, removeWords, c(stopwords(language),
                                                    custom.stopwords))
    corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
    # This line seems to require SnowballC package installed in the system
    # It removes commond word endings, such as "es", "ed" and "'s" for english
    corpus.tmp <- tm_map(corpus.tmp, stemDocument, language = language)
    # This line converts the corpus to TextDocument, required for tm >= v.0.6
    # http://stackoverflow.com/a/24206825
    corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
    print(inherits(corpus.tmp, "TextDocument"))
    
    return(corpus.tmp)
}

# Function to generate term document matrices
generateTDM <- function(text.collection, language = "english", sparse = 0.7) {
    # Instantiate Corpus
    text.corpus <- Corpus(VectorSource(text.collection))
    
    # Clean corpus
    text.corpus.clean <- cleanCorpus(text.corpus, language,
                                     custom.functions=c(convertPrettyApostrophe,
                                                        separateSlashWords,
                                                        separateEmail))
    
    # Create term document matrix
    text.tdm <- TermDocumentMatrix(text.corpus.clean)
    
    # Remove sparse terms: terms that do not add much value to the analysis
    # Set a reasonable level of sparseness
    text.tdm <- removeSparseTerms(text.tdm, sparse)
    
    return(text.tdm)
}

# Convert the Term Document Matrices to a data frame
convertTDM <- function(tdm) {
    # Convert the TDM into numeric data matrix and transpose to have each
    # text as row and term as column
    text.mat <- t(data.matrix(tdm))
    text.df <- as.data.frame(text.mat,
                             stringsAsfactors = FALSE,
                             row.names = as.character(1:dim(text.mat)[1]))
    return(text.df)
}

################################################################################
# Main program
################################################################################

# Load data
posts <- loadPosts(post.cache)
tags <- loadTags(post.cache)

# Generage term document matrix function on all texts
tdm <- generateTDM(posts$content)

# Convert term document matrix to data frame and add it to posts data frame
tdm.stack <- convertTDM(tdm)
tdm.stack <- cbind(posts[c("author", "date")], tdm.stack)

