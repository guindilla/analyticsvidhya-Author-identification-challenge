################################################################################
# Load libraries
################################################################################
library(httr)
library(lubridate)
library(XML)

################################################################################
# Configuration options
################################################################################
authors <- c("kunalj", "tavish")
blog.url <- "http://www.analyticsvidhya.com/blog/author/"
limit.date <- "2014-07-07"

################################################################################
# Helper functions
################################################################################
scrape.urls <- function(blog.url, author) {
    # Identify the author page on the Abalytics Vidhya blog and recursively
    # download all the posts authored by said writter.
    #
    # Args:
    #   blog.url: URL of the Abalytics Vidhya blog
    #   author: Name of the author of the posts
    #
    # Returns:
    #   Vector of characters with URLs of all articles
    
    links <- vector()
    
    my.url <- paste0(blog.url, author)
    
    repeat{
        my.content <- content(GET(my.url), as="text")
        parsedHTML <- htmlParse(my.content, asText=TRUE)
        page.links <- xpathSApply(parsedHTML,
                                  "//h2[@class='post-title entry-title']/a",
                                  xmlGetAttr, 'href')
        ## Next two lines do the same as the line above, kept for reference only
        # links <- xpathSApply(parsedHTML, "//a[@rel='bookmark']/@href")
        # links <- as.vector(links)
        
        ## This line also identifies the dates of the articles. As this
        # information will be extracted later on with other information (tags,
        # etc.) we keep it here for reference purposes only.
        # dates <- xpathSApply(parsedHTML, "//span[@class='value-title']", xmlGetAttr, 'title')
        
        links <- c(links, page.links)
        
        my.url <- xpathSApply(parsedHTML, "//p[@class='previous']/a", xmlGetAttr, 'href')
        if(length(my.url) == 0) {
            break
        }
    }

    return(links)
}

scrape.post <- function(file) {
    # Extracts from the file passed as an argument the date of article
    # publish, day of article publish, tags of article publish and the content
    # of the article. 
    #
    # Args:
    #   file: HTML code from a post of the Abalytics Vidhya blog 
    #
    # Returns:
    #   A list with the characteristics of the post
    
    parsedHTML <- htmlParse(file, asText=TRUE)

    title <- xpathSApply(parsedHTML,
                         "//title",
                         xmlValue)
    author <- xpathSApply(parsedHTML,
                          "//span[@class='fn nickname']",
                          xmlValue)
    date <- xpathSApply(parsedHTML,
                        "//span[@class='value-title']",
                        xmlGetAttr, 'title')
    date <- ymd_hm(date)
    week.day <- wday(date)
    content <- xpathSApply(parsedHTML,
                           "//div[@class='entry-content clearfix']",
                           xmlValue)
    tags <- xpathSApply(parsedHTML,
                        "//p[@class='post-tags']/a",
                        xmlValue)
    list(title = title,
         author = author,
         date = date,
         week.day = week.day,
         content = content,
         tags = tags)
}

download.urls <- function(urls, directory){
    # Downloads and stores a vector of urls on a specific location.
    #
    # Args:
    #   urls: Vector of urls to download
    #   directory: Folder where the downloaded url's will be stored
    #
    # Returns:
    #   Nothing
    
    urls <- sort(urls)
    i <- 0
    for (u in urls) {
        i <- i + 1
        download.file(u, paste0(directory, "/", sprintf("%.3d", i), ".html"), "auto")
    }
}

################################################################################
# Main program
################################################################################
urls <- vector()
for(a in authors) {
    urls <- c(urls, scrape.urls(blog.url, a))
}

download.urls(urls, "../data/")

my.content <- paste(readLines("1.txt"), sep="\n")
scrape.post(my.content)
