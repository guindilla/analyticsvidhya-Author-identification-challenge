################################################################################
# Load libraries
################################################################################
library(httr)
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
        content <- content(GET(my.url), as="text")
        parsedHTML <- htmlParse(content, asText=TRUE)
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
    typeof(links)
    return(links)
}

################################################################################
# Main program
################################################################################
urls <- vector()
for(a in authors) {
    urls <- c(urls, scrape.urls(blog.url, a))
}