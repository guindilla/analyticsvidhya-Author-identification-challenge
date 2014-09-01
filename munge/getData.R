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
post.storage <- "../data"
post.cache <- "../cache"

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
    # Early titles had the " | Analytics Vidhya" string, let's remove it
    title <- gsub("( \\| Analytics Vidhya)$", "", title)
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

################################################################################
# Main program
################################################################################

# Prepare data frames where information will be stored
posts <- data.frame(title=character(),
                    author=factor(),
                    date=numeric(),
                    week.day=numeric(),
                    content=character())
tags <- data.frame(title=character(),
                   author=factor(),
                   tags=character(),
                   stringsAsFactors = FALSE)

for (a in authors) {
    # Get URLs of posts for the author
    urls <- scrape.urls(blog.url, a)
    
    # Create author folders
    a.path <- sprintf("%s/%s", post.storage, a)
    if (!file.exists(a.path)) {
        dir.create(a.path)
    }
    
    for (u in urls) {
        # Download each file
        my.content <- content(GET(u), as="text")
        
        # Scrap the file and store it as an html file
        post.scraped <- scrape.post(my.content)
        f.path <- sprintf("%s/%s-%s.html",
                          a.path,
                          format(post.scraped$date, format="%Y%m%d"),
                          # Removing forbiden characters in windows path
                          gsub(pattern = "(\\\\|/|:|*|\\?|<|>|\\|)",
                               replacement = "",
                               x = post.scraped$title))
        writeLines(my.content, f.path)
        
        # Saving the scrapped post to a dataframe
        posts <- rbind(posts, data.frame(title=post.scraped$title,
                                         author=post.scraped$author,
                                         date=post.scraped$date,
                                         week.day=post.scraped$week.day,
                                         content=post.scraped$content))
        tags <- rbind(tags, data.frame(title=rep(post.scraped$title,
                                                 length(post.scraped$tags)),
                                       author=rep(post.scraped$author,
                                                  length(post.scraped$tags)),
                                       tags=post.scraped$tags))
    }
}

# Required type adjustements on the data frames
posts$title <- as.character(posts$title)
posts$week.day <- as.integer(posts$week.day)
posts$date <- as.POSIXct(as.integer(posts$date), origin = "1970-01-01")
posts$content <- as.character(posts$content)

tags$title <- as.character(tags$title)

# Save data frames into csv files
write.csv(posts, paste(post.cache, "posts.csv", sep="/"), row.names=FALSE)
write.csv(tags, paste(post.cache, "tags.csv", sep="/"), row.names=FALSE)