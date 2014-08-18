library(httr)

authors <- c("kunalj", "tavish")
url <- "http://www.analyticsvidhya.com/blog/author/"

get.author.urls <- function(url, author) {
    # Where to find all articles?
    my.url <- paste0(url, author)
    links <- vector()
    output <- data.frame()
    repeat{
        content <- content(GET(my.url), as="text")
        parsedHTML <- htmlParse(content, asText=TRUE)
        links <- xpathSApply(parsedHTML, "//h2[@class='post-title entry-title']/a", xmlGetAttr, 'href')
        ## Next two lines do the same as the line above, kept for reference only
        # links <- xpathSApply(parsedHTML, "//a[@rel='bookmark']/@href")
        # links <- as.vector(links)
        dates <- xpathSApply(parsedHTML, "//span[@class='value-title']", xmlGetAttr, 'title')
        output <- rbind(output, cbind(dates, links))
        my.url <- xpathSApply(parsedHTML, "//p[@class='previous']/a", xmlGetAttr, 'href')
        if(length(my.url) == 0) {
            break
        }
    }
    output$links <- as.character(output$links)
    output$dates <- as.Date(output$dates)
    return(output)
}
# List: An ordered collection of objects (components). A list allows you to
# gather a variety of (possibly unrelated) objects under one name.
# Use lists here.
kunal.urls <- get.author.urls(url, "kunalj")
limit.date <- "2014-07-07"
kunal.urls.train <- kunal.urls[kunal.urls$dates < limit.date, ]
kunal.urls.test <- kunal.urls[kunal.urls$dates >= limit.date, ]


