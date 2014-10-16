get_links <- function(webpage) {
    library(XML)
    doc <- htmlParse(webpage)
    links.temp <- as.character(xpathSApply(doc, "//a/@href"))
    web.urls <- links.temp[grep("http(s)?://", links.temp)]
    free(doc)
    return(web.urls)
}
