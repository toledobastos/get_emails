get_emails_webpage <- function(urls, return.df=F, email.href=F) {
    # scrap emails from webpages
    library(XML)
    library(stringr)
    library(RCurl)
    # define email pattern
    emailpattern <- '([a-zA-Z0-9._-]*-)?[[:alnum:]\\-_.%+]+@[[:alnum:]\\-_.%+]+\\.[[:alpha:]]+'
    emails <- list()
    # download cert if required
    if(.Platform$OS.type == "windows") { if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm") }
    # download webpages and extract emails
    for(i in 1:length(urls)) {
        if(email.href==F){
            web.temp <- try(gsub("mailto:(.*?)@", "", getURL(urls[i], cainfo = "cacert.perm")))
            emails[i] <- try(str_extract_all(web.temp, emailpattern))
            cat(".")
        } else {
            web.temp <- try(as.character(xpathSApply(htmlParse(urls[i]), "//a/@href")))
            emails[[i]] <- try(as.list(gsub("mailto:", "", web.temp[grep("mailto:", web.temp)])))
            cat(".")
        }
    }
    if(return.df==T) {
        # create dataframe with aggregate results
        max.length <- max(unlist(lapply(emails, length)))
        emails.df <- do.call(rbind.data.frame, lapply(emails, function(v) { c(v, rep(NA, max.length-length(v)))}))
        colnames(emails.df) <- paste0("Email_", seq(1:max.length))
        results <- cbind(data.frame(URL=as.character(urls)), emails.df)
        return(results)
        } else {
            return(unlist(emails))
        }
}
