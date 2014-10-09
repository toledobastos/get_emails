get_emails_text <- function(text, return.df=F) {
    # scrap emails from text
    library(stringr)
    emailpattern <- '([a-zA-Z0-9._-]*-)?[[:alnum:]\\-_.%+]+@[[:alnum:]\\-_.%+]+\\.[[:alpha:]]+'
    emails <- list()
    for(i in 1:length(text)) {
    emails[i] <- try(str_extract_all(text[i], emailpattern))
    cat(".")
    }
    return(unlist(emails))
}