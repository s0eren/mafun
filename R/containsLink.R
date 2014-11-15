#' containsLink
#'
#' @description This function checks whether a specific Tweet-Input contains reference to another webpage.
#' 
#' @details Due to the shortness of Tweets, user often share a link to another webpage. This function will check whether a link is included in the text.
#' 
#' The function will search through the input string and return TRUE if 'http' or 'https' was found.
#' @param tweet A character string OR a vector of character strings.
#' @export
#' @examples
#' tweet1 <- 'RT @@aSuperTwitterUser I am awesome!'
#' tweet2 <- 'This looks interessting: http://t.co/06455gbz7'
#' 
#' containsLink(tweet1)
#' [1] FALSE
#' 
#' containsLink(tweet2)
#' [1] TRUE

containsLink <- function(tweet) {
  tweet <- tolower(tweet)
  linkID <- c('http','https')
  containsLink <- grepl(paste(linkID, collapse = "|"),tweet)
  return(containsLink) 
}




