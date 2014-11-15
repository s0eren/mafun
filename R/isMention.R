#' isMention
#'
#' @description This function checks whether a specific Tweet-Input contains an @@-mention.
#' 
#' @details In Twitter, a reference to another Twitter-User is usually done by adding an @@ followed by the user's account name. Tweets containing an @@ are called '@@-mention'.
#' 
#' The function will search through the input string and return TRUE if any @@ was found.
#' @param tweet A character string OR a vector of character strings.
#' @export
#' @examples
#' tweet1 <- 'RT @@aSuperTwitterUser I am awesome!'
#' tweet2 <- 'I am awesome!'
#' 
#' isMention(tweet1)
#' [1] TRUE
#' 
#' isMention(tweet2)
#' [1] FALSE

isMention <- function(tweet) {
  tweet <- tolower(tweet)
  isMention <- grepl('@',tweet)
  return(isMention) 
}

