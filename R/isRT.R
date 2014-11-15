#' isRT
#'
#' @description This function checks whether a specific Tweet-Input can be categorized as Retweet.
#' 
#' @details Retweets in Twitter are indicated by starting with: 'RT'.
#' 
#' To ensure proper working the input string is transformed to lower case and the function checks whether the first to elements of the string are 'rt'
#' @param tweet A character string OR a vector of character strings.
#' @export
#' @examples
#' tweet1 <- 'RT @@aSuperTwitterUser I am awesome!'
#' tweet2 <- 'I am awesome!'
#' 
#' isRT(tweet1)
#' [1] TRUE
#' 
#' isRT(tweet2)
#' [1] FALSE

isRT <- function(tweet) {
  tweet <- tolower(tweet)
  isRT <- substr(tweet,1,2) == 'rt'
  return(isRT) 
}