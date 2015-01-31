#' EmoticonScore
#'
#' @description This function classifies a document as "positive", "negative" or "neutral" on the occurances of smile and frown emoticons.
#' 
#' @details The functions searches through the text of given documents and returns +1 if the text contains a smile-emoticon, 
#' -1 if it contains a frown emoticon and 0 if no emoticons were found in the text.
#' 
#' The functions provides default vectors for smile- and frown-emoticons, which can be overwritten with self-defined vectors.
#' 
#' @param doc A character string OR a vector of character strings.
#' @param smile A vector of smile-emoticons.
#' @param frown A vector of frown-emoticons.
#' @export
#' @examples
#' tweet1 <- 'I am totally happy :-)'
#' tweet2 <- 'I feel neutral'
#' tweet3 <- 'I am totally sad =('
#' 
#' tweets <- c(tweet1, tweet2, tweet3)
#' 
#' sapply(tweets, EmoticonScore)
#' I am totally happy :-)         I feel neutral    I am totally sad =( 
#'                      1                      0                     -1


EmoticonScore <- function(doc, smile=smile, frown=frown){
  # Define Standard-Emoticons  ----------------------------------------------
  smile <- c("\\:\\-\\)","\\:\\)","\\=\\)","\\:\\-D","\\:D","8\\-D","8D","x\\-D","xD","X\\-D","XD")
  frown <- c("\\:\\-\\(", "\\:\\(", "\\=\\(", "\\;\\(", "\\:\\-\\|","\\:\\|","\\:\\'\\-\\(")
  #  ------------------------------------------------------------------------
  
  #  ------------------------------------------------------------------------
  score <- numeric(length(doc))
  
  if(grepl(paste(smile, collapse='|'), doc)) {score <- 1}
  else if(grepl(paste(frown, collapse='|'), doc)) {score <- -1}
  #  ------------------------------------------------------------------------
  
  return(score)
}