#' EmoticonScore
#'
#' @description This function classifies a document as "positive", "negative" or "neutral" on the occurances of smile and frown emoticons.
#' 
#' @details The functions searches through the text of given documents and returns +1 if the text contains a smile-emoticon, 
#' -1 if it contains a frown emoticon and 0 if no emoticons were found in the text.
#' 
#' The functions provides default vectors for smile- and frown-emoticons, inspired by the Read (2005), which can be overwritten with self-defined vectors.
#' To see the vectors, type "EmoticonScore" (without brackets) into the console and press Enter.
#' 
#' @seealso Read, Jonathon, ed. (2005): Using Emoticons to reduce Dependency in Machine Learning Techniques for Sentiment Classification. ACL Student Research Workshop. (2005). Association for Computational Linguistics. Ann Arbor.
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
  smile <- c(" \\:\\-\\) "," \\:\\) "," \\=\\) "," \\:\\-D "," \\:D "," 8\\-D "," 8D "," x\\-D "," xD "," X\\-D "," XD ")
  frown <- c(" \\:\\-\\( ", " \\:\\( ", " \\=\\( ", " \\;\\( ", " \\:\\-\\| "," \\:\\| "," \\:\\'\\-\\( ")
  #  ------------------------------------------------------------------------
  
  #  ------------------------------------------------------------------------
  score <- numeric(length(doc))
  
  if(grepl(paste(smile, collapse='|'), doc)) {score <- 1}
  else if(grepl(paste(frown, collapse='|'), doc)) {score <- -1}
  #  ------------------------------------------------------------------------
  
  return(score)
}