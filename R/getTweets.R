#' getTweets
#'
#' @description The function will read a directory of JSON-Files containing Output of Twitter's Streaming API and transform them to a \code{data.frame} in R. If multiple files are handed over to the function it allows to combine them to a single \code{data.frame}.
#' 
#' Please keep in mind that the function is mainly designed to work with Tweets containing German parties.
#' 
#' @details 
#' Default variables to be kept: \code{created_at, id_str, text, source, in_reply_to_status_id_str, in_reply_to_user_id_str, retweet_count, favorite_count, retweeted, lang,
#' timestamp_ms, user.id_str, user.screen_name, user.followers_count, user.friends_count, user.listed_count, user.favourites_count, user.statuses_count,
#' user.time_zone, geo.type, geo.coordinates, coordinates.type, coordinates.coordinates, retweeted_status.id_str, entities.hashtags, entities.trends,
#' entities.urls, entities.user_mentions, entities.symbols, entities.media, extended_entities.media}.
#' A full list of all available variable can be found on \href{https://dev.twitter.com/overview/api/tweets}{dev.twitter.com}.
#' 
#' If \code{restrictToPartyTweets} is set to \code{TRUE} variables checking the mentioning of the following parties will be created: CDU, CSU, SPD, Linke, Gruene, AfD, FDP. 
#' Additionaaly all Tweets not mentioning any of the parties will be deleted.
#' 
#' @param dir Link to a folder, where JSON-Files are stored. Default is current working directory.
#' @param VarsToKeep A vector of variable names that should be kept. For default variables see: Details.
#' @param restrictToPartyTweets If \code{TRUE}, only Tweets mentioning a German Party will be kept. Default is \code{TRUE}. For a list of the respective parties see: Details.
#' @param restrictToLang Setting this value to a 2-digit country ISO-Code will ensure that only Tweets in this language will be kept.
#' @param wordlistPositive Link to file containing the positive wordliste the Tweets will be compared with. 
#' @param wordlistNegative Link to file containing the negative wordliste the Tweets will be compared with. 
#' @param combine If \code{TRUE}, individual data.frames containing Tweets will be combined to one. Default is \code{TRUE}
#' @export
#' @examples 
#' getTweets <- function(dir='C:/user/tweets', 
#'                      varsToKeep='text', 
#'                      wordlistPositive='C:/user/dictionary/pos.txt', 
#'                      wordlistNegative='C:/user/dictionary/neg.txt', 
#'                      combine=FALSE)
#' 
#' @import jsonlite
#' @import plyr
#' @import stringr



getTweets <- function(dir=getwd(), varsToKeep=defaultVars, restrictToPartyTweets=TRUE, restrictToLang='de', wordlistPositive, wordlistNegative, combine=TRUE){
  # Install and Load Packages -----------------------------------------------
  # install packages if necessary
  if(!require('jsonlite')) install.packages('jsonlite')
  if(!require('plyr')) install.packages('plyr')
  if(!require('stringr')) install.packages('stringr')
  
  # Load Packages
  require(jsonlite)
  require(plyr)
  require(stringr)
  
  
  # Create Folder for Output ------------------------------------------------
  dir.create(file.path(dir,'output'), recursive=T, showWarnings = FALSE)
  #  ------------------------------------------------------------------------
             
  # Define Standard-Variables to Keep ---------------------------------------
  defaultVars <- c('created_at',
                 'id_str',
                 'text',
                 'source',
                 'in_reply_to_status_id_str',
                 'in_reply_to_user_id_str',
                 'retweet_count',
                 'favorite_count',
                 'retweeted',
                 'lang',
                 'timestamp_ms',
                 'user.id_str',
                 'user.screen_name',
                 'user.followers_count',
                 'user.friends_count',
                 'user.listed_count',
                 'user.favourites_count',
                 'user.statuses_count',
                 'user.time_zone',
                 'geo.type',
                 'geo.coordinates',
                 'coordinates.type',
                 'coordinates.coordinates',
                 'retweeted_status.id_str',
                 'entities.hashtags',
                 'entities.trends',
                 'entities.urls',
                 'entities.user_mentions',
                 'entities.symbols',
                 'entities.media',
                 'extended_entities.media')
  #  ------------------------------------------------------------------------
  
  # Read Positive and Negative Wordlist -------------------------------------
  wordlist_positive <- scan(wordlistPositive, what='character', quiet=T)
  wordlist_negative <- scan(wordlistNegative, what='character', quiet=T)
  #  ------------------------------------------------------------------------
  
  # Match Positive ----------------------------------------------------------
  matchPositive <- function(x) return(sum(!is.na(match(unlist(str_split(x, pattern='\\s+')), wordlist_positive))))
  #  ------------------------------------------------------------------------
  
  # Match Negative ----------------------------------------------------------
  matchNegative <- function(x) return(sum(!is.na(match(unlist(str_split(x, pattern='\\s+')), wordlist_negative))))
  #  ------------------------------------------------------------------------
  
  # Get Input-Files ---------------------------------------------------------
  inputFiles <- list.files(path=dir, pattern='.txt')
  #  ------------------------------------------------------------------------
  
  # Read Data from each File ------------------------------------------------
  for(i in 1:length(inputFiles)){
    
    # Read Data -------------------------------------------------------------
    Tweets <- fromJSON(file.path(dir,inputFiles[i]), flatten=T, unicode=F)
    #  ----------------------------------------------------------------------
    
    # Delete Empty Rows -----------------------------------------------------
    Tweets <- Tweets[is.na(Tweets$created_at)!=TRUE,]
    #  ----------------------------------------------------------------------
    
    # Restrict to specific language -----------------------------------------
    Tweets <- Tweets[Tweets$lang==restrictToLang,]
    #  ----------------------------------------------------------------------
    
    # Remove unnessecary columns --------------------------------------------
    Tweets <- subset(Tweets, select=(varsToKeep))
    #  ----------------------------------------------------------------------
    
    # Backup Original Tweet -------------------------------------------------
    if(any(varsToKeep=='text')) Tweets$text_org <- Tweets$text
    #  ----------------------------------------------------------------------
    
    # Transform Text --------------------------------------------------------
    # Change Text to-lower
    Tweets$text <- tolower(Tweets$text_org)
    
    # Remove Punctuation
    Tweets$text  <- gsub('[[:punct:]]', ' ', Tweets$text)
    
    # Remove Control Characters
    Tweets$text  <- gsub('[[:cntrl:]]', ' ', Tweets$text)
    
    # Remove Digits
    Tweets$text  <- gsub('\\d+', ' ', Tweets$text)
    #  ----------------------------------------------------------------------
    
    # Restrict to Party-Tweets ----------------------------------------------
    if(restrictToPartyTweets){
      # Create Variables for Party-Mentions
      Tweets$cdu    <- grepl('cdu',Tweets$text)
      Tweets$csu    <- grepl('csu',Tweets$text)
      Tweets$union  <- as.logical(Tweets$cdu + Tweets$csu)
      Tweets$spd    <- grepl('spd',Tweets$text)
      Tweets$linke  <- grepl('linke',Tweets$text)
      Tweets$gruene <- grepl('gruene',Tweets$text)
      Tweets$afd    <- grepl('afd',Tweets$text)
      Tweets$fdp    <- grepl('fdp',Tweets$text)
      
      # Party-Columns
      partyColumns <- c('union','spd','linke','gruene','afd','fdp')
      
      # Delete Tweets without party-mentions
      Tweets <- Tweets[apply(Tweets[,partyColumns],1,sum)>0,]
    }
    #  ----------------------------------------------------------------------
    
    # Positive and Negative Matches -----------------------------------------
    Tweets$positive <- sapply(Tweets$text, matchPositive)
    Tweets$negative <- sapply(Tweets$text, matchNegative)
    #  ----------------------------------------------------------------------
    
    # Create Score ----------------------------------------------------------
    Tweets$score  <-  Tweets$positive - Tweets$negative
    #  ----------------------------------------------------------------------
    
    # Save Tweets as R-Object -----------------------------------------------
    save(Tweets, file=paste(dir, '/output/',inputFiles[i],'.R', sep=''))
    #  ----------------------------------------------------------------------
    
    # Generate Return-Output ------------------------------------------------
    # Number of Tweets per Party
    if(restrictToPartyTweets) returnParties <- apply(Tweets[,partyColumns],2,sum)
    
    # Proportion of Non-Zero-Scored-Tweets
    returnNon0 <- length(which(Tweets$score!=0)) / nrow(Tweets) * 100
    
    # OUTPUT
    cat('\n \n', rep('=', 75), sep='')
    cat('\n',nrow(Tweets), 'Tweets have been processed and stored :) \n')
    cat('\n Find Tweets here:', paste(dir,'output/',inputFiles[i],'.R', sep=''))
    cat('\n \n')
    
    if(restrictToPartyTweets) cat('\n Tweets per Party:\n'); print(returnParties)
    
    cat('\n Proportion of Non-Zero-Scored-Tweets: \n', returnNon0, '%')
    
    cat('\n \n', rep('=', 75), sep='')
    #  ----------------------------------------------------------------------
    
  }  
  #  ------------------------------------------------------------------------ 
  
  # Combine individual Files to one data.frame ------------------------------
  outputFiles <- list.files(path=paste(dir,'/output',sep=''), pattern='.R')
  
  TweetsCombine <- lapply(outputFiles, function(x) get(load(paste(dir,'/output/',x,sep=''))))
  Tweets <- do.call(rbind, TweetsCombine)
  save(Tweets, file=paste(dir, '/output/TweetsCombined.R', sep=''))
}
  





  












