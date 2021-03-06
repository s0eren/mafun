% Generated by roxygen2 (4.0.2): do not edit by hand
\name{isTrueMention}
\alias{isTrueMention}
\title{isTrueMention}
\usage{
isTrueMention(tweet)
}
\arguments{
\item{tweet}{A character string OR a vector of character strings.}
}
\description{
This function checks whether a specific Tweet-Input contains a true @-mention.
}
\details{
In Twitter, a reference to another Twitter-User is usually done by adding an @ followed by the user's account name. Tweets containing an @ are called '@-mention'.

A true @-mention is declared as a message that starts with an @-sign. It will only appear on the other user's timeline and not to the followers.

The function will search through the input string and return TRUE if an @ was found as first character.
}
\examples{
tweet1 <- 'RT @aSuperTwitterUser I am awesome!'
tweet2 <- 'I am awesome!'
tweet3 <- '@aSuperTwitterUser You are indeed awesome!'

isTrueMention(tweet1)
[1] FALSE

isTrueMention(tweet2)
[1] FALSE

isTrueMention(tweet3)
[1] TRUE
}

